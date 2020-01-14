{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = readFile f >>= typeOfString

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseExpr s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars t     = case t of
    (TInt) -> []
    (TBool) -> []
    (t1 :=> t2) -> L.nub ((freeTVars t1)++(freeTVars t2))
    (TVar id) -> [id]
    (TList l) -> freeTVars l

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars s     = case s of
    (Mono t) -> freeTVars t
    (Forall id p) -> L.filter f l
                              where
                                f = (\x -> (x /= id))
                                l = freeTVars p

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Lookup a variable in the type environment  
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar a [] = (TVar a)
lookupTVar a ((y,s):sub)
  | a == y    = s
  | otherwise = lookupTVar a sub

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar a sub = L.filter f sub
  where
    f = (\(x,y) -> (x /= a)) -- Filter matching vars
     
-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where  
  apply sub t = case t of
    (TInt) -> TInt
    (TBool) -> TBool
    (t1 :=> t2) -> ((apply sub t1) :=> (apply sub t2))
    (TVar id) -> lookupTVar id sub
    (TList l) -> list (apply sub l)


-- | Apply substitution to poly-type
instance Substitutable Poly where    
  apply sub s = case s of
    (Mono t) -> Mono (apply sub t)
    (Forall t p) -> Forall t $ mono
                      where
                        sub' = removeTVar t sub
                        mono = apply sub' p


-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = (a,t):sub'
                        where
                          sub' = apply ((a,t):sub) sub
      
--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving Show

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar $ "a" ++ show n      
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n

extendStateR :: InferState -> InferState -> InferState
extendStateR s1 s2 = L.foldl f base args
                          where
                            f a x = extendState a (fst x) (snd x)
                            base = s1
                            args = stSub s2

-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t
  | (TVar a) == t          = st -- Return unchanged state
  | L.elem a (freeTVars t) = throw(Error ("type error: cannot unify " ++ (show a) ++ " and " ++ (show t) ++ " (occurs check)")) -- Throw error as cannot unify
  | otherwise              = extendState st a t -- Extend state
  
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st t1 t2 = case (t1, t2) of
  (TInt, TInt)         -> st               -- unchanged
  (TBool, TBool)       -> st               -- unchanged
  (TVar x, t)          -> unifyTVar st x t -- unifyTVar
  (t, TVar x)          -> unifyTVar st x t -- unifyTVar
  (TList l1, TList l2) -> unify st l1 l2   -- breakup list
  (a :=> b, c :=> d)   -> unify (unify st a c) b d -- unify b+d with the state of a+c
  (a, b)               -> throw(Error ("type error: cannot unify " ++ (show a) ++ " and " ++ (show b))) -- catch errors

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = (st, typ)
  where
    cnt      = stCnt st
    sub      = stSub st
    poly     = Mono (lookupTVar x sub)
    (_, typ) = instantiate cnt poly

infer st gamma (ELam x e) = (st', tX' :=> tBody)
  where
    (TVar fresh) = freshTV (stCnt st)
    gamma'       = extendTypeEnv x (Mono (TVar fresh)) gamma
    (st', tBody) = infer st gamma' e
    tX'          = apply (stSub st') (TVar fresh)

infer st gamma (EApp e1 e2) = (st', typ')
  where
    (s1, t1)     = infer st gamma e1
    (s2, t2)     = infer s1 gamma e2
    (TVar fresh) = freshTV (stCnt st)
    s3           = InferState (stSub st) ((stCnt st) + 1)
    st'          = unify s3 (t1) (t2 :=> (TVar fresh))
    typ'         = lookupTVar fresh (stSub st')

infer st gamma (ELet x e1 e2)  = infer st' typ' e2
  where
    (st', t1) = infer st gamma e1
    poly = generalize gamma t1
    typ' = extendTypeEnv x poly gamma

infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = L.foldl f base args
                        where
                          f a x = Forall x a
                          base  = Mono t
                          args  = L.nub ((freeTVars t) L.\\ (freeTVars gamma))
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n (Mono t)         = (n, t)
instantiate n (Forall typ pol) = instantiate (n+1) (apply [(typ, (freshTV n))] pol)

      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt :=> TInt :=> TInt)
  , ("-",    Mono $ TInt :=> TInt :=> TInt)
  , ("*",    Mono $ TInt :=> TInt :=> TInt)
  , ("/",    Mono $ TInt :=> TInt :=> TInt) 
  , ("==",   Forall "a" (Mono $ "a" :=> "a" :=> TBool))
  , ("!=",   Forall "a" (Mono $ "a" :=> "a" :=> TBool))
  , ("<",    Mono $ TInt :=> TInt :=> TBool)
  , ("<=",   Mono $ TInt :=> TInt :=> TBool)
  , ("&&",   Mono $ TBool :=> TBool :=> TBool)
  , ("||",   Mono $ TBool :=> TBool :=> TBool)
  , ("if",   Forall "a" (Mono $ TBool :=> "a" :=> "a" :=> "a"))
  -- lists: 
  , ("[]",   Forall "a" (Mono $ (TList "a")))
  , (":",    Forall "a" (Mono $ "a" :=> (TList (TVar "a") ) :=> (TList (TVar "a"))))
  , ("head", Forall "a" (Mono $ (TList "a") :=> "a"))
  , ("tail", Forall "a" (Mono $ (TList "a") :=> "a"))
  ]
