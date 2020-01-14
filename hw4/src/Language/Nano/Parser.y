{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators Precedence Order
%left '||'      -- LOWEST
%left '&&'
%nonassoc '==' '/=' '<' '<='
%right ':'
%left '-' '+'
%left '*'
%left ID let in -- HIGHEST
%%

-- Top level functions: let, lambda, elif
Top  : ID '=' Top                { $3 }                      -- Simple assignment
     | let ID '=' Top in Top     { ELet $2 $4 $6 }           -- Let w/ 1 ID
     | let ID ManyI '=' Top in Top { ELet $2 (mkLam $3 $5) $7 } -- Let w/ n iD's
     | '\\' ID '->' Top          { ELam $2 $4 }              -- Lamda
     | if Top then Top else Top  { EIf $2 $4 $6 }            -- Elif
     | Bin                       { $1 }                      -- Skip to next level

-- Binary level functions: math & boolean comparison
Bin  : Bin '+' Bin               { EBin Plus $1 $3 }  -- Plus (+)
     | Bin '-' Bin               { EBin Minus $1 $3 } -- Minus (-)
     | Bin '*' Bin               { EBin Mul $1 $3 }   -- Multiply (*)
     | Bin '<' Bin               { EBin Lt $1 $3 }    -- Less than (<)
     | Bin '<=' Bin              { EBin Le $1 $3 }    -- Less equal (<=)
     | Bin '==' Bin              { EBin Eq $1 $3 }    -- Equal (==)
     | Bin '/=' Bin              { EBin Ne $1 $3 }    -- Not Equal (/=)
     | Bin '&&' Bin              { EBin And $1 $3 }   -- And (&&)
     | Bin '||' Bin              { EBin Or $1 $3 }    -- Or (||)
     | Bin ':' Bin               { EBin Cons $1 $3 }  -- Cons (:)
     | App                       { $1 } -- Skip to next lvel

-- Apply level functions: Apply func2 to func
App  : App Base                  { EApp $1 $2} -- f x (apply "x" as an arg to "f")
     | Base                      { $1 }        -- Skip to next level

-- Base level functions: aka primatives - Value, ID, true/false, and parenthesis
Base : TNUM                      { EInt $1 }     -- Int (value)
     | ID                        { EVar $1 }     -- ID (variable)
     | true                      { EBool True }  -- True
     | false                     { EBool False } -- False
     | '[' ']'                   { ENil }        -- Empty list (Nil)
     | '[' ManyC ']'             { $2 }          -- NONempty list
     | '(' Top ')'               { $2 }          -- Jump back to top to allow "infinite" level depth functions

-- Many IDs: match multiple ids and convert into list
ManyI : ID ManyI                 { $1:$2 } -- cons element to list
      | ID                       { [$1] }  -- create list

-- Many Commas: allow a list of n elements to be comma seperated
ManyC : Top ',' ManyC            { EBin Cons $1 $3 }   -- More to the list; return value:(recur until no more)
      | Top                      { EBin Cons $1 ENil } -- End of list; return with value:nil

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
