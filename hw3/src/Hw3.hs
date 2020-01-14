{- | CSE 130: All about fold.

     For this assignment, you may use the following library functions:

     length
     append (++)
     map
     foldl'
     foldr
     unzip
     zip
     reverse

  Use www.haskell.org/hoogle to learn more about the above.

  Do not change the skeleton code! The point of this assignment
  is to figure out how the functions can be written this way
  (using fold). You may only replace the `error "TBD:..."` terms.

-}

module Hw3 where

import Prelude hiding (replicate, sum)
import Data.List (foldl')

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'

--------------------------------------------------------------------------------
-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f a x = a + (x * x) -- Add squred num to previous value
   base  = 0           -- "starting" values

--------------------------------------------------------------------------------
-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24

pipe :: [(a -> a)] -> (a -> a)
pipe fs   = foldLeft f base fs
  where
    f a x = a.x -- Apply x to a
    base  = id  -- Base: Identity function returns input unchanged

--------------------------------------------------------------------------------
-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep []     = ""
sepConcat sep (x:xs) = foldLeft f base l
  where
    f a x            = a ++ sep ++ x -- Add base value, seperator, then value
    base             = "" ++ x       -- Make base case the head ("" ++ is need to force string type)
    l                = xs            -- Make new list the tail

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------
-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ str ++ "]"   -- Wrap result in []
  where str = sepConcat ", " (map f xs) -- print array from map (map applies func "f" to list xs)

--------------------------------------------------------------------------------
-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
clone x n | n == 0    = []                   -- Base case: return empty array
          | otherwise = [x] ++ clone x (n-1) -- Recur: Add ele and recurr

type BigInt = [Int]

--------------------------------------------------------------------------------
-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- [0,0,9,9] [1,0,0,2]
--
-- >>> padZero [1,0,0,2] [9,9]
-- [1,0,0,2] [0,0,9,9]

padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2 
  | len1 == len2 = (l1, l2)                   -- Return pair unchanged
  | len1 > len2  = (l1, (clone 0 diff) ++ l2) -- l1 is bigger, pad l2
  | len1 < len2  = ((clone 0 diff) ++ l1, l2) -- l2 is bigger, pad l1
    where
      len1 = length l1         -- len of list 1
      len2 = length l2         -- len of list 2
      diff = abs (len1 - len2) -- find diff in length

--------------------------------------------------------------------------------
-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero [] = []
removeZero (d:ds)
  | d /= 0    = d:ds          -- Base: return remaining b/c N was found
  | otherwise = removeZero ds -- Recurse on tail


--------------------------------------------------------------------------------
-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

-- Helper
front :: [a] -> a -- Returns head of list
front [x] = x
front (x:xs) = x

body :: [a] -> [a] -- Returns body/tail of list
body [] = []
body (x:xs) = xs

bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2     = removeZero res
  where
    (l1', l2')   = padZero l1 l2
    res          = foldLeft f base args
    f a x        =    [((fst x) + (snd x) + front a) `div` 10] -- Carry is stored here: Add digits in a pair, add the carry, and only take the FIRST digit
                   ++ [((fst x) + (snd x) + front a) `mod` 10] -- Add digits in a pair, add the carry, and only take the LAST digit
                   ++ (body a)                                 -- Dont add the head (carry) to the list
    base         = [0]                                         -- Base sum (also carry) is 0
    args         = reverse(zip l1' l2')                        -- Pair up digits using zip, and reverse the order


--------------------------------------------------------------------------------
-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]

mulByDigit :: Int -> BigInt -> BigInt
mulByDigit i n = removeZero res
  where
    res          = foldLeft f base args
    f a x        =    [((x*i) + front a) `div` 10] -- Carry is stored here: Mult digit by value, add the carry, and only take the FIRST digit
                   ++ [((x*i) + front a) `mod` 10] -- Mult digit by value, add the carry, and only take the LAST digit
                   ++ (body a)                     -- Dont add the head (carry) to the list
    base         = [0]                             -- Base sum (also carry) is 0
    args         = reverse n                       -- Reverse order of digits to LSD first

--------------------------------------------------------------------------------
-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldLeft f base args
    f a x    = (fst a ++ [0],                                -- First value in pair keeps track of your zeros
                bigAdd (snd a) ((mulByDigit x l1)++(fst a))) -- Add existing acculm to the product of a row and digit
    base     = ([],[0])                                      -- init the base pair
    args     = reverse l2                                    -- Reverse array to start from LSD
