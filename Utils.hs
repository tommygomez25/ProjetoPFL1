module Utils where

import Data.List (sortBy)
import Data.Char
import Data.Function (on)

import Types

printVars :: [(Char, Int)] -> String
printVars [] = ""
printVars (x:xs) | (snd x) == 0 = ""
                 | (snd x) == 1 = "*" ++ [fst x]
                 | otherwise  = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ printVars xs

{-
printMono :: Mono -> String
printMono m | coef m == (fromInteger (round (coef m))) = " " ++ init(init(s)) ++ printVars (vars m) ++ " "
            | otherwise = " " ++ s ++ printVars (vars m) ++ " "
              where s = show (abs (coef m))
-} -- dont need to worry about floats anymore

printMono :: Mono -> String
printMono m = " " ++ s ++ printVars (vars m) ++ " "
              where s = show (abs (coef m))

printPoly :: Poly -> String
printPoly [] = ""
printPoly (x:xs) = if((coef x) > 0) then( "+" ++ printMono x ++ printPoly xs) else ("-" ++ printMono x ++ printPoly xs)

clearPrint :: String -> String
clearPrint s = tail (tail (init s))

-- Auxiliar function to check if a monomial has the variable 'c'
containsVar :: Char -> [(Char, Int)] -> Bool
containsVar c l = or [fst a == c | a <- l]

-- Auxiliar function to remove monomials with null coeficient
removeNulls :: Poly -> Poly
removeNulls x = [a | a <- x, coef a /= 0]

-- Auxiliar function to sort the polynomial terms in ascending degree and variable
sortSum :: Mono -> Mono -> Ordering
sortSum x y
  | vars x < vars y = LT
  | vars x > vars y = GT
  | vars x == vars y = compare (coef x) (coef y)

sortToSum = sortBy sortSum

{-
sortNormalize :: Mono -> Mono -> Ordering
sortNormalize x y
  | snd (vars x) < snd (vars y) = GT
  | snd (vars x) > snd (vars y) = LT
  | snd (vars x) == snd (vars y) = compare (vars x) (vars y)
-}

--sortToNormalize m = sortBy  (compare `on` ( length . snd)) (vars m)
