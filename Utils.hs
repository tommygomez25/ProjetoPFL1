module Utils where

import Data.List (sortBy)
import Data.Char
import Data.Function (on)

import Types

printVars :: [(Char, Int)] -> String
printVars [] = ""
printVars (x:xs) | (snd x) == 0 = "" ++ printVars xs
                 | (snd x) == 1 = "*" ++ [fst x] ++ printVars xs
                 | otherwise  = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ printVars xs

toNegative :: Int -> Int
toNegative n = n * (-1)

printMono :: Mono -> String
printMono m = " " ++ s ++ printVars (vars m) ++ " "
              where s = show (abs (coef m))

printPoly :: Poly -> String
printPoly [] = ""
printPoly (x:xs) = if((coef x) >= 0) then( "+" ++ printMono x ++ printPoly xs) else ("-" ++ printMono x ++ printPoly xs)

clearPrint :: String -> String
clearPrint "" = "0"
clearPrint s | length s > 0 && s !! 0 == '-' = init s
             | otherwise = tail(tail (init s)) --remove first '+'

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

getExpSum :: Mono -> Int
getExpSum m = sum [snd a | a <- vars m]

getVarOrder :: Mono -> Int
getVarOrder m = sum [fromEnum (fst a) | a <- vars m]

sortNormalize :: Mono -> Mono -> Ordering
sortNormalize x y
  | sum1 < sum2 = GT
  | sum1 > sum2 = LT
  | sum1 == sum2 = compare (sumVar1) (sumVar2)
  where sum1 = getExpSum x
        sum2 = getExpSum y
        sumVar1 = getVarOrder x
        sumVar2 = getVarOrder y

sortToNormalize = sortBy sortNormalize

normalizeVars :: [(Char, Int)] -> [(Char, Int)] --assumes list of vars is sorted by var
normalizeVars [] = []
normalizeVars [x] = [x]
normalizeVars (x:y:xs) = if (fst x == fst y) then (normalizeVars ((fst x, snd x + snd y) : xs)) else (x : normalizeVars (y:xs))

sortVars :: (Char, Int) -> (Char, Int) -> Ordering
sortVars x y = compare (fst x) (fst y)

sortByVars = sortBy sortVars
