module Utils where

import Data.List (sortBy)
import Data.Char

import Types

printMono :: Mono -> String
printMono m = s -- ++ [(fst i : "^") : show (snd i) | i <- vars m]
              where s = show ( coef m)

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
