module DiffPoly where

import Types
import Utils

-- Function to calculate monomial derivative in order to 'a'
diffMono :: Char -> Mono -> Mono
diffMono a m | containsVar a (vars m) = Mono (auxCoef a m) (auxVars a m)
             | otherwise = Mono 0 [('-', 0)]

-- Auxiliar function that returns the derivative of the list of variables of a monomial
auxVars :: Char -> Mono -> [(Char, Int)]
auxVars a m = [if(fst i == a) then((fst i, (snd i) - 1)) else (i) | i <- vars m]

-- Auxiliar function that returns the new monomial coefficient according to its derivative (old_coeficient * derivative_variable_degree)
auxCoef :: Char -> Mono -> Int
auxCoef a m = head [ (snd i)*(coef m) | i <- vars m, fst i == a]

-- Function to calculate a polynimial derivative in order to 'a'
diffPoly :: Char -> Poly -> Poly
diffPoly _ [] = []
diffPoly a (x:xs) = removeNulls (diffMono a x : diffPoly a xs)
