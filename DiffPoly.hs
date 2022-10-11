module DiffPoly where

import Types
import Utils

test :: Poly
test = [Mono 2 [('a',3)],
        Mono 3 [('b',3), ('a',3)],
        Mono 1 [('c',3)],
        Mono 3 [('a',3)]]

diffMono :: Char -> Mono -> Mono  --percorrer lista de variaveis, se encontrar a variavel retorna o monomio derivado, else retorna Monomio 0
diffMono a m | containsVar a (vars m) = Mono (auxCoef a m) (auxVars a m)
             | otherwise = Mono 0 [('-', 0)]

auxVars :: Char -> Mono -> [(Char, Int)]
auxVars a m = [if(fst i == a) then((fst i, (snd i) - 1)) else (i) | i <- vars m]

auxCoef :: Char -> Mono -> Float
auxCoef a m = head [ fromIntegral (snd i)*(coef m) | i <- vars m, fst i == a]


diffPoly :: Char -> Poly -> Poly
diffPoly _ [] = []
diffPoly a (x:xs) = removeNulls (diffMono a x : diffPoly a xs)
