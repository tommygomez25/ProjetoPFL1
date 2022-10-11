module MulPoly where

import Data.List (sort)

import Types

mulVars :: [(Char, Int)] -> [(Char, Int)] -- recebe lista concatenada das variaveis dos dois monomios
mulVars [] = []
mulVars [x] = [x]
mulVars (x:y:xs) = if (fst x == fst y) then (mulVars([(fst x, snd x + snd y)] ++ xs)) else (x : mulVars (y:xs))

mulMono :: Mono -> Mono -> Mono
mulMono a b = Mono (coef a * coef b) (mulVars vars1)
              where vars1 = sort ((vars a) ++ (vars b))

mulPoly :: Poly -> Poly -> Poly
mulPoly a b = [mulMono x y | x <- a , y <- b]
