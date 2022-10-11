module AddPoly where

import Types


addMono :: Mono -> Mono -> Mono
addMono a b = Mono (coef a + coef b) (vars a)

addPoly :: Poly -> Poly -- função tem que receber o polinomio ordenado por variável
addPoly [] = []
addPoly [x] = [x]
addPoly (x:y:xs) = if(vars x == vars y) then (addPoly ((addMono x y):xs) ) else (x : (addPoly (y:xs)))
