module AddPoly where

import Types
import Utils
import Data.List(sort)

-- Function to add two monomyals, if they are not summable , returns a list containing the monomials passed as argument
addMono :: [Mono] -> [Mono] -> [Mono]
addMono a b = if (vars (head a) == vars (head b)) then ([Mono (coef (head a) + coef (head b)) (vars (head a))] ++ tail a ++ tail b) else a++b

-- Function to add two monomyals knowing they can be added
addMonoCBA :: Mono -> Mono -> Mono
addMonoCBA a b = Mono (coef a + coef b) (vars a)

-- Function to add two polynomials, the parameter is the two polynomials concatenated and ordered by variable and degree
addPoly :: Poly -> Poly
addPoly [] = []
addPoly [x] = [x]
addPoly (x:y:xs) = if(sort (vars x) == sort ( vars y)) then (addPoly ((addMonoCBA x y):xs) ) else (x : (addPoly (y:xs)))

-- Release function to add two polynomials
somaP :: Poly -> Poly -> Poly
somaP p1 p2 = addPoly p
              where p = sortToSum (p1 ++ p2)
