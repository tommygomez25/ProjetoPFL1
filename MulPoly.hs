module MulPoly where

import Data.List (sort)
import Types

-- Auxiliar function to multiply the variables lists, the parameter is the two variable lists concatenated
mulVars :: [(Char, Int)] -> [(Char, Int)]
mulVars [] = []
mulVars [x] = [x]
mulVars (x:y:xs) = if (fst x == fst y) then (mulVars([(fst x, snd x + snd y)] ++ xs)) else (x : mulVars (y:xs))

-- Function to multiply monomyals
mulMono :: [Mono] -> [Mono] -> [Mono]
mulMono a b = [Mono (coef (head a) * coef(head b)) (mulVars vars1)]
              where vars1 = sort ((vars (head a)) ++ (vars (head b)))

mulMono' :: Mono -> Mono -> Mono
mulMono' a b = Mono ((coef a) * (coef b)) (mulVars vars1)
                where vars1 = sort (vars a ++ vars b)

-- Function to multiply polynomials
multiplyP :: Poly -> Poly -> Poly
multiplyP a b = [mulMono' x y | x <- a , y <- b]
