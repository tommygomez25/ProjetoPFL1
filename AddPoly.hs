module AddPoly where

import Types

-- Function to add two monomyals (assumes they are addable based on the condition used in addPoly)
addMono :: [Mono] -> [Mono] -> [Mono]
addMono a b = if (vars (head a) == vars (head b)) then [Mono (coef (head a) + coef (head b)) (vars (head a))] else a++b

addMonoKnowingTheyCanBeAdded :: Mono -> Mono -> Mono
addMonoKnowingTheyCanBeAdded a b = Mono (coef a + coef b) (vars a)

cleanVarsWithExpZero:: [(Char,Int)] -> [(Char,Int)]
cleanVarsWithExpZero l | length auxList == 0 = [('-',0)]
                       | otherwise = auxList
                          where auxList = [x | x<-l, snd x /= 0]

filterExpZero :: Poly -> Poly
filterExpZero p = [Mono (coef x) (cleanVarsWithExpZero (vars x)) | x <- p]

-- Function to add two polynomials, the parameter is the two polynomials concatenated and ordered by variable and degree
addPoly :: Poly -> Poly
addPoly [] = []
addPoly [x] = [x]
addPoly (x:y:xs) = if(vars x == vars y) then (addPoly ((addMonoKnowingTheyCanBeAdded x y):xs) ) else (x : (addPoly (y:xs)))
