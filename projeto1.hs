import Data.List (sortBy)
import Data.Ord (comparing)

data Mono = Mono
  { coef :: Float,
    vars :: [(Char,Int)] -- coef , [variable, degree]
  }
  deriving (Show, Eq)


type Poly = [Mono]

-- Monomios p ajudar
monomios :: Poly
monomios = [Mono 2 [('a',3)],
            Mono 3 [('b',3), ('a',3)],
            Mono 1 [('c',3)],
            Mono 3 [('a',3)]]

pSomavel :: Poly
pSomavel = [Mono 3 [('b',3), ('a',3)],
            Mono 2 [('a',3)],
            Mono (-2) [('a',3)],
            Mono 3 [('c',3)]]

removeNulls :: Poly -> Poly
removeNulls x = [a | a <- x, coef a /= 0]

mulPoly :: Poly -> Poly -> Poly
mulPoly a b = [mulMono x y | x <- a , y <- b]


sumMono :: Mono -> Mono -> Mono
sumMono a b = Mono (coef a + coef b) (vars a)


sortToMul :: Mono -> Mono -> Ordering -- descending degree, ascending variable
sortToMul x y
  | vars x  < vars y = LT
  | vars x > vars y = GT
  | vars x == vars y = compare (coef x) (coef y)


mulMono :: Mono -> Mono -> Mono
mulMono (Mono a1 x:xs) (Mono a2 y:ys) | b1 == b2 = Mono (a1*a2) [(b1,c1+c2)]
                                                | otherwise = Mono (a1*a2) [(b1,c1),(b1,c2)]


sortt = sortBy sortToMul


{-
sortToNormalize :: Mono -> Mono -> Ordering -- descending degree, ascending variable
sortToNormalize x y
  | vars x < vars y = GT
  | vars x > vars y = LT
  | vars x == vars y = compare (var x) (var y)

sortPolyNormalize = sortBy sortToNormalize

concatPoly :: Poly -> Poly -> Poly
concatPoly x y = x ++ y -}

sumPoly :: Poly -> Poly --parametro sao os dois polinomios a somar concatenados e na forma normalizada
sumPoly [] = []
sumPoly [x] = [x]
sumPoly (x:y:xs) = if(vars x == vars y) then (sumPoly ((sumMono x y):xs) ) else (x : (sumPoly (y:xs)))



main = do
  putStr "Polinomio?"
  --p <- getLine
  --parsePoly p
