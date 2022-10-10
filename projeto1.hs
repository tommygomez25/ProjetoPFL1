import Data.List (sortBy)
import Data.Ord (comparing)

data Mono = Mono
  { coef :: Float,
    var :: Char,
    deg :: Integer
  } -- [coeficiente, variavel, grau]
  deriving (Show, Eq)

type Poly = [Mono]

-- Monomios p ajudar
monomios :: Poly
monomios = [Mono 2 'b' 3,
            Mono 4 'a' 3,
            Mono 10 'd' 3,
            Mono 2 'c' 2,
            Mono 4 'a' 3,
            Mono 10 'd' 3]


monomiosNormal :: Poly
monomiosNormal = sortPoly monomios

removeNulls :: Poly -> Poly
removeNulls x = [a | a <- x, coef a /= 0]


sumMono :: Mono -> Mono -> Mono
sumMono a b = Mono (coef a + coef b) (var a) (deg a)

mulMono :: Mono -> Mono -> Mono
mulMono a b | var a /= var b = Mono 0 'a' 1
            | otherwise = Mono coef1 var1 deg1
                            where coef1 = coef a * coef b
                                  var1 = var a
                                  deg1 = deg a + deg b


sortToNormalize :: Mono -> Mono -> Ordering -- ascending variable, descending degree
sortToNormalize x y
  | var x < var y = LT
  | var x > var y = GT
  | var x == var y = flip compare (deg x) (deg y)


sortPoly = sortBy sortToNormalize

concatPoly :: Poly -> Poly -> Poly
concatPoly x y = x ++ y

sumPoly :: Poly -> Poly --parametro sao os dois polinomios a somar concatenados e na forma normalizada
sumPoly [] = []
sumPoly [x] = [x]
sumPoly (x:y:xs) = if(var x == var y && deg x == deg y) then (sumPoly ((sumMono x y):xs) ) else (x : (sumPoly (y:xs)))



main = do
  putStr "Polinomio?"
  --p <- getLine
  --parsePoly p
