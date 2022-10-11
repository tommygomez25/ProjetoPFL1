module Types where

data Mono = Mono
  { coef :: Float,
    vars :: [(Char,Int)] -- coef , [variable, degree]
  }
  deriving (Show, Eq)


type Poly = [Mono]
