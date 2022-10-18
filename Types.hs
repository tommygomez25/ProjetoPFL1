module Types where

data Mono = Mono
  { coef :: Int,
    vars :: [(Char,Int)] -- coef , [variable, degree]
  }
  deriving (Show, Eq)


type Poly = [Mono]

data Token
  = PlusTok
  | TimesTok
  | ExpTok
  | MinusTok
  | VarTok Char
  | IntTok Int
  | MonoTok Mono
  deriving (Show)

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Leaf

data Expr
  = MonoLit Mono
  | VarLit Char
  | IntLit Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show)
