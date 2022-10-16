module Projeto1 where

import Data.List (sortBy)
import Data.List (sort)
import Data.Ord (comparing)
import Types
import AddPoly
import MulPoly
import DiffPoly
import Utils
import Tree

-- Monomios p ajudar
monomios :: Poly
monomios = [Mono 2 [('a',3)],
            Mono 3 [('b',3), ('a',3)],
            Mono 1 [('c',3)],
            Mono 3 [('a',3)]]
testProduto :: Poly
testProduto = [Mono 2 [('x',2)],
              Mono 1 [('x',1)],
              Mono 1 [('y',2)],
              Mono 1 [('x',1)]]
pSomavel :: Poly
pSomavel = [Mono 3 [('b',3), ('a',3)],
            Mono 2 [('a',3)],
            Mono (-2) [('a',3)],
            Mono 3 [('c',3)]]

testNormalizar :: Poly
testNormalizar = [Mono 2 [('x',3)],
              Mono 1 [('y',3)],
              Mono 1 [('x',2)],
              Mono 1 [('y',2)]]


menu :: IO ()
menu = do
  putStrLn "Qual a operação?"
  putStr "1-Normalizar polinómio\n2-Derivada de polinómio\n"
  option <- getLine
  case option of
    --"1" -> askPoly
    "2" -> askVarAndPoly
    _ -> do  putStrLn "Opção inválida";
              menu

{-
askPoly :: IO ()
askPoly = do
  putStrLn "Qual é o polinómio?\n"
  p <- getLine
  return
-}

askVarAndPoly :: IO ()
askVarAndPoly = do
  putStrLn "Qual é o polinómio?"
  p1 <- getLine
  putStrLn "Derivar em ordem a que variável?"
  p2 <- getChar
  putStrLn "O resultado é:"
  putStrLn (clearPrint (printPoly (diffPoly (p2) (addPoly (sortToSum (eval (parse (lexer p1))))))))
