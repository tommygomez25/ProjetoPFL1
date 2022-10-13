module Projeto1 where

import Data.List (sortBy)
import Data.List (sort)
import Data.Ord (comparing)

import Types
import AddPoly
import MulPoly
import DiffPoly
import Utils

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

{-
menu :: IO ()
menu = do
  putStrLn "Qual a operação?"
  putStr "1-Normalizar polinómio\n2-Adicionar polinómios\n3-Multiplicar polinómios\n4-Derivada de polinómio\n"
  option <- getLine
  case option of
    "1" -> askPoly
    "2" -> askPolys
    "3" -> askPolys
    "4" -> askPoly
    _ -> do  putStrLn "Opção inválida";
              menu


askPoly :: IO ()
askPoly = do
  putStrLn "Qual é o polinómio?\n"
  p <- getLine
  return


askPolys :: IO ()
askPolys = do
  putStrLn "Qual é o primeiro polinómio?"
  p1 <- getLine
  putStrLn "Qual é o segundo polinómio?"
  p2 <- getLine
  return
-}
