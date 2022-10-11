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

main = do
  putStr "Polinomio?"
  --p <- getLine
  --parsePoly p
