module Calculator where

import Data.List (sortBy)
import Data.List (sort)
import Data.Ord (comparing)
import Types
import AddPoly
import MulPoly
import DiffPoly
import Utils
import Tree

normP :: String -> String
normP s =  clearPrint(printPoly(sortToNormalize(removeNulls(addPoly(sortToSum(normalizePoly (filterExpZero(eval(parse(lexer(s)))))))))))

diff :: Char -> String -> String
diff a s = (clearPrint (printPoly (diffPoly (a) (addPoly (sortToSum (normalizePoly(filterExpZero(eval (parse (lexer s))))))))))

sumP :: String -> String -> String
sumP s1 s2= clearPrint( printPoly ( removeNulls(filterExpZero(sortToNormalize (addPoly(sortToSum(somaP ( normalizePoly(filterExpZero(eval (parse (lexer s1))))) ( normalizePoly(filterExpZero(eval (parse (lexer s2))))))))))))

mulP :: String -> String -> String
mulP s1 s2 = clearPrint( printPoly ( removeNulls(filterExpZero(sortToNormalize (addPoly(sortToSum(multiplyP ( normalizePoly(filterExpZero(eval (parse (lexer s1))))) ( normalizePoly(filterExpZero(eval (parse (lexer s2))))))))))))
