module Tree where

import Types
import AddPoly
import MulPoly
import Utils
import Data.Char


lexer :: String -> [Token]
lexer [] = []
lexer ('+' : xs) = PlusTok : lexer xs
lexer ('*' : xs) = TimesTok : lexer xs
lexer ('^' : xs) = ExpTok : lexer xs
lexer ('-' : xs) = MinusTok : lexer xs
lexer (chr : xs) | isSpace chr = lexer xs -- ignores whitespaces, newlines and tab characters
                 | isAlpha chr = VarTok chr : lexer xs

lexer str@(chr : _ ) | isDigit chr = IntTok (stringToInt digitStr) : lexer xs
  where
    (digitStr,xs) = break (not . isDigit) str
    -- function to convert a string to integer
    stringToInt :: String -> Int
    stringToInt  = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

-------
parseOtherVars :: [Token] -> ([(Char, Int)], [Token])
parseOtherVars [] = ([], [])
parseOtherVars (TimesTok : VarTok a : ExpTok : IntTok d : TimesTok : VarTok v : restTokens) = ([(a, d)] ++ fst t, snd t)
                                                                                              where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseOtherVars (TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = ([(a, 1)] ++ fst t, snd t)
                                                                                              where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)

parseOtherVars (TimesTok : VarTok a : ExpTok : IntTok d : restTokens) = ([(a,d)], restTokens)
parseOtherVars (TimesTok : VarTok a : restTokens) = ([(a,1)], restTokens)


parseIntOrExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (toNegative n) ([(a,m)] ++ fst t)), snd t)
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (toNegative n) ([(a,1)] ++ fst t)), snd t)
                                                                                                      where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono n ([(a,m)] ++ fst t)), snd t)
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono n ([(a,1)] ++ fst t)), snd t)
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)

parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono (toNegative n) [(a,m)]), restTokens)
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : restTokens) = Just (MonoLit (Mono (toNegative n) [(a,1)]), restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono n [(a,m)]), restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : restTokens) = Just (MonoLit (Mono n [(a,1)]), restTokens)

parseIntOrExpr (MinusTok : IntTok n : restTokens) = Just (IntLit (toNegative n),   restTokens)
parseIntOrExpr (IntTok n : restTokens) = Just (IntLit n,   restTokens)
parseIntOrExpr (MinusTok : VarTok a : restTokens) = Just (MonoLit (Mono (-1)[(a,1)]),   restTokens)
parseIntOrExpr (VarTok a : restTokens) = Just (MonoLit(Mono 1 [(a,1)]),   restTokens)

parseIntOrExpr tokens = Nothing


parseProdOrIntOrExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrExpr tokens
  = case parseIntOrExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) ->
          case parseProdOrIntOrExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result


parseSumOrProdOrIntOrExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrExpr tokens
  = case parseProdOrIntOrExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) ->
          case parseSumOrProdOrIntOrExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      Just (expr1, (MinusTok : restTokens1)) ->
          case parseSumOrProdOrIntOrExpr (MinusTok : restTokens1) of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrExpr tokens of
    Just (expr, []) -> expr
    _               -> error "Could not parse input"


eval :: Expr -> [Mono]
eval (MonoLit m) = [Mono (coef m) (vars m)]
eval (IntLit n) = [Mono n [('-',0)]]
eval (Add expr1 expr2) = addMono (eval expr1) (eval expr2)
eval (Mult expr1 expr2) = mulMono (eval expr1) (eval expr2)

parseP :: String -> Poly
parseP s = eval (parse (lexer(s)))
