module Tree where

import Types
import AddPoly
import MulPoly
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : xs) = PlusTok : lexer xs
lexer ('*' : xs) = TimesTok : lexer xs
lexer ('^' : xs) = ExpTok : lexer xs
lexer (chr : xs) | isSpace chr = lexer xs -- ignores whitespaces, newlines and tab characters
                 | isAlpha chr = VarTok chr : lexer xs

lexer str@(chr : _ ) | isDigit chr = IntTok (stringToInt digitStr) : lexer xs
  where
    (digitStr,xs) = break (not . isDigit) str
    -- function to convert a string to integer
    stringToInt :: String -> Int
    stringToInt  = foldl (\acc chr -> 10 * acc + digitToInt chr) 0


parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m:TimesTok : VarTok v: restTokens) = Just (MonoLit (Mono n [(a,m)] ++ fst (parseOtherVars TimesTok: [VarTok v] ++ restTokens)), snd(parseOtherVars TimesTok: [VarTok v] ++ restTokens))
parseIntOrParenExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m: restTokens) = Just (MonoLit (Mono n [(a,m)]), restTokens)
parseIntOrParenExpr (OpenTok : restTokens1) = case parseSumOrProdOrIntOrParenExpr restTokens1 of
                                                Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
                                                Just _ -> Nothing -- no closing parenthesis
                                                Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing



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
