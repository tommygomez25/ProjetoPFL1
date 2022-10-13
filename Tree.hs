module Tree where

import Types
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : xs) = PlusTok : lexer xs
lexer ('*' : xs) = TimesTok : lexer xs
lexer ('(' : xs) = OpenTok : lexer xs
lexer (')' : xs) = CloseTok : lexer xs
lexer ('^' : xs) = ExpTok : lexer xs
lexer (chr : xs) | isSpace chr = lexer xs -- ignores whitespaces, newlines and tab characters
                 | isAlpha chr = VarTok chr : lexer xs

lexer str@(chr : _ ) | isDigit chr = IntTok (stringToInt digitStr) : lexer xs
  where
    (digitStr,xs) = break (not . isDigit) str
    -- function to convert a string to integer
    stringToInt :: String -> Int
    stringToInt  = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

-------
parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m: restTokens) = Just (MonoLit (Mono n [(a,m)]), restTokens)
parseIntOrParenExpr (OpenTok : restTokens1) = case parseSumOrProdOrIntOrParenExpr restTokens1 of
                                                Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
                                                Just _ -> Nothing -- no closing parenthesis
                                                Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing


parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parseIntOrParenExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) ->
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result


parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) ->
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr
    _               -> error "Could not parse input"
