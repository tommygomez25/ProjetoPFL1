module Tree where

import Types
import AddPoly
import MulPoly
import Utils
import Data.Char

-- Function to decompose the string containing the polynomial into a sequence of tokens
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



-- Parser to convert tokens of monomials with more than one variable
parseOtherVars :: [Token] -> ([(Char, Int)], [Token])
parseOtherVars [] = ([], [])
parseOtherVars (TimesTok : VarTok a : ExpTok : IntTok d : TimesTok : VarTok v : restTokens) = ([(a, d)] ++ fst t, snd t)
                                                                                              where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseOtherVars (TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = ([(a, 1)] ++ fst t, snd t)
                                                                                              where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)

parseOtherVars (TimesTok : VarTok a : ExpTok : IntTok d : restTokens) = ([(a,d)], restTokens)
parseOtherVars (TimesTok : VarTok a : restTokens) = ([(a,1)], restTokens)


-- Parse the literals ( monomial or integer)
parseIntOrExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (toNegative n) ([(a,m)] ++ fst t)), snd t) --negative monomial with multiple variables
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (toNegative n) ([(a,1)] ++ fst t)), snd t) -- same as above, but when the user inputs "-2*x" instead of "-2*x^1"
                                                                                                      where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono n ([(a,m)] ++ fst t)), snd t) -- monomial with multiple variables
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono n ([(a,1)] ++ fst t)), snd t)                -- same as above, but when the user inputs "2*x" instead of "2*x^1"
                                                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (MinusTok : VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (-1) ([(a,m)] ++ fst t)), snd t)  -- negative monomial with coef -1 , for example "-x^2*y^2"
                                                                                            where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr (MinusTok : VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono (-1) ([(a,1)] ++ fst t)), snd t)                      -- same as above, but when the exponent is omitted, for example "-2*x*y^2"
                                                                        where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr ( VarTok a : ExpTok : IntTok m : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono 1 ([(a,m)] ++ fst t)), snd t)               -- parse positive monomials with multiple variables and coef omitted
                                                                                  where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)
parseIntOrExpr ( VarTok a : TimesTok : VarTok v : restTokens) = Just (MonoLit (Mono 1 ([(a,1)] ++ fst t)), snd t)                                   -- parse monimals with the coef and exponent omitted "2*x*y"
                                                                where t = parseOtherVars ([TimesTok, VarTok v] ++ restTokens)

parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono (toNegative n) [(a,m)]), restTokens) -- monimals with a single varibale
parseIntOrExpr (MinusTok : IntTok n : TimesTok : VarTok a : restTokens) = Just (MonoLit (Mono (toNegative n) [(a,1)]), restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono n [(a,m)]), restTokens)
parseIntOrExpr (IntTok n : TimesTok : VarTok a : restTokens) = Just (MonoLit (Mono n [(a,1)]), restTokens)

parseIntOrExpr (MinusTok : VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono (-1) [(a,m)]), restTokens) -- monomials with coef omitted and a single variable
parseIntOrExpr ( VarTok a : ExpTok : IntTok m : restTokens) = Just (MonoLit (Mono 1 [(a,m)]), restTokens)

parseIntOrExpr (MinusTok : IntTok n : restTokens) = Just (IntLit (toNegative n),   restTokens) -- integer literals
parseIntOrExpr (IntTok n : restTokens) = Just (IntLit n,   restTokens)
parseIntOrExpr (MinusTok : VarTok a : restTokens) = Just (MonoLit (Mono (-1)[(a,1)]),   restTokens) -- mono literals in the form "-x", coef and exp omitted
parseIntOrExpr (VarTok a : restTokens) = Just (MonoLit(Mono 1 [(a,1)]),   restTokens) -- mono literals in the form "x", coef and exp omitted

parseIntOrExpr tokens = Nothing

-- Parse product or literal
parseProdOrIntOrExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrExpr tokens
  = case parseIntOrExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) ->
          case parseProdOrIntOrExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

--Parse sum, product or literal
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

-- Function that calls the parser
parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrExpr tokens of
    Just (expr, []) -> expr
    _               -> error "Could not parse input"


-- Evaluator function, converting expressions to monomials and its operations
eval :: Expr -> [Mono]
eval (MonoLit m) = [Mono (coef m) (vars m)]
eval (IntLit n) = [Mono n [('-',0)]]
eval (Add expr1 expr2) = addMono (eval expr1) (eval expr2)
eval (Mult expr1 expr2) = mulMono (eval expr1) (eval expr2)

-- Aux function to aggregate the 3 main functions responsible for parsing the string
parseP :: String -> Poly
parseP s = eval (parse (lexer(s)))
