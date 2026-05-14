module Lexer (lexer, Token (..)) where

import Data.Char

data Token
  = TokIntCst Int
  | TokIdent String
  | TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokSemiCol
  | TokVoid
  | TokReturn
  | TokInt
  | TokTilde
  | TokUnaryMinus
  | TokDoubleMinus
  deriving (Eq, Show)

lexer :: String -> Either String [Token]
lexer [] = Right []
lexer (c : cs)
  | isSpace c = lexer cs
  | isAlpha c =
      let (word, rest) = span isAlphaNum (c : cs)
          tok = case word of
            "int" -> TokInt
            "return" -> TokReturn
            "void" -> TokVoid
            _ -> TokIdent word
       in (tok :) <$> (lexer rest)
  | isDigit c =
      let (digits, rest) = span isDigit (c : cs)
       in (TokIntCst (read digits) :) <$> (lexer rest)
  | c == '(' = (TokLeftParen :) <$> lexer cs
  | c == ')' = (TokRightParen :) <$> lexer cs
  | c == '{' = (TokLeftBrace :) <$> lexer cs
  | c == '}' = (TokRightBrace :) <$> lexer cs
  | c == ';' = (TokSemiCol :) <$> lexer cs
  | c == '-' = case (head cs) of
      '-' -> (TokDoubleMinus :) <$> lexer (drop 1 cs)
      _ -> (TokUnaryMinus :) <$> lexer cs
  | c == '~' = (TokTilde :) <$> lexer cs
  | otherwise = Left ("Unexpected character: " ++ [c])
