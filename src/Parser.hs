module Parser (parseProgram, parse) where

import Ast
import Control.Applicative (Alternative (..))
import Lexer

newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}

instance Functor Parser where
  fmap f p = do
    x <- p
    return (f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)

  pf <*> pa = do
    f <- pf
    a <- pa
    return (f a)

instance Monad Parser where
  Parser p >>= f = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (result, rest) ->
        let Parser p2 = f result
         in p2 rest

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      res -> res

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = Parser $ \input ->
  case input of
    (t : ts) | f t -> Just (t, ts)
    _ -> Nothing

token :: Token -> Parser Token
token t = satisfy (== t)

symbol :: Token -> Parser ()
symbol t = () <$ token t

optionalSymbol :: Token -> Parser ()
optionalSymbol t = symbol t <|> pure ()

int :: Parser Int
int = Parser $ \input ->
  case input of
    (TokIntCst n : rest) -> Just (n, rest)
    _ -> Nothing

ident :: Parser String
ident = Parser $ \input ->
  case input of
    (TokIdent s : ts) -> Just (s, ts)
    _ -> Nothing

parseExp :: Parser Exp
parseExp = Parser $ \input -> 
  case input of
    (TokInt)

parseStatement :: Parser Statement
parseStatement = do
  symbol TokReturn
  e <- parseExp
  symbol TokSemiCol
  return (Return e)

parseFuncDef :: Parser FuncDef
parseFuncDef = do
  symbol TokInt
  name <- ident
  symbol TokLeftParen
  optionalSymbol TokVoid
  symbol TokRightParen
  symbol TokLeftBrace
  stmt <- parseStatement
  symbol TokRightBrace
  return (Function name stmt)

parseProgram :: Parser Program
parseProgram = Program <$> parseFuncDef

parse :: Parser a -> [Token] -> Maybe a
parse p tokens =
  case runParser p tokens of
    Just (result, []) -> Just result
    _ -> Nothing
