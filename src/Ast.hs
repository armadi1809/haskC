module Ast (Exp (..), Statement (..), FuncDef (..), Program (..)) where

data Exp = ConstInt Int
  deriving (Eq, Show)

data Statement = Return Exp
  deriving (Eq, Show)

data FuncDef = Function String Statement
  deriving (Eq, Show)

data Program = Program FuncDef
  deriving (Eq, Show)