module Ast (Exp (..), Statement (..), FuncDef (..), Program (..), UnaryOp (..)) where

data Exp = ConstInt Int | Unary UnaryOp Exp
  deriving (Eq, Show)

data Statement = Return Exp
  deriving (Eq, Show)

data FuncDef = Function String Statement
  deriving (Eq, Show)

data UnaryOp = Complement | Negate
  deriving (Eq, Show)

data Program = Program FuncDef
  deriving (Eq, Show)