module Tacky (Program (..), FuncDef (..), Instruction (..), Val (..), UnaryOp (..)) where

data UnaryOp = Complement | Negate
  deriving (Eq, Show)

data Val = ConstInt Int | Var String
  deriving (Eq, Show)

data Instruction = Return Val | Unary UnaryOp Val Val
  deriving (Eq, Show)

data FuncDef = Function String [Instruction]
  deriving (Eq, Show)

data Program = Program FuncDef