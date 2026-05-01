module Assembly (Program (..), FuncDef (..), Operand (..), Instruction (..)) where

data Operand
  = Imm Int
  | Register
  deriving (Eq, Show)

data Instruction
  = Mov Operand Operand
  | Return
  deriving (Eq, Show)

data FuncDef
  = Function String [Instruction]
  deriving (Eq, Show)

data Program = Program FuncDef
  deriving (Eq, Show)