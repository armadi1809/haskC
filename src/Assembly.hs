module Assembly (Program (..), FuncDef (..), Operand (..), Instruction (..), Reg (..), UnaryOp (..)) where

data Operand
  = Imm Int
  | Register Reg
  | Pseudo String
  | Stack Int
  deriving (Eq, Show)

data Instruction
  = Mov Operand Operand
  | Unary UnaryOp Operand
  | AllocateStack Int
  | Return
  deriving (Eq, Show)

data FuncDef
  = Function String [Instruction]
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

data Reg = Ax | R10
  deriving (Eq, Show)

data Program = Program FuncDef
  deriving (Eq, Show)