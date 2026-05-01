module Assembly (Program (..), FuncDef (..), Operand (..), Instruction (..)) where

data Operand
  = Imm Int
  | Register

data Instruction
  = Mov Operand Operand
  | Return

data FuncDef
  = Function String [Instruction]

data Program = Program FuncDef