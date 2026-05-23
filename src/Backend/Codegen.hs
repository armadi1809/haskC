module Backend.Codegen (gen) where

import qualified Assembly as Ass
import qualified Tacky as T

-- convertExpression :: T.Exp -> Ass.Operand
-- convertExpression (ConstInt v) = Ass.Imm v

convertOperand :: T.Val -> Ass.Operand
convertOperand op = case op of
  T.ConstInt v -> Ass.Imm v
  T.Var ident -> Ass.Pseudo ident

convertUnaryOp :: T.UnaryOp -> Ass.UnaryOp
convertUnaryOp T.Complement = Ass.Not
convertUnaryOp T.Negate = Ass.Neg

-- convertStatement :: T.Statement -> [Ass.Instruction]
-- convertStatement (Return e) =
--   let v = convertExpression e
--    in [Ass.Mov v Ass.Register, Ass.Return]

convertTInstruction :: T.Instruction -> [Ass.Instruction]
convertTInstruction (T.Return val) =
  let op = convertOperand val
   in [Ass.Mov op op, Ass.Return]
convertTInstruction (T.Unary op src dest) =
  let srcOp = convertOperand src
      dstOp = convertOperand dest
   in [Ass.Mov srcOp dstOp, Ass.Unary (convertUnaryOp op) dstOp]

convertTInstructions :: [T.Instruction] -> [Ass.Instruction]
convertTInstructions [] = []
convertTInstructions (tInstruct : rest) = convertTInstruction tInstruct ++ (convertTInstructions rest)

convertFunction :: T.FuncDef -> Ass.FuncDef
convertFunction (T.Function name instructions) = Ass.Function name (convertTInstructions instructions)

gen :: T.Program -> Ass.Program
gen (T.Program fnDef) = Ass.Program (convertFunction fnDef)
