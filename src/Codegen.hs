module Codegen (gen) where

import qualified Assembly as Ass
import Ast

convertExpression :: Ast.Exp -> Ass.Operand
convertExpression (ConstInt v) = Ass.Imm v

convertStatement :: Ast.Statement -> [Ass.Instruction]
convertStatement (Return e) =
  let v = convertExpression e
   in [Ass.Mov v Ass.Register]

convertFunction :: Ast.FuncDef -> Ass.FuncDef
convertFunction (Ast.Function name statement) = Ass.Function name (convertStatement statement)

gen :: Ast.Program -> Ass.Program
gen (Ast.Program fnDef) = Ass.Program (convertFunction fnDef)
