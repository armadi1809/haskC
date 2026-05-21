module TackyGen (gen) where

import Ast
import Tacky

convertOp :: Ast.UnaryOp -> Tacky.UnaryOp
convertOp Ast.Complement = Tacky.Complement
convertOp Ast.Negate = Tacky.Negate

convertExpression :: Ast.Exp -> ([Instruction], Tacky.Val)
convertExpression (Ast.ConstInt v) = ([], Tacky.ConstInt v)
convertExpression (Ast.Unary op inner) =
  let (eval_inner, v) = convertExpression inner
      dst_name = "TODO: Generate unique Id here"
      dst = Tacky.Var dst_name
      tacky_op = convertOp op
      instructions = eval_inner ++ [Tacky.Unary tacky_op v dst]
   in (instructions, dst)

convertStatement :: Ast.Statement -> [Tacky.Instruction]
convertStatement (Ast.Return e) =
  let (eval_exp, v) = convertExpression e
   in eval_exp ++ [Tacky.Return v]

convertFuncDef :: Ast.FuncDef -> Tacky.FuncDef
convertFuncDef (Ast.Function name statement) = Tacky.Function name (convertStatement statement)

gen :: Ast.Program -> Tacky.Program
gen (Ast.Program function) = Tacky.Program (convertFuncDef function)