module Backend.FixInst (fixupProgram) where

import qualified Assembly as Ass

fixupInstruction :: Ass.Instruction -> [Ass.Instruction]
fixupInstruction (Ass.Mov (Ass.Stack src) (Ass.Stack dst)) =
  [Ass.Mov (Ass.Stack src) (Ass.Register Ass.R10), Ass.Mov (Ass.Register Ass.R10) (Ass.Stack dst)]
fixupInstruction inst = [inst]

fixupFunction :: Int -> Ass.FuncDef -> Ass.FuncDef
fixupFunction lastOffset (Ass.Function name insts) =
  Ass.Function name ((Ass.AllocateStack (-lastOffset)) : concatMap fixupInstruction insts)

fixupProgram :: Int -> Ass.Program -> Ass.Program
fixupProgram lastOffset (Ass.Program fn) = Ass.Program (fixupFunction lastOffset fn)