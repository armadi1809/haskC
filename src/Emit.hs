module Emit (emit) where

import qualified Assembly as Ass

showOperand :: Ass.Operand -> String
showOperand (Ass.Imm v) = show v
showOperand Ass.Register = "%eax"

emitInstruction :: FilePath -> Ass.Instruction -> IO ()
emitInstruction assemblyFile inst = case inst of
  Ass.Mov op1 op2 -> appendFile assemblyFile ("\t" ++ "movl " ++ showOperand op1 ++ "," ++ showOperand op2 ++ "\n")
  Ass.Return -> appendFile assemblyFile "ret\n"

emitFunction :: FilePath -> Ass.FuncDef -> IO ()
emitFunction assemblyFile (Ass.Function name instructions) = do
  let label = "_" ++ name ++ ":"
      funcDesc = ".globl " ++ label ++ "\n" ++ label ++ "\n"
  appendFile assemblyFile funcDesc
  mapM_ (emitInstruction assemblyFile) instructions

emit :: FilePath -> Ass.Program -> IO ()
emit assemblyFile (Ass.Program fnDef) = emitFunction assemblyFile fnDef