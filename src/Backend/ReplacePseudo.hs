module Backend.ReplacePseudo (replacePseudosInFunction, replacePseudos) where

import qualified Assembly as Ass
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map

type State = (Int, Map.Map String Int)

replaceOperand :: State -> Ass.Operand -> (State, Ass.Operand)
replaceOperand (currOffset, m) (Ass.Pseudo s) = case (Map.lookup s m) of
  Just offset -> ((currOffset, m), Ass.Stack offset)
  Nothing ->
    let newOffset = currOffset - 4
        newM = Map.insert s newOffset m
     in ((newOffset, newM), Ass.Stack newOffset)
replaceOperand s op = (s, op)

replacePseudosInInstruction :: State -> Ass.Instruction -> (State, Ass.Instruction)
replacePseudosInInstruction s (Ass.Mov src dst) =
  let (state1, newSrc) = replaceOperand s src
      (state2, newDst) = replaceOperand state1 dst
   in (state2, Ass.Mov newSrc newDst)
replacePseudosInInstruction s (Ass.Unary op dst) =
  let (state1, newDst) = replaceOperand s dst
   in (state1, Ass.Unary op newDst)
replacePseudosInInstruction s (Ass.Return) = (s, Ass.Return)
replacePseudosInInstruction _ _ = undefined -- Should never reach here

replacePseudosInFunction :: Ass.FuncDef -> (Ass.FuncDef, Int)
replacePseudosInFunction (Ass.Function name insts) =
  let ((finalOffset, _), newInsts) = mapAccumL replacePseudosInInstruction (0, Map.empty) insts
   in (Ass.Function name newInsts, finalOffset)

replacePseudos :: Ass.Program -> (Ass.Program, Int)
replacePseudos (Ass.Program fn) =
  let (newFn, finalOffset) = replacePseudosInFunction fn
   in (Ass.Program newFn, finalOffset)