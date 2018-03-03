module Symbolic where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import Types
import Util

symRun :: Int -> Prog -> SymState -> Trace
symRun maxDepth prog state@(pc, _, _, _, _) =
  case prog ! (Offset pc) of
    Just Done -> T.Node state []
    Just instr ->
      if maxDepth > 0 then
        let newStates = symStep state instr
            children = symRun (maxDepth - 1) prog <$> newStates
        in T.Node state children
      else
        T.Node state []
    Nothing ->
      error $ "No instruction at " <> show pc

symStep :: SymState -> Instr -> [SymState]
symStep (pc, i, mem, l:r:stack, cs) Add = pure (pc+1, i, mem, SAdd l r : stack, cs)
symStep _ Add = error "Add expects two arguments."
symStep (pc, i, mem, stack, cs) Read = pure (pc+1, i+1, mem, SAny i : stack, cs)
symStep (pc, i, mem, stack, cs) (Push w) = pure (pc+1, i, mem, SCon w : stack, cs)
symStep (pc, i, mem, _:stack, cs) Pop = pure (pc+1, i, mem, stack, cs)
symStep _ Pop = error "Pop expects one argument."
symStep (pc, i, mem, w:stack, cs) Dup = pure (pc+1, i, mem, w:w:stack, cs)
symStep _ Dup = error "Dup expects one argument."
symStep (pc, i, mem, _:stack, cs) Print = pure (pc+1, i, mem, stack, cs)
symStep _ Print = error "Print expects one argument."
symStep (pc, i, mem, x:y:stack, cs) Swap = pure (pc+1, i, mem, y:x:stack, cs)
symStep _ Swap = error "Swap expects two arguments."
symStep (pc, i, mem, cond:SCon addr:stack, cs) JmpIf =
  [ (pc+1, i, mem, stack, SNot cond : cs)
  , (wordToInt addr, i, mem, stack, cond : cs)
  ]
symStep (pc, i, mem, _:_:stack, cs) JmpIf =
  -- If the jump address is not concrete, don't explore that branch
  -- The jump could be to anywhere in the program.
  pure (pc+1, i, mem, stack, cs)
symStep _ JmpIf = error "JmpIf expects two arguments."
symStep (pc, i, mem, w:stack, cs) Over = pure (pc+1, i, mem, w:stack <> [w], cs)
symStep _ Over = error "Over expects one argument."
symStep (pc, i, mem, w:stack, cs) RotL = pure (pc+1, i, mem, stack <> [w], cs)
symStep _ RotL = error "RotL expects one argument."
symStep (pc, i, mem, w:stack, cs) Not = pure (pc+1, i, mem, SNot w:stack, cs)
symStep _ Not = error "Not expects one argument."
symStep (pc, i, mem, l:r:stack, cs) And = pure (pc+1, i, mem, SAnd l r:stack, cs)
symStep _ And = error "And expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Or = pure (pc+1, i, mem, SOr l r:stack, cs)
symStep _ Or = error "Or expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Lt = pure (pc+1, i, mem, SLt l r: stack, cs)
symStep _ Lt = error "Lt expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Eq = pure (pc+1, i, mem, SEq l r: stack, cs)
symStep _ Eq = error "Eq expects two arguments."
symStep (pc, i, mem, SCon addr:w:stack, cs) Store = pure (pc+1, i, M.insert addr w mem, stack, cs)
symStep (pc, i, mem, _:_:stack, cs) Store =
  -- Only handle concrete addresses for now.
  pure (pc+1, i, mem, stack, cs)
symStep _ Store = error "Store expects two arguments."
symStep (pc, i, mem, SCon addr:stack, cs) Load =
  case M.lookup addr mem of
    Just w -> pure (pc+1, i, mem, w:stack, cs)
    Nothing -> error "Nothing to Load at address."
symStep (pc, i, mem, _:stack, cs) Load =
  -- Only handle concrete addresses for now.
  pure (pc+1, i+1, mem, SAny i: stack, cs)
symStep _ Load = error "Store expects two arguments."
symStep _ Done = error "No step for Done"

defaultSymState :: SymState
defaultSymState = (0, 0, M.empty, [], [])

renderSym :: Sym -> String
renderSym (SAdd l r) = "(" <> renderSym l <> " + " <> renderSym r <> ")"
renderSym (SCon w) = show (wordToSignedInt w)
renderSym (SAny i) = valName i
renderSym (SEq l r) = renderSym l <> " = " <> renderSym r
renderSym (SNot c) = "~(" <> renderSym c <> ")"
renderSym (SAnd l r) = renderSym l <> " and " <> renderSym r
renderSym (SOr l r) = renderSym l <> " or " <> renderSym r
renderSym (SLt l r) = renderSym l <> " < " <> renderSym r
