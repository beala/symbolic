module Symbolic where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import Types
import Util

symbolic :: Int -> Prog -> SymState -> Trace
symbolic i prog st@(pc, _, _, _, _) =
  let Just instr = prog ! (Offset pc)
      newState = symStep st instr
  in
    if instr /= Done && i > 0
    then T.Node st (symbolic (i-1) prog <$> newState)
    else T.Node st []

symStep :: SymState -> Instr -> [SymState]
symStep (pc, i, mem, l:r:stack, cs) Add = pure (pc+1, i, mem, CAdd l r : stack, cs)
symStep _ Add = error "Add expects two arguments."
symStep (pc, i, mem, stack, cs) Read = pure (pc+1, i+1, mem, CAny i : stack, cs)
symStep (pc, i, mem, stack, cs) (Push w) = pure (pc+1, i, mem, CCon w : stack, cs)
symStep (pc, i, mem, _:stack, cs) Pop = pure (pc+1, i, mem, stack, cs)
symStep _ Pop = error "Pop expects one argument."
symStep (pc, i, mem, w:stack, cs) Dup = pure (pc+1, i, mem, w:w:stack, cs)
symStep _ Dup = error "Dup expects one argument."
symStep (pc, i, mem, _:stack, cs) Print = pure (pc+1, i, mem, stack, cs)
symStep _ Print = error "Print expects one argument."
symStep (pc, i, mem, x:y:stack, cs) Swap = pure (pc+1, i, mem, y:x:stack, cs)
symStep _ Swap = error "Swap expects two arguments."
symStep (pc, i, mem, cond:CCon addr:stack, cs) JmpIf =
  [ (pc+1, i, mem, stack, CEq cond (CCon 0) : cs)
  , (wordToInt addr, i, mem, stack, CNot (CEq cond (CCon 0)) : cs)
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
symStep (pc, i, mem, w:stack, cs) Not = pure (pc+1, i, mem, CNot w:stack, cs)
symStep _ Not = error "Not expects one argument."
symStep (pc, i, mem, l:r:stack, cs) And = pure (pc+1, i, mem, CAnd l r:stack, cs)
symStep _ And = error "And expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Or = pure (pc+1, i, mem, COr l r:stack, cs)
symStep _ Or = error "Or expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Lt = pure (pc+1, i, mem, CLt l r: stack, cs)
symStep _ Lt = error "Lt expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Eq = pure (pc+1, i, mem, CEq l r: stack, cs)
symStep _ Eq = error "Eq expects two arguments."
symStep (pc, i, mem, CCon addr:w:stack, cs) Store = pure (pc+1, i, M.insert addr w mem, stack, cs)
symStep (pc, i, mem, _:_:stack, cs) Store =
  -- Only handle concrete addresses for now.
  pure (pc+1, i, mem, stack, cs)
symStep _ Store = error "Store expects two arguments."
symStep (pc, i, mem, CCon addr:stack, cs) Load =
  case M.lookup addr mem of
    Just w -> pure (pc+1, i, mem, w:stack, cs)
    Nothing -> error "Nothing to Load at address."
symStep (pc, i, mem, _:stack, cs) Load =
  -- Only handle concrete addresses for now.
  pure (pc+1, i+1, mem, CAny i: stack, cs)
symStep _ Load = error "Store expects two arguments."
symStep _ Done = error "No step for Done"

defaultSymState :: SymState
defaultSymState = (0, 0, M.empty, [], [])

renderConstraint :: Constraint -> String
renderConstraint (CAdd l r) = renderConstraint l <> " + " <> renderConstraint r
renderConstraint (CCon w) = show (wordToSignedInt w)
renderConstraint (CAny i) = valName i
renderConstraint (CEq l r) = renderConstraint l <> " = " <> renderConstraint r
renderConstraint (CNot c) = "~(" <> renderConstraint c <> ")"
renderConstraint (CAnd l r) = renderConstraint l <> " and " <> renderConstraint r
renderConstraint (COr l r) = renderConstraint l <> " or " <> renderConstraint r
renderConstraint (CLt l r) = renderConstraint l <> " < " <> renderConstraint r
