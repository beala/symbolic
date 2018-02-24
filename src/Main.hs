{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Bits as B
import qualified Data.Tree as T
import qualified Data.SBV as S

import Prelude (getLine, read)

import System.IO (hFlush, stdout)
import Control.Monad (when)

data Instr = Add
           | JmpIf
           | And
           | Or
           | Not
           | Lt
           | Eq
           | Push Word32
           | Store
           | Load
           | Pop
           | Read
           | Print
           | Swap
           | Dup
           | Over
           | RotL
           | Done deriving (Eq, Show)

-- | A program is a list of instructions.
type Prog = Array Instr

-- | Memory is a list of 32 bit words.
type Mem = M.Map Word32 Word32

-- | State: (program counter, memory, stack)
type State = (Int, Mem, [Word32])

run :: Bool -> Prog -> State-> IO [Word32]
run trace prg st@(pc, _, stack) = do
  let Just instr = (prg ! (Offset pc))
  when trace $ putStrLn $ "Trace: " <> (show pc) <> " " <> (show instr) <> " " <> (show stack)
  if instr == Done
    then return stack
    else step st instr >>= run trace prg

step :: State -> Instr -> IO State
step (pc, mem, l:r:stack) Add = return (pc+1, mem, l+r : stack)
step _ Add = error "Add expects two argument."
step (pc, mem, cond:addr:stack) JmpIf = return $
  if cond == 0 then (pc+1, mem, stack)
  else (wordToInt addr, mem, stack)
step _ JmpIf = error "JmpIf expects 2 arguments."
step (pc, mem, l:r:stack) And =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' && r')
  in
    return (pc+1, mem, res:stack)
step _ And = error "And expects two arguments."
step (pc, mem, l:r:stack) Or =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' || r')
  in
    return (pc+1, mem, res:stack)
step _ Or = error "Or expects two arguments."
step (pc, mem, a:stack) Not =
  let a' = wordToBool a
      res = boolToWord (not a')
  in
    return (pc+1, mem, res:stack)
step _ Not = error "Not expects one argument."
step (pc, mem, l:r:stack) Lt =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' < r')
  in
    return (pc+1, mem, res:stack)
step _ Lt = error "Lt expects two arguments."
step (pc, mem, l:r:stack) Eq =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' == r')
  in
    return (pc+1, mem, res:stack)
step _ Eq = error "Eq expects two arguments."
step (pc, mem, stack) (Push w) =
  return (pc + 1, mem, w:stack)
step (pc, mem, _:stack) (Pop) =
  return (pc+1, mem, stack)
step _ Pop = error "Pop expects one argument."
step (pc, mem, addr:w:stack) Store =
  let mem' = M.insert addr w mem
  in return (pc + 1, mem', stack)
step _ Store = error "Store expects two arguments."
step (pc, mem, addr:stack) Load =
  case  M.lookup addr mem of
    Just w -> return (pc + 1, mem, w:stack)
    Nothing -> error "Nothing to load at address."
step _ Load = error "Load expects one argument."
step (pc, mem, stack) Read = do
  putStr "? "
  hFlush stdout
  w <- getLine
  return (pc+1, mem, (read w):stack)
step (pc, mem, w:stack) Print = do
  putStrLn (show w)
  return (pc+1, mem, stack)
step _ Print = error "Print expects one argument."
step (pc, mem, x:y:stack) Swap =
  return (pc+1, mem, y:x:stack)
step _ Swap = error "Swap expects two arguments."
step (pc, mem, w:stack) Dup =
  return (pc+1, mem, w:w:stack)
step _ Dup = error "Dup expects one argument."
step (pc, mem, w:stack) Over =
  return (pc+1, mem, w:stack <> [w])
step _ Over = error "Over expects one argument."
step (pc, mem, w:stack) RotL =
  return (pc+1, mem, stack <> [w])
step _ RotL = error "RotL expects one argument."
step _ Done =
  error "No state transition for Done!!"

wordToInt :: Word32 -> Int
wordToInt = fromIntegral . toInteger

wordToSignedInt :: Word32 -> Int
wordToSignedInt w =
  if isNegative w then
    (-(wordToInt (twosComplement w)))
  else
    wordToInt w

wordToBool :: Word32 -> Bool
wordToBool 0 = False
wordToBool _ = True

boolToWord :: Bool -> Word32
boolToWord True = 1
boolToWord False = 0

twosComplement :: Word32 -> Word32
twosComplement i = 1 + B.complement i

isNegative :: Word32 -> Bool
isNegative w = B.testBit w 31

data Constraint = CAdd Constraint Constraint
                | CEq Constraint Constraint
                | CNot Constraint
                | COr Constraint Constraint
                | CCon Word32
                | CAnd Constraint Constraint
                | CLt Constraint Constraint
                | CAny Int deriving (Show, Eq)

renderConstraint :: Constraint -> String
renderConstraint (CAdd l r) = renderConstraint l <> " + " <> renderConstraint r
renderConstraint (CCon w) = show (wordToSignedInt w)
renderConstraint (CAny i) = "val_" <> (show i)
renderConstraint (CEq l r) = renderConstraint l <> " = " <> renderConstraint r
renderConstraint (CNot c) = "~(" <> renderConstraint c <> ")"
renderConstraint (CAnd l r) = renderConstraint l <> " ∧ " <> renderConstraint r
renderConstraint (COr l r) = renderConstraint l <> " ∨ " <> renderConstraint r
renderConstraint (CLt l r) = renderConstraint l <> " < " <> renderConstraint r

type SymState = (Int, Int, M.Map Word32 Constraint, [Constraint], [Constraint])

type Trace = T.Tree SymState

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
  [ (pc+1, i, mem, stack, (CEq cond (CCon 0)) : cs)
  , (wordToInt addr, i, mem, stack, (CNot (CEq cond (CCon 0))):cs)
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

main :: IO ()
main = do
  args <- getArgs
  let trace = elem "-t" args
  let prog = listToProgram multiply
  putStrLn $ show prog
  stack <- run trace prog (0, M.empty, [])
  putStrLn $ show $ wordToSignedInt <$> stack

  let traces = symbolic 25 prog defaultSymState
  putStrLn $ fromString $ T.drawTree $ fmap (toList . show . \(pc,_,_,st,cs) -> (pc, renderConstraint <$> st, renderConstraint <$> cs)) traces

countDown :: [Instr]
countDown = [ Read
            , Push (twosComplement 1)
            , Add
            , Dup
            , Print
            , Dup
            , Push 1
            , Swap
            , JmpIf
            , Pop
            , Done
            ]

multiply :: [Instr]
multiply = [ Read
           , Dup
           , Read
           , Push (twosComplement 1) -- start
           , Add
           , Dup
           , Not
           , Push 18 -- end address
           , Swap
           , JmpIf
           , RotL
           , Dup
           , RotL
           , Add
           , RotL
           , Push 3 -- start address
           , Push 1
           , JmpIf
           , Pop --end
           , Pop
           , Print
           , Done ]

addInputs :: [Instr]
addInputs = [ Read
            , Read
            , Add
            , Push 0
            , Store
            , Push 9
            , Read
            , JmpIf
            , Done
            , Push 0
            , Load
            , Done ]

listToProgram :: [Instr] -> Prog
listToProgram = fromList
