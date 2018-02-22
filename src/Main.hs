{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import Control.Monad.ST
import qualified Data.Bits as B
import qualified Data.Tree as T

import Prelude (getLine, read)

import System.IO (hFlush, stdout)
import Control.Monad (when)

import Debug.Trace

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
           | Rot
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
  when trace $ putStrLn $ "Trace: " <> (show pc) <> " " <> (show instr)
  if instr == Done
    then return stack
    else step st instr >>= run trace prg

step :: State -> Instr -> IO State
step (pc, mem, l:r:stack) Add = return (pc+1, mem, l+r : stack)
step (pc, mem, cond:addr:stack) JmpIf = return $
  if cond == 0 then (pc+1, mem, stack)
  else (wordToInt addr, mem, stack)
step (pc, mem, l:r:stack) And =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' && r')
  in
    return (pc+1, mem, res:stack)
step (pc, mem, l:r:stack) Or =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' || r')
  in
    return (pc+1, mem, res:stack)
step (pc, mem, a:stack) Not =
  let a' = wordToBool a
      res = boolToWord a'
  in
    return (pc+1, mem, res:stack)  
step (pc, mem, l:r:stack) Lt =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' < r')
  in
    return (pc+1, mem, res:stack)
step (pc, mem, l:r:stack) Eq =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' == r')
  in
    return (pc+1, mem, res:stack)
step (pc, mem, stack) (Push w) =
  return (pc + 1, mem, w:stack)
step (pc, mem, w:stack) (Pop) =
  return (pc+1, mem, stack)
step (pc, mem, addr:w:stack) Store =
  let mem' = M.insert addr w mem
  in return (pc + 1, mem', stack)
step (pc, mem, addr:stack) Load =
  let Just w = M.lookup addr mem
  in return (pc + 1, mem, w:stack)
step (pc, mem, stack) Read = do
  putStr "? "
  hFlush stdout
  w <- getLine
  return (pc+1, mem, (read w):stack)
step (pc, mem, w:stack) Print = do
  putStrLn (show w)
  return (pc+1, mem, stack)
step (pc, mem, x:y:stack) Swap =
  return (pc+1, mem, y:x:stack)
step (pc, mem, w:stack) Dup =
  return (pc+1, mem, w:w:stack)
step (pc, mem, w:stack) Over =
  return (pc+1, mem, w:stack <> [w])
step (pc, mem, w:stack) Rot =
  return (pc+1, mem, stack <> [w])
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
                | CCon Word32
                | CAny Int deriving (Show, Eq)

renderConstraint :: Constraint -> String
renderConstraint (CAdd l r) = renderConstraint l <> " + " <> renderConstraint r
renderConstraint (CCon w) = show (wordToSignedInt w)
renderConstraint (CAny i) = "var_" <> (show i)
renderConstraint (CEq l r) = renderConstraint l <> " = " <> renderConstraint r
renderConstraint (CNot c) = "~(" <> renderConstraint c <> ")"

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
symStep (pc, i, mem, stack, cs) Read = pure (pc+1, i+1, mem, CAny i : stack, cs)
symStep (pc, i, mem, stack, cs) (Push w) = pure (pc+1, i, mem, CCon w : stack, cs)
symStep (pc, i, mem, w:stack, cs) Dup = pure (pc+1, i, mem, w:w:stack, cs)
symStep (pc, i, mem, w:stack, cs) Print = pure (pc+1, i, mem, stack, cs)
symStep (pc, i, mem, x:y:stack, cs) Swap = pure (pc+1, i, mem, y:x:stack, cs)
symStep (pc, i, mem, cond:CCon addr:stack, cs) JmpIf =
  [ (pc+1, i, mem, stack, (CEq cond (CCon 0)) : cs)
  , (wordToInt addr, i, mem, stack, (CNot (CEq cond (CCon 0))):cs)
  ]
symStep (pc, i, mem, cond:addr:stack, cs) JmpIf =
  -- If the jump address is not concrete, don't explore the branch
  pure (pc+1, i, mem, stack, cs)
symStep (pc, i, mem, _:stack, cs) Pop = pure (pc+1, i, mem, stack, cs)
symStep (pc, i, mem, w:stack, cs) Over = pure (pc+1, i, mem, w:stack <> [w], cs)
symStep (pc, i, mem, w:stack, cs) Rot = pure (pc+1, i, mem, stack <> [w], cs)
symStep _ Done = error "No step for Done"

defaultSymState = (0, 0, M.empty, [], [])

main :: IO ()
main = do
  args <- getArgs
  let trace = elem "-t" args
  let prog = listToProgram countDown
  putStrLn $ show prog
  stack <- run trace prog (0, M.empty, [])
  putStrLn $ show $ wordToSignedInt <$> stack

  let traces = symbolic 20 prog (0, 0, M.empty, [], [])
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
