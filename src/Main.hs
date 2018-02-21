{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import Control.Monad.ST
import qualified Data.Bits as B

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
           | Done deriving (Eq, Show)

-- | A program is a list of instructions.
type Prog = Array Instr

-- | Memory is a list of 32 bit words.
type Mem = M.Map Word32 Word32

-- | State: (program counter, memory, stack)
type State = (Int, Mem, [Word32])

run :: State -> Prog -> [Word32]
run st@(pc, _, stack) prg =
  let Just instr = (prg ! (Offset pc))
      newState = run' st instr
  in
    if instr == Done then stack
    else run newState prg

run' :: State -> Instr -> State
run' (pc, mem, l:r:stack) Add = (pc+1, mem, l+r : stack)
run' (pc, mem, l:r:stack) JmpIf =
  if l == 0 then (pc+1, mem, stack)
  else (wordToInt r, mem, stack)
run' (pc, mem, l:r:stack) And =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' && r')
  in
    (pc+1, mem, res:stack)
run' (pc, mem, l:r:stack) Or =
  let l' = wordToBool l
      r' = wordToBool r
      res = boolToWord (l' || r')
  in
    (pc+1, mem, res:stack)
run' (pc, mem, a:stack) Not =
  let a' = wordToBool a
      res = boolToWord a'
  in
    (pc+1, mem, res:stack)  
run' (pc, mem, l:r:stack) Lt =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' < r')
  in
    (pc+1, mem, res:stack)
run' (pc, mem, l:r:stack) Eq =
  let l' = wordToInt l
      r' = wordToInt r
      res = boolToWord (l' == r')
  in
    (pc+1, mem, res:stack)
run' (pc, mem, stack) (Push w) = (pc + 1, mem, w:stack)
run' (pc, mem, addr:w:stack) Store =
  let mem' = M.insert addr w mem
  in (pc + 1, mem', stack)
run' (pc, mem, addr:stack) Load =
  let Just w = M.lookup addr mem
  in (pc + 1, mem, w:stack)
    
run' _ Done = error "No state transition for Done!!"

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

main :: IO ()
main = do
  let Right prog = runST $ build 32 $ do
        append $ Push (twosComplement 100)
        append $ Push 2
        append Add
        append Done
  putStrLn $ show prog
  let x:xs = run (0, M.empty, []) prog
  putStrLn $ show $ wordToSignedInt x
