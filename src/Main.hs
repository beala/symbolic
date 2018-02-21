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
           | Write
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
step (pc, mem, l:r:stack) JmpIf = return $
  if l == 0 then (pc+1, mem, stack)
  else (wordToInt r, mem, stack)
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
step (pc, mem, w:stack) Write = do
  putStrLn (show w)
  return (pc+1, mem, stack)
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

main :: IO ()
main = do
  let Right prog = runST $ build 32 $ do
        append $ Read
        append $ Read
        append Add
        append $ Push 0
        append Store
        append $ Push 9
        append $ Read
        append JmpIf
        append Done
        append $ Push 0
        append Load
        append Done
  putStrLn $ show prog
  stack <- run True prog (0, M.empty, [])
  putStrLn $ show $ wordToSignedInt <$> stack
