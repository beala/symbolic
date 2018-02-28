module Concrete where

import Foundation
import Foundation.Collection
import Prelude (getLine, read)

import qualified Data.Map.Strict as M
import Control.Monad (when)
import System.IO (hFlush, stdout)

import Types
import Util

run :: Bool -> Prog -> State-> IO [Word32]
run trace prg st@(pc, _, stack) =
  case prg ! (Offset pc) of
    Just Done -> return stack
    Just instr -> do
      when trace $ putStrLn $ "Trace: " <> (show pc) <> " " <> (show instr) <> " " <> (show stack)
      step st instr >>= run trace prg
    _ -> error ("No instruction at " <> show pc)

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
