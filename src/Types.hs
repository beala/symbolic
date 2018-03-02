module Types where

import Foundation

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

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

data Sym = SAdd Sym Sym
         | SEq Sym Sym
         | SNot Sym
         | SOr Sym Sym
         | SCon Word32
         | SAnd Sym Sym
         | SLt Sym Sym
         | SAny Int
         deriving (Show, Eq, Ord)

type SymState = (Int, Int, M.Map Word32 Sym, [Sym], [Sym])

type Trace = T.Tree SymState
