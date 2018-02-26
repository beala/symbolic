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

data Constraint = CAdd Constraint Constraint
                | CEq Constraint Constraint
                | CNot Constraint
                | COr Constraint Constraint
                | CCon Word32
                | CAnd Constraint Constraint
                | CLt Constraint Constraint
                | CAny Int deriving (Show, Eq)

type SymState = (Int, Int, M.Map Word32 Constraint, [Constraint], [Constraint])

type Trace = T.Tree SymState
