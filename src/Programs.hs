module Programs where

import Foundation

import Types
import Util

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

countUp :: [Instr]
countUp = [ Push 0
          , Read
          , Dup
          , RotL
          , RotL
          , Push 1
          , Add
          , Dup
          , Print
          , Dup
          , RotL
          , Eq
          , Not
          , Push 2
          , Swap
          , JmpIf
          , Done]

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
