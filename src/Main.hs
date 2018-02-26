module Main where

import Foundation

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import Concrete
import Util
import Programs
import SMT
import Symbolic

main :: IO ()
main = do
  args <- getArgs
  let trace = elem "-t" args
  let prog = listToProgram multiply
  putStrLn $ show prog
  stack <- run trace prog (0, M.empty, [])
  putStrLn $ show $ wordToSignedInt <$> stack
  let traces = symbolic 20 prog defaultSymState
  putStrLn $ fromString $ T.drawTree $ fmap (toList . show . \(pc,_,_,st,cs) -> (pc, renderConstraint <$> st, renderConstraint <$> cs)) traces
  solvedTraces <- solveSym traces
  putStrLn $ fromString $ T.drawTree $ fmap (toList . renderSolvedState) solvedTraces



