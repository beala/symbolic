module SMT where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Control.Monad.State.Lazy as St
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import qualified Data.SBV.Dynamic as S

import Types
import Util
import Symbolic

type SValMap = M.Map Int (S.Symbolic S.SVal)

constraintToSMT :: Constraint
                -> St.State SValMap (S.Symbolic S.SVal)
constraintToSMT (CEq l r) = do
  l' <- constraintToSMT l
  r' <- constraintToSMT r
  return $ sValToSWord <$> (S.svEqual <$> l' <*> r')
constraintToSMT (CAdd l r) = do
  l' <- constraintToSMT l
  r' <- constraintToSMT r
  return $ S.svPlus <$> l' <*> r'
constraintToSMT (CCon w) =
  return $ return $ wordToSVal w
constraintToSMT (CNot c) = do
  c' <- constraintToSMT c
  return $ sValToSWord <$> S.svNot <$> (sValToSBool <$> c')
constraintToSMT (COr l r) = do
  l' <- constraintToSMT l
  r' <- constraintToSMT r
  return $ sValToSWord <$> (S.svOr
                            <$> (sValToSBool <$> (l'))
                            <*> (sValToSBool <$> (r')))
constraintToSMT (CAnd l r) = do
  l' <- constraintToSMT l
  r' <- constraintToSMT r
  return $ sValToSWord <$> (S.svAnd
                             <$> (sValToSBool <$> (l'))
                             <*> (sValToSBool <$> (r')))
constraintToSMT (CLt l r) = do
  l' <- constraintToSMT l
  r' <- constraintToSMT r
  return $  sValToSWord <$> (S.svLessThan
                             <$> (l')
                             <*> (r'))
constraintToSMT (CAny i) = do
  m <- St.get
  case M.lookup i m of
    Just val -> return val
    Nothing -> do
      let val = ask >>= liftIO . S.svMkSymVar (Just S.EX) (S.KBounded False 32) (Just readableName)
      St.modify (M.insert i val)
      return val
  where readableName = toList . valName $ i

wordToSVal :: Word32 -> S.SVal
wordToSVal w = S.svInteger (S.KBounded False 32) (toInteger w)

sValToSBool :: S.SVal -> S.SVal
sValToSBool w = w `S.svNotEqual` (wordToSVal 0)

sValToSWord :: S.SVal -> S.SVal
sValToSWord w = S.svIte w (wordToSVal 1) (wordToSVal 0) 

renderSMTResult :: S.SMTResult -> String
renderSMTResult (S.Unsatisfiable _) = "Unsatisfiable"
renderSMTResult s@(S.Satisfiable _ _) = renderDict $ M.mapKeys fromList $ S.getModelDictionary s
renderSMTResult _ = "Error"

renderSolvedState :: SolvedState -> String
renderSolvedState (SolvedState (pc,_,_,st,cs) c) =
  "PC: " <> show pc <> "\n" <>
  "Stack: " <> show (renderConstraint <$> st) <> "\n" <>
  "Path Constraints: " <> show (renderConstraint <$> cs) <> "\n" <>
  "Solved Values: " <> renderSMTResult c
                    
renderDict :: (Show v) => M.Map String v -> String
renderDict m =
  foldr toStr "" (M.toList m)
  where toStr (k,v) s = k <> ": " <> show v <> ", " <> s

data SolvedState = SolvedState SymState S.SMTResult

solveSym :: Trace -> IO (T.Tree SolvedState)
solveSym (T.Node state@(_, _, _, _, cs) c) = do
  let smtExpr = conjoin (St.evalState (traverse constraintToSMT cs) M.empty)
  S.SatResult smtRes <- S.satWith S.z3 smtExpr
  children <- traverse solveSym c
  return $ T.Node (SolvedState state smtRes) children

conjoin :: [S.Symbolic S.SVal] -> S.Symbolic S.SVal
conjoin (x:xs) = S.svAnd <$> (sValToSBool <$> x) <*> (conjoin xs)
conjoin [] = return S.svTrue
