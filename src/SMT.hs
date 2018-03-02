module SMT where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Data.Set as S
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import qualified Data.SBV.Dynamic as S

import Types
import Util
import Symbolic

type SValMap = M.Map Int (S.Symbolic S.SVal)

-- | Walk the constraint gathering up the free
-- | variables.
gatherFree :: Sym -> S.Set Sym
gatherFree c@(SAny _) = S.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r) = gatherFree l <> gatherFree r
gatherFree (SNot c) = gatherFree c
gatherFree (SOr l r) = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SLt l r) = gatherFree l <> gatherFree r
gatherFree (SCon _) = mempty

-- | Create an existential word of `i` bits with
-- | the name `name`.
sWordEx :: Int -> String -> S.Symbolic S.SVal
sWordEx i name =  ask >>= liftIO . S.svMkSymVar (Just S.EX) (S.KBounded False i) (Just (toList name))

-- | Create existential SVals for each of CAny's in the input.
createSym :: [Sym] -> S.Symbolic (M.Map Int S.SVal)
createSym cs = do
  pairs <- traverse createSymPair cs
  return $  M.fromList pairs
  where readableName i = valName $ i
        createSymPair (SAny i) = do
          v <- sWordEx 32 (readableName i)
          return (i, v)
        createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a
-- | symbolic value the SMT solver can solve.
-- | Each constraint in the list is conjoined
-- | with the others.
toSMT :: [Sym] -> S.Symbolic S.SVal
toSMT c = do
  let freeVars = gatherFree (foldr SAnd (SCon 1) c)
  sValMap <- createSym (S.toList freeVars)
  smts <- traverse (constraintToSMT sValMap) c
  return $ conjoin smts

constraintToSMT :: M.Map Int S.SVal -> Sym -> S.Symbolic S.SVal
constraintToSMT m (SEq l r) =
  sValToSWord <$> (S.svEqual <$> constraintToSMT m l <*> constraintToSMT m r)
constraintToSMT m (SAdd l r) =
  S.svPlus <$> constraintToSMT m l <*> constraintToSMT m r
constraintToSMT _ (SCon w) =  return $ wordToSVal w
constraintToSMT m (SNot c) =
  let c' = constraintToSMT m c
  in sValToSWord <$> (S.svNot <$> (sValToSBool <$> c'))
constraintToSMT m (SOr l r) =
  let l' = sValToSBool <$> constraintToSMT m l
      r' = sValToSBool <$> constraintToSMT m r
  in
  sValToSWord <$> (S.svOr <$> l' <*> r')
constraintToSMT m (SAnd l r) =
  let l' = sValToSBool <$> constraintToSMT m l
      r' = sValToSBool <$> constraintToSMT m r
  in
    sValToSWord <$> (S.svAnd <$> l' <*> r')
constraintToSMT m (SLt l r) =
  sValToSWord <$> (S.svLessThan <$> constraintToSMT m l <*> constraintToSMT m r)
constraintToSMT m (SAny i) = do
  case M.lookup i m of
    Just val -> return val
    Nothing -> error "Missing symbolic variable."
    
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
  "Stack: " <> show (renderSym <$> st) <> "\n" <>
  "Path Syms: " <> show (renderSym (foldr SAnd (SCon 1) cs)) <> "\n" <>
  "Solved Values: " <> renderSMTResult c
                    
renderDict :: (Show v) => M.Map String v -> String
renderDict m =
  foldr toStr "" (M.toList m)
  where toStr (k,v) s = k <> " = " <> show v <> ", " <> s

data SolvedState = SolvedState SymState S.SMTResult

solveSym :: Trace -> IO (T.Tree SolvedState)
solveSym (T.Node state@(_, _, _, _, cs) c) = do
  let smtExpr = toSMT cs
  S.SatResult smtRes <- S.satWith S.z3 smtExpr
  children <- traverse solveSym c
  return $ T.Node (SolvedState state smtRes) children

conjoin :: [S.SVal] -> S.SVal
conjoin = foldr (S.svAnd . sValToSBool) S.svTrue
