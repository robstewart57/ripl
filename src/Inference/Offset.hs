module Inference.Offset where

import qualified AbsCAL as C
import qualified AbsRIPL as R
import Types
import AstMappings
import Data.Maybe
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Debug.Trace

-- directionOfRHS :: R.AssignRHS -> ImplicitDataflow -> Direction
-- directionOfRHS rhs dfGraph = Rowwise
maxLookAhead exp = go 0 exp
  where
    go i (R.ExprIndex (R.IndexPlus j))
      | j > i = j
      | otherwise = i
    go i (R.ExprAdd e1 e2) = maximum [i, maxLookAhead e1, maxLookAhead e2]
    go i (R.ExprMinus e1 e2) = maximum [i, maxLookAhead e1, maxLookAhead e2]
    go i (R.ExprDiv e1 e2) = maximum [i, maxLookAhead e1, maxLookAhead e2]
    go i (R.ExprShiftR e1 e2) = maximum [i, maxLookAhead e1, maxLookAhead e2]
    go i (R.ExprShiftL e1 e2) = maximum [i, maxLookAhead e1, maxLookAhead e2]
    go i (R.ExprBracketed e1) = maximum [i, maxLookAhead e1]
    go i (R.ExprListExprs (R.ExprListC es)) = maximum (i : map maxLookAhead es)
    go i _ = i

maxLookBack exp = go 0 exp
  where
    go i (R.ExprIndex (R.IndexMinus j))
      | j > i = j
      | otherwise = i
    go i (R.ExprAdd e1 e2) = maximum [i, maxLookBack e1, maxLookBack e2]
    go i (R.ExprMinus e1 e2) = maximum [i, maxLookBack e1, maxLookBack e2]
    go i (R.ExprDiv e1 e2) = maximum [i, maxLookBack e1, maxLookBack e2]
    go i (R.ExprShiftR e1 e2) = maximum [i, maxLookBack e1, maxLookBack e2]
    go i (R.ExprShiftL e1 e2) = maximum [i, maxLookBack e1, maxLookBack e2]
    go i (R.ExprBracketed e1) = maximum [i, maxLookBack e1]
    go i (R.ExprListExprs (R.ExprListC es)) = maximum (i : map maxLookBack es)
    go i _ = i
