module SkeletonTemplates.Fold where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

foldActor :: String -> R.Exp -> R.Exp -> R.TwoVarFun -> ImplicitDataflow -> C.Actor
foldActor actorName expState expRhs fun@(R.TwoVarFunC vars1 vars2 exp) dataflow =
  undefined
