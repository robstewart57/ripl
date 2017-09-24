module SkeletonTemplates.Map where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

foldActor :: String -> Exp -> R.TwoVarFun -> ImplicitDataflow -> C.Actor
foldActor actorName expRhs fun@(R.TwoVarFunC vars exp)o dataflow =
  undefined
