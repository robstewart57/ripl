module Inference.Colour where

import qualified AbsCAL as C
import qualified AbsRIPL as R
import Types
import AstMappings
import Data.Maybe
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Debug.Trace

-- TODO: implement
inferColour :: R.AssignSkelRHS
               -> Chans
inferColour rhs = evalExp e
  where
    e = case rhs of
          R.MapSkel _ (R.OneVarFunC ids exp) -> exp
          R.ZipWithSkel _ (R.ManyVarFunC _ exp) -> exp
          R.FoldSkel _ _ (R.TwoVarFunC _ _ exp) -> exp
    evalExp exp = case outputArgCount exp of
        3 -> Chan3
        1 -> Chan1
        _ -> error "illegal tuple return value"
