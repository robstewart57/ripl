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
inferColour rhs =
  case rhs of
    R.MapSkel _ fun ->
      Chan3
