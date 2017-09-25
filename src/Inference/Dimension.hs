module Inference.Dimension where

import qualified AbsCAL as C
import qualified AbsRIPL as R
import Types
import AstMappings
import Data.Maybe
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Debug.Trace

dimensionOfRHSId :: R.AssignSkelRHS -> ImplicitDataflow -> Dimension
dimensionOfRHSId (R.ScanSkel rhsId _ _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.SplitXSkel _ rhsId) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.SplitYSkel _ rhsId) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.FoldSkel _ (R.ExprVar (R.VarC rhsId)) _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.FoldScalarSkel rhsId _ _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.FoldVectorSkel rhsId _ _ _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.Stencil1DSkel rhsId _ _ _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
dimensionOfRHSId (R.Stencil2DSkel rhsId _ _ _) dfMap =
  let varNode = fromJust (Map.lookup rhsId dfMap)
  in fromJust (dim varNode)
-- dimensionOfRHSId (R.ConvolveSkel rhsId _ _ _) dfMap =
--   let varNode = fromJust (Map.lookup rhsId dfMap)
--   in fromJust (dim varNode)
-- dimensionOfRHSId (R.IUnzipFilter2DSkel rhsId _ _ _ _) dfMap =
--   let varNode = fromJust (Map.lookup rhsId dfMap)
--   in fromJust (dim varNode)

inferDimension :: Dimension
               -- -> Direction
               -- -> Direction
               -> R.AssignSkelRHS
               -> [Dimension]
inferDimension dim@(Dim2 w h) {- incomingDirection dir -} rhs =
  case rhs of
    (R.MapSkel _ fun) ->
      [Dim2 w h]
    (R.Stencil1DSkel _ _ _ _) -> [dim]
    (R.Stencil2DSkel _ _ _ _) -> [dim]
    (R.ScanSkel identRHS _ _) -> [dim] -- Dimension 1 1
    (R.FoldSkel exps _ _) ->
      case exps of
        R.ExprTuple stateExps ->
          map stateVarDimension stateExps
        e@(R.ExprGenArray{}) ->
          [stateVarDimension e]
      where
        stateVarDimension (R.ExprGenArray (R.ExprTuple dimensions)) =
          case length dimensions of
            1 -> Dim1 (expToInt (dimensions !! 0))
            2 -> Dim2 (expToInt (dimensions !! 0)) (expToInt (dimensions !! 1))
            3 -> Dim3 (expToInt (dimensions !! 0)) (expToInt (dimensions !! 1)) (expToInt (dimensions !! 2))
        expToInt (R.ExprInt i) = i
    (R.FoldScalarSkel _ _ _) -> [Dim2 1 1]
    (R.FoldVectorSkel _ vectorLength _ _) -> [Dim2 vectorLength 1]
    (R.ZipWithSkel _ (R.ManyVarFunC _ (R.ExprListExprs (R.ExprListC exps)))) ->
      [Dim2 (w * fromIntegral (length exps)) h]
    R.ZipWithSkel {} -> [dim]
    R.ZipWithScalarSkel {} -> [dim]
    -- R.ZipWithVectorSkel {} -> dim
    -- TODO evaluate 2nd argument (an exp) to an int,
    -- rather than assuming that the exp is just an int expression.
    (R.ScaleSkel (R.ExprInt wScale) (R.ExprInt hScale) _) -> [Dim2 (w*wScale) (h*hScale)]
    (R.SplitXSkel _ _) -> [Dim2 (round (fromIntegral w/2)) (h)]
    (R.SplitYSkel _ _) -> [Dim2 (w) (round (fromIntegral h/2))]
    _ -> error ("dimension inference unsupported for skeleton: " ++ show rhs)

{-
inOutRatio :: R.AnonFunDiscreteUnary -> Direction -> (Integer, Integer)
inOutRatio (R.AnonFunDiscreteUnaryC (R.VarListC ls) (R.ExprListC ys)) dir =
  let ratio =
        round
          (((fromIntegral (length ys)) :: Double) /
           ((fromIntegral (length ls)) :: Double))
  in case dir of
       Rowwise -> (ratio, 1)
       Columnwise -> (ratio, 1) -- (1,ratio)
inOutRatio (R.AnonFunDiscreteUnaryC (R.VarListC ls) (R.ExprRepeatTokensC (R.ExprInt n) var)) dir =
  let ratio = n
  in case dir of
       Rowwise -> (ratio, 1)
       Columnwise -> (ratio, 1) -- (1,ratio)
inOutRatio funPattern dir =
  error ("unsupported pattern in inOutRatio: " ++ show funPattern)
-}
