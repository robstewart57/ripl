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
               -> Direction
               -> Direction
               -> R.AssignSkelRHS
               -> Dimension
inferDimension dim@(Dimension w h) incomingDirection dir rhs =
  case rhs of
    (R.MapSkel _ fun) ->
      Dimension w h
      -- let (widthRatio, heightRatio) = (w,h)
      -- in Dimension (w * widthRatio) (h * heightRatio)
    -- (R.ImapSkel _ _) -> dim
    -- (R.TransposeSkel _)
    --  -> Dimension h w
    -- -- TODO this.
    -- (R.AppendSkel ident1 ident2) ->
    --   case dir of
    --     Rowwise -> Dimension 1 1
    --     Columnwise -> Dimension 1 1
    -- -- this is a simple inference of unzip for now.
    -- (R.IUnzipSkel _ _ _) -> Dimension (round ((fromInteger w) / 2.0)) h
    -- -- TODO: massive hack, implement this properly
    -- (R.UnzipSkel _ (R.AnonFunC _ (R.ExprsBracketed exps))) ->
    --   Dimension (round ((fromInteger w) / fromIntegral (length exps))) h
    -- (R.ConvolveSkel _ _ _ _) -> dim
    (R.Stencil1DSkel _ _ _ _) -> dim
    (R.Stencil2DSkel _ _ _ _) -> dim
    -- (R.IUnzipFilter2DSkel _ _ _ _ _) -> -- dim
    --   Dimension w (round ((fromInteger h) / 2.0))
    (R.ScanSkel identRHS _ _) -> dim -- Dimension 1 1
    (R.FoldScalarSkel _ _ _) -> Dimension 1 1
    (R.FoldVectorSkel _ vectorLength _ _) -> Dimension vectorLength 1
    -- (R.RepeatSkel _ exp) -> Dimension (riplExpToInt exp) 1
    (R.ZipWithSkel _ (R.ManyVarFunC _ (R.ExprListExprs (R.ExprListC exps)))) ->
      Dimension (w * fromIntegral (length exps)) h
    R.ZipWithSkel {} -> dim
    R.ZipWithScalarSkel {} -> dim
    -- R.ZipWithVectorSkel {} -> dim
    -- TODO evaluate 2nd argument (an exp) to an int,
    -- rather than assuming that the exp is just an int expression.
    (R.ScaleSkel (R.ExprInt wScale) (R.ExprInt hScale) _) -> Dimension (w*wScale) (h*hScale)
    (R.SplitXSkel _ _) -> Dimension (round (fromIntegral w/2)) (h)
    (R.SplitYSkel _ _) -> Dimension (w) (round (fromIntegral h/2))
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
