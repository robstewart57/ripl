module Inference.BitWidth where

import qualified AbsCAL as C
import qualified AbsRIPL as R
import Types
import AstMappings
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map
import Data.Word
import Data.Int
import Debug.Trace

-- | list of expressions in the body of a skeleton instance.
--   Finds the worst case (i.e. biggest) unsigned int required
--   in that list of expressions.
maxBitWdth :: [R.Exp] -> ImplicitDataflow -> Int
maxBitWdth exps dfGraph = foldr max 0 (map (bitwidth dfGraph) exps)

-- | returns the worst case bit width for the two expressions
--   combined with the given arithmetic operator.
operatorBitWidthBinary :: ImplicitDataflow
                       -> R.Exp
                       -> R.Exp
                       -> (Int -> Int -> t)
                       -> t
operatorBitWidthBinary dfGraph e1 e2 op =
  let e1BitWidth = bitwidth dfGraph e1
      e2BitWidth = bitwidth dfGraph e2
      worstCaseType = (e1BitWidth `op` e2BitWidth)
  in worstCaseType

-- | returns the worst case bit width for an expression
--   with the given unary operator.
operatorBitWidthUnary dfGraph e1 op =
  let e1BitWidth = bitwidth dfGraph e1
      worstCaseType = (op e1BitWidth)
  in worstCaseType

-- | finds the bitwidth of an existing variable previously
--   defined in a RIPL program.
bitwidth :: ImplicitDataflow -> R.Exp -> Int
bitwidth dfGraph exp@(R.ExprIndexHere) = 1
bitwidth dfGraph exp@(R.ExprIndex e) = 1
-- bitwidth dfGraph exp@(R.ExprBracketed e) = bitwidth dfGraph e
bitwidth dfGraph exp@(R.ExprInt i) = (fromIntegral i)
bitwidth dfGraph exp@(R.ExprVar (R.VarC v)) =
  if isNothing (Map.lookup v dfGraph)
    then 0
    else (fromJust . maxBitWidth . fromJust . Map.lookup v) dfGraph
bitwidth dfGraph exp@(R.ExprIndexedVector v _) =
  (fromJust . maxBitWidth . fromJust . Map.lookup v) dfGraph
bitwidth dfGraph exp@(R.ExprAdd e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (+)
bitwidth dfGraph exp@(R.ExprMinus e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (-)
bitwidth dfGraph exp@(R.ExprMul e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (*)
bitwidth dfGraph exp@(R.ExprMin e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (min)
bitwidth dfGraph exp@(R.ExprShiftR e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (shiftR)
bitwidth dfGraph exp@(R.ExprShiftL e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (shiftL)
bitwidth dfGraph exp@(R.ExprMax e1 e2) =
  operatorBitWidthBinary dfGraph e1 e2 (max)
bitwidth dfGraph exp@(R.ExprAbs e1) = operatorBitWidthUnary dfGraph e1 (abs)
bitwidth dfGraph exp@(R.ExprIfThenElse e1 e2 e3) =
  operatorBitWidthBinary dfGraph e2 e3 (max)
bitwidth dfGraph exp@(R.ExprVectorMod v e modifier) =
  let x = (fromJust . maxBitWidth . fromJust . Map.lookup v) dfGraph
  in x
bitwidth dfGraph exp@(R.ExprDiv e1 e2) =
  let e1BitWidth = bitwidth dfGraph e1
      e2BitWidth = bitwidth dfGraph e2
      worstCaseType = round (fromIntegral e1BitWidth / fromIntegral e2BitWidth)
  in worstCaseType
bitwidth dfGraph exp =
  error ("Inference.bitwidth unsupported expr: " ++ show exp)

-- | for the given constructor, return an integer to be
--   used in the type signature for the corresponding actor.
maxValue :: CalBitWidth -> Int
maxValue bw =
  case bw of
    CalUInt8 -> fromIntegral (maxBound :: Word8)
    CalUInt16 -> fromIntegral (maxBound :: Word16)
    CalUInt32 -> fromIntegral (maxBound :: Word32)
