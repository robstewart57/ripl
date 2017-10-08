module AstMappings where

import Data.List

import qualified AbsRIPL as R
import qualified PrintRIPL as R
import qualified AbsCAL as C
import Types
import Debug.Trace

idsFromRHS :: R.AssignSkelRHS -> [R.Ident]
idsFromRHS (R.MapSkel ident _) = [ident]
idsFromRHS (R.FoldSkel stateExp ident _) = [ident]
-- idsFromRHS (R.ImapSkel ident _) = [ident]
-- idsFromRHS (R.UnzipSkel ident _) = [ident]
-- idsFromRHS (R.IUnzipSkel ident _ _) = [ident]
-- idsFromRHS (R.TransposeSkel ident) = [ident]
-- idsFromRHS (R.AppendSkel ident1 ident2) = [ident1, ident2]
idsFromRHS (R.ScanSkel ident _ _) = [ident]
idsFromRHS (R.SplitXSkel _ ident) = [ident]
idsFromRHS (R.SplitYSkel _ ident) = [ident]
idsFromRHS (R.FoldScalarSkel ident _ _) = [ident]
idsFromRHS (R.FoldVectorSkel ident _ _ _) = [ident]
-- idsFromRHS (R.ConvolveSkel ident _ _ _) = [ident]
idsFromRHS (R.Stencil1DSkel ident _ _ _) = [ident]
idsFromRHS (R.Stencil2DSkel ident _ _ _) = [ident]
-- idsFromRHS (R.IUnzipFilter2DSkel ident _ _ _ _) = [ident]
-- idsFromRHS (R.RepeatSkel ident _) = [ident]
idsFromRHS (R.ZipWithSkel idents _) =
  concatMap (\(R.IdentSpaceSepC x) ->
         case x of
           R.IdentsOneId ident -> [ident]
           R.IdentsManyIds idents -> idents) idents
idsFromRHS (R.ZipWithScalarSkel (R.ExprVar (R.VarC ident1)) ident2 _) =
  map (\ident -> ident) [ident1,ident2]
-- idsFromRHS (R.ZipWithVectorSkel idents _) =
--   map (\(R.IdentSpaceSepC ident) -> ident) idents
idsFromRHS (R.ScaleSkel _ _ ident) = [ident]
idsFromRHS rhs =
  error ("unsupported RHS in AstMappings.idsFromRHS: " ++ show rhs)

idsFromExp :: R.Exp -> [R.Ident]
idsFromExp (R.ExprIndexedVector ident e) =
  ident : idsFromExp e
idsFromExp (R.ExprTuple exps) =
  concatMap idsFromExp exps
idsFromExp (R.ExprVar (R.VarC ident)) =
  [ident]
idsFromExp (R.ExprDiv e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprMul e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprAdd e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprMinus e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprIfThenElse e1 e2 e3) =
  idsFromExp e1 ++ idsFromExp e2 ++ idsFromExp e3
idsFromExp (R.ExprGT e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprGTE e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprLT e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprLTE e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprEq e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprMin e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprMax e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprInt i) =
  []
idsFromExp (R.ExprVectorMod ident (R.ExprListC es) _) =
  concatMap idsFromExp es

idsFromExp e = error ("idsFromExp doesn't support: " ++ (show e))

inputArgs  :: R.Idents -> [R.Ident]
inputArgs idents
  = case idents of
       R.IdentsOneId ident -> [ident]
       R.IdentsManyIds ids -> ids

inputArgCount  :: R.Idents -> Int
inputArgCount idents
  = case idents of
       R.IdentsOneId ident -> 1
       R.IdentsManyIds ids -> length ids

outputArgs :: R.Exp -> [R.Exp]
outputArgs (R.ExprTuple exps) = exps
outputArgs e = [e]

outputArgCount :: R.Exp -> Int
outputArgCount (R.ExprTuple exps) = length exps
outputArgCount _ = 1

globalIdentsElemUnary :: R.OneVarFun -> [R.Ident]
globalIdentsElemUnary (R.OneVarFunC idents exp)
  = ((nub (idsFromExp exp)) \\
    (case idents of
       R.IdentsOneId ident -> [ident]
       R.IdentsManyIds ids -> ids))

globalIdentsElemBinary :: R.TwoVarFun -> [R.Ident]
globalIdentsElemBinary (R.TwoVarFunC idents1 idents2 exp)
  = ((nub (idsFromExp exp)) \\
    ((case idents1 of
       R.IdentsOneId ident -> [ident]
       R.IdentsManyIds ids -> ids)
     ++
     case idents2 of
       R.IdentsOneId ident -> [ident]
       R.IdentsManyIds ids -> ids))



-- TODO: deprecate in favour of idsFromRHS?
idFromRHS :: R.AssignSkelRHS -> R.Ident
idFromRHS rhs = head (idsFromRHS rhs)

-- idFromRHS (R.MapSkel ident _) = ident
-- idFromRHS (R.ImapSkel ident _) = ident
-- idFromRHS (R.IUnzipSkel ident _ _) = ident
-- idFromRHS (R.TransposeSkel ident) = ident
-- idFromRHS (R.TransposeSkel ident) = ident
idToString :: R.Ident -> String
idToString (R.Ident s) = s

guardFromDimension comparator ident dim =
  comparator
  (C.EIdent (C.Ident (ident ++ "_count")))
  multExp
  where
    multExp =
      case dim of
        (Dim1 w) ->
          mkInt w
        (Dim2 w h) ->
          (C.BEMult (mkInt w) (mkInt h))
        (Dim3 w h z) ->
          (C.BEMult (C.BEMult (mkInt w) (mkInt h)) (mkInt z))

guardFromDimensionEQ :: String -> Dimension -> C.Exp
guardFromDimensionEQ = guardFromDimension C.BEEQ

guardFromDimensionLT :: String -> Dimension -> C.Exp
guardFromDimensionLT = guardFromDimension C.BELT

mkInt' i = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))

expRiplToCal :: R.Exp -> C.Exp
expRiplToCal (R.ExprInt i) = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))
expRiplToCal (R.ExprVar (R.VarC (R.Ident ident))) = (C.EIdent (C.Ident ident))
expRiplToCal (R.ExprMod e1 e2) = C.BEMod (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprAdd e1 e2) = C.BEAdd (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprMinus e1 e2) = C.BENeg (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprMul e1 e2) = C.BEMult (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprDiv e1 e2) = C.BEDiv (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprTuple [e1]) = C.BrExpCons (expRiplToCal e1)
expRiplToCal (R.ExprIndex (R.IndexPlus i)) =
  C.LitExpCons (C.IntLitExpr (C.IntegerLit i))
expRiplToCal (R.ExprIndex (R.IndexMinus i)) =
  C.LitExpCons (C.IntLitExpr (C.IntegerLit (-i)))
expRiplToCal (R.ExprIndexHere) = C.LitExpCons (C.IntLitExpr (C.IntegerLit 0)) -- TODO: implement
expRiplToCal (R.ExprIndexedVector (R.Ident ident) idxExp) =
  C.IndSExpCons (C.IndExpr (C.Ident ident) (C.BExp (expRiplToCal idxExp)))
expRiplToCal (R.ExprListExprs ls) =
  error ("ExprList unsupported in expRiplToCal: " ++ show ls)
expRiplToCal (R.ExprAbs e1) =
  C.IfExpCons
    (C.IfExpr
       (C.BEGTE (expRiplToCal e1) (mkInt' 0))
       (expRiplToCal e1)
       (C.UENeg ((expRiplToCal e1))))
expRiplToCal (R.ExprMin e1 e2) =
  C.IfExpCons
    (C.IfExpr
       (C.BELT (expRiplToCal e1) (expRiplToCal e2))
       (expRiplToCal e1)
       (expRiplToCal e2))
expRiplToCal (R.ExprMax e1 e2) =
  C.IfExpCons
    (C.IfExpr
       (C.BEGT (expRiplToCal e1) (expRiplToCal e2))
       (expRiplToCal e1)
       (expRiplToCal e2))
expRiplToCal (R.ExprIfThenElse e1 e2 e3) =
  C.IfExpCons (C.IfExpr (expRiplToCal e1) (expRiplToCal e2) (expRiplToCal e3))
expRiplToCal (R.ExprGT e1 e2) = C.BEGT (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprGTE e1 e2) = C.BEGTE (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprLT e1 e2) = C.BELT (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprLTE e1 e2) = C.BELTE (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprEq e1 e2) = C.BEEQ (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprShiftR e1 e2) =
  C.BEBSRight (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprShiftL e1 e2) =
  C.BEBSLeft (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprTuple [e1]) = C.BrExpCons (expRiplToCal e1)
expRiplToCal something =
  error $ "Unsupported exp in expRiplToCal: " ++ show something

-- a special expression for foldVector, e.g.
-- (\pixel hist -> hist[pixel]++)
--
-- hist[pixel]++ is not an expression, it is a statement:
--     hist[pixel] := hist[pixel] + 1;
--
-- ExprVectorMod (Ident "hist") (ExprVar (VarC (Ident "pixel"))) VectorModIncr
calExpToStmt :: R.Exp -> C.SemiColonSeparatedStatement
calExpToStmt (R.ExprVectorMod vectorIdent indexExps elementModifier) =
  C.AssignStt
    (C.AssStmtIdx
       (idRiplToCal vectorIdent)
       (C.Idx (map C.BExp index))
       rhsExp)
  where
    index =
      map
      (\riplExp -> ((expRiplToCal riplExp)))
      (let R.ExprListC exps = indexExps in exps)
    rhsExp =
      case elementModifier of
        R.VectorModIncr ->
          C.BEAdd
          (C.EIdentArr (idRiplToCal vectorIdent) (map C.BExp index))
          (mkInt 1)
        R.VectorModDecr ->
          C.BENeg
          (C.IdBrSExpCons (idRiplToCal vectorIdent)
          index)
          (mkInt 1)

riplVarToInputPattern vars =
  map (\(ident,portNum) ->
         -- let R.Ident identStrs = ident
         -- in
           C.InPattTagIds (C.Ident ("In" ++ show portNum ++ "_" ++ show 1)) [idRiplToCal ident]
      )
  (zip
    (case vars of
        R.IdentsOneId ident -> [ident]
        R.IdentsManyIds idents -> idents)
    [1..])

riplVarListToInputPattern vars =
  let identStrs = vars -- map (\(R.VarC (R.Ident s)) -> s) vars
  in C.InPattTagIds (C.Ident "In1") (map C.Ident identStrs)

riplExpToOutputPattern exp =
  map (\(exp,portNum) ->
         C.OutPattTagIds
         (C.Ident("Out" ++ show portNum ++ "_" ++ show 1))
         [C.OutTokenExp (expRiplToCal exp)]
      )
  (zip (case exp of
          (R.ExprTuple exps) -> exps
          e -> [e])
   [1..])

  -- let outExpTokens =
  --       case exp of
  --         R.ExprListExprs (R.ExprListC exps) ->
  --           let calExps = map expRiplToCal exps
  --           in map C.OutTokenExp calExps
  --         R.ExprListExprs (R.ExprRepeatTokensC (R.ExprInt n) exp) ->
  --           let calExp = expRiplToCal exp
  --           in replicate (fromInteger n) (C.OutTokenExp calExp)
  --         _ ->
  --           let calExp = expRiplToCal exp
  --           in [C.OutTokenExp calExp]
  -- in C.OutPattTagIds (C.Ident "Out1") outExpTokens

zeroExp = C.LitExpCons (C.IntLitExpr (C.IntegerLit 0))

intCalExp i = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))

riplExpToInt (R.ExprInt i) = i
riplExpToInt (R.ExprIntNeg i) = -i
riplExpToInt (R.ExprMul e1 e2) = riplExpToInt e1 * riplExpToInt e2
riplExpToInt (R.ExprTuple [e1]) = riplExpToInt e1
riplExpToInt e = error ("unsupported exp in calExpToInt: " ++ show e)

arrayType = C.TypParam C.TList []

uintType = C.TypParam C.TUint [C.TypeAttrSizeDf (intCalExp 8)]

intType x = C.TypParam C.TInt [C.TypeAttrSizeDf (intCalExp x)]

identCalExp s = C.EIdent (C.Ident s)

strToCalExp :: String -> C.Exp
strToCalExp = read

idRiplToCalExp (R.Ident id) = C.EIdent (C.Ident id)

idRiplToCal (R.Ident id) = C.Ident id

idCalToRipl (C.Ident id) = R.Ident id

idRiplShow (R.Ident id) = id

idCalShow (C.Ident id) = id

concatIDs (R.Ident id1) (R.Ident id2) = R.Ident $ id1 ++ id2

interleaveIDs (R.Ident id1) s (R.Ident id2) = R.Ident $ id1 ++ s ++ id2

prefixCalId s (C.Ident id) = C.Ident (s ++ id)

postfixRiplId (R.Ident id) s = R.Ident (id ++ s)

boolCalType = C.TypNonParam C.TBool

uintCalType i =
  C.TypParam
    C.TUint
    [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]

intCalType i =
  C.TypParam
    C.TInt
    [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]

intTypeParamSize i =
  C.TypParam
    C.TUint
    [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]

varNot var =
  C.SemiColonSeparatedStmt
    (C.AssignStt
       (C.AssStmt (C.Ident var) (C.UENot (C.EIdent (C.Ident var)))))

varIncr var =
  C.SemiColonSeparatedStmt
    (C.AssignStt
       (C.AssStmt (C.Ident var) (C.BEAdd (C.EIdent (C.Ident var)) (mkInt 1))))

varSetInt var intVal =
  C.SemiColonSeparatedStmt
    (C.AssignStt (C.AssStmt (C.Ident var) (mkInt intVal)))

varSetExp var exp =
  C.SemiColonSeparatedStmt (C.AssignStt (C.AssStmt (C.Ident var) exp))

varSetExpIdx var exp indexNumbers =
  C.SemiColonSeparatedStmt
    (C.AssignStt (C.AssStmtIdx (C.Ident var) indexes exp))
  where
    indexes = C.Idx $ map (\i -> C.BExp (mkInt i)) indexNumbers

varSetExpIdxExp var exp indexExps =
  C.SemiColonSeparatedStmt
    (C.AssignStt (C.AssStmtIdx (C.Ident var) indexes exp))
  where
    indexes = C.Idx $ map (\e -> C.BExp e) indexExps

arrayUpdate arrayName idx rhsVar =
  C.SemiColonSeparatedStmt
    (C.AssignStt
       (C.AssStmtIdx
          (C.Ident arrayName)
          (C.Idx [(C.BExp (C.EIdent (C.Ident idx)))])
          (C.EIdent (C.Ident rhsVar))))

arrayExpUpdate arrayName idxExp rhsVar =
  C.SemiColonSeparatedStmt
    (C.AssignStt
       (C.AssStmtIdx
          (C.Ident arrayName)
          (C.Idx [idxExp])
          (C.EIdent (C.Ident rhsVar))))

mkVar s = C.EIdent (C.Ident s)

mkBool True = C.LitExpCons C.TrueLitExpr
mkBool False = C.LitExpCons C.FalseLitExpr

mkInt i = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))

mkUIntType i =
  C.TypParam
    C.TUint
    [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]

mkIntType i =
  C.TypParam
    C.TInt
    [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]
