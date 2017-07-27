module AstMappings where

import Data.List

import qualified AbsRIPL as R
import qualified PrintRIPL as R
import qualified AbsCAL as C

idsFromRHS :: R.AssignSkelRHS -> [R.Ident]
idsFromRHS (R.MapSkel ident _) = [ident]
-- idsFromRHS (R.ImapSkel ident _) = [ident]
-- idsFromRHS (R.UnzipSkel ident _) = [ident]
-- idsFromRHS (R.IUnzipSkel ident _ _) = [ident]
-- idsFromRHS (R.TransposeSkel ident) = [ident]
-- idsFromRHS (R.AppendSkel ident1 ident2) = [ident1, ident2]
idsFromRHS (R.ScanSkel ident _ _) = [ident]
idsFromRHS (R.FoldScalarSkel ident _ _) = [ident]
idsFromRHS (R.FoldVectorSkel ident _ _ _) = [ident]
-- idsFromRHS (R.ConvolveSkel ident _ _ _) = [ident]
-- idsFromRHS (R.Filter2DSkel ident _ _ _) = [ident]
-- idsFromRHS (R.IUnzipFilter2DSkel ident _ _ _ _) = [ident]
-- idsFromRHS (R.RepeatSkel ident _) = [ident]
idsFromRHS (R.ZipWithSkel idents _) =
  map (\(R.IdentSpaceSepC ident) -> ident) idents
-- idsFromRHS (R.ZipWithScalarSkel idents _) =
--   map (\(R.IdentSpaceSepC ident) -> ident) idents
-- idsFromRHS (R.ZipWithVectorSkel idents _) =
--   map (\(R.IdentSpaceSepC ident) -> ident) idents
idsFromRHS (R.ScaleSkel _ _ ident) = [ident]
idsFromRHS rhs =
  error ("unsupported RHS in AstMappings.idsFromRHS: " ++ show rhs)

idsFromExp :: R.Exp -> [R.Ident]
idsFromExp (R.ExprIndexedVector ident e) =
  ident : idsFromExp e
idsFromExp (R.ExprBracketed e) =
  idsFromExp e
idsFromExp (R.ExprVar (R.VarC ident)) =
  [ident]
idsFromExp (R.ExprDiv e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprMul e1 e2) =
  idsFromExp e1 ++ idsFromExp e2
idsFromExp (R.ExprInt i) =
  []

idsFromExp e = error ("idsFromExp doesn't support: " ++ (show e))

globalIdentsElemUnary :: R.AnonFunElemUnary -> [R.Ident]
globalIdentsElemUnary (R.AnonFunElemUnaryC ident exp)
  = ((idsFromExp exp) \\ [ident])

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

mkInt' i = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))

expRiplToCal :: R.Exp -> C.Exp
expRiplToCal (R.ExprInt i) = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))
expRiplToCal (R.ExprVar (R.VarC (R.Ident ident))) = (C.EIdent (C.Ident ident))
expRiplToCal (R.ExprAdd e1 e2) = C.BEAdd (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprMinus e1 e2) = C.BENeg (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprMul e1 e2) = C.BEMult (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprDiv e1 e2) = C.BEDiv (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprBracketed e1) = C.BrExpCons (expRiplToCal e1)
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
expRiplToCal (R.ExprShiftR e1 e2) =
  C.BEBSRight (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprShiftL e1 e2) =
  C.BEBSLeft (expRiplToCal e1) (expRiplToCal e2)
expRiplToCal (R.ExprBracketed e1) = C.BrExpCons (expRiplToCal e1)
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
calExpToStmt (R.ExprVectorMod vectorIdent indexExp elementModifier) =
  C.AssignStt
    (C.AssStmtIdx
       (idRiplToCal vectorIdent)
       (C.Idx [(C.BExp (expRiplToCal indexExp))])
       rhsExp)
  where
    rhsExp =
      case elementModifier of
        R.VectorModIncr ->
          C.BEAdd
            (C.IndSExpCons
               (C.IndExpr
                  (idRiplToCal vectorIdent)
                  (C.BExp (expRiplToCal indexExp))))
            (mkInt 1)
        R.VectorModDecr ->
          C.BENeg
            (C.IndSExpCons
               (C.IndExpr
                  (idRiplToCal vectorIdent)
                  (C.BExp (expRiplToCal indexExp))))
            (mkInt 1)

riplVarToInputPattern var =
  let R.Ident identStrs = var -- map (\(R.VarC (R.Ident s)) -> s) vars
  in C.InPattTagIds (C.Ident "In1") (map C.Ident [identStrs])

riplVarListToInputPattern (R.VarListC vars) =
  let identStrs = map (\(R.VarC (R.Ident s)) -> s) vars
  in C.InPattTagIds (C.Ident "In1") (map C.Ident identStrs)

riplExpToOutputPattern exp =
  let outExpTokens =
        case exp of
          R.ExprListExprs (R.ExprListC exps) ->
            let calExps = map expRiplToCal exps
            in map C.OutTokenExp calExps
          R.ExprListExprs (R.ExprRepeatTokensC (R.ExprInt n) exp) ->
            let calExp = expRiplToCal exp
            in replicate (fromInteger n) (C.OutTokenExp calExp)
          _ ->
            let calExp = expRiplToCal exp
            in [C.OutTokenExp calExp]
      -- error ("unsupported exp pattern in riplExpToOutputPattern: " ++ show exp)
  in C.OutPattTagIds (C.Ident "Out1") outExpTokens

zeroExp = C.LitExpCons (C.IntLitExpr (C.IntegerLit 0))

intCalExp i = C.LitExpCons (C.IntLitExpr (C.IntegerLit i))

riplExpToInt (R.ExprInt i) = i
riplExpToInt (R.ExprIntNeg i) = -i
riplExpToInt (R.ExprMul e1 e2) = riplExpToInt e1 * riplExpToInt e2
riplExpToInt (R.ExprBracketed e1) = riplExpToInt e1
riplExpToInt e = error ("unsupported exp in calExpToInt: " ++ show e)

arrayType = C.TypParam C.TList []

uintType = C.TypParam C.TUint [C.TypeAttrSizeDf (intCalExp 8)]

identCalExp s = C.EIdent (C.Ident s)

strToCalExp :: String -> C.Exp
strToCalExp = read

idRiplToCalExp (R.Ident id) = C.EIdent (C.Ident id)

idRiplToCal (R.Ident id) = C.Ident id

idCalToRipl (C.Ident id) = R.Ident id

idRiplShow (R.Ident id) = id

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
