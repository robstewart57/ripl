module SkeletonTemplates.Iunzip
  ( iunzipActor
  ) where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import SkeletonTemplates.Identity
import Inference.Offset
import Types

iunzipActor :: String
            -> R.AnonFunIndexed
            -> R.AnonFunIndexed
            -> Dimension
            -> C.Actor
iunzipActor actorName (R.AnonFunIndexedC anonFun1) (R.AnonFunIndexedC anonFun2) dim =
  actor
  where
    actor =
      C.ActrSchd
        pathName
        imports
        (C.Ident actorName)
        []
        ioSig
        globalVars
        actions
        actionSchedule
        priorities
    actions =
      [ initAction initialMidPoint
      , streamAction "streamFun1" "Out1" anonFun1
      , streamAction "streamFun2" "Out2" anonFun2
      , streamActionEnd "streamEndFun1" ["Out1", "Out2"] anonFun1 dim
      , streamActionEnd "streamEndFun2" ["Out1", "Out2"] anonFun2 dim
      ]
    pathName = C.PathN [C.PNameCons (C.Ident "cal")]
    imports = []
    inType = mkIntType 16
    outType = mkIntType 16
    ioSig =
      C.IOSg
        [C.PortDcl inType (C.Ident "In1")]
        [C.PortDcl outType (C.Ident "Out1"), C.PortDcl outType (C.Ident "Out2")]
    globalVars =
      [ consumed
      , localBufferSize
      , localMidPoint
      , localCircularBufferVar
      , localMaxLookAhead
      ]
    actionSchedule =
      C.SchedfsmFSM
        (C.Ident "s0")
        [ C.StTrans (C.Ident "s0") (C.Ident "firstNPixels") (C.Ident "s1")
        , C.StTrans (C.Ident "s1") (C.Ident "streamFun1") (C.Ident "s2")
        , C.StTrans (C.Ident "s2") (C.Ident "streamFun2") (C.Ident "s1")
        , C.StTrans (C.Ident "s1") (C.Ident "streamEndFun1") (C.Ident "s0")
        , C.StTrans (C.Ident "s2") (C.Ident "streamEndFun2") (C.Ident "s0")
        ]
    priorities =
      [ C.PriOrd
          [ C.PriInEQ (C.Ident "streamEndFun1") (C.Ident "streamFun1") []
          , C.PriInEQ (C.Ident "streamEndFun2") (C.Ident "streamFun2") []
          ]
      ]
    -- TODO: generalise to examine range of both functions
    maxAhead = maxLookAhead anonFun1
    maxBack = maxLookBack anonFun2
    bufSize = maxAhead + maxBack + 1
    initialMidPoint = round (fromInteger (bufSize - 1) / 2.0) :: Integer
    consumed =
      C.GlobVarDecl
        (C.VDeclExpMut (mkIntType 32) (C.Ident "consumed") [] (mkInt 0))
    localBufferSize =
      C.GlobVarDecl
        (C.VDeclExpIMut (mkIntType 8) (C.Ident "bufferSize") [] (mkInt bufSize))
    localMidPoint =
      C.GlobVarDecl
        (C.VDeclExpMut
           (mkUIntType 8)
           (C.Ident "midPoint")
           []
           (mkInt initialMidPoint))
    localMaxLookAhead =
      C.GlobVarDecl
        (C.VDeclExpIMut
           (mkUIntType 8)
           (C.Ident "maxLookAhead")
           []
           (mkInt maxAhead))
    localCircularBufferVar =
      C.GlobVarDecl
        (C.VDeclExpMut
           (mkIntType 16)
           (C.Ident "circularBuffer")
           [C.BExp (mkInt bufSize)]
           (C.LstExpCons
              (C.ListComp
                 (C.ListExp (replicate (fromInteger bufSize) ((mkInt 0)))))))

initAction countToMidPoint =
  C.ActionCode (C.AnActn (C.ActnTagsStmts actionTagDecl actionHead stmts))
  where
    actionTagDecl = C.ActnTagDecl [(C.Ident "firstNPixels")]
    actionHead = C.ActnHead [inputPattern] []
    inputPattern =
      C.InPattTagIds
        (C.Ident "In1")
        (map (\i -> C.Ident ("t" ++ show i)) [1 .. countToMidPoint + 1])
    stmts =
      C.SemiColonSeparatedStmt
        (C.AssignStt
           (C.AssStmt
              (C.Ident "consumed")
              (C.BEAdd
                 (C.EIdent (C.Ident "consumed"))
                 (mkInt (countToMidPoint + 1))))) :
      map
        (\i ->
           C.SemiColonSeparatedStmt
             (C.AssignStt
                (C.AssStmtIdx
                   (C.Ident "circularBuffer")
                   (C.Idx [(C.BExp (mkInt (i - 1)))])
                   (C.EIdent (C.Ident ("t" ++ show i))))))
        [1 .. countToMidPoint + 1]

streamAction actionTag portToWriteTo exp =
  C.ActionCode (C.AnActn (C.ActnTagsStmts actionTagDecl actionHead stmts))
  where
    actionTagDecl = C.ActnTagDecl [(C.Ident actionTag)]
    actionHead = C.ActnHeadVars [inputPattern] [outputPattern] localVar
    localVar =
      C.LocVarsDecl [C.LocVarDecl (C.VDecl (mkIntType 16) (C.Ident "out") [])]
    inputPattern = C.InPattTagIds (C.Ident "In1") [C.Ident "t1"]
    outputPattern =
      C.OutPattTagIds
        (C.Ident portToWriteTo)
        [C.OutTokenExp (C.EIdent (C.Ident "out"))]
    stmts = [consumeTokenStmt, getOutStmt, modifyMidPointStmt, consumedIncrStmt]
      where
        consumeTokenStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmtIdx
                  (C.Ident "circularBuffer")
                  (C.Idx
                     [ (C.BExp
                          (C.BEMod
                             (C.BEAdd
                                (C.EIdent (C.Ident "midPoint"))
                                (C.EIdent (C.Ident "maxLookAhead")))
                             (C.EIdent (C.Ident "bufferSize"))))
                     ])
                  (C.EIdent (C.Ident "t1"))))
        getOutStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (C.Ident "out") (indexedExpr exp)))
        modifyMidPointStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (C.Ident "midPoint")
                  (C.BEMod
                     (C.BrExpCons
                        (C.BEAdd
                           (C.EIdent (C.Ident "midPoint"))
                           (C.LitExpCons (C.IntLitExpr (C.IntegerLit 1)))))
                     (C.EIdent (C.Ident "bufferSize")))))
        consumedIncrStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (C.Ident "consumed")
                  (C.BEAdd (C.EIdent (C.Ident "consumed")) (mkInt 1))))
    indexedExpr (R.ExprAdd e1 e2) = C.BEAdd (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprMinus e1 e2) = C.BENeg (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprDiv e1 e2) = C.BEDiv (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftR e1 e2) =
      C.BEBSRight (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftL e1 e2) =
      C.BEBSLeft (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprInt e1) = C.LitExpCons (C.IntLitExpr (C.IntegerLit e1))
    indexedExpr (R.ExprMin e1 e2) =
      C.IfExpCons
        (C.IfExpr
           (C.BELT (indexedExpr e1) (indexedExpr e2))
           (indexedExpr e1)
           (indexedExpr e2))
    indexedExpr (R.ExprMax e1 e2) =
      C.IfExpCons
        (C.IfExpr
           (C.BEGT (indexedExpr e1) (indexedExpr e2))
           (indexedExpr e1)
           (indexedExpr e2))
    indexedExpr (R.ExprIndex (R.IndexPlus i)) =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [ (C.BExp
             (C.BEMod
                (C.BEAdd
                   (C.EIdent (C.Ident "midPoint"))
                   (C.LitExpCons (C.IntLitExpr (C.IntegerLit i))))
                (C.EIdent (C.Ident "bufferSize"))))
        ]
    indexedExpr (R.ExprIndex (R.IndexMinus i)) =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [ (C.BExp
             (C.IfExpCons
                (C.IfExpr
                   (C.BEGTE
                      (C.BENeg (C.EIdent (C.Ident "midPoint")) (mkInt i))
                      (mkInt 0))
                   (C.BENeg (C.EIdent (C.Ident "midPoint")) (mkInt i))
                   (C.BENeg (C.EIdent (C.Ident "bufferSize")) (mkInt i)))))
        ]
    indexedExpr R.ExprIndexHere =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [(C.BExp (C.EIdent (C.Ident "midPoint")))]
    indexedExpr (R.ExprBracketed e1) = C.BrExpCons (indexedExpr e1)
    indexedExpr e = error ("unsupported exp in unzip actor: " ++ show e)

streamActionEnd actionTag portsToWriteTo exp (Dimension width height) =
  C.ActionCode (C.AnActn (C.ActnTagsStmts actionTagDecl actionHead stmts))
  where
    actionTagDecl = C.ActnTagDecl [(C.Ident actionTag)]
    actionHead = C.ActnHeadGuardedVars [] outputPattern guardExps localVar
    guardExps =
      [C.BEEQ (C.EIdent (C.Ident "consumed")) (mkInt (width * height))]
    localVar =
      C.LocVarsDecl [C.LocVarDecl (C.VDecl (mkIntType 16) (C.Ident "out") [])]
    outputPattern =
      map
        (\portToWriteTo ->
           C.OutPattTagIds
             (C.Ident portToWriteTo)
             [C.OutTokenExp (C.EIdent (C.Ident "out"))])
        portsToWriteTo
    stmts = [getOutStmt, modifyMidPointStmt]
      where
        getOutStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (C.Ident "out") (indexedExpr exp)))
        modifyMidPointStmt =
          C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (C.Ident "midPoint")
                  (C.BEMod
                     (C.BrExpCons
                        (C.BEAdd
                           (C.EIdent (C.Ident "midPoint"))
                           (C.LitExpCons (C.IntLitExpr (C.IntegerLit 1)))))
                     (C.EIdent (C.Ident "bufferSize")))))
    indexedExpr (R.ExprAdd e1 e2) = C.BEAdd (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprMinus e1 e2) = C.BENeg (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprDiv e1 e2) = C.BEDiv (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftR e1 e2) =
      C.BEBSRight (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftL e1 e2) =
      C.BEBSLeft (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprInt e1) = C.LitExpCons (C.IntLitExpr (C.IntegerLit e1))
    indexedExpr (R.ExprMin e1 e2) =
      C.IfExpCons
        (C.IfExpr
           (C.BELT (indexedExpr e1) (indexedExpr e2))
           (indexedExpr e1)
           (indexedExpr e2))
    indexedExpr (R.ExprMax e1 e2) =
      C.IfExpCons
        (C.IfExpr
           (C.BEGT (indexedExpr e1) (indexedExpr e2))
           (indexedExpr e1)
           (indexedExpr e2))
    indexedExpr (R.ExprIndex (R.IndexPlus i)) =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [ (C.BExp
             (C.BEMod
                (C.BEAdd
                   (C.EIdent (C.Ident "midPoint"))
                   (C.LitExpCons (C.IntLitExpr (C.IntegerLit i))))
                (C.EIdent (C.Ident "bufferSize"))))
        ]
    indexedExpr (R.ExprIndex (R.IndexMinus i)) =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [ (C.BExp
             (C.IfExpCons
                (C.IfExpr
                   (C.BEGTE
                      (C.BENeg (C.EIdent (C.Ident "midPoint")) (mkInt i))
                      (mkInt 0))
                   (C.BENeg (C.EIdent (C.Ident "midPoint")) (mkInt i))
                   (C.BENeg (C.EIdent (C.Ident "bufferSize")) (mkInt i)))))
        ]
    indexedExpr R.ExprIndexHere =
      C.EIdentArr
        (C.Ident "circularBuffer")
        [(C.BExp (C.EIdent (C.Ident "midPoint")))]
    indexedExpr (R.ExprBracketed e1) = C.BrExpCons (indexedExpr e1)
    indexedExpr e = error ("unsupported exp in unzip actor: " ++ show e)
