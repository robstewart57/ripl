module SkeletonTemplates.Imap where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Inference.Offset
import SkeletonTemplates.Identity

imapActor :: String -> R.AnonFunIndexed -> C.Type -> C.Type -> C.Actor
imapActor actorName (R.AnonFunIndexedC anonFunExp) incomingType outgoingType =
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
        [initAction initialMidPoint, streamAction anonFunExp]
        actionSchedule
        []
    pathName = C.PathN [C.PNameCons (C.Ident "cal")]
    imports = []
    inType = incomingType
    outType = outgoingType
    ioSig =
      C.IOSg
        [C.PortDcl inType (C.Ident "In1")]
        [C.PortDcl outType (C.Ident "Out1")]
    globalVars =
      [ localBufferSize
      , localMidPoint
      , localCircularBufferVar
      , localMaxLookAhead
      ]
    actionSchedule =
      C.SchedfsmFSM
        (C.Ident "s0")
        [ C.StTrans (C.Ident "s0") (C.Ident "init") (C.Ident "s1")
        , C.StTrans (C.Ident "s1") (C.Ident "stream") (C.Ident "s1")
        ]
    maxAhead = maxLookAhead anonFunExp
    maxBack = maxLookBack anonFunExp
    bufSize = maxAhead + maxBack + 1
    initialMidPoint = round (fromInteger (bufSize - 1) / 2.0) :: Integer
    localBufferSize =
      C.GlobVarDecl
        (C.VDeclExpIMut (mkUIntType 8) (C.Ident "bufferSize") [] (mkInt bufSize))
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
    actionTagDecl = C.ActnTagDecl [(C.Ident "init")]
    actionHead = C.ActnHead [inputPattern] []
    inputPattern =
      C.InPattTagIds
        (C.Ident "In1")
        (map (\i -> C.Ident ("t" ++ show i)) [1 .. countToMidPoint + 1])
    stmts =
      map
        (\i ->
           C.SemiColonSeparatedStmt
             (C.AssignStt
                (C.AssStmtIdx
                   (C.Ident "circularBuffer")
                   (C.Idx [(C.BExp (mkInt (i - 1)))])
                   (C.EIdent (C.Ident ("t" ++ show i))))))
        [1 .. countToMidPoint + 1]

streamAction exp =
  C.ActionCode (C.AnActn (C.ActnTagsStmts actionTagDecl actionHead stmts))
  where
    actionTagDecl = C.ActnTagDecl [(C.Ident "stream")]
    actionHead = C.ActnHeadVars [inputPattern] [outputPattern] localVar
    localVar =
      C.LocVarsDecl [C.LocVarDecl (C.VDecl (mkIntType 16) (C.Ident "out") [])]
    inputPattern = C.InPattTagIds (C.Ident "In1") [C.Ident "t1"]
    outputPattern =
      C.OutPattTagIds
        (C.Ident "Out1")
        [C.OutTokenExp (C.EIdent (C.Ident "out"))]
    stmts = [consumeTokenStmt, getOutStmt, modifyMidPointStmt]
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
    indexedExpr (R.ExprAdd e1 e2) = C.BEAdd (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprMinus e1 e2) = C.BENeg (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprDiv e1 e2) = C.BEDiv (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftR e1 e2) =
      C.BEBSRight (indexedExpr e1) (indexedExpr e2)
    indexedExpr (R.ExprShiftL e1 e2) =
      C.BEBSLeft (indexedExpr e1) (indexedExpr e2)
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
    indexedExpr e = expRiplToCal e
