module SkeletonTemplates.FoldVector where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import SkeletonTemplates.CalTypes
import Types

foldVectorActor
  :: String
  -> Integer
  -> Integer
  -> Dimension
  -> R.AnonFunBinary
  -> C.Type
  -> C.Type
  -> C.Actor
foldVectorActor actorName vectorLength initValue (Dimension w h) (R.AnonFunBinaryC pixelIdent stateIdent exp) incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = incomingType
      outType = outgoingType
      foldActionInPattern =
        [C.InPattTagIds (C.Ident "In1") [idRiplToCal pixelIdent]]
      foldActionOutPattern = []
      foldActionGuard =
        C.BrExpCons
          (C.BELT
             (C.EIdent (C.Ident "tokenCount"))
             (C.EIdent (C.Ident "streamSize")))
      foldActionHead =
        C.ActnHeadGuarded
          foldActionInPattern
          foldActionOutPattern
          [foldActionGuard]
      foldActionBody =
        [ C.SemiColonSeparatedStmt (calExpToStmt exp)
        , C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (C.Ident "tokenCount")
                  (C.BEAdd (C.EIdent (C.Ident "tokenCount")) (intCalExp 1))))
        ]
      foldAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "fold"])
                foldActionHead
                foldActionBody))
      outputStateInPattern = []
      outputStateOutPattern =
        [ C.OutPattTagIds
            (C.Ident "Out1")
            [ C.OutTokenExp
                (C.IndSExpCons
                   (C.IndExpr
                      (idRiplToCal stateIdent)
                      (C.BExp (C.EIdent (C.Ident "tokensSent")))))
            ]
        ]
      outputStateActionGuards =
        [ C.BrExpCons
            (C.BEEQ
               (C.EIdent (C.Ident "tokenCount"))
               (C.EIdent (C.Ident "streamSize")))
        , C.BrExpCons
            (C.BELT
               (C.EIdent (C.Ident "tokensSent"))
               (C.EIdent (C.Ident "vectorLength")))
        ]
      outputStateActionHead =
        C.ActnHeadGuarded
          outputStateInPattern
          outputStateOutPattern
          outputStateActionGuards
      outputStateBody = [varIncr "tokensSent"]
      outputStateAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "outputState"])
                outputStateActionHead
                outputStateBody))
      outputDoneInPattern = []
      outputDoneOutPattern = []
      outputDoneActionGuards =
        [ C.BrExpCons
            (C.BEEQ
               (C.EIdent (C.Ident "tokenCount"))
               (C.EIdent (C.Ident "streamSize")))
        , C.BrExpCons
            (C.BEEQ
               (C.EIdent (C.Ident "tokensSent"))
               (C.EIdent (C.Ident "vectorLength")))
        ]
      outputDoneActionHead =
        C.ActnHeadGuarded
          outputDoneInPattern
          outputDoneOutPattern
          outputDoneActionGuards
      outputDoneBody =
        [ varSetInt "tokensSent" 0
        , varSetInt "tokenCount" 0
        , varSetExp
            (idRiplShow stateIdent)
            (initVectorState initValue vectorLength)
        ]
      outputDoneAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "outputDone"])
                outputDoneActionHead
                outputDoneBody))
      priorityBlock =
        [ C.PriOrd
            [ C.PriInEQ (C.Ident "fold") (C.Ident "outputState") []
            , C.PriInEQ (C.Ident "fold") (C.Ident "outputDone") []
            ]
        ]
      globalVars =
        [ C.GlobVarDecl
            (C.VDeclExpIMut
               (uintCalType 32)
               (C.Ident "streamSize")
               []
               (mkInt (w * h)))
        , C.GlobVarDecl
            (C.VDeclExpIMut
               (intCalType 16)
               (C.Ident "initValue")
               []
               (mkInt initValue))
        , C.GlobVarDecl
            (C.VDeclExpIMut
               (intCalType 16)
               (C.Ident "vectorLength")
               []
               (mkInt vectorLength))
        , C.GlobVarDecl
            (C.VDeclExpMut
               (intCalType 32)
               (idRiplToCal stateIdent)
               [C.BExp (mkInt vectorLength)]
               (initVectorState initValue vectorLength))
          -- (C.LstExpCons (C.ListComp (C.ListExpGen [mkVar "initValue"] (C.GenExpr [C.GeneratorExpr (mkUIntType 16) (C.Ident "i") (C.BEList (mkInt 0) (mkInt vectorLength))])))))
        , C.GlobVarDecl
            (C.VDeclExpMut (uintCalType 32) (C.Ident "tokenCount") [] (mkInt 0))
        , C.GlobVarDecl
            (C.VDeclExpMut (uintCalType 16) (C.Ident "tokensSent") [] (mkInt 0))
        ]
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       globalVars
       [foldAction, outputStateAction, outputDoneAction]
       priorityBlock

--                        , varSetExp (idRiplShow stateIdent) (C.LstExpCons (C.ListComp (C.ListExpGen [mkVar "initValue"] (C.GenExpr [C.GeneratorExpr (mkUIntType 16) (C.Ident "i") (C.BEList (mkInt 0) (mkInt vectorLength))]))))
initVectorState :: Integer -> Integer -> C.Exp
-- Explicit list version
-- initVectorState initValue vectorLength = C.LstExpCons (C.ListComp (C.ListExp (map mkInt (replicate (fromIntegral vectorLength) initValue))))
-- List comprehension version
initVectorState initValue vectorLength =
  (C.LstExpCons
     (C.ListComp
        (C.ListExpGen
           [mkVar "initValue"]
           (C.GenExpr
              [ C.GeneratorExpr
                  (mkUIntType 16)
                  (C.Ident "i")
                  (C.BEList (mkInt 0) (mkInt (vectorLength - 1)))
              ]))))
