module SkeletonTemplates.FoldScalar where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import SkeletonTemplates.CalTypes
import Types

foldScalarActor :: String
                -> Integer
                -> Dimension
                -> R.TwoVarFun
                -> C.Type
                -> C.Type
                -> C.Actor
foldScalarActor actorName initValue (Dimension w h) (R.TwoVarFunC pixelIdent stateIdent exp) incomingType outgoingType =
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
        [ C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (idRiplToCal stateIdent) (expRiplToCal exp)))
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
            [C.OutTokenExp (C.EIdent (C.Ident "outVar"))]
        ]
      outputStateActionGuard =
        C.BrExpCons
          (C.BEEQ
             (C.EIdent (C.Ident "tokenCount"))
             (C.EIdent (C.Ident "streamSize")))
      outputStateActionHead =
        C.ActnHeadGuardedVars
          outputStateInPattern
          outputStateOutPattern
          [outputStateActionGuard]
          (C.LocVarsDecl
             [ C.LocVarDecl
                 (C.VDeclExpMut (intCalType 16) (C.Ident "outVar") [] (mkInt 0))
             ])
      outputStateBody =
        [ C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (C.Ident "tokenCount") (intCalExp 0)))
        , C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt (C.Ident "outVar") (C.EIdent (idRiplToCal stateIdent))))
        , C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (idRiplToCal stateIdent)
                  (C.EIdent (C.Ident "initValue"))))
        ]
      outputStateAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "outputState"])
                outputStateActionHead
                outputStateBody))
      priorityBlock =
        [C.PriOrd [C.PriInEQ (C.Ident "fold") (C.Ident "outputState") []]]
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
            (C.VDeclExpMut
               (intCalType 16)
               (idRiplToCal stateIdent)
               []
               (C.EIdent (C.Ident "initValue")))
        , C.GlobVarDecl
            (C.VDeclExpMut (uintCalType 32) (C.Ident "tokenCount") [] (mkInt 0))
        ]
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       globalVars
       [foldAction, outputStateAction]
       priorityBlock
