module SkeletonTemplates.Repeat where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

repeatActor :: String -> Integer -> C.Actor
repeatActor actorName numRepetitions =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = mkIntType 16
      outType = mkIntType 16
      resetInPattern = [C.InPattTagIds (C.Ident "In1") [C.Ident "x"]]
      resetOutPattern = []
      resetGuard =
        C.BrExpCons
          (C.BEEQ
             (C.EIdent (C.Ident "tokenCount"))
             (C.EIdent (C.Ident "streamSize")))
      resetActionHead =
        C.ActnHeadGuarded resetInPattern resetOutPattern [resetGuard]
      resetActionBody =
        [ C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (C.Ident "val") (C.EIdent (C.Ident "x"))))
        , C.SemiColonSeparatedStmt
            (C.AssignStt (C.AssStmt (C.Ident "tokenCount") (intCalExp 0)))
        ]
      resetAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "reset"])
                resetActionHead
                resetActionBody))
      sendInPattern = []
      sendOutPattern =
        [ C.OutPattTagIds
            (C.Ident "Out1")
            [C.OutTokenExp (C.EIdent (C.Ident "val"))]
        ]
      sendGuard =
        C.BrExpCons
          (C.BELT
             (C.EIdent (C.Ident "tokenCount"))
             (C.EIdent (C.Ident "streamSize")))
      sendActionHead =
        C.ActnHeadGuarded sendInPattern sendOutPattern [sendGuard]
      sendActionBody =
        [ C.SemiColonSeparatedStmt
            (C.AssignStt
               (C.AssStmt
                  (C.Ident "tokenCount")
                  (C.BEAdd (C.EIdent (C.Ident "tokenCount")) (intCalExp 1))))
        ]
      sendAction =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "send"])
                sendActionHead
                sendActionBody))
      priorityBlock =
        [C.PriOrd [C.PriInEQ (C.Ident "send") (C.Ident "reset") []]]
      globalVars =
        [ C.GlobVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "streamSize")
               []
               (mkInt numRepetitions))
        , C.GlobVarDecl (C.VDecl (intCalType 16) (C.Ident "val") [])
        , C.GlobVarDecl
            (C.VDeclExpMut
               (intCalType 32)
               (C.Ident "tokenCount")
               []
               (mkInt numRepetitions))
        ]
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       globalVars
       [resetAction, sendAction]
       priorityBlock
