module SkeletonTemplates.ZipWithScalar where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

zipWithScalarActor :: String
                   -> R.TwoVarFun
                   -> Dimension
                   -> C.Type
                   -> C.Type
                   -> C.Actor
zipWithScalarActor actorName (R.TwoVarFunC ident1 ident2 exp) (Dimension width height) incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1"), C.PortDcl inType (C.Ident "In2")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = incomingType
      outType = outgoingType
      inputPattern =
        let streamIdent = ident1 -- head identExps
        in [ C.InPattTagIds (C.Ident "In1") [C.Ident "scalarVal"]
           , C.InPattTagIds (C.Ident "In2") [idRiplToCal streamIdent]
           ]
      outputPattern = riplExpToOutputPattern exp
      actionHead = C.ActnHead inputPattern [outputPattern]
      actions =
        [ receiveScalarAction
        , zipStreamAction width height exp
        , doneAction width height
        ]
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       globalVars
       actions
       fsmSchedule
       []

fsmSchedule :: C.ActionSchedule
fsmSchedule =
  C.SchedfsmFSM
    (C.Ident "s0")
    [ C.StTrans (C.Ident "s0") (C.Ident "receiveScalar") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "zipStream") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "done") (C.Ident "s0")
    ]

globalVars =
  [ C.GlobVarDecl
      (C.VDeclExpMut (intCalType 16) (C.Ident "scalarValue") [] (mkInt 0))
  , C.GlobVarDecl (C.VDeclExpMut (intCalType 32) (C.Ident "count") [] (mkInt 0))
  ]

receiveScalarAction =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts
          (C.ActnTagDecl [C.Ident "receiveScalar"])
          actionHead
          statements))
  where
    actionHead = C.ActnHead [C.InPattTagIds (C.Ident "In1") [(C.Ident "x")]] []
    statements =
      [ C.SemiColonSeparatedStmt
          (C.AssignStt
             (C.AssStmt (C.Ident "scalarValue") (C.EIdent (C.Ident "x"))))
      ]

zipStreamAction width height exp =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts
          (C.ActnTagDecl [C.Ident "zipStream"])
          actionHead
          statements))
  where
    actionHead =
      C.ActnHeadGuarded
        [C.InPattTagIds (C.Ident "In2") [(C.Ident "x")]]
        [riplExpToOutputPattern exp]
        guard
    guard = [C.BELT (C.EIdent (C.Ident "count")) (mkInt (width * height))]
    statements =
      [ C.SemiColonSeparatedStmt
          (C.AssignStt
             (C.AssStmt
                (C.Ident "count")
                (C.BEAdd (C.EIdent (C.Ident "count")) (mkInt 1))))
      ]

doneAction width height =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts (C.ActnTagDecl [C.Ident "done"]) actionHead statements))
  where
    actionHead = C.ActnHeadGuarded [] [] guard
    guard = [C.BEEQ (C.EIdent (C.Ident "count")) (mkInt (width * height))]
    statements =
      [ C.SemiColonSeparatedStmt
          (C.AssignStt (C.AssStmt (C.Ident "count") (mkInt 0)))
      ]
