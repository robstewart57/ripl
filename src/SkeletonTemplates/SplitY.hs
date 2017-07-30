module SkeletonTemplates.SplitY where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import SkeletonTemplates.CalTypes
import Types

splitYActor :: String
          -> Dimension
          -> Integer
          -> C.Ident
          -> C.Type
          -> C.Type
          -> C.Actor
splitYActor actorName (Dimension width height) splitFactor rhsIdent incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1"),C.PortDcl outType (C.Ident "Out2")]
      inType = incomingType
      outType = outgoingType
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       (globalVars width splitFactor)
       [ rowAction "row1Action" "Out1" incomingType outgoingType
       , rowAction "row2Action" "Out2" incomingType outgoingType
       , consumeAction incomingType outgoingType
       , receiveRowDoneAction
       , sendRowDoneAction
       ]
       fsmSchedule
       [] -- priorityBlock

consumeAction incomingType outgoingType =
  action
  where
   inType = incomingType
   outType = outgoingType
   inPattern =
     [C.InPattTagIds
      (C.Ident "In1")
      [C.Ident "x"]]

   outPattern = []
   outActionHead =
        C.ActnHeadGuarded
          inPattern
          outPattern
          [guard]
   guard = C.BELT
           (C.EIdent (C.Ident "consumed"))
           (C.BEMult (C.EIdent (C.Ident "width")) (C.EIdent (C.Ident ("splitFactor"))))
   action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "consumeRows"])
                outActionHead
                stmts))
   stmts =
     [ C.SemiColonSeparatedStmt
       (C.AssignStt
        (C.AssStmtIdx
         (C.Ident "rows")
         (C.Idx
          [C.BExp (C.EIdent (C.Ident "consumed"))])
         (C.EIdent (C.Ident "x"))))
     , varIncr "consumed"
     ]


receiveRowDoneAction  =
  action
  where
   inPattern  = []
   outPattern = []
   outActionHead =
        C.ActnHeadGuarded
          inPattern
          outPattern
          [guard]
   guard = C.BEEQ
           (C.EIdent (C.Ident "consumed"))
           (C.BEMult (C.EIdent (C.Ident "width")) (C.EIdent (C.Ident ("splitFactor"))))
   action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "receiveRowDone"])
                outActionHead
                stmts))
   stmts =
     [ varSetInt "consumed" 0
     ]

sendRowDoneAction  =
  action
  where
   inPattern  = []
   outPattern = []
   outActionHead =
        C.ActnHeadGuarded
          inPattern
          outPattern
          [guard]
   guard = C.BEEQ
           (C.EIdent (C.Ident "outputted"))
           (C.BEMult (C.EIdent (C.Ident "width")) (C.EIdent (C.Ident ("splitFactor"))))
   action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "sendRowDone"])
                outActionHead
                stmts))
   stmts =
     [ varSetInt "outputted" 0
     ]


globalVars width splitFactor =
  [ C.GlobVarDecl
    (C.VDeclExpIMut
     (mkIntType 32)
     (C.Ident "width")
     []
     (mkInt width))
  , C.GlobVarDecl
    (C.VDeclExpIMut
     (mkIntType 32)
     (C.Ident "splitFactor")
     []
     (mkInt splitFactor))
  , C.GlobVarDecl
    (C.VDeclExpMut
     (mkIntType 32)
     (C.Ident "outputted")
     []
     (mkInt 0))
  , C.GlobVarDecl
    (C.VDeclExpMut
     (mkIntType 32)
     (C.Ident "consumed")
     []
     (mkInt 0))
  , C.GlobVarDecl
    (C.VDecl
     (mkIntType 32)
     (C.Ident "rows")
     [C.BExp
      (C.BEMult
       (C.EIdent (C.Ident "width"))
       (C.EIdent (C.Ident "splitFactor"))
      )])
  ]

rowAction actionName outputPortName incomingType outgoingType =
  action
  where
   inType = incomingType
   outType = outgoingType
   inPattern =
        []
   outPattern =
     [C.OutPattTagIds
      (C.Ident outputPortName)
      [C.OutTokenExp
       (C.EIdentArr
        (C.Ident "rows")
        [C.BExp (C.EIdent (C.Ident "outputted"))])]]
   outActionHead =
        C.ActnHeadGuarded
          inPattern
          outPattern
          [guard]
   guard = C.BELT
           (C.EIdent (C.Ident "outputted"))
           (C.BEMult (C.EIdent (C.Ident "width")) (C.EIdent (C.Ident ("splitFactor"))))
   action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident actionName])
                outActionHead
                stmts))
   stmts =
     [ varIncr "outputted"
     ]


fsmSchedule :: C.ActionSchedule
fsmSchedule =
  C.SchedfsmFSM
    (C.Ident "s0")
    [ C.StTrans (C.Ident "s0") (C.Ident "consumeRows") (C.Ident "s0")
    , C.StTrans (C.Ident "s0") (C.Ident "receiveRowDone") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "row1Action") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "sendRowDone") (C.Ident "s2")
    , C.StTrans (C.Ident "s2") (C.Ident "consumeRows") (C.Ident "s2")
    , C.StTrans (C.Ident "s2") (C.Ident "receiveRowDone") (C.Ident "s3")
    , C.StTrans (C.Ident "s3") (C.Ident "row2Action") (C.Ident "s3")
    , C.StTrans (C.Ident "s3") (C.Ident "sendRowDone") (C.Ident "s0")
    ]

priorityBlock =
  [ C.PriOrd
    [ C.PriInEQ (C.Ident "consumeRows") (C.Ident "row1Action") []
    , C.PriInEQ (C.Ident "consumeRows") (C.Ident "row2Action") []
    ]
  ]
