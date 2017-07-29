module SkeletonTemplates.ZipWithVector where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

zipWithVectorActor
  :: String
  -> R.TwoVarFun
  -> Dimension
  -> Dimension
  -> C.Type
  -> C.Type
  -> C.Type
  -> C.Actor
zipWithVectorActor actorName (R.TwoVarFunC pixelIdent stateIdent exp) (Dimension width height) vecDim@(Dimension vecWidth _) incomingType1 incomingType2 outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType1 (C.Ident "In1"), C.PortDcl inType2 (C.Ident "In2")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType1 = incomingType1
      inType2 = incomingType2
      outType = outgoingType
      inputPattern =
        [ C.InPattTagIds (C.Ident "In1") [idRiplToCal pixelIdent]
        , C.InPattTagIds (C.Ident "In2") [C.Ident "vectorVal"]
        ]
      outputPattern = riplExpToOutputPattern exp
      actionHead = C.ActnHead inputPattern [outputPattern]
      actions =
        [ receiveVectorAction vecWidth height pixelIdent stateIdent
        , zipStreamAction width height exp pixelIdent
        , doneAction width height
        ]
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       (globalVars stateIdent vecWidth)
       actions
       fsmSchedule
       []

-- schedule fsm  s0 :
-- s0 (receiveVector) --> s0;
-- s0 (zipStream) --> s1;
-- s1 (zipStream) --> s1;
-- s1 (done) --> s0;
-- end
fsmSchedule :: C.ActionSchedule
fsmSchedule =
  C.SchedfsmFSM
    (C.Ident "s0")
    [ C.StTrans (C.Ident "s0") (C.Ident "receiveVector") (C.Ident "s0")
    , C.StTrans (C.Ident "s0") (C.Ident "zipStream") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "zipStream") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "done") (C.Ident "s0")
    ]

globalVars stateIdent vectorLength =
  [ C.GlobVarDecl
      (C.VDeclExpMut
         (intCalType 16)
         (idRiplToCal stateIdent)
         [C.BExp (mkInt vectorLength)]
         (initVectorState 0 vectorLength))
  , C.GlobVarDecl (C.VDeclExpMut (intCalType 32) (C.Ident "count") [] (mkInt 0))
  , C.GlobVarDecl
      (C.VDeclExpMut (intCalType 32) (C.Ident "vectorCount") [] (mkInt 0))
  , C.GlobVarDecl
      (C.VDeclExpMut
         (intCalType 32)
         (C.Ident "vectorLength")
         []
         (mkInt vectorLength))
  ]

-- receiveVector: action In1:[x] ==>
-- guard (vectorCount < vectorLength)
-- do vectorValue[vectorCount] := x;
--    vectorCount := vectorCount + 1;
-- end
receiveVectorAction width height pixelIdent (R.Ident stateIdent) =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts
          (C.ActnTagDecl [C.Ident "receiveVector"])
          actionHead
          statements))
  where
    actionHead =
      C.ActnHeadGuarded
        [C.InPattTagIds (C.Ident "In2") [idRiplToCal pixelIdent]]
        []
        guardExps
    statements =
      [ varSetExpIdxExp
          (stateIdent)
          (C.EIdent (idRiplToCal pixelIdent))
          [mkVar "vectorCount"]
      , varIncr "vectorCount"
      ]
    guardExps =
      [C.BrExpCons (C.BELT (mkVar "vectorCount") (mkVar "vectorLength"))]

--    statements = [ C.SemiColonSeparatedStmt (C.AssignStt (C.AssStmt (C.Ident "vectorValue") (C.EIdent (C.Ident "x")))) ]
-- zipStream: action In2:[p] ==> Out1:[vectorValue[p]]
-- guard count < (imageWidth * imageHeight)
--       , vectorCount = vectorLength
-- do
-- count := count + 1;
-- end
zipStreamAction width height exp pixelIdent =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts
          (C.ActnTagDecl [C.Ident "zipStream"])
          actionHead
          statements))
  where
    actionHead =
      C.ActnHeadGuarded
        [C.InPattTagIds (C.Ident "In1") [idRiplToCal pixelIdent]]
        [riplExpToOutputPattern exp]
        guard
    guard =
      [ C.BELT (C.EIdent (C.Ident "count")) (mkInt (width * height))
      , C.BEEQ (mkVar "vectorCount") (mkVar "vectorLength")
      ]
    statements = [varIncr "count"]

-- done: action ==>
-- guard count = (imageWidth * imageHeight)
-- do count := 0;
--    vectorCount := 0;
-- end
doneAction width height =
  C.ActionCode
    (C.AnActn
       (C.ActnTagsStmts (C.ActnTagDecl [C.Ident "done"]) actionHead statements))
  where
    actionHead = C.ActnHeadGuarded [] [] guard
    guard = [C.BEEQ (C.EIdent (C.Ident "count")) (mkInt (width * height))]
    statements = [varSetInt "vectorCount" 0, varSetInt "count" 0]

initVectorState initValue vectorLength =
  (C.LstExpCons
     (C.ListComp
        (C.ListExpGen
           [mkInt initValue]
           (C.GenExpr
              [ C.GeneratorExpr
                  (mkUIntType 16)
                  (C.Ident "i")
                  (C.BEList (mkInt 0) (mkInt (vectorLength - 1)))
              ]))))
