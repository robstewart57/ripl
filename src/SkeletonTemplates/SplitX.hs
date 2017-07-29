module SkeletonTemplates.SplitX where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import SkeletonTemplates.CalTypes
import Types

splitXActor :: String
          -> Integer
          -> C.Ident
          -> C.Type
          -> C.Type
          -> C.Actor
splitXActor actorName splitFactor rhsIdent incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1"),C.PortDcl outType (C.Ident "Out2")]
      inType = incomingType
      outType = outgoingType
      inPattern =
        if splitFactor > 1
        then [C.InPattTagIdsRepeat (C.Ident "In1") [C.Ident "arr"] (C.RptClause (mkInt splitFactor))]
        else [C.InPattTagIds (C.Ident "In1") [C.Ident "arr"]]
      out1Pattern =
        if splitFactor > 1
        then [C.OutPattTagIdsRepeat (C.Ident "Out1") [C.OutTokenExp (C.EIdent (C.Ident "arr"))] (C.RptClause (mkInt splitFactor))]
        else [C.OutPattTagIds (C.Ident "Out1") [C.OutTokenExp (C.EIdent (C.Ident "arr"))]]
      out2Pattern =
        if splitFactor > 1
        then [C.OutPattTagIdsRepeat (C.Ident "Out2") [C.OutTokenExp (C.EIdent (C.Ident "arr"))] (C.RptClause (mkInt splitFactor))]
        else [C.OutPattTagIds (C.Ident "Out2") [C.OutTokenExp (C.EIdent (C.Ident "arr"))]]
      out1ActionHead =
        C.ActnHead
          inPattern
          out1Pattern
      out2ActionHead =
        C.ActnHead
          inPattern
          out2Pattern
      out1Action =
        C.ActionCode
          (C.AnActn
             (C.ActnTags
                (C.ActnTagDecl [C.Ident "out1Action"])
                out1ActionHead))
      out2Action =
        C.ActionCode
          (C.AnActn
             (C.ActnTags
                (C.ActnTagDecl [C.Ident "out2Action"])
                out2ActionHead))
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [out1Action, out2Action]
       fsmSchedule
       []

fsmSchedule :: C.ActionSchedule
fsmSchedule =
  C.SchedfsmFSM
    (C.Ident "s0")
    [ C.StTrans (C.Ident "s0") (C.Ident "out1Action") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "out2Action") (C.Ident "s0")
    ]
