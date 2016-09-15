module SkeletonTemplates.Identity where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes

identityActor :: String -> C.Type -> C.Type -> C.Actor
identityActor actorName incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = incomingType
      outType = outgoingType
      inputPattern = C.InPattTagIds (C.Ident "In1") [(C.Ident "x")]
      outputPattern =
        C.OutPattTagIds
          (C.Ident "Out1")
          [C.OutTokenExp (C.EIdent (C.Ident "x"))]
      actionHead = C.ActnHead [inputPattern] [outputPattern]
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []

identity2To1Actor :: String -> C.Actor
identity2To1Actor actorName =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1"), C.PortDcl inType (C.Ident "In2")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = mkIntType 16
      outType = mkIntType 16
      inputPattern1 = C.InPattTagIds (C.Ident "In1") [(C.Ident "x")]
      inputPattern2 = C.InPattTagIds (C.Ident "In2") [(C.Ident "y")]
      outputPattern1 =
        C.OutPattTagIds
          (C.Ident "Out1")
          [C.OutTokenExp (C.EIdent (C.Ident "x"))]
      actionHead = C.ActnHead [inputPattern1, inputPattern2] [outputPattern1]
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []

identity1To2Actor :: String -> C.Actor
identity1To2Actor actorName =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [ C.PortDcl outType (C.Ident "Out1")
          , C.PortDcl outType (C.Ident "Out2")
          ]
      inType = mkIntType 16
      outType = mkIntType 16
      inputPattern =
        C.InPattTagIds (C.Ident "In1") [(C.Ident "x"), (C.Ident "y")]
      outputPattern1 =
        C.OutPattTagIds
          (C.Ident "Out1")
          [C.OutTokenExp (C.EIdent (C.Ident "x"))]
      outputPattern2 =
        C.OutPattTagIds
          (C.Ident "Out2")
          [C.OutTokenExp (C.EIdent (C.Ident "y"))]
      actionHead = C.ActnHead [inputPattern] [outputPattern1, outputPattern2]
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []

consumerActor :: String -> C.Actor
consumerActor actorName =
  let ioSig = C.IOSg [C.PortDcl inType (C.Ident "In1")] []
      inType = mkIntType 16
      inputPattern = C.InPattTagIds (C.Ident "In1") [(C.Ident "x")]
      actionHead = C.ActnHead [inputPattern] []
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []
