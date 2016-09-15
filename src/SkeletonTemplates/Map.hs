module SkeletonTemplates.Map where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

mapActor :: String -> R.AnonFunDiscreteUnary -> C.Type -> C.Type -> C.Actor
mapActor actorName (R.AnonFunDiscreteUnaryC varList exp) calTypeIncoming calTypeOutgoing =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = calTypeIncoming
      outType = calTypeOutgoing
      inputPattern = riplVarListToInputPattern varList
      outputPattern = riplExpToOutputPattern (R.ExprListExprs exp)
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
