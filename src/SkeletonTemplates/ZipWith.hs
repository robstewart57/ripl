module SkeletonTemplates.ZipWith where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes

zipWithActor :: String -> R.ManyVarFun -> C.Type -> C.Type -> C.Actor
zipWithActor actorName (R.ManyVarFunC identExps exp) incomingType outgoingType =
  let ioSig =
        C.IOSg
          (map
             (\i -> C.PortDcl inType (C.Ident ("In" ++ show i)))
             [1 .. length identExps])
          -- [ C.PortDcl inType (C.Ident "In1")
          -- , C.PortDcl inType (C.Ident "In2")
          -- ]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = incomingType
      outType = outgoingType
      inputPattern =
        map
          (\(i, ident) ->
             C.InPattTagIds (C.Ident ("In" ++ show i)) [idRiplToCal ident])
          (zip
             [1 .. length identExps]
             (map
                (\(R.ExpSpaceSepC (R.ExprVar (R.VarC ident))) -> ident)
                identExps))
      -- [ C.InPattTagIds (C.Ident "In1") [idRiplToCal id1]
      -- , C.InPattTagIds (C.Ident "In2") [idRiplToCal id2]
      -- ]
      outputPattern = riplExpToOutputPattern exp
      actionHead = C.ActnHead inputPattern [outputPattern]
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts (C.ActnTagDecl [C.Ident "zipWith"]) actionHead []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []
