module SkeletonTemplates.Unzip
  ( unzipActor
  ) where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import SkeletonTemplates.Identity
import Inference.Dimension

unzipActor :: String -> R.AnonFun -> C.Actor
unzipActor actorName (R.AnonFunC identExps anonFunExp) =
  let [R.ExpSpaceSepC (R.ExprListExprs (R.ExprListC expsOut))] = identExps
      ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          (map
             (\i -> C.PortDcl outType (C.Ident ("Out" ++ show i)))
             [1 .. length expsOut])
      inType = mkIntType 16
      outType = mkIntType 16
      inputPattern =
        C.InPattTagIds
          (C.Ident "In1")
          (map (\(R.ExprVar (R.VarC (R.Ident ident))) -> C.Ident ident) expsOut)
      -- (map (\(R.ExpSpaceSepC (R.ExprVar (R.VarC (R.Ident ident)))) -> C.Ident ident) identExps)
      outputPattern =
        let R.ExprsBracketed tupleOutput = anonFunExp
        in map
             (\(i, (R.ExprVar (R.VarC (R.Ident ident)))) ->
                C.OutPattTagIds
                  (C.Ident ("Out" ++ show i))
                  [C.OutTokenExp (C.EIdent (C.Ident ident))])
             (zip [1 ..] tupleOutput)
      actionHead = C.ActnHead [inputPattern] outputPattern
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
