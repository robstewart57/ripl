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
          (concatMap
             (\i ->
                map (\j ->
                C.PortDcl inType (C.Ident ("In" ++ show j ++ "_" ++ show i))
                    )
                [1 .. inputArgCount ((map (\(R.IdentSpaceSepC idents) -> idents) identExps) !! (i-1))]
             )
             [1 .. length identExps])

          -- [C.PortDcl outType (C.Ident "Out1")]
          (map (\i ->
                  C.PortDcl outType (C.Ident ("Out" ++ show i))
               )
            [1 .. outputArgCount exp])

      inType = incomingType
      outType = outgoingType
      inputPattern =
          (concatMap
             (\i ->
                map (\j ->
                C.InPattTagIds (C.Ident ("In" ++ show j ++ "_" ++ show i))
                    )
                [1 .. inputArgCount ((map (\(R.IdentSpaceSepC idents) -> idents) identExps) !! (i-1))]
             )
             [1 .. length identExps])


        -- map
        --   (\(i, ident) ->
        --      C.InPattTagIds (C.Ident ("In" ++ show i)) [idRiplToCal ident])
        --   (zip
        --      [1 .. length identExps]
        --      (map
        --         (\(R.ExpSpaceSepC (R.ExprVar (R.VarC ident))) -> ident)
        --         identExps))

      outputPattern = undefined
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
