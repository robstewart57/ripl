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
                  C.PortDcl outType (C.Ident ("Out" ++ show i ++ "_" ++ show 1))
               )
            [1 .. outputArgCount exp])

      inType = incomingType
      outType = outgoingType
      inputPattern =
        let f i = zip [1,2..] (let R.IdentSpaceSepC tuple = identExps !! (i-1)
                               in case tuple of
                                    R.IdentsOneId ident -> [ident]
                                    R.IdentsManyIds idents -> idents)
        in
          (concatMap
             (\i ->
                map (\(j,ident) ->
                C.InPattTagIds (C.Ident ("In" ++ show j ++ "_" ++ show i)) [idRiplToCal ident]
                    )
                (f i)
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

      outputPattern =
        map (\(i,exp) ->
               C.OutPattTagIds
               (C.Ident ("Out" ++ show i ++ "_" ++ show 1))
               [C.OutTokenExp (expRiplToCal exp)]
            )
        (zip [1..] (outputArgs exp))
        -- [1 .. outputArgCount exp]
      actionHead = C.ActnHead inputPattern outputPattern
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
