module SkeletonTemplates.Map where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Data.List
import Data.Maybe
import Debug.Trace
import SkeletonTemplates.Common
import SkeletonTemplates.CalTypes
import Types

mapActor :: String -> [R.Ident] -> [R.Ident] -> R.AssignSkelRHS -> VarInfo -> (C.Actor)
mapActor actorName inputs outputs skel@(R.MapSkel identRhs fun@(R.OneVarFunC rhsIdents exp)) dataflow =
  let ports =
        -- ( map (\i -> C.PortDcl inType (C.Ident ("In" ++ show i))) [1 .. portsFromRhsId identRhs dataflow]
        -- , map (\i -> C.PortDcl outType (C.Ident ("Out" ++ show i)))
        -- [1 .. outputArgCount exp])
        ( map (\var -> C.PortDcl (intType 32) (idRiplToCal var)) (inputPorts (R.ExprVar (R.VarC identRhs)) dataflow)
        , map (\name -> C.PortDcl (intType 32) (idRiplToCal name)) outputs)
      inType = intCalType 16
      outType = intCalType 16
      inputPattern = riplVarToInputPattern identRhs rhsIdents
      outputPattern =
        riplExpToOutputPattern outputs exp
      actionHead = C.ActnHead inputPattern outputPattern
      action =
        ("the_action"
         , C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                [])))
      preloads = processGlobalVarsOneVarFunc dataflow fun
  in actor preloads action actorName ports

    -- C.Actr
    --    (C.PathN [C.PNameCons (C.Ident "cal")])
    --    []
    --    (C.Ident actorName)
    --    []
    --    ioSig
    --    globalVarsRHSLoad
    --    ([action] ++ actionsRHSLoad)
    --    []

actor :: [([C.GlobalVarDecl],C.PortDecl,(String,C.CodeBlock))] -> (String,C.CodeBlock) -> String -> ([C.PortDecl],[C.PortDecl]) -> (C.Actor)
actor [] (_,action) actorName (ins,outs) =
  C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       (C.IOSg ins outs)
       []
       [action]
       []

actor preloads (riplActionName,riplAction) actorName (ins,outs) =
  C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       (C.IOSg (ins ++ map (\(_,portDecl,_) -> portDecl) preloads) outs)
       (concatMap (\(globarVars,_,_) -> globarVars) preloads)
       ([riplAction] ++ map snd (map (\(_,_,act) -> act) preloads))
       schedule
       []
  where
    schedule = C.SchedfsmFSM (C.Ident "s0") (transLoad ++ riplActionTrans (length transLoad))
    transLoad = map (\(i,(name,action)) -> C.StTrans (C.Ident ("s" ++ show i)) (C.Ident name) (C.Ident ("s" ++ show (i+1)))) (zip [0,1..] (map (\(_,_,act) -> act) preloads))

    -- TODO: this has to reset to "s0" once the action has consumed
    -- its payload
    riplActionTrans i = [C.StTrans (C.Ident ("s" ++ show i)) (C.Ident riplActionName) (C.Ident ("s" ++ show (i)))]


  --     globalVars = [C.GlobVarDecl (C.VDecl int16Type (C.Ident "lut") [(C.BExp (C.LitExpCons (C.IntLitExpr (C.IntegerLit 100))))])]
  --     portDecl   = [C.PortDcl int16Type (C.Ident "lutPort")]
  --     inputPattern = C.InPattTagIdsRepeat (C.Ident ("lut" ++ "Port")) [C.Ident ("data_" ++ "lut")] (C.RptClause ((C.LitExpCons (C.IntLitExpr (C.IntegerLit 100)))))
  --     actionHead = C.ActnHead [inputPattern] []
  --     consumeAction =
  --       [("load_" ++ "lut"
  --       , C.ActionCode
  --         (C.AnActn
  --            (C.ActnTagsStmts
  --               (C.ActnTagDecl [C.Ident ("load_" ++ "lut")])
  --               actionHead
  --               [])))]
  -- in (globalVars,portDecl,consumeAction)


      -- for any variables within the user defined function, stream
      -- them in first.
      -- globalVars = undefined
        -- step 1: if an identifier is on the RHS of ->, but doesn't
        --         appear as a lambda identifier on the LHS of ->,
        --         assume it is a LHS variable elsewhere in the RIPL
        --         program. So get a list of such variables

        -- step 2: lookup the dimensions for each variable

        -- step 3: if a (Mx1) dimension, create a 1D CAL array of size
        --         N with the name of the global variable. Else for
        --         (MxN) dimensions, create a 2D  CAL array for size
        --         [M][N] with the name of the global variable.

        -- step 4:
