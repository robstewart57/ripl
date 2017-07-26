module SkeletonTemplates.Map where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

mapActor :: String -> R.AnonFunElemUnary -> C.Type -> C.Type -> ImplicitDataflow -> (C.Actor)
mapActor actorName fun@(R.AnonFunElemUnaryC var exp) calTypeIncoming calTypeOutgoing dataflow =
  let ports =
        -- C.IOSg
          ( [C.PortDcl inType (C.Ident "In1")]
          , [C.PortDcl outType (C.Ident "Out1")])
      inType = calTypeIncoming
      outType = calTypeOutgoing
      inputPattern = riplVarToInputPattern var
      outputPattern = riplExpToOutputPattern (R.ExprListExprs (R.ExprListC [exp]))
      actionHead = C.ActnHead [inputPattern] [outputPattern]
      action =
        ("the_action"
         , C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                [])))
      preloads = processGlobalVars dataflow fun
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

actor :: [(C.GlobalVarDecl,C.PortDecl,(String,C.CodeBlock))] -> (String,C.CodeBlock) -> String -> ([C.PortDecl],[C.PortDecl]) -> (C.Actor)
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
       (map (\(globarVar,_,_) -> globarVar) preloads)
       ([riplAction] ++ map snd (map (\(_,_,act) -> act) preloads))
       schedule
       []
  where
    schedule = C.SchedfsmFSM (C.Ident "s0") (transLoad ++ riplActionTrans (length transLoad))
    transLoad = map (\(i,(name,action)) -> C.StTrans (C.Ident ("s" ++ show i)) (C.Ident name) (C.Ident ("s" ++ show (i+1)))) (zip [0,1..] (map (\(_,_,act) -> act) preloads))

    -- TODO: this has to reset to "s0" once the action has consumed
    -- its payload
    riplActionTrans i = [C.StTrans (C.Ident ("s" ++ show i)) (C.Ident riplActionName) (C.Ident ("s" ++ show (i)))]

processGlobalVar :: ImplicitDataflow -> R.Ident -> (C.GlobalVarDecl,C.PortDecl,(String,C.CodeBlock))
processGlobalVar varLookup ident@(R.Ident identStr) =
  let varNode = fromJust (Map.lookup ident varLookup)
      Dimension w h = fromJust (dim varNode)

      varDecl = C.GlobVarDecl (C.VDecl (calIntType 32) (idRiplToCal ident) [(C.BExp (C.LitExpCons (C.IntLitExpr (C.IntegerLit (w*h)))))])
      portDecl = C.PortDcl (calIntType 32) (C.Ident (idRiplShow ident ++ "Port"))
      inputPattern = C.InPattTagIdsRepeat (C.Ident (idRiplShow ident ++ "Port")) [C.Ident ("data_" ++ idRiplShow ident)] (C.RptClause (C.LitExpCons (C.IntLitExpr (C.IntegerLit (w*h)))))
      actionHead = C.ActnHead [inputPattern] []

      consumeLoop =
        C.EndSeparatedStmt
        (C.ForEachStt
         (C.ForeachStmtsSt
         [C.ForeachGen (calIntType 32) (C.Ident "i") (C.BEList (intCalExp 0) (intCalExp ((w*h)-1)))]
           [C.SemiColonSeparatedStmt
            (C.AssignStt
             (C.AssStmtIdx
              (C.Ident identStr)
              (C.Idx [C.BExp (C.EIdent (C.Ident "i"))])
              (C.EIdentArr (C.Ident ("data_" ++ identStr)) [C.BExp (C.EIdent (C.Ident "i"))])))]))

      action =
        ("load_" ++ idRiplShow ident
        , C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident ("load_" ++ "lut")])
                actionHead
                [consumeLoop])))

  in (varDecl,portDecl,action)

processGlobalVars ::
  ImplicitDataflow ->
  R.AnonFunElemUnary ->
  [(C.GlobalVarDecl -- to contain the data to be preloaded
  , C.PortDecl      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVars dataflow fun@(R.AnonFunElemUnaryC var exp) =
  let globalIds = globalIdentsElemUnary fun
  in map (processGlobalVar dataflow) globalIds

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
