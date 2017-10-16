module SkeletonTemplates.Fold where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types
import SkeletonTemplates.Common

foldActor :: String -> [R.Ident] -> R.Exp -> R.Exp -> R.TwoVarProc -> VarInfo -> C.Actor
foldActor actorName outputs expState rhsExp fun@(R.TwoVarProcC vars1 vars2 stmts) dataflow =
  let ports =
        (
          -- case countInputPorts rhsExp of
          --  0 -> []
          -- n -> map (\i -> C.PortDcl (intType 32) (C.Ident ("In" ++ show i))) [1 .. n]

          map (\var -> C.PortDcl (intType 32) (idRiplToCal var)) (inputPorts rhsExp dataflow)

        -- , map (\i -> C.PortDcl (intType 32) (C.Ident ("Out" ++ show i))) [1 .. countNumStates expState])
        -- ( map (\name -> C.PortDcl (intType 32) (idRiplToCal name)) (inputPortNames rhsExp)
        -- , map (\name -> C.PortDcl (intType 32) (idRiplToCal name)) (outputPortNames expState))
        , map (\name -> C.PortDcl (intType 32) (idRiplToCal name)) outputs)

      countNumStates (R.ExprTuple xs) = length xs
      countNumStates _ = 1

      -- (foldedOverDimension,_) =
      --   case rhsExp of
      --     R.ExprVar (R.VarC rhsId) ->
      --       -- dimensionOfVar rhsId dataflow
      --       fromJust $ Map.lookup rhsId dataflow
      --     R.ExprRangeArray rangeExp ->
      --       dimensionFromTuple rangeExp

      (foldedOverCountVarName,foldedOverDimension) =
        case rhsExp of
          R.ExprVar (R.VarC (R.Ident rhsId)) ->
            (rhsId,fst ( fromJust (Map.lookup (R.Ident rhsId) dataflow)))
          R.ExprRangeArray rangeExp ->
            (actorName ++ "_range" , fst (dimensionFromTuple rangeExp))
      actions =
        [
        -- consumeAction rhsId
          (if length (inputPorts rhsExp dataflow) > 0
           then foldActionInputPorts foldedOverDimension foldedOverCountVarName vars2 stmts
           else foldActionRange actorName foldedOverDimension vars2 stmts)
        ]
        ++
        map
        (\((outIdent,initaliseExp),outputLhs) ->
          outputAction
          (outIdent,initaliseExp)
          outputLhs
          -- outIdx
          foldedOverDimension
          -- (dimensionOfVar (R.Ident actorName) dataflow)
          foldedOverDimension
          actorName
          foldedOverCountVarName
          expState
          -- stateBindings
        )
        (zip stateBindings outputs)

        -- , outputAction
        --   outputs
        --   foldedOverDimension
        --   -- (dimensionOfVar (R.Ident actorName) dataflow)
        --   foldedOverDimension
        --   actorName
        --   foldedOverCountVarName
        --   expState
        --   stateBindings
        -- ]
      stateBindings = stateExpNameBindings expState vars1
      preloads =
        -- trace ("preloads: " ++ show (processGlobalVarsTwoVarProc dataflow fun)) $
        processGlobalVarsTwoVarProc dataflow fun

      dimensionCountsForGenArray dimensions boundArrayName =
            map (\i ->
                    C.GlobVarDecl
                    (C.VDeclExpMut
                     (intType 16)
                     (C.Ident (boundArrayName ++ "_d" ++ show i)) [] (mkInt 0))
                ) [1..dimensions]

      stateVarsGenArrays (R.ExprGenArray (R.ExprTuple es)) =
        case vars1 of
          R.IdentsOneId (R.Ident bindVarNameS) ->
            dimensionCountsForGenArray (length es) bindVarNameS
          R.IdentsManyIds boundIdents ->
            concatMap
            (dimensionCountsForGenArray (length es))
            (map (\(R.Ident s) -> s) boundIdents)

      stateVarsGenArrays (R.ExprTuple xs) =
        concatMap stateVarsGenArrays xs

      stateVars = genState vars1 expState
                  ++
                  [ C.GlobVarDecl (C.VDeclExpMut (intType 32) (C.Ident (foldedOverCountVarName ++ "_count")) [] (mkInt 0)) ]
                  ++
                  (map (\x ->
                    C.GlobVarDecl (C.VDeclExpMut (intType 32) (C.Ident ((idRiplShow (fst x)) ++ "_output_count")) [] (mkInt 0)))
                    stateBindings)
                  -- ++
                  -- [ C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident (actorName ++ "_output_count")) [] (mkInt 0)) ]

                  -- TODO this is needed
                  -- (map (\x ->
                  --         (C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident ("output_count")) [] (mkInt 0)))
                  --         (fst x))
                  --   stateBindings)
                  -- ++


  in actor preloads stateVars actions actorName ports


genState :: R.Idents -> R.Exp -> [C.GlobalVarDecl]
genState foldStateVarTuple expInitState  =
  case foldStateVarTuple of
    R.IdentsOneId bindVarName ->
      case expInitState of
        (R.ExprGenArray (R.ExprTuple es)) ->
          mkGlobVar es bindVarName
    (R.IdentsManyIds idents) ->
      case expInitState of
        (R.ExprTuple xs) ->
          concatMap (\(i,ident) ->
                  let (R.ExprGenArray (R.ExprTuple es)) = xs !! i
                  in mkGlobVar es ident
              ) (zip [0..] idents)

  where mkGlobVar es bindVarName@(R.Ident bindVarNameS) =
          C.GlobVarDecl
           (C.VDecl
           (intType 32)
           (idRiplToCal bindVarName)
           (map (C.BExp . expRiplToCal) es))
          :
           map (\i ->
                   C.GlobVarDecl
                   (C.VDeclExpMut
                     (intType 16)
                     (C.Ident (bindVarNameS ++ "_d" ++ show i)) [] (mkInt 0))
               ) [1..length es]




consumeAction (R.Ident rhsId) = ("consume_" ++ rhsId, action)
  where
    inputPattern = undefined -- riplVarToInputPattern vars
    outputPattern = undefined -- riplExpToOutputPattern exp
    actionHead = C.ActnHead inputPattern outputPattern
    action = undefined

foldActionInputPorts rhsIdDimension rhsId streamVars stmtsRhs = ("fold", action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident "fold_action"]
    actionHead =
      C.ActnHeadGuarded inputPatterns outputPatterns [guardExp]
    inputPatterns =
      map (\(tokenName,i) ->
             C.InPattTagIds
             (C.Ident (rhsId ++ show i))
             [idRiplToCal tokenName])
      (zip
        (case streamVars of
            R.IdentsOneId ident -> [ident]
            R.IdentsManyIds idents -> idents)
        [1 .. inputArgCount streamVars])
    outputPatterns =
      []
    guardExp =
      guardFromDimensionLT
      rhsId
      -- TODO: only if the input stream is Parallel should the
      -- last dimension be dropped, because the last dimension
      -- comes in via parallel ports.
      (dropLastDimension rhsIdDimension)
    statements =
      [ varIncr (rhsId ++ "_count") ]
      -- , C.SemiColonSeparatedStmt (calExpToStmt expRhs)
      ++ map stmtRiplToCal stmtsRhs

foldActionRange actorName rhsIdDimension streamVars stmtsRhs = ("fold", action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident "fold_action"]
    actionHead =
      C.ActnHeadGuarded inputPatterns outputPatterns [guardExp]
    inputPatterns =
      []
    outputPatterns =
      []
    statements =
      [ loopOverDimension
        rhsIdDimension
        (case streamVars of
           R.IdentsOneId ident -> [idRiplToCal ident]
           R.IdentsManyIds idents -> map idRiplToCal idents)
        (map stmtRiplToCal stmtsRhs)
      ]
      ++
      [ varIncr (actorName ++ "_range_count") ]
    guardExp = guardFromDimensionLT (actorName ++ "_range") rhsIdDimension

stateExpNameBindings :: R.Exp -> R.Idents -> [(R.Ident,R.Exp)]
stateExpNameBindings expState stateLambdaName =
  case stateLambdaName of
    R.IdentsOneId ident ->
      [(ident,expState)]
    R.IdentsManyIds idents ->
      case expState of
        R.ExprTuple states ->
          zip idents states


-- TODO: where there are multiple output states, the actor function
-- should map over this outputAction function, one output function for
-- each initialised state variable.
outputAction stateBinding@(output,initialiseExp) (R.Ident lhsId) rhsIdDimension lhsIdDimension actorName rhsId expState =
  ("output_" ++ idRiplShow output ++ "_" ++ actorName, action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident ("output_" ++ lhsId ++ "_" ++ actorName ++ "_action")]
    actionHead =
      C.ActnHeadGuarded inputPatterns outputPatterns guardExp
    inputPatterns = []
    outputPatterns =
      -- map (\((bindingName,initialiseExp)) ->
      [
             C.OutPattTagIds
             (idRiplToCal (R.Ident lhsId))
             [
               C.OutTokenExp $
               (case initialiseExp of
                  R.ExprGenArray (R.ExprTuple es) ->
                    C.EIdentArr
                    (idRiplToCal output)
                    (map (\i ->
                             C.BExp (C.EIdent (C.Ident (idRiplShow output ++ "_d" ++ show i)))
                         ) [1..length es])
                    -- C.EIdentArr
                    -- (idRiplToCal output)
                    -- (map (\i ->
                    --          C.BExp (C.EIdent (C.Ident (idRiplShow bindingName ++ "_d" ++ show i)))
                    --      ) [1..length es])
               )
             ]
      ]
          -- )
      -- (zip (stateBindings::[(R.Ident,R.Exp)]) [1..])
      -- (zip3 (stateBindings::[(R.Ident,R.Exp)]) outputs [0,1..])
      -- stateBindings

    guardExp =
      [
        -- (guardFromDimensionLT (idRiplShow output ++ "_output") undefined)
        -- ,
      (guardFromDimensionEQ (rhsId) (dropLastDimension lhsIdDimension))
      ]
    statements =
      ifResetStatementsAll
      ++
      [ varIncr (idRiplShow output ++ "_output_count") ]
      ++
      ifOutputCountDoneStatements

    resetBodyStatement =
      [C.SemiColonSeparatedStmt $
        C.AssignStt
        (C.AssStmtIdx
         (idRiplToCal output)
         -- (C.Idx (map C.BExp index))
         (C.Idx (map C.BExp
                (case lhsIdDimension of
                   Dim3 x y z ->
                     map mkInt [x,y,z]
                   Dim2 x y ->
                     map mkInt [x,y]
                   Dim1 x ->
                     map mkInt [x])))
         (mkInt 0))
      ]

    ifOutputCountDoneStatements =
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       (guardFromDimensionEQ (idRiplShow output ++ "_output") (dropLastDimension lhsIdDimension))
         [ loopOverDimension lhsIdDimension (map (\i -> C.Ident ("x" ++ show i)) [0..]) resetBodyStatement
         , varSetInt (rhsId ++ "_count") 0
         , varSetInt (idRiplShow output ++ "_output_count") 0
         ]
       ))
      ]

    ifResetStatements (bindingName,exp) =
      case exp of
        R.ExprGenArray (R.ExprTuple es) ->
          ifThenGo
          bindingName
          (length es)
          (map (\(R.ExprInt i) -> i) es)
          0

    ifThenGo (R.Ident bindNameS) 1 sizes tuplePos =
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (bindNameS ++ "_d1")))
         (mkInt (sizes!!tuplePos)))
       -- then branch
       ([ varSetInt (bindNameS ++ "_d1") 0
        ]
       )))
      ]

    ifThenGo bindName@(R.Ident bindNameS) recur sizes tuplePos =
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (bindNameS ++ "_d" ++ show recur)))
         (mkInt (sizes!!tuplePos)))
       -- then branch
       ([ varSetInt (bindNameS ++ "_d" ++ show recur) 0
        , varIncr (bindNameS ++ "_d" ++ show (recur-1))
        ]
        ++ ifThenGo bindName (recur-1) sizes (tuplePos+1))
       -- else branch
       [ varIncr (bindNameS ++ "_d" ++ show recur) ]
       ))
       ]

    ifResetStatementsAll =
      -- concatMap ifResetStatements (stateBindings::[(R.Ident,R.Exp)])
      ifResetStatements stateBinding


actor preloads stateVars riplActions actorName (ins,outs) =
  C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       (C.IOSg (ins ++ map (\(_,portDecl,_) -> portDecl) preloads) outs)
       (stateVars ++ (concatMap (\(globarVars,_,_) -> globarVars) preloads))
       (map snd (map (\(_,_,act) -> act) preloads) ++ map snd riplActions ++ [resetAction])
       schedule
       priorityBlock
  where
    priorityBlock = []
    -- transLoad = map (\(i,(name,action)) -> C.StTrans (C.Ident ("s" ++ show i)) (C.Ident name) (C.Ident ("s" ++ show (i+1)))) (zip [0,1..] (map (\(_,_,act) -> act) preloads))
    -- riplActionTrans i = [] -- [C.StTrans (C.Ident ("s" ++ show i)) (C.Ident riplActionName) (C.Ident ("s" ++ show (i)))]
    schedule =
      C.SchedfsmFSM
      (C.Ident "s0")
       --C.StTrans (C.Ident ("s" ++ show sNum)) (C.Ident actionLabel) (C.Ident ("s" ++ show (sNum+1))))
      ( concatMap
         (\((_,_,(actionLabel,_)),sNum) ->
            transitions actionLabel sNum)
         (zip preloads [0,1..])
        ++
        -- [ C.StTrans (C.Ident ("s" ++ show (length preloads))) (C.Ident "fold_action") (C.Ident ("s" ++ show (length preloads+1))) ]
        transitions "fold_action" (length preloads)
        ++
        (concatMap (\(i,C.PortDcl _ (C.Ident outVar)) -> outputTransitions outVar i) (zip [length preloads+1..] (outs)))
        -- ++
        -- (concatMap (\(i,C.PortDcl _ (C.Ident outVar)) -> outputTransitionsLast outVar i) (zip [(length preloads+1) + (length outs - 1)..] [last outs]))
        ++
        [ C.StTrans (C.Ident ("s" ++ show (length preloads + length outs + 1))) (C.Ident "resetAction") (C.Ident ("s0")) ]
      )

    transitions actionLabel i =
      [ C.StTrans (C.Ident ("s" ++ show i)) (C.Ident actionLabel) (C.Ident ("s" ++ show (i+1)))
      , C.StTrans (C.Ident ("s" ++ show (i+1))) (C.Ident actionLabel) (C.Ident ("s" ++ show (i+1)))
      ]

    outputTransitions outputVarName i =
      [ C.StTrans (C.Ident ("s" ++ show i)) (C.Ident ("output_" ++ outputVarName ++ "_" ++ actorName ++ "_action")) (C.Ident ("s" ++ show (i+1)))
      , C.StTrans (C.Ident ("s" ++ show (i+1))) (C.Ident ("output_" ++ outputVarName ++ "_" ++ actorName ++ "_action")) (C.Ident ("s" ++ show (i+1)))
      ]

    outputTransitionsLast outputVarName i =
      [ C.StTrans (C.Ident ("s" ++ show i)) (C.Ident ("output_" ++ outputVarName ++ "_" ++ actorName ++ "_action")) (C.Ident ("s" ++ show (i+1)))
      , C.StTrans (C.Ident ("s" ++ show (i+1))) (C.Ident ("output_" ++ outputVarName ++ "_" ++ actorName ++ "_action")) (C.Ident ("s" ++ show (i+1)))
      ]

    resetAction =
      C.ActionCode
      (C.AnActn
       (C.ActnTags
        (C.ActnTagDecl [C.Ident "resetAction"])
        (C.ActnHead [] [])))
