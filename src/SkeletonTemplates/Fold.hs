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
foldActor actorName outputs expState rhsExp fun@(R.TwoVarProcC vars1 vars2 stmts) varInfo =
  let ports =
        ( map (\var -> C.PortDcl (intType 32) (idRiplToCal var)) (inputPorts rhsExp varInfo)
        , map (\name -> C.PortDcl (intType 32) (idRiplToCal name)) outputs)

      countNumStates (R.ExprTuple xs) = length xs
      countNumStates _ = 1

      (foldedOverCountVarName,foldedOverDimension) =
        case rhsExp of
          R.ExprVar (R.VarC (R.Ident rhsId)) ->
            (rhsId,fst ( fromJust (Map.lookup (R.Ident rhsId) varInfo)))
          R.ExprRangeArray rangeExp ->
            (actorName ++ "_range" , fst (dimensionFromTuple rangeExp Sequential))
      actions =
        [
        -- consumeAction rhsId
          if length (inputPorts rhsExp varInfo) > 0
          then (foldActionInputPorts foldedOverDimension foldedOverCountVarName vars2 stmts,True)
          else (foldActionRange actorName foldedOverDimension vars2 stmts,False)
        ]
        ++
        map
        (\((outIdent,initialiseExp),outputLhs,bindingIdx) ->
           let tupleArray =
                 case initialiseExp of
                   R.ExprGenArray e -> e
                   R.ExprGenRGB e -> e
           in
          (
          outputAction
          outputs
          (outIdent,initialiseExp)
          outputLhs
          foldedOverDimension
          (fst (dimensionFromStateExp initialiseExp bindingIdx))
          -- (fst (dimensionFromTuple tupleArray Sequential))
          -- (case dimensionFromStateExp expState bindingIdx of
          --    (_,Sequential) ->
          --      fst (dimensionFromTuple tupleArray Sequential)
          --    (_,Parallel) ->
          --      fst (dimensionFromTuple tupleArray Parallel)
          -- )

          actorName
          foldedOverCountVarName
          expState
          bindingIdx
          , True
          )
        )
        (zip3 stateBindings outputs ([0,1..] :: [Int]))

      stateBindings = stateExpNameBindings expState vars1
      preloads =
        -- trace ("preloads: " ++ show (processGlobalVarsTwoVarProc dataflow fun)) $
        processGlobalVarsTwoVarProc varInfo fun

      dimensionCountsForGenArray dimensions boundArrayName =
            map (\i ->
                    C.GlobVarDecl
                    (C.VDeclExpMut
                     (intType 16)
                     (C.Ident (boundArrayName ++ "_d" ++ show i)) [] (mkInt 0))
                ) [1..dimensions]

      -- stateVarsGenArrays (R.ExprGenArray (R.ExprTuple es)) =
      --   case vars1 of
      --     R.IdentsOneId (R.Ident bindVarNameS) ->
      --       dimensionCountsForGenArray (length es) bindVarNameS
      --     R.IdentsManyIds boundIdents ->
      --       concatMap
      --       (dimensionCountsForGenArray (length es))
      --       (map (\(R.Ident s) -> s) boundIdents)

      -- stateVarsGenArrays (R.ExprTuple xs) =
      --   concatMap stateVarsGenArrays xs

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
        (R.ExprGenArray (R.ExprTuple es)) ->
          mkGlobVar es (head idents)
        (R.ExprGenRGB (R.ExprTuple es)) ->
          -- add a 3rd dimension on X/Y for R, G and B
          let rgbTuple = es ++ [R.ExprInt 3]
          in
          mkGlobVar rgbTuple (head idents)

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
      (guardFromDimensionLT
      rhsId
      -- TODO: only if the input stream is Parallel should the
      -- last dimension be dropped, because the last dimension
      -- comes in via parallel ports.
      (dropLastDimension rhsIdDimension))

    statements =
      [ varIncr (rhsId ++ "_count") ]
      -- , C.SemiColonSeparatedStmt (calExpToStmt expRhs)
      ++ catMaybes (map stmtRiplToCal stmtsRhs)

foldActionRange actorName rhsIdDimension streamVars stmtsRhs = ("fold", action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident "fold_action"]
    actionHead =
      case concatMap actionVars stmtsRhs of
        [] ->
          C.ActnHead inputPatterns outputPatterns
        xs ->
          C.ActnHeadVars inputPatterns outputPatterns (C.LocVarsDecl xs)
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
        (catMaybes (map stmtRiplToCal stmtsRhs))
      ]
      ++
      [ varIncr (actorName ++ "_range_count") ]
      -- fold actions with nested for loops don't need guards
      -- guardExp =
      --
      -- [ guardFromDimensionLT (actorName ++ "_range") rhsIdDimension ]

stateExpNameBindings :: R.Exp -> R.Idents -> [(R.Ident,R.Exp)]
stateExpNameBindings expState stateLambdaName =
  case stateLambdaName of
    R.IdentsOneId ident ->
      [(ident,expState)]
    R.IdentsManyIds idents ->
      case expState of
        R.ExprTuple states ->
          zip idents states
        R.ExprGenArray tupleExpr ->
          case length idents of
            -- don't know why IdentsOneId isn't being matched here
            1 -> [(head idents, expState)]
        R.ExprGenRGB tupleExpr ->
          case length idents of
            -- don't know why IdentsOneId isn't being matched here
            1 -> [(head idents, expState)]


-- TODO: where there are multiple output states, the actor function
-- should map over this outputAction function, one output function for
-- each initialised state variable.
outputAction outputs stateBinding@(lambdaBindingVar,initialiseExp) (R.Ident lhsId) rhsIdDimension lhsIdDimension actorName rhsId expState bindingIdx =
  ("output_" ++ lhsId ++ "_" ++ actorName, action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident ("output_" ++ lhsId ++ "_" ++ actorName ++ "_action")]
    actionHead =
      C.ActnHeadGuarded inputPatterns outputPatterns guardExp
    inputPatterns = []
    outputPatterns =
      case dimensionFromStateExp expState bindingIdx of
        (dim,Sequential) -> [outputSequential]
        (dim,Parallel)   ->
          case dim of
            Dim1{} -> [outputSequential]
            Dim2{} -> outputParallel 2
            Dim3{} -> outputParallel 3

    outputParallel n =
      -- trace (show outputs) $
      map
      (\(outputPort,i) ->
      C.OutPattTagIds
      (idRiplToCal outputPort)
      [
        C.OutTokenExp $
        (let es = case initialiseExp of
                    R.ExprGenArray (R.ExprTuple es) -> es
                    R.ExprGenRGB (R.ExprTuple es) -> es
         in
           C.EIdentArr
           (idRiplToCal lambdaBindingVar)
           (map
            (\j ->
                C.BExp (C.EIdent (C.Ident (idRiplShow lambdaBindingVar ++ "_d" ++ show j)))
            )
            [1..length es]
           ++
           [ C.BExp (mkInt i) ]
           )
        )
      ]
      )
      (zip outputs [0,1..])


    outputSequential =
      C.OutPattTagIds
      (idRiplToCal (R.Ident lhsId))
      [
        C.OutTokenExp $
        (let es = case initialiseExp of
                    R.ExprGenArray (R.ExprTuple es) -> es
                    R.ExprGenRGB (R.ExprTuple es) -> es
         in
           C.EIdentArr
           (idRiplToCal lambdaBindingVar)
           (map (\i ->
                    C.BExp (C.EIdent (C.Ident (idRiplShow lambdaBindingVar ++ "_d" ++ show i)))
                ) [1..length es])
        )
      ]

    guardExp =
      [
        -- (guardFromDimensionLT (idRiplShow output ++ "_output") undefined)
        -- ,
      (guardFromDimensionLT (idRiplShow lambdaBindingVar ++ "_output") ({-dropLastDimension-} lhsIdDimension))
      ]
    statements =
      ifResetStatementsAll
      ++
      [ varIncr (idRiplShow lambdaBindingVar ++ "_output_count") ]
      ++
      ifOutputCountDoneStatements

    resetBodyStatement =
      [C.SemiColonSeparatedStmt $
        C.AssignStt
        (C.AssStmtIdx
         (idRiplToCal lambdaBindingVar)
         -- (C.Idx (map C.BExp index))
         (C.Idx (map C.BExp
                (case lhsIdDimension of
                   Dim3 x y z ->
                     map (\i -> C.EIdent (C.Ident ("x"++show i))) [0..2]
                     --map mkInt [x,y,z]
                   Dim2 x y ->
                     map (\i -> C.EIdent (C.Ident ("x"++show i))) [0..1]
                     --map mkInt [x,y]
                   Dim1 x ->
                     map (\i -> C.EIdent (C.Ident ("x"++show i))) [0]
                     --map mkInt [x])))
                )))
         (mkInt 0))
      ]

    ifOutputCountDoneStatements =
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       (guardFromDimensionEQ (idRiplShow lambdaBindingVar ++ "_output") ({-dropLastDimension-} lhsIdDimension))
         [ loopOverDimension lhsIdDimension (map (\i -> C.Ident ("x" ++ show i)) [0..]) resetBodyStatement
         , varSetInt (rhsId ++ "_count") 0
         , varSetInt (idRiplShow lambdaBindingVar ++ "_output_count") 0
         ]
       ))
      ]

    ifResetStatements (bindingName,exp) =
      let es = case exp of
            R.ExprGenArray (R.ExprTuple es) -> es
            R.ExprGenRGB (R.ExprTuple es) -> es
      in
          ifThenGo
          bindingName
          (length es)
          (map (\(R.ExprInt i) -> i) es)
          0

    ifThenGo (R.Ident bindNameS) 1 sizes tuplePos =
      [ varIncr (bindNameS ++ "_d" ++ show (tuplePos+1)) ]
      ++
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (bindNameS ++ ("_d"++show (tuplePos+1)))))
         (mkInt ((sizes!!tuplePos))))
       -- then branch
       ([ varSetInt (bindNameS ++ "_d"++show (tuplePos+1)) 0 ]
       )))
      ]

    ifThenGo bindName@(R.Ident bindNameS) recur sizes tuplePos =
      [ varIncr (bindNameS ++ "_d" ++ show (tuplePos+1)) ]
      ++
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (bindNameS ++ "_d" ++ show (tuplePos+1))))
         (mkInt ((sizes!!tuplePos))))
       -- then branch
       ([ varSetInt (bindNameS ++ "_d" ++ show (tuplePos+1)) 0
        -- , varIncr (bindNameS ++ "_d" ++ show (tuplePos+2))
        ]
        ++ ifThenGo bindName (recur-1) sizes (tuplePos+1))
       -- else branch
       -- [  ]
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
       (C.IOSg (ins ++ concatMap (\(_,portDecl,_) -> portDecl) preloads) outs)
       (stateVars ++ (concatMap (\(globarVars,_,_) -> globarVars) preloads))
       (map snd (map (\(_,_,act) -> act) preloads) ++ map (snd . fst) riplActions ++ [resetAction])
       schedule
       [priorityBlock]
  where
    priorityBlock =
      C.PriOrd $
      (
        concatMap
        (\preloadActionName ->
           map
          (\riplActionName ->
             C.PriInEQ (C.Ident preloadActionName) (C.Ident (riplActionName ++ "_action")) []
          )
          (map (fst . fst) riplActions)
        )
      )
      (map (\(_,_,(x,_)) -> x) preloads)
      ++
      map
      (\riplAction ->
          C.PriInEQ (C.Ident (riplAction ++ "_action")) (C.Ident ("resetAction")) []
      )
      (map (fst . fst) riplActions)

      -- (zip (map fst preloads) (map fst riplActions))


    -- transLoad = map (\(i,(name,action)) -> C.StTrans (C.Ident ("s" ++ show i)) (C.Ident name) (C.Ident ("s" ++ show (i+1)))) (zip [0,1..] (map (\(_,_,act) -> act) preloads))
    -- riplActionTrans i = [] -- [C.StTrans (C.Ident ("s" ++ show i)) (C.Ident riplActionName) (C.Ident ("s" ++ show (i)))]
    schedule =
      C.SchedfsmFSM
      (C.Ident "s0")
       --C.StTrans (C.Ident ("s" ++ show sNum)) (C.Ident actionLabel) (C.Ident ("s" ++ show (sNum+1))))
      ( concatMap
         (\((_,_,(actionLabel,_)),sNum) ->
            transitionsRepeat actionLabel sNum)
         (zip preloads [0,1..])
         ++
         concatMap
         (\(((actionLabel,_action),shouldRepeat),sNum) ->
            if shouldRepeat
            then transitionsRepeat (actionLabel++"_action") sNum
            else transitionsNoRepeat (actionLabel++"_action") sNum
            )
         (zip riplActions [length preloads..])
        -- ++
        -- transitionsRepeat "fold_action" (length preloads)
        -- ++
        -- (concatMap (\(i,C.PortDcl _ (C.Ident outVar)) -> outputTransitions outVar i) (zip [length preloads+1..] ([head outs] {- big hack -})))
        ++
        [ C.StTrans (C.Ident ("s" ++ show (length preloads + {-length outs + {big hack} -} 2))) (C.Ident "resetAction") (C.Ident ("s0")) ]
      )

    transitionsNoRepeat actionLabel i =
      [ C.StTrans (C.Ident ("s" ++ show i)) (C.Ident actionLabel) (C.Ident ("s" ++ show (i+1)))
      ]

    transitionsRepeat actionLabel i =
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
