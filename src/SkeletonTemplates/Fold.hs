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

foldActor :: String -> R.Exp -> R.Ident -> R.TwoVarFun -> ImplicitDataflow -> C.Actor
foldActor actorName expState rhsId fun@(R.TwoVarFunC vars1 vars2 exp) dataflow =
  let ports =
        ( map (\i -> C.PortDcl (intType 32) (C.Ident ("In" ++ show i ++ "_" ++ show 1))) [1 .. inputArgCount vars2]
        , map (\i -> C.PortDcl (intType 32) (C.Ident ("Out" ++ show i ++ "_" ++ show 1))) [1 .. outputArgCount exp])
      actions =
        [
        -- consumeAction rhsId
          foldAction (dimensionOfVar rhsId dataflow) rhsId vars2 exp
        , outputAction
          (dimensionOfVar rhsId dataflow)
          (dimensionOfVar (R.Ident actorName) dataflow)
          actorName
          rhsId
          expState
          stateBindings
        ]
      stateBindings = stateExpNameBindings expState vars1
      preloads = processGlobalVarsTwoVarFunc dataflow fun
      stateVars = genState expState vars1
                  ++
                  [ C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident ((idRiplShow rhsId) ++ "_count")) [] (mkInt 0)) ]
                  ++
                  (map (\x ->
                    C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident ((idRiplShow (fst x)) ++ "_count")) [] (mkInt 0)))
                    stateBindings)
                  ++
                  [ C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident (actorName ++ "_output_count")) [] (mkInt 0)) ]
                  ++
                  -- TODO this is needed
                  -- (map (\x ->
                  --         (C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident ("output_count")) [] (mkInt 0)))
                  --         (fst x))
                  --   stateBindings)
                  -- ++
                  (case expState of
                     R.ExprGenArray (R.ExprTuple es) ->
                       case vars1 of
                         R.IdentsOneId (R.Ident bindVarNameS) ->
                           map (\i ->
                                  C.GlobVarDecl (C.VDeclExpMut (intType 16) (C.Ident (bindVarNameS ++ "_d" ++ show i)) [] (mkInt 0))
                               ) [1..length es])
  in actor preloads stateVars actions actorName ports


genState :: R.Exp -> R.Idents -> [C.GlobalVarDecl]
genState expInitState foldStateVarTuple =
  case expInitState of
    (R.ExprGenArray (R.ExprTuple es)) ->
      case foldStateVarTuple of
        R.IdentsOneId bindVarName ->
          [C.GlobVarDecl $
           C.VDecl
           (intType 16)
           (idRiplToCal bindVarName)
           (map (C.BExp . expRiplToCal) es)]

consumeAction (R.Ident rhsId) = ("consume_" ++ rhsId, action)
  where
    inputPattern = undefined -- riplVarToInputPattern vars
    outputPattern = undefined -- riplExpToOutputPattern exp
    actionHead = C.ActnHead inputPattern outputPattern
    action = undefined

foldAction rhsIdDimension (R.Ident rhsId) streamVars expRhs = ("fold", action)
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
             (C.Ident ("In" ++ show i ++ "_" ++ show 1))
             [idRiplToCal tokenName])
      (zip
        (case streamVars of
            R.IdentsOneId ident -> [ident]
            R.IdentsManyIds idents -> idents)
        [1 .. inputArgCount streamVars])
    outputPatterns =
      []
    guardExp = guardFromDimensionLT rhsId rhsIdDimension
    statements =
      [ varIncr (rhsId ++ "_count")
      , C.SemiColonSeparatedStmt (calExpToStmt expRhs)
      ]

stateExpNameBindings :: R.Exp -> R.Idents -> [(R.Ident,R.Exp)]
stateExpNameBindings expState stateLambdaName =
  case stateLambdaName of
    R.IdentsOneId ident ->
      [(ident,expState)]


-- TODO: where there are multiple output states, the actor function
-- should map over this outputAction function, one output function for
-- each initialised state variable.
outputAction rhsIdDimension lhsIdDimension lhsId (R.Ident rhsId) expState stateBindings =
  ("output_" ++ lhsId, action)
  where
    action =
      C.ActionCode $ C.AnActn $
      C.ActnTagsStmts tag actionHead statements
    tag = C.ActnTagDecl [C.Ident ("output_" ++ lhsId ++ "_action")]
    actionHead =
      C.ActnHeadGuarded inputPatterns outputPatterns [guardExp]
    inputPatterns = []
    outputPatterns =
      map (\((bindingName,initialiseExp),portNum) ->
             C.OutPattTagIds
             (C.Ident ("Out" ++ show portNum ++ "_1")) -- hack
             [
               C.OutTokenExp $
               (case initialiseExp of
                  R.ExprGenArray (R.ExprTuple es) ->
                    C.EIdentArr
                    (idRiplToCal (fst (head stateBindings))) -- hack
                    (map (\i ->
                             C.BExp (C.EIdent (C.Ident (idRiplShow bindingName ++ "_d" ++ show i)))
                         ) [1..length es])
               )
             ]
          )
      (zip (stateBindings::[(R.Ident,R.Exp)]) [1..])

    guardExp =
      C.BEAnd
      (guardFromDimensionEQ rhsId rhsIdDimension)
      (guardFromDimensionLT (idRiplShow (fst (head stateBindings)))  lhsIdDimension) -- hack
    statements =
      ifResetStatementsAll
      ++
      [ varIncr (lhsId ++ "_output_count") ]
      ++
      ifOutputCountDoneStatements

    resetBodyStatement =
      [C.SemiColonSeparatedStmt $
        C.AssignStt
        (C.AssStmtIdx
         (idRiplToCal (fst (head stateBindings))) -- hack
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
       (guardFromDimensionEQ (lhsId ++ "_output") lhsIdDimension)
         [ loopOverDimension lhsIdDimension resetBodyStatement
         , varSetInt (rhsId ++ "_count") 0
         , varSetInt (lhsId ++ "_output_count") 0
         ]
       ))
      ]

    loopOverDimension dimension statements =
      loopDimensionGo dimension statements 1

    -- loopDimensionGo (Dim3 d1 d2 d3) statements 3 =

    loopDimensionGo (Dim3 d1 d2 d3) statements i =
      trace (show i) $
      case i of
        3 ->
          C.EndSeparatedStmt
          (C.ForEachStt
           (C.ForeachStmtsSt
            [ C.ForeachGen
              (intType 16)
              (C.Ident ("x" ++ show 3))
              (C.BEList (mkInt 0) (mkInt (d3-1)))
            ]
           statements))
        m ->
          C.EndSeparatedStmt
          (C.ForEachStt
           (C.ForeachStmtsSt
            [ C.ForeachGen
              (intType 16)
              (C.Ident ("x" ++ show m))
              (C.BEList (mkInt 0) (mkInt ([d1,d2,d3]!!(m-1)-1)))
            ]
            [ loopDimensionGo (Dim3 d1 d2 d3) statements (m+1) ]
           ))

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
      concatMap ifResetStatements (stateBindings::[(R.Ident,R.Exp)])


actor preloads stateVars riplActions actorName (ins,outs) =
  C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       (C.IOSg (ins ++ map (\(_,portDecl,_) -> portDecl) preloads) outs)
       (stateVars ++ (map (\(globarVar,_,_) -> globarVar) preloads))
       (map snd riplActions ++ map snd (map (\(_,_,act) -> act) preloads))
       []
  where
    transLoad = map (\(i,(name,action)) -> C.StTrans (C.Ident ("s" ++ show i)) (C.Ident name) (C.Ident ("s" ++ show (i+1)))) (zip [0,1..] (map (\(_,_,act) -> act) preloads))
    riplActionTrans i = [] -- [C.StTrans (C.Ident ("s" ++ show i)) (C.Ident riplActionName) (C.Ident ("s" ++ show (i)))]
