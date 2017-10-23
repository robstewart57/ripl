module SkeletonTemplates.Common where

import Types
import qualified AbsCAL as C
import qualified AbsRIPL as R
import AstMappings
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

-- dimensionOfVar :: R.Ident -> VarInfo -> Dimension
-- dimensionOfVar ident dataflow =
--   (dim (fromJust (Map.lookup ident dataflow)))

inputPortNames (R.ExprRangeArray exp) =
  []
inputPortNames (R.ExprVar (R.VarC ident)) =
  [ident]

outputPortNames (R.ExprTuple xs) =
  map (\i -> R.Ident ("gen" ++ show i)) [1..length xs]

outputPortNames (R.ExprGenArray (R.ExprTuple xs)) =
  [R.Ident "gen1"]
        -- map (\i -> R.Ident ("gen" ++ show i)) [1..length xs]

inputPorts (R.ExprRangeArray exp) _dataflow =
  []
inputPorts (R.ExprVar (R.VarC ident)) dataflow =
  case fst (fromJust (Map.lookup ident dataflow)) of
    Dim1{} -> [ident]
    Dim3{} -> [ R.Ident (idRiplShow ident ++ "1")
              , R.Ident (idRiplShow ident ++ "2")
              , R.Ident (idRiplShow ident ++ "3")
              ]

portsFromRhsId ident varInfo =
  let (dim,streamMode) = fromJust (Map.lookup ident varInfo)
  in case streamMode of
    Sequential -> 1
    Parallel ->
      case dim of
        Dim1{} -> 1
        Dim2{} -> 2
        Dim3{} -> 3

dimensionFromStateExp stateExp posIdx =
  case stateExp of
    R.ExprGenArray tupleExp ->
      dimensionFromTuple tupleExp Sequential
    R.ExprTuple genArrays ->
      let R.ExprGenArray tuple = (genArrays !! posIdx)
       in dimensionFromTuple tuple Sequential
    R.ExprGenRGB tupleExp ->
      let Dim2 x y = fst $ dimensionFromTuple tupleExp Parallel
      in
      -- add a 3rd dimension in the X/Y space for R, G and B
      (Dim3 x y 3, Parallel)

dimensionOfInput :: R.AssignSkelRHS -> VarInfo -> R.Ident -> Int -> (R.Ident,(Dimension,StreamMode))
dimensionOfInput (R.MapSkel ident _) varInfo lhsIdent _ =
  (lhsIdent, fromJust $ Map.lookup ident varInfo)
dimensionOfInput (R.FoldSkel initStateExp foldedOverDimension _) varInfo lhsIdent i =
  (lhsIdent , dimensionFromStateExp initStateExp i)
  -- case initStateExp of
  --   --  assumes i == 1
  --   -- R.ExprVar (R.VarC v) ->
  --   --   trace (show (Map.lookup v varInfo)) $
  --   --   (lhsIdent , fromJust $ Map.lookup v varInfo)
  --   --  assumes i == 1
  --   R.ExprGenArray tupleExp ->
  --     (lhsIdent , dimensionFromTuple tupleExp Sequential)
  --   R.ExprTuple genArrays ->
  --     (lhsIdent ,
  --      let R.ExprGenArray tuple = (genArrays !! i)
  --      in dimensionFromTuple tuple Sequential)
  --   R.ExprGenRGB tupleExp ->
  --     let Dim2 x y = fst $ dimensionFromTuple tupleExp Parallel
  --     in
  --     -- add a 3rd dimension in the X/Y space for R, G and B
  --     (lhsIdent , (Dim3 x y 3, Parallel))
  --   e -> error (show e)

processGlobalVarsTwoVarProc ::
  VarInfo ->
  R.TwoVarProc ->
  [([C.GlobalVarDecl] -- to contain the data to be preloaded
  , [C.PortDecl]      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsTwoVarProc dataflow foldExp@(R.TwoVarProcC var1 var2 stmts) =
  let globalIds = newIdentsInStatements foldExp
  in -- trace ("Global: " ++ show globalIds) $
     map (processGlobalVar dataflow) globalIds

processGlobalVarsTwoVarFunc ::
  VarInfo ->
  R.TwoVarFun ->
  [([C.GlobalVarDecl] -- to contain the data to be preloaded
  , [C.PortDecl]      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsTwoVarFunc dataflow fun@(R.TwoVarFunC var1 var2 exp) =
  let globalIds = globalIdentsElemBinary fun
  in map (processGlobalVar dataflow) globalIds

processGlobalVarsOneVarFunc ::
  VarInfo ->
  R.OneVarFun ->
  [([C.GlobalVarDecl] -- to contain the data to be preloaded
  , [C.PortDecl]      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsOneVarFunc dataflow fun@(R.OneVarFunC var exp) =
  let globalIds = globalIdentsElemUnary fun
  in map (processGlobalVar dataflow) globalIds


populateArray arrayName dimensionsList = go (length dimensionsList) 0
  where
    go 1 idx =
      [ varIncr (arrayName ++ "_d" ++ show (idx+1)) ]
      ++
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (arrayName ++ "_d"++ show (idx+1))))
         (mkInt ((dimensionsList!!idx) )))
       -- the statement in the innermost loop body
       ([
           varSetInt (arrayName ++ "_d"++ show (idx+1)) 0
        ])))
      ]

    go n idx =
      [ varIncr (arrayName ++ "_d" ++ show (idx+1)) ]
      ++
      [C.EndSeparatedStmt
      (C.IfStt
       (C.IfOneSt
       -- condition
       (C.BEEQ
         (C.EIdent (C.Ident (arrayName ++ "_d" ++ show (idx+1))))
         (mkInt ((dimensionsList!!idx) )))
       -- then branch
       ([ varSetInt (arrayName ++ "_d" ++ show (idx+1)) 0
        ]
        ++ go (n-1) (idx+1))
       -- else branch
       -- [ varIncr (arrayName ++ "_d" ++ show (idx+1)) ]
       ))
       ]

processGlobalVar :: VarInfo -> R.Ident -> ([C.GlobalVarDecl],[C.PortDecl],(String,C.CodeBlock))
processGlobalVar varLookup ident@(R.Ident identStr) =
  -- TODO: implement loop generation from this dimension
  -- let (Dim2 w h,_) = (Dim2 10 10,undefined) -- fromJust (Map.lookup ident varLookup)

  let varDecls =
        C.GlobVarDecl
        (C.VDecl
         (calIntType 16)
         (idRiplToCal ident)
         (map (C.BExp . mkInt) indexingValues))
        :
        C.GlobVarDecl
        (C.VDecl
         (calUIntType 32)
         (C.Ident (identStr ++ "_count"))
         [])
        :
        map
        (\i ->
        C.GlobVarDecl
        (C.VDecl
         (calUIntType 32)
         (C.Ident (identStr ++ "_d" ++ show i))
         [])
        )
        [1..length indexingValues]

      indexingValues   =
        dimensionsAsList dimension
      indexingBrackets =
        map (\i -> C.BExp (C.EIdent i)) dimVars

      streamMode = trace (show ident) $
        snd (fromJust (Map.lookup ident varLookup))
      dimension =
        fst (fromJust (Map.lookup ident varLookup))

      portDecls = --C.PortDcl (calIntType 32) (C.Ident (idRiplShow ident))
        -- error (show ident ++ " ," ++ show (fst (fromJust (Map.lookup ident varLookup)))) $
        case streamMode of
          Sequential ->
            [ C.PortDcl (calIntType 32) (C.Ident (idRiplShow ident)) ]
          Parallel ->
            case dimension of
              Dim1{} ->
                [ C.PortDcl (calIntType 32) (C.Ident (idRiplShow ident)) ]
              Dim2{} ->
                [ C.PortDcl (calIntType 32) (C.Ident ((idRiplShow ident) ++ "1"))
                , C.PortDcl (calIntType 32) (C.Ident ((idRiplShow ident) ++ "2"))
                ]
              Dim3{} ->
                [ C.PortDcl (calIntType 32) (C.Ident ((idRiplShow ident) ++ "1"))
                , C.PortDcl (calIntType 32) (C.Ident ((idRiplShow ident) ++ "2"))
                , C.PortDcl (calIntType 32) (C.Ident ((idRiplShow ident) ++ "3"))
                ]


      -- inputPattern = C.InPattTagIdsRepeat (C.Ident (idRiplShow ident ++ "Port")) [C.Ident ("data_" ++ idRiplShow ident)] (C.RptClause (C.LitExpCons (C.IntLitExpr (C.IntegerLit (w*h)))))
      inputPatterns =
        case streamMode of
          Sequential ->
            [ C.InPattTagIds
              (C.Ident (idRiplShow ident))
              [C.Ident "token"]
            ]
          Parallel ->
            case dimension of
              Dim1{} ->
                [ C.InPattTagIds
                  (C.Ident (idRiplShow ident))
                  [C.Ident "token"]
                ]
              Dim2{} ->
                map (\i ->
                C.InPattTagIds
                (C.Ident ((idRiplShow ident) ++ show i))
                [C.Ident ("token" ++ show i)])
                [1,2]
              Dim3{} ->
                map (\i ->
                C.InPattTagIds
                (C.Ident ((idRiplShow ident) ++ show i))
                [C.Ident ("token" ++ show i)])
                [1,2,3]

      actionHead =
        C.ActnHeadGuarded
        inputPatterns
        []
        -- [guardFromDimensionLT identStr dimension]
        (case streamMode of
          Sequential -> [guardFromDimensionLT identStr dimension]
          Parallel ->
            case dimension of
              Dim1{} -> [guardFromDimensionLT identStr dimension]
              Dim2{} -> [guardFromDimensionLT identStr (dropLastDimension dimension)]
              Dim3{} -> [guardFromDimensionLT identStr (dropLastDimension dimension)])

      sequentialConsume =
        C.SemiColonSeparatedStmt
        (C.AssignStt
          (C.AssStmtIdx
            (C.Ident identStr)
            (C.Idx (map (\x -> C.BExp (C.EIdent (C.Ident (identStr ++ "_d" ++ show x)))) [1..fromIntegral (length indexingValues)]))
            (C.EIdent (C.Ident "token"))))

      parallelConsume n =
        map
        (\i ->
           C.SemiColonSeparatedStmt
        (C.AssignStt
          (C.AssStmtIdx
            (C.Ident identStr)
            (C.Idx
             ((map (\x -> C.BExp (C.EIdent (C.Ident (identStr ++ "_d" ++ show x)))) [1..n-1])
             ++ [C.BExp (mkInt (i-1))])
            )
            (C.EIdent (C.Ident ("token" ++ show i)))))
        )
        [1..n]

      consumeStatement =
        case streamMode of
          Sequential -> [sequentialConsume]
          Parallel ->
            case dimension of
              Dim1{} -> [sequentialConsume]
              Dim2{} -> parallelConsume 2
              Dim3{} -> parallelConsume 3

      consumeLoop =
        populateArray identStr indexingValues
        ++
        consumeStatement
        ++
        -- TODO: to support multiple image frames, this variable will
        -- need to be reset.
        [ varIncr (identStr ++ "_count") ]



      dimVars = map (\i -> C.Ident ("x" ++ show i))
                [1..case dimension of Dim1{} -> 1;Dim2{} -> 2;Dim3{} -> 3]

      -- consumeLoop =
      --   loopOverDimension
      --   dimension
      --   dimVars
      --   [ (C.SemiColonSeparatedStmt
      --      (C.AssignStt
      --       (C.AssStmtIdx
      --        (idRiplToCal ident)
      --        (C.Idx indexingBrackets)
      --        (C.EIdent (C.Ident "token")))))
      --   ]

        -- C.EndSeparatedStmt
        -- (C.ForEachStt
        --  (C.ForeachStmtsSt
        --  [C.ForeachGen (calIntType 32) (C.Ident "i") (C.BEList (intCalExp 0) (intCalExp ((w*h)-1)))]
        --    [C.SemiColonSeparatedStmt
        --     (C.AssignStt
        --      (C.AssStmtIdx
        --       (C.Ident identStr)
        --       (C.Idx [C.BExp (C.EIdent (C.Ident "i"))])
        --       (C.EIdentArr (C.Ident ("data_" ++ identStr)) [C.BExp (C.EIdent (C.Ident "i"))])))]))

      action =
        ("load_" ++ identStr
        , C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident ("load_" ++ identStr)])
                actionHead
                consumeLoop)))

  in (varDecls,portDecls,action)


loopOverDimension dimension dimensionVars statements =
  loopDimensionGo dimension dimensionVars statements 1

loopDimensionGo (Dim3 d1 d2 d3) dimensionVars statements i =
  case i of
    3 ->
      C.EndSeparatedStmt
        (C.ForEachStt
           (C.ForeachStmtsSt
              [ C.ForeachGen
                  (intType 16)
                  (dimensionVars !! (2))
                  (C.BEList (mkInt 0) (mkInt (d3 - 1)))
              ]
              statements))
    m ->
      C.EndSeparatedStmt
        (C.ForEachStt
           (C.ForeachStmtsSt
              [ C.ForeachGen
                  (intType 16)
                  (dimensionVars !! (m-1))
                  (C.BEList (mkInt 0) (mkInt ([d1, d2, d3] !! (m - 1) - 1)))
              ]
              [loopDimensionGo
               (Dim3 d1 d2 d3)
               dimensionVars
               statements
               (m + 1)]))

loopDimensionGo (Dim2 d1 d2) dimensionVars statements i =
  case i of
    2 ->
      C.EndSeparatedStmt
        (C.ForEachStt
           (C.ForeachStmtsSt
              [ C.ForeachGen
                  (intType 16)
                  (dimensionVars !! 1)
                  (C.BEList (mkInt 0) (mkInt (d2 - 1)))
              ]
              statements))
    m ->
      C.EndSeparatedStmt
        (C.ForEachStt
           (C.ForeachStmtsSt
              [ C.ForeachGen
                  (intType 16)
                  (dimensionVars !! (m-1))
                  (C.BEList (mkInt 0) (mkInt ([d1, d2] !! (m - 1) - 1)))
              ]
              [loopDimensionGo
               (Dim2 d1 d2)
               dimensionVars
               statements
               (m + 1)]))


loopDimensionGo dim dimensionVars statements i =
  error ("unsupport dimension for loop: " ++ show dim)
