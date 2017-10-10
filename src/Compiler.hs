{-# LANGUAGE ScopedTypeVariables, LambdaCase, BangPatterns #-}

module Compiler
  ( compile
  ) where

import qualified AbsRIPL as R
import qualified PrintRIPL as R
import qualified AbsCAL as C

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Debug.Trace
import System.IO.Unsafe
import Safe
import Types

import AstMappings
import SkeletonTemplates.Map
import SkeletonTemplates.Fold
-- import SkeletonTemplates.Stencil1D
-- import SkeletonTemplates.Stencil2D
-- import SkeletonTemplates.SplitX
-- import SkeletonTemplates.SplitY
-- import SkeletonTemplates.Scale
-- import SkeletonTemplates.Scan
-- import SkeletonTemplates.FoldScalar
-- import SkeletonTemplates.FoldVector
import SkeletonTemplates.ZipWith
-- import SkeletonTemplates.ZipWithScalar
import SkeletonTemplates.Identity
import SkeletonTemplates.Parameters
import Inference.Offset
import Inference.Dimension
import Inference.BitWidth
import Inference.Colour
import Pass.Inliner
import Debug.Trace

compile :: R.Program -> Integer -> (CalProject, Dimension, Int, [String],(Chans,Chans))
compile (R.ProgramC _ functions imRead@(R.ImageInC imReadLhsIdent colourType w h) stmts (R.DataOutC outIdent)) numFrames =
  let !inlinedStmts = inlineFunctions functions stmts
      astToIr =
        inferStreamDimensionsChannelsBitWidth imRead .
        deriveDataflow imRead outIdent
      dfGraph = trace
                ("ASSIGN LHSs: " ++ (concatMap (\(R.AssignSkelC idents _) -> show idents ++ "\n") inlinedStmts))
                (astToIr inlinedStmts)
      (unusedActors, actors) = genActors outIdent dfGraph numFrames
      showIdent (R.Ident x) = x
      network = createDataflowWires dfGraph actors
      outImageColourChans = chans $ fromJust $ Map.lookup outIdent dfGraph
      outImageDim = fromJust $ dim $ fromJust $ Map.lookup outIdent dfGraph
      outImageBitWidth =
        fromJust $ maxBitWidth $ fromJust $ Map.lookup outIdent dfGraph
      outImageBitWidthUIint = calTypeFromCalBWUInt (correctBW outImageBitWidth)
  -- in traces (map show inlinedStmts) "Inlined statements" $
  in ( CalProject actors network
     , outImageDim
     , outImageBitWidthUIint
     , unusedActors
     , ( case colourType of R.ColourTypeGray -> Chan1; R.ColourTypeRGB -> Chan3
       , outImageColourChans)
     )
nodesLeftToRight :: R.Ident -> ImplicitDataflow -> [(R.Ident, VarNode)]
nodesLeftToRight _notUsed mp =
  let xs = Map.toList mp
  in sortBy
       (\(_, VarNode _ _ _ _ _ _ _ i _) (_, VarNode _ _ _ _ _ _ _ j _) ->
          if i < j
            then LT
            else if i == j
                   then EQ
                   else GT)
       xs

inferStreamDimensionsChannelsBitWidth :: R.ImageIn
                                             -> ImplicitDataflow
                                             -> ImplicitDataflow
inferStreamDimensionsChannelsBitWidth (R.ImageInC imReadLhsIdent colourType w h) dfGraph =
  traces
    ((map
        (\(lhsIdent, node) ->
           idToString lhsIdent ++
           ": " ++
           ",\t maxBitWidth: " ++
           show (fromJust (maxBitWidth node)) ++
           ",\t " ++ show (fromJust (dim node)) ++
           ",\t " ++ show (chans node)))
       (Map.toList inferredNodes))
    "Nodes"
    inferredNodes
  where
    inferredNodes = bitWidthInferred -- Map.fromList dimensionsInferred
    orderedNodesFromImRead =
      (imReadLhsIdent, imReadNode imReadLhsIdent colourType w h) :
      nodesLeftToRight imReadLhsIdent dfGraph :: [(R.Ident, VarNode)]
    singletonMapWithImRead =
      Map.singleton imReadLhsIdent (imReadNode imReadLhsIdent colourType w h)
    -- directionsInferred = trace ("NODES: " ++ show (map fst orderedNodesFromImRead)) $
    --   foldl
    --     inferDir
    --     (singletonMapWithImRead :: ImplicitDataflow)
    --     orderedNodesFromImRead
    dimensionsInferred =
      foldl
        inferDimColour
        (singletonMapWithImRead :: ImplicitDataflow)
        orderedNodesFromImRead
        -- (nodesLeftToRight imReadLhsIdent directionsInferred)
    bitWidthInferred =
      foldl
        inferBitWidth
        (singletonMapWithImRead :: ImplicitDataflow)
        (nodesLeftToRight imReadLhsIdent dimensionsInferred)
    inferDimColour :: ImplicitDataflow -> (R.Ident, VarNode) -> ImplicitDataflow
    inferDimColour mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs) dimn bitW isIn isOut ln chans) =
      let idDependedOn = case rhs of
            -- the second argument is the image, i.e.
            -- zipWith [p..] image1 ...
            R.ZipWithScalarSkel{} -> idsFromRHS rhs !! 1
            _ -> head (idsFromRHS rhs)
          incomingDimension =
            case rhs of
              (R.FoldSkel _ (R.ExprRangeArray exp) func) ->
                dimensionFromTuple exp
              _ ->
                fromJust $ dim $ fromJust $ Map.lookup idDependedOn mp
          incomingColour = chans
             -- colourType (fromJust (Map.lookup idDependedOn mp))
          -- incomingDirection =
          --   fromJust $ direction $ fromJust $ Map.lookup idDependedOn mp
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (SkelRHS rhs)
              (Just
                 ((inferDimension incomingDimension rhs) !! (idIdx - 1)))
              bitW
              isIn
              isOut
              ln
              (inferColour rhs)
           )
           mp
    inferDimColour mp (idLHS, VarNode idIdx lhsIdent (ImWriteRHS numColourChans outIdent) dimn bitW isIn isOut ln chans) =
      let idDependedOn = outIdent
          incomingDimension = dim $ fromJust $ Map.lookup idDependedOn mp
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (ImWriteRHS numColourChans outIdent)
              incomingDimension
              bitW
              isIn
              isOut
              ln
              chans
           )
           mp
    inferDimColour mp (idLHS, VarNode idIdx lhsIdent (ImReadRHS colourChans w h) dim bitW isIn isOut ln chans) =
      Map.insert
        idLHS
        (VarNode
           idIdx
           lhsIdent
           (ImReadRHS colourChans w h)
           (Just (Dim2 w h))
           bitW
           isIn
           isOut
           ln
           chans
        )
        mp
    inferBitWidth :: ImplicitDataflow -> (R.Ident, VarNode) -> ImplicitDataflow
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs@(R.ScanSkel identRhs _ _)) dimn _ isIn isOut ln chans) =
      let !exps = expsWithRenamedVars rhs
          !bitW = (maxBitWdth exps mp)
          repeatedFirings =
            let (Dim2 w h) =
                  (fromJust . dim . fromJust . Map.lookup identRhs) mp
            in w * h
          maxBW = Just $ bitW * fromIntegral repeatedFirings
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dimn maxBW isIn isOut ln chans)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs@(R.FoldVectorSkel identRhs vectorLength _ _)) dimn _ isIn isOut ln chans) =
      let !exps = expsWithRenamedVars rhs
          !bitW = maxBitWdth exps mp
          repeatedFirings = vectorLength
          maxBW = Just $ bitW * fromIntegral repeatedFirings
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dimn maxBW isIn isOut ln chans)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs) dimn _ isIn isOut ln chans) =
      let !exps = expsWithRenamedVars rhs
          incomingBW = int16Type
            -- ((fromJust . maxBitWidth . fromJust . Map.lookup (head (idsFromRHS rhs)))
            --  dfGraph)
          bitW = Just 16 -- Just (max (maxBitWdth exps mp) incomingBW)
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dimn bitW isIn isOut ln chans)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (ImWriteRHS numColourChans outIdent) dimn _ isIn isOut ln chans) =
      let bitW = Just 257
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (ImWriteRHS numColourChans outIdent)
              dimn
              bitW
              isIn
              isOut
              ln
              chans
           )
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (ImReadRHS colourChans w h) dimn _ isIn isOut ln chans) =
      let bitW = Just 257
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (ImReadRHS colourChans w h) dimn bitW isIn isOut ln chans)
           mp

-- hack for now, using replaceExprs from Pass.Inliner used to fold function args into RHSs
renameExpsInSkelRHS :: [R.Ident] -> [R.Var] -> [R.Exp] -> [R.Exp]
renameExpsInSkelRHS rhsIdents lambdaArgs exprs =
  let pretendFunArgs =
        map (\rhsIdent -> R.FunArgExprC (R.ExprVar (R.VarC rhsIdent))) rhsIdents
      pretendReplacements =
        map (\lambdaArg -> R.FunArgExprC (R.ExprVar lambdaArg)) lambdaArgs
      renameMap = Map.fromList (zip pretendReplacements pretendFunArgs)
      renamedExprs = replaceExprs exprs renameMap
  in renamedExprs

expsWithRenamedVars :: R.AssignSkelRHS -> [R.Exp]
expsWithRenamedVars (R.MapSkel rhsIdent (R.OneVarFunC vars exp)) =
  renameExpsInSkelRHS [rhsIdent] (map R.VarC (inputArgs vars)) [exp]
expsWithRenamedVars (R.FoldSkel stateExp rhsExp (R.TwoVarProcC vars1 vars2 stmts)) =
  []
  -- TODO
  -- renameExpsInSkelRHS (idsFromExp rhsExp) (map R.VarC (inputArgs vars1) ++ map R.VarC (inputArgs vars2)) [exp]
expsWithRenamedVars (R.FoldScalarSkel rhsIdent _ (R.TwoVarFunC vars1 vars2 exp)) =
  renameExpsInSkelRHS [rhsIdent] (map R.VarC (inputArgs vars1) ++ map R.VarC (inputArgs vars2)) [exp]
expsWithRenamedVars (R.FoldVectorSkel rhsIdent _ _ (R.TwoVarFunC vars1 vars2 exp)) =
  renameExpsInSkelRHS [rhsIdent] (map R.VarC (inputArgs vars1) ++ map R.VarC (inputArgs vars2)) [exp]
expsWithRenamedVars (R.ScanSkel rhsIdent _ (R.TwoVarFunC vars1 vars2 exp)) =
  renameExpsInSkelRHS [rhsIdent] (map R.VarC (inputArgs vars1) ++ map R.VarC (inputArgs vars2)) [exp]
-- expsWithRenamedVars (R.ZipWithScalarSkel (R.ExprVar (R.VarC ident1)) ident2 (R.TwoVarFunC id1 id2 exp)) =
--   renameExpsInSkelRHS
--     (map (\(ident) -> ident) [ident1,ident2])
--     (map (\(v) -> R.VarC v) [id1,id2])
--     [exp]
expsWithRenamedVars (R.ZipWithSkel rhsIdents (R.ManyVarFunC idExps exp)) =
  renameExpsInSkelRHS
    (concatMap (\(R.IdentSpaceSepC x) ->
                  case x of
                    R.IdentsOneId ident -> [ident]
                    R.IdentsManyIds idents -> idents) rhsIdents)
    (concatMap (\(R.IdentSpaceSepC x) ->
                  case x of
                    R.IdentsOneId ident -> [R.VarC ident]
                    R.IdentsManyIds idents -> map R.VarC idents) idExps)
    [exp]
expsWithRenamedVars (R.Stencil1DSkel rhsIdent _ _ (R.Stencil1DFunC xLoc exp)) = []
expsWithRenamedVars (R.Stencil2DSkel rhsIdent _ _ (R.Stencil2DFunC (R.VarListC idExps) xLoc yLoc exp)) =
  renameExpsInSkelRHS
    [rhsIdent]
    (map (\v -> v) idExps)
    [exp]
expsWithRenamedVars (R.ScaleSkel rhsIdent exp1 exp2) = [] -- TODO.
expsWithRenamedVars (R.SplitXSkel _ rhsIdent) = []
expsWithRenamedVars (R.SplitYSkel _ rhsIdent) = []

-- this is needed so that bitwidth analysis can be done on the RHS image
expsWithRenamedVars rhs =
  error ("Compiler.expsWithRenamedVars unsupported RHS: " ++ show rhs)

rhsSkelToNode :: Int -> R.Ident -> R.AssignSkelRHS -> Int -> VarNode
rhsSkelToNode idIdx (R.Ident identLhs) rhs stmtNum =
  VarNode
    idIdx
    identLhs
    (SkelRHS rhs)
    Nothing
    Nothing
    False
    False
    stmtNum
    Chan1 -- temporary

imReadNode :: R.Ident -> R.ColourType -> Integer -> Integer -> VarNode
imReadNode (R.Ident lhsIdent) colourType w h =
  let colourChans =
        case colourType of
          R.ColourTypeGray -> Chan1
          R.ColourTypeRGB -> Chan3
  in
  VarNode
    1
    lhsIdent
    (ImReadRHS colourChans w h)
    (Just (Dim2 w h))
    (Just 255)
    False
    False
    1
    colourChans

outNode outIdent@(R.Ident rhsId) varLookup =
  let dir = Just Rowwise -- TODO implement
      dimension = (Dim2 10 10) -- TODO implement
      bitW = 255
      outColourType =
        case varRHS (fromJust (Map.lookup outIdent varLookup)) of
          SkelRHS rhs -> inferColour rhs
          -- must be imread, temporary hack:
          _ ->  Chan3
        -- let SkelRHS rhs = varRHS (fromJust (Map.lookup outIdent varLookup))
        -- in inferColour rhs
  in VarNode
       1
       rhsId
       (ImWriteRHS outColourType outIdent)
       (Just dimension)
       (Just 255)
       False
       True
       100000000
       outColourType
       -- (chans (fromJust (Map.lookup outIdent varLookup)))

deriveDataflow :: R.ImageIn -> R.Ident -> [R.Assignment] -> ImplicitDataflow
deriveDataflow (R.ImageInC imReadLhsIdent colourType w h) outIdent stmts = varMap -- connectedGraph
  where
    varMap = Map.insert (R.Ident "out") (outNode outIdent varMap') varMap'
    varMap' =
      Map.fromList $
      [ (imReadLhsIdent, imReadNode imReadLhsIdent colourType w h)
      ] ++
      concatMap mkNode (zip [1 ..] stmts)
    mkNode (stmtNum, R.AssignSkelC (R.IdentsOneId lhsIdent) rhs) =
      [(lhsIdent, rhsSkelToNode 1 lhsIdent rhs stmtNum)]
    mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs) =
      map
        (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
        (zip idents [1 ..])
    -- mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitXSkel splitFactor identRs)) =
    --   map
    --     (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
    --     (zip idents [1 ..])
    -- mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitYSkel splitFactor identRs)) =
    --   map
    --     (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
    --     (zip idents [1 ..])
    mkNode x  = error ("mkNode: " ++ show x)

hashIdToInt :: R.Ident -> Int
hashIdToInt (R.Ident s) = hash (T.pack s)

traces :: [String] -> String -> a -> a
traces strings msg expr =
  unsafePerformIO $ do
    traceIO ""
    traceIO msg
    traceIO (replicate (length msg) '=')
    mapM_ traceIO strings
    return expr

showConn (Connection src dest) = showEP src ++ " --> " ++ showEP dest

showEP (Actor name port) = name ++ ":" ++ port
showEP (Port io) = "port:" ++ show io

createDataflowWires :: ImplicitDataflow -> [Actor] -> Connections
createDataflowWires dfGraph actors =
  traces (reverse (map showConn conns)) "deriving connections" conns
  where
    conns =
      nub $
      concatMap
        (\(lhsIdent, varNode) -> connectDeps lhsIdent varNode)
        (Map.toList dfGraph)
    connectDeps :: R.Ident -> VarNode -> Connections
    connectDeps lhsIdent (VarNode idIdx _ rhs _ _ _ _ _ colourChans) =
      case rhs of
        (ImReadRHS colourChans _ _) ->
          let imReadConns =
                map (\portNum ->
                Connection
                { src = Port ("In" ++ show portNum)
                , dest = Actor (idToString lhsIdent) ("In"++show portNum ++ "_" ++ show 1)
                })
                [1..case colourChans of Chan3 -> 3; Chan1 -> 1]
          in imReadConns
        (ImWriteRHS colourChans rhsIdent) ->
          let outConns =
                map (\portNum ->
                Connection
                { src = Actor (idToString rhsIdent) ("Out" ++ show portNum ++ "_" ++ show 1)
                , dest = Port ("Out" ++ show portNum)})
                [1..case colourChans of Chan3 -> 3; Chan1 -> 1]
          in outConns
        (SkelRHS (R.MapSkel (R.Ident fromIdent) fun)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
          ++ mkFuncDepConnsElemUnary lhsIdent fun
        (SkelRHS (R.ScanSkel (R.Ident fromIdent) _ _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.SplitXSkel _ (R.Ident fromIdent))) ->
          mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitX") colourChans
          ++ mkConn (R.Ident (fromIdent ++ "_splitX")) 1 1 fromIdent colourChans
        (SkelRHS (R.SplitYSkel _ (R.Ident fromIdent))) ->
          mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitY") colourChans
          ++ mkConn (R.Ident (fromIdent ++ "_splitY")) 1 1 fromIdent colourChans
        (SkelRHS (R.FoldSkel _ (R.ExprVar (R.VarC (R.Ident fromIdent))) _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.FoldSkel _ _ _)) ->
          []
        (SkelRHS (R.FoldScalarSkel (R.Ident fromIdent) _ _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.FoldVectorSkel (R.Ident fromIdent) _ _ _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.Stencil1DSkel (R.Ident fromIdent) _ _ _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.Stencil2DSkel (R.Ident fromIdent) _ _ _)) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.ScaleSkel _ _ (R.Ident fromIdent))) ->
          mkConn lhsIdent 1 idIdx fromIdent colourChans
        (SkelRHS (R.ZipWithSkel idents _)) ->
          concatMap
            (\(i, R.IdentSpaceSepC idents) ->
               case idents of
                 R.IdentsOneId (R.Ident fromIdent) ->
                   mkConn lhsIdent i idIdx fromIdent colourChans
                 R.IdentsManyIds idents -> error "error connecting zipWith")
            (zip [1 ..] idents)
        (SkelRHS (R.ZipWithScalarSkel (R.ExprVar (R.VarC id1)) id2 _)) ->
          concatMap
            (\(i, (R.Ident fromIdent)) ->
               mkConn lhsIdent i idIdx fromIdent colourChans)
            (zip [1 ..] [id1,id2])
        rhs -> error ("unsupported rhs in createDataflowWires: " ++ show rhs)
    mkConn (R.Ident lhsIdent) inIdx outIdx fromIdent colourChans =
      -- TODO: how to deal with naming when multiple inputs,
      -- i.e. zipWith
      -- this was the previous solution:
      -- Connection
      -- { src = Actor fromIdent ("Out" ++ show outIdx)
      -- , dest = Actor lhsIdent ("In" ++ show inIdx)
      -- }
      map (\i ->
      Connection
      { src = Actor fromIdent ("Out" ++ show i ++ "_" ++ show outIdx)
      , dest = Actor lhsIdent ("In"  ++ show i ++ "_" ++ show inIdx)
      })
      [1..case colourChans of Chan3 -> 3; Chan1 -> 1]
    mkFuncDepConnsElemUnary (R.Ident lhsIdent) fun@(R.OneVarFunC ident exp) =
      let idents = globalIdentsElemUnary fun
      in map (\ident@(R.Ident identStr) ->
                Connection
                { src  = Actor identStr ("Out1")
                , dest = Actor lhsIdent (identStr ++ "Port")
                }
             ) idents

genActors :: R.Ident -> ImplicitDataflow -> Integer -> ([String], [Actor])
genActors outIdent dfGraph numFrames =
  (unusedVariableActors, nub $ concatMap mkActor (Map.elems dfGraph))
  where
    mkActor (VarNode idIdx lhsIdent (ImReadRHS numColourChans wIn hIn) _dimension _bitW _ _isOut _ _) =
      let outNode = fromJust $ Map.lookup outIdent dfGraph
          (Dim2 wOut hOut) = fromJust (dim outNode)
      in [ RiplUnit
             "std.headers"
             "Parameters"
             (parametersActor wIn hIn wOut hOut numFrames)
         , skeletonToIdentityActor (R.Ident lhsIdent) numColourChans
         ]
    mkActor (VarNode idIdx lhsIdent (ImWriteRHS numColourChans outIdent) _dimension _bitW _ _isOut _ _) =
      [] -- [skeletonToIdentityActor outIdent]
    mkActor (VarNode idIdx lhsIdent (SkelRHS rhs) dimension _bitW _ _isOut _ _) =
      case rhs of
        (R.MapSkel _rhsIdent _anonFun) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        (R.Stencil1DSkel rhsIdent windowWidth windowHeight kernelValues) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.Stencil2DSkel rhsIdent windowWidth windowHeight kernelValues) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.ScanSkel _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.SplitXSkel _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.SplitYSkel _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.FoldSkel _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.FoldScalarSkel _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.FoldVectorSkel _ _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.ZipWithSkel _ _) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        (R.ZipWithScalarSkel _ _ _) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        (R.ScaleSkel _ _ _) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        _ -> error ("Unsupported rhs in genActors: " ++ show rhs)
    unusedVariableActors =
      let allLhsIdents = Map.keys dfGraph
          allRhsIdents =
            concatMap
              (\(VarNode _ _ rhs _ _ _ _ _ _) ->
                 case rhs of
                   SkelRHS assRHS -> idsFromRHS assRHS
                   _ -> [])
              (Map.elems dfGraph) :: [R.Ident]
          unusedIdents =
            filter
              (\ident ->
                 not (elem ident allRhsIdents) &&
                 ident /= outIdent && ident /= (R.Ident "out"))
              (nub allLhsIdents) -- TODO: nub is a hack, why duplicates?
          -- in traces (map show unusedIdents) "Unused variables" $ map (\ident -> varToConsumerActor ident) unusedIdents
      in map (\(R.Ident ident) -> ident) unusedIdents

skeletonToActors :: String
                 -> Dimension
                 -> R.AssignSkelRHS
                 -> ImplicitDataflow
                 -> [Actor]
skeletonToActors lhsId _ (R.MapSkel identRhs anonFun) dfGraph =
  let bitWidthIncoming =
        (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = (lhsId)
       , actorAST = mapActor lhsId anonFun calTypeIncoming calTypeOutgoing dfGraph
       }
     ]

skeletonToActors lhsId dim (R.FoldSkel stateExp identExp anonFun) dfGraph =
  let bitWidthIncoming =
        case identExp of
          R.ExprVar (R.VarC identRhs) ->
            (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = (lhsId)
       , actorAST =
           foldActor
             lhsId
             stateExp
             identExp
             anonFun
             dfGraph
       }
     ]

-- skeletonToActors lhsId dim@(Dimension width height) (R.Stencil1DSkel identRhs winWidth winHeight userDefinedFunc) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            stencil1DActor
--              lhsId
--              dim
--              userDefinedFunc
--              calTypeIncoming
--              calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.Stencil2DSkel identRhs winWidth winHeight userDefinedFunc) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            stencil2DActor
--              lhsId
--              dim
--              userDefinedFunc
--              calTypeIncoming
--              calTypeOutgoing
--        }
--      ]


-- skeletonToActors lhsId dim@(Dimension width height) (R.ScanSkel identRhs initInt anonFun) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            scanActor (lhsId) initInt dim anonFun calTypeIncoming calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.SplitXSkel splitFactor rhsId) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = idRiplShow rhsId ++ "_splitX"
--        , actorAST =
--            splitXActor (idRiplShow rhsId ++ "_splitX") splitFactor (idRiplToCal rhsId) calTypeIncoming calTypeOutgoing
--        }
--      , RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
--        }
--   ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.SplitYSkel splitFactor rhsId) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = idRiplShow rhsId ++ "_splitY"
--        , actorAST =
--            splitYActor (idRiplShow rhsId ++ "_splitY") dim splitFactor (idRiplToCal rhsId) calTypeIncoming calTypeOutgoing
--        }
--      , RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
--        }
--   ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.FoldScalarSkel identRhs initInt anonFun) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            foldScalarActor
--              (lhsId)
--              initInt
--              dim
--              anonFun
--              calTypeIncoming
--              calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.FoldVectorSkel identRhs vectorLength initInt anonFun) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing =  calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            foldVectorActor
--              (lhsId)
--              vectorLength
--              initInt
--              dim
--              anonFun
--              calTypeIncoming
--              calTypeOutgoing
--        }
--      ]
skeletonToActors lhsId (Dim2 width height) (R.ZipWithSkel identsRhs exp) dfGraph =
  let bitWidthIncoming =
        let bitWidths =
              map
                (\(R.IdentSpaceSepC x) ->
                   case x of
                     R.IdentsOneId identRhs ->
                       (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
                       dfGraph
                     R.IdentsManyIds idents ->
                       error "error inferring bitwidths from zipWith"
                     )
                identsRhs
        in maximum bitWidths
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = lhsId
       , actorAST = zipWithActor (lhsId) exp calTypeIncoming calTypeOutgoing
       }
     ]

skeletonToActors lhsId (Dim2 width height) (R.FoldSkel expState expRhs expFun) dfGraph =
  [ RiplActor
    { package = "cal"
    , actorName = lhsId
    , actorAST = foldActor lhsId expState expRhs expFun dfGraph
    }
  ]

-- skeletonToActors lhsId dim@(Dimension width height) (R.ZipWithScalarSkel initVarExp ident1 exp) dfGraph =
--   let bitWidthIncoming =
--         let bitWidths =
--               map
--                 (\identRhs ->
--                    (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
--                      dfGraph)
--                 [ident1]
--         in maximum bitWidths
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST =
--            zipWithScalarActor (lhsId) exp dim calTypeIncoming calTypeOutgoing
--        }
--      ]

-- skeletonToActors lhsId (Dimension width height) (R.ScaleSkel scaleFactorWidth scaleFactorHeight identRhs) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--       dimension@(Dimension widthPreScale heightPreScale) =
--            (fromJust . dim . fromJust . Map.lookup identRhs) dfGraph
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST = scaleActor lhsId scaleFactorWidth scaleFactorHeight widthPreScale calTypeIncoming calTypeOutgoing
--        }
--      ]
skeletonToActors _ _ rhs _ =
  error ("unsupported rhs in skeletonToActors: " ++ show rhs)

skeletonToIdentityActor :: R.Ident -> Chans -> Actor
skeletonToIdentityActor ident numColourChans =
  RiplActor
  { package = "cal"
  , actorName = (idToString ident)
  , actorAST =
      identityActor
        (idToString ident)
        (C.TypParam
           C.TInt
           [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit 32)))])
        (C.TypParam
           C.TInt
           [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit 32)))])
        numColourChans
  }

varToConsumerActor :: R.Ident -> Actor
varToConsumerActor ident =
  RiplActor
  { package = "cal"
  , actorName = (idToString ident)
  , actorAST = consumerActor (idToString ident)
  }

maybeError :: String -> Maybe a -> a
maybeError s Nothing = error s
maybeError _ (Just a) = a
