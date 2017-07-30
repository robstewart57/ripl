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
-- import SkeletonTemplates.Repeat
-- import SkeletonTemplates.Imap
-- import SkeletonTemplates.Unzip
-- import SkeletonTemplates.Iunzip
-- import SkeletonTemplates.Convolve
import SkeletonTemplates.Stencil1D
import SkeletonTemplates.Stencil2D
-- import SkeletonTemplates.IUnzipFilter2D
import SkeletonTemplates.SplitX
import SkeletonTemplates.SplitY
import SkeletonTemplates.Scale
import SkeletonTemplates.Scan
import SkeletonTemplates.FoldScalar
import SkeletonTemplates.FoldVector
import SkeletonTemplates.ZipWith
import SkeletonTemplates.ZipWithScalar
-- import SkeletonTemplates.ZipWithVector
-- import SkeletonTemplates.Transpose
import SkeletonTemplates.Identity
import SkeletonTemplates.Parameters
import Inference.Offset
import Inference.Dimension
import Inference.BitWidth
import Pass.Inliner
import Debug.Trace

compile :: R.Program -> Integer -> (CalProject, Dimension, Int, [String])
compile (R.ProgramC _ functions imRead@(R.ImageInC imReadLhsIdent w h) stmts (R.DataOutC outIdent)) numFrames =
  let !inlinedStmts = inlineFunctions functions stmts
      astToIr =
        inferStreamDirectionsAndDimensionAndBitWidth imRead .
        deriveDataflow imRead outIdent
      dfGraph = trace
                ("ASSIGN LHSs: " ++ (concatMap (\(R.AssignSkelC idents _) -> show idents ++ "\n") inlinedStmts))
                (astToIr inlinedStmts)
      (unusedActors, actors) = genActors outIdent dfGraph numFrames
      showIdent (R.Ident x) = x
      network = createDataflowWires dfGraph actors
      outImageDim = fromJust $ dim $ fromJust $ Map.lookup outIdent dfGraph
      outImageBitWidth =
        fromJust $ maxBitWidth $ fromJust $ Map.lookup outIdent dfGraph
      outImageBitWidthUIint = calTypeFromCalBWUInt (correctBW outImageBitWidth)
  -- in traces (map show inlinedStmts) "Inlined statements" $
  in ( CalProject actors network
     , outImageDim
     , outImageBitWidthUIint
     , unusedActors)

nodesLeftToRight :: R.Ident -> ImplicitDataflow -> [(R.Ident, VarNode)]
nodesLeftToRight _notUsed mp =
  let xs = Map.toList mp
  in sortBy
       (\(_, VarNode _ _ _ _ _ _ _ _ i) (_, VarNode _ _ _ _ _ _ _ _ j) ->
          if i < j
            then LT
            else if i == j
                   then EQ
                   else GT)
       xs

inferStreamDirectionsAndDimensionAndBitWidth :: R.ImageIn
                                             -> ImplicitDataflow
                                             -> ImplicitDataflow
inferStreamDirectionsAndDimensionAndBitWidth (R.ImageInC imReadLhsIdent w h) dfGraph =
  traces
    ((map
        (\(lhsIdent, node) ->
           idToString lhsIdent ++
           ": " ++
           show (fromJust (direction node)) ++
           ",\t maxBitWidth: " ++
           show (fromJust (maxBitWidth node)) ++
           ",\t " ++ show (fromJust (dim node))))
       (Map.toList inferredNodes))
    "Nodes"
    inferredNodes
  where
    !inferredNodes = bitWidthInferred -- Map.fromList dimensionsInferred
    orderedNodesFromImRead =
      (imReadLhsIdent, imReadNode imReadLhsIdent w h) :
      nodesLeftToRight imReadLhsIdent dfGraph :: [(R.Ident, VarNode)]
    singletonMapWithImRead =
      Map.singleton imReadLhsIdent (imReadNode imReadLhsIdent w h)
    directionsInferred = trace ("NODES: " ++ show (map fst orderedNodesFromImRead)) $
      foldl
        inferDir
        (singletonMapWithImRead :: ImplicitDataflow)
        orderedNodesFromImRead
    dimensionsInferred =
      foldl
        inferDim
        (singletonMapWithImRead :: ImplicitDataflow)
        (nodesLeftToRight imReadLhsIdent directionsInferred)
    !bitWidthInferred =
      foldl
        inferBitWidth
        (singletonMapWithImRead :: ImplicitDataflow)
        (nodesLeftToRight imReadLhsIdent dimensionsInferred)
    inferDim :: ImplicitDataflow -> (R.Ident, VarNode) -> ImplicitDataflow
    inferDim mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs) dirn dimn bitW isIn isOut ln) =
      let idDependedOn = head (idsFromRHS rhs)
          incomingDimension =
            fromJust $ dim $ fromJust $ Map.lookup idDependedOn mp
          incomingDirection =
            fromJust $ direction $ fromJust $ Map.lookup idDependedOn mp
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (SkelRHS rhs)
              dirn
              (Just
                 (inferDimension
                    incomingDimension
                    incomingDirection
                    (fromJust dirn)
                    rhs))
              bitW
              isIn
              isOut
              ln)
           mp
    inferDim mp (idLHS, VarNode idIdx lhsIdent (ImWriteRHS outIdent) dirn dimn bitW isIn isOut ln) =
      let idDependedOn = outIdent
          incomingDimension = dim $ fromJust $ Map.lookup idDependedOn mp
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (ImWriteRHS outIdent)
              dirn
              incomingDimension
              bitW
              isIn
              isOut
              ln)
           mp
    inferDim mp (idLHS, VarNode idIdx lhsIdent (ImReadRHS w h) dirn dim bitW isIn isOut ln) =
      Map.insert
        idLHS
        (VarNode
           idIdx
           lhsIdent
           (ImReadRHS w h)
           dirn
           (Just (Dimension w h))
           bitW
           isIn
           isOut
           ln)
        mp
    inferDir :: ImplicitDataflow -> (R.Ident, VarNode) -> ImplicitDataflow
    inferDir mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs) _ dim bitW isIn isOut ln) =
      let idDependedOn = head (idsFromRHS rhs)
          incomingDirection =
            direction $
            fromMaybe
              (error
                 ("inferDir: cannt infer direction for : " ++
                  idRiplShow idDependedOn ++
                  ", in:\n" ++ show rhs ++ "\nin scope variables:\n" ++ show (Map.keys mp)))
              (Map.lookup idDependedOn mp)
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (SkelRHS rhs)
              incomingDirection
              dim
              bitW
              isIn
              isOut
              ln)
           mp
    inferDir mp (idLHS, VarNode idIdx lhsIdent (ImReadRHS w h) _ dim bitW isIn isOut ln) =
      Map.insert
        idLHS
        (VarNode
           idIdx
           lhsIdent
           (ImReadRHS w h)
           (Just Rowwise)
           dim
           bitW
           isIn
           isOut
           ln)
        mp
    inferDir mp (idLHS, VarNode idIdx lhsIdent (ImWriteRHS outIdent) _ dim bitW isIn isOut ln) =
      let idDependedOn = outIdent
          incomingDirection = direction $ -- fromJust $ Map.lookup idDependedOn mp
            fromMaybe
              (error
                 ("inferDir: cannt infer direction in 'out' for : " ++
                  idRiplShow idDependedOn ++
                  "\nin scope variables:\n" ++ show (Map.keys mp)))
              (Map.lookup idDependedOn mp)
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (ImWriteRHS outIdent)
              incomingDirection
              dim
              bitW
              isIn
              isOut
              ln)
           mp
    inferBitWidth :: ImplicitDataflow -> (R.Ident, VarNode) -> ImplicitDataflow
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs@(R.ScanSkel identRhs _ _)) dirn dimn _ isIn isOut ln) =
      let !exps = expsWithRenamedVars rhs
          !bitW = (maxBitWdth exps mp)
          repeatedFirings =
            let (Dimension w h) =
                  (fromJust . dim . fromJust . Map.lookup identRhs) mp
            in w * h
          maxBW = Just $ bitW * fromIntegral repeatedFirings
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dirn dimn maxBW isIn isOut ln)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs@(R.FoldVectorSkel identRhs vectorLength _ _)) dirn dimn _ isIn isOut ln) =
      let !exps = expsWithRenamedVars rhs
          !bitW = maxBitWdth exps mp
          repeatedFirings = vectorLength
          maxBW = Just $ bitW * fromIntegral repeatedFirings
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dirn dimn maxBW isIn isOut ln)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (SkelRHS rhs) dirn dimn _ isIn isOut ln) =
      let !exps = expsWithRenamedVars rhs
          incomingBW = int16Type
            -- ((fromJust . maxBitWidth . fromJust . Map.lookup (head (idsFromRHS rhs)))
            --  dfGraph)
          bitW = Just 16 -- Just (max (maxBitWdth exps mp) incomingBW)
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (SkelRHS rhs) dirn dimn bitW isIn isOut ln)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (ImWriteRHS outIdent) dirn dimn _ isIn isOut ln) =
      let bitW = Just 257
      in Map.insert
           idLHS
           (VarNode
              idIdx
              lhsIdent
              (ImWriteRHS outIdent)
              dirn
              dimn
              bitW
              isIn
              isOut
              ln)
           mp
    inferBitWidth mp (idLHS, VarNode idIdx lhsIdent (ImReadRHS w h) dirn dimn _ isIn isOut ln) =
      let bitW = Just 257
      in Map.insert
           idLHS
           (VarNode idIdx lhsIdent (ImReadRHS w h) dirn dimn bitW isIn isOut ln)
           mp
    -- support potentially multiple incoming (e.g. zipWith), and multiple incoming must all agree on direction.
    incomingStreamDirectionGo :: [VarNode] -> Maybe Direction -> Direction
    incomingStreamDirectionGo [] Nothing =
      error "No incoming links, cannot infer row/column stream direction"
    incomingStreamDirectionGo [] (Just d) = d :: Direction
    incomingStreamDirectionGo (x:xs) Nothing =
      incomingStreamDirectionGo
        xs
        (Just
           (maybeError
              ("incomingStreamDirectionGo: Nothing, incoming links: " ++
               ", X: " ++ show x)
              (direction x) {- show incomingLinks ++ -}
            ))
    incomingStreamDirectionGo (x:xs) (Just d) =
      if d == maybeError "incomingStreamDirectionGo (Just d)" (direction x)
        then incomingStreamDirectionGo xs (Just (fromJust (direction x)))
        else error
               "incoming streams to a variable are not traversing in the same dimnension, i.e. either row or columnwise"

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
expsWithRenamedVars (R.MapSkel rhsIdent (R.OneVarFunC id1 exp)) =
  renameExpsInSkelRHS [rhsIdent] [R.VarC id1] [exp]
expsWithRenamedVars (R.FoldScalarSkel rhsIdent _ (R.TwoVarFunC id1 id2 exp)) =
  renameExpsInSkelRHS [rhsIdent] [R.VarC id2, R.VarC id1] [exp]
expsWithRenamedVars (R.FoldVectorSkel rhsIdent _ _ (R.TwoVarFunC id1 id2 exp)) =
  renameExpsInSkelRHS [rhsIdent] [R.VarC id2, R.VarC id1] [exp]
expsWithRenamedVars (R.ScanSkel rhsIdent _ (R.TwoVarFunC id1 id2 exp)) =
  renameExpsInSkelRHS [rhsIdent] [R.VarC id2, R.VarC id1] [exp]
-- expsWithRenamedVars (R.ZipWithScalarSkel rhsIdents (R.AnonFunC idExps exp)) =
--   renameExpsInSkelRHS
--     (map (\(R.IdentSpaceSepC ident) -> ident) rhsIdents)
--     (map (\(R.ExpSpaceSepC (R.ExprVar v)) -> v) idExps)
--     [exp]
-- expsWithRenamedVars (R.ZipWithVectorSkel rhsIdents (R.AnonFunBinaryC id1 id2 exp)) =
--   let exps =
--         renameExpsInSkelRHS
--           (map (\(R.IdentSpaceSepC ident) -> ident) rhsIdents)
--           [R.VarC id1, R.VarC id2]
--           [exp]
--   in exps
expsWithRenamedVars (R.ZipWithSkel rhsIdents (R.ManyVarFunC idExps exp)) =
  renameExpsInSkelRHS
    (map (\(R.IdentSpaceSepC ident) -> ident) rhsIdents)
    (map (\(R.ExpSpaceSepC (R.ExprVar v)) -> v) idExps)
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
    Nothing
    False
    False
    stmtNum

imReadNode :: R.Ident -> Integer -> Integer -> VarNode
imReadNode (R.Ident lhsIdent) w h =
  VarNode
    1
    lhsIdent
    (ImReadRHS w h)
    (Just Rowwise)
    (Just (Dimension w h))
    (Just 255)
    False
    False
    1

outNode outIdent@(R.Ident rhsId) =
  let dir = Just Rowwise -- TODO implement
      dimension = (Dimension 10 10) -- TODO implement
      bitW = 255
  in VarNode
       1
       rhsId
       (ImWriteRHS outIdent)
       dir
       (Just dimension)
       (Just 255)
       False
       True
       100000000

deriveDataflow :: R.ImageIn -> R.Ident -> [R.Assignment] -> ImplicitDataflow
deriveDataflow (R.ImageInC imReadLhsIdent w h) outIdent stmts = varMap -- connectedGraph
  where
    varMap =
      Map.fromList $
      [ (imReadLhsIdent, imReadNode imReadLhsIdent w h)
      , (R.Ident "out", outNode outIdent)
      ] ++
      concatMap mkNode (zip [1 ..] stmts)
    mkNode (stmtNum, R.AssignSkelC (R.IdentsOneId lhsIdent) rhs) =
      [(lhsIdent, rhsSkelToNode 1 lhsIdent rhs stmtNum)]
    mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitXSkel splitFactor identRs)) =
      map
        (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
        (zip idents [1 ..])
    mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitYSkel splitFactor identRs)) =
      map
        (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
        (zip idents [1 ..])

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
    connectDeps lhsIdent (VarNode idIdx _ rhs _ _ _ _ _ _) =
      case rhs of
        (ImReadRHS {}) ->
          let imReadConn =
                Connection
                {src = Port In, dest = Actor (idToString lhsIdent) "In1"}
          in [imReadConn]
        (ImWriteRHS rhsIdent) ->
          let outConn =
                Connection
                {src = Actor (idToString rhsIdent) "Out1", dest = Port Out}
          in [outConn]
        -- (SkelRHS (R.UnzipSkel (R.Ident fromIdent) _)) ->
        --   [ mkConn lhsIdent 1 idIdx (fromIdent ++ "_unzip")
        --   , mkConn (R.Ident (fromIdent ++ "_unzip")) 1 1 fromIdent
        --   ]
        -- (SkelRHS (R.IUnzipSkel (R.Ident fromIdent) _ _)) ->
        --   [ mkConn lhsIdent 1 idIdx (fromIdent ++ "_iunzip")
        --   , mkConn (R.Ident (fromIdent ++ "_iunzip")) 1 1 fromIdent
        --   ]
        -- (SkelRHS (R.IUnzipFilter2DSkel (R.Ident fromIdent) _ _ _ _)) ->
        --   [ mkConn lhsIdent 1 idIdx (fromIdent ++ "_iunzipFilter2D")
        --   , mkConn (R.Ident (fromIdent ++ "_iunzipFilter2D")) 1 1 fromIdent
        --   ]
        (SkelRHS (R.MapSkel (R.Ident fromIdent) fun)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
          ++ mkFuncDepConnsElemUnary lhsIdent fun
        -- (SkelRHS (R.ImapSkel (R.Ident fromIdent) _)) ->
        --   [mkConn lhsIdent 1 idIdx fromIdent]
        -- (SkelRHS (R.TransposeSkel (R.Ident fromIdent))) ->
        --   [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.ScanSkel (R.Ident fromIdent) _ _)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.SplitXSkel _ (R.Ident fromIdent))) ->
          [ mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitX")
          , mkConn (R.Ident (fromIdent ++ "_splitX")) 1 1 fromIdent
          ]
        (SkelRHS (R.SplitYSkel _ (R.Ident fromIdent))) ->
          [ mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitY")
          , mkConn (R.Ident (fromIdent ++ "_splitY")) 1 1 fromIdent
          ]
        (SkelRHS (R.FoldScalarSkel (R.Ident fromIdent) _ _)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.FoldVectorSkel (R.Ident fromIdent) _ _ _)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        -- (SkelRHS (R.ConvolveSkel (R.Ident fromIdent) _ _ _)) ->
        --   [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.Stencil1DSkel (R.Ident fromIdent) _ _ _)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.Stencil2DSkel (R.Ident fromIdent) _ _ _)) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.ScaleSkel _ _ (R.Ident fromIdent))) ->
          [mkConn lhsIdent 1 idIdx fromIdent]
        -- (SkelRHS (R.RepeatSkel (R.Ident fromIdent) _)) ->
        --   [mkConn lhsIdent 1 idIdx fromIdent]
        (SkelRHS (R.ZipWithSkel idents _)) ->
          map
            (\(i, R.IdentSpaceSepC (R.Ident fromIdent)) ->
               mkConn lhsIdent i idIdx fromIdent)
            (zip [1 ..] idents)
        -- (SkelRHS (R.ZipWithScalarSkel idents _)) ->
        --   map
        --     (\(i, R.IdentSpaceSepC (R.Ident fromIdent)) ->
        --        mkConn lhsIdent i idIdx fromIdent)
        --     (zip [1 ..] idents)
        -- (SkelRHS (R.ZipWithVectorSkel idents _)) ->
        --   map
        --     (\(i, R.IdentSpaceSepC (R.Ident fromIdent)) ->
        --        mkConn lhsIdent i idIdx fromIdent)
        --     (zip [1 ..] idents)
        rhs -> error ("unsupported rhs in createDataflowWires: " ++ show rhs)
    mkConn (R.Ident lhsIdent) inIdx outIdx fromIdent =
      Connection
      { src = Actor fromIdent ("Out" ++ show outIdx)
      , dest = Actor lhsIdent ("In" ++ show inIdx)
      }
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
    mkActor (VarNode idIdx lhsIdent (ImReadRHS wIn hIn) _direction _dimension _bitW _ _isOut _) =
      let outNode = fromJust $ Map.lookup outIdent dfGraph
          (Dimension wOut hOut) = fromJust (dim outNode)
      in [ RiplUnit
             "std.headers"
             "Parameters"
             (parametersActor wIn hIn wOut hOut numFrames)
         , skeletonToIdentityActor (R.Ident lhsIdent)
         ]
    mkActor (VarNode idIdx lhsIdent (ImWriteRHS outIdent) _direction _dimension _bitW _ _isOut _) =
      [] -- [skeletonToIdentityActor outIdent]
    mkActor (VarNode idIdx lhsIdent (SkelRHS rhs) _direction dimension _bitW _ _isOut _) =
      case rhs of
        (R.MapSkel _rhsIdent _anonFun) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.ImapSkel _rhsIdent _anonFun) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.UnzipSkel _rhsIdent _anonFun) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.IUnzipSkel _rhsIdent _anonFun1 _anonFun2) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.TransposeSkel _rhsIdent) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.AppendSkel rhsIdent1 rhsIdent2) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.ConvolveSkel rhsIdent windowWidth windowHeight kernelValues) ->
        --   skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.Stencil1DSkel rhsIdent windowWidth windowHeight kernelValues) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.Stencil2DSkel rhsIdent windowWidth windowHeight kernelValues) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        -- (R.IUnzipFilter2DSkel rhsIdent windowWidth windowHeight e1 e2) ->
        --   skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.ScanSkel _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.SplitXSkel _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.SplitYSkel _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.FoldScalarSkel _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        (R.FoldVectorSkel _ _ _ _) ->
          skeletonToActors lhsIdent (dimensionOfRHSId rhs dfGraph) rhs dfGraph
        -- (R.RepeatSkel _ _) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        (R.ZipWithSkel _ _) ->
          skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.ZipWithScalarSkel _ _) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
        -- (R.ZipWithVectorSkel _ _) ->
        --   skeletonToActors lhsIdent (fromJust dimension) rhs dfGraph
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
skeletonToActors lhsId dim@(Dimension width height) (R.Stencil1DSkel identRhs winWidth winHeight userDefinedFunc) dfGraph =
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
       , actorAST =
           stencil1DActor
             lhsId
             dim
             userDefinedFunc
             calTypeIncoming
             calTypeOutgoing
       }
     ]
skeletonToActors lhsId dim@(Dimension width height) (R.Stencil2DSkel identRhs winWidth winHeight userDefinedFunc) dfGraph =
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
       , actorAST =
           stencil2DActor
             lhsId
             dim
             userDefinedFunc
             calTypeIncoming
             calTypeOutgoing
       }
     ]

-- skeletonToActors lhsId _ (R.ImapSkel identRhs anonFun) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST = imapActor lhsId anonFun calTypeIncoming calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId _ (R.UnzipSkel identRhs anonFun) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = idRiplShow identRhs ++ "_unzip"
--        , actorAST = unzipActor (idRiplShow identRhs ++ "_unzip") anonFun
--        }
--      , RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId _ (R.IUnzipSkel identRhs anonFun1 anonFun2) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW (max thisBitWidth bitWidthIncoming))
--   in let dimension@(Dimension widthPreTransposed heightPreTransposed) =
--            (fromJust . dim . fromJust . Map.lookup identRhs) dfGraph
--      in [ RiplActor
--           { package = "cal"
--           , actorName = idRiplShow identRhs ++ "_iunzip"
--           , actorAST =
--               iunzipActor
--                 (idRiplShow identRhs ++ "_iunzip")
--                 anonFun1
--                 anonFun2
--                 dimension
--           }
--         , RiplActor
--           { package = "cal"
--           , actorName = lhsId
--           , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
--           }
--         ]
-- skeletonToActors lhsId (Dimension width height) (R.TransposeSkel identRhs) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in let (Dimension widthPreTransposed heightPreTransposed) =
--            (fromJust . dim . fromJust . Map.lookup identRhs) dfGraph
--      in [ RiplActor
--           { package = "cal"
--           , actorName = (lhsId)
--           , actorAST =
--               transposeActorUsingAccumulatorActions
--                 (lhsId)
--                 widthPreTransposed
--                 heightPreTransposed
--                 calTypeIncoming
--                 calTypeOutgoing
--           }
--         ]
-- skeletonToActors lhsId dim@(Dimension width height) (R.ConvolveSkel identRhs winWidth winHeight (R.KernelValuesC kernelValues)) dfGraph =
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
--            convolveActor
--              lhsId
--              dim
--              (map (\(R.KernelValueC exp) -> exp) kernelValues)
--              calTypeIncoming
--              calTypeOutgoing
--        }
--      ]
-- skeletonToActors lhsId dim'@(Dimension width height) (R.IUnzipFilter2DSkel identRhs winWidth winHeight anonFun1 anonFun2) dfGraph =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in let dimension@(Dimension widthPreTransposed heightPreTransposed) =
--            (fromJust . dim . fromJust . Map.lookup identRhs) dfGraph
--      in [ RiplActor
--           { package = "cal"
--           , actorName = idRiplShow identRhs ++ "_iunzipFilter2D"
--           , actorAST =
--               iunzipFilter2DActor
--                 (idRiplShow identRhs ++ "_iunzipFilter2D")
--                 dimension
--                 anonFun1
--                 anonFun2
--                 calTypeIncoming
--                 calTypeOutgoing
--           }
--         , RiplActor
--           { package = "cal"
--           , actorName = lhsId
--           , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
--           }
--         ]

skeletonToActors lhsId dim@(Dimension width height) (R.ScanSkel identRhs initInt anonFun) dfGraph =
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
       , actorAST =
           scanActor (lhsId) initInt dim anonFun calTypeIncoming calTypeOutgoing
       }
     ]
skeletonToActors lhsId dim@(Dimension width height) (R.SplitXSkel splitFactor rhsId) dfGraph =
  let bitWidthIncoming =
        (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = idRiplShow rhsId ++ "_splitX"
       , actorAST =
           splitXActor (idRiplShow rhsId ++ "_splitX") splitFactor (idRiplToCal rhsId) calTypeIncoming calTypeOutgoing
       }
     , RiplActor
       { package = "cal"
       , actorName = lhsId
       , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
       }
  ]
skeletonToActors lhsId dim@(Dimension width height) (R.SplitYSkel splitFactor rhsId) dfGraph =
  let bitWidthIncoming =
        (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = idRiplShow rhsId ++ "_splitY"
       , actorAST =
           splitYActor (idRiplShow rhsId ++ "_splitY") dim splitFactor (idRiplToCal rhsId) calTypeIncoming calTypeOutgoing
       }
     , RiplActor
       { package = "cal"
       , actorName = lhsId
       , actorAST = identityActor lhsId calTypeIncoming calTypeOutgoing
       }
  ]
skeletonToActors lhsId dim@(Dimension width height) (R.FoldScalarSkel identRhs initInt anonFun) dfGraph =
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
       , actorAST =
           foldScalarActor
             (lhsId)
             initInt
             dim
             anonFun
             calTypeIncoming
             calTypeOutgoing
       }
     ]
skeletonToActors lhsId dim@(Dimension width height) (R.FoldVectorSkel identRhs vectorLength initInt anonFun) dfGraph =
  let bitWidthIncoming =
        (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing =  calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = (lhsId)
       , actorAST =
           foldVectorActor
             (lhsId)
             vectorLength
             initInt
             dim
             anonFun
             calTypeIncoming
             calTypeOutgoing
       }
     ]
skeletonToActors lhsId (Dimension width height) (R.ZipWithSkel identsRhs exp) dfGraph =
  let bitWidthIncoming =
        let bitWidths =
              map
                (\(R.IdentSpaceSepC identRhs) ->
                   (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
                     dfGraph)
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
skeletonToActors lhsId dim@(Dimension width height) (R.ZipWithScalarSkel ident1 initInt exp) dfGraph =
  let bitWidthIncoming =
        let bitWidths =
              map
                (\identRhs ->
                   (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
                     dfGraph)
                [ident1]
        in maximum bitWidths
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  in [ RiplActor
       { package = "cal"
       , actorName = lhsId
       , actorAST =
           zipWithScalarActor (lhsId) exp dim calTypeIncoming calTypeOutgoing
       }
     ]
-- skeletonToActors lhsId dim'@(Dimension width height) (R.ZipWithVectorSkel identRhs@[imageId, R.IdentSpaceSepC vectId] exp) dfGraph =
--   let bitWidthIncoming1 =
--         let R.IdentSpaceSepC identRhs1 = identRhs !! 0
--         in (fromJust . maxBitWidth . fromJust . Map.lookup identRhs1) dfGraph
--       bitWidthIncoming2 =
--         let R.IdentSpaceSepC identRhs2 = identRhs !! 1
--         in (fromJust . maxBitWidth . fromJust . Map.lookup identRhs2) dfGraph
--       calTypeIncoming1 = calTypeFromCalBW (correctBW bitWidthIncoming1)
--       calTypeIncoming2 = calTypeFromCalBW (correctBW bitWidthIncoming2)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            dfGraph)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in let vectorDim = (fromJust . dim . fromJust . Map.lookup vectId) dfGraph
--      in [ RiplActor
--           { package = "cal"
--           , actorName = lhsId
--           , actorAST =
--               zipWithVectorActor
--                 (lhsId)
--                 exp
--                 dim'
--                 vectorDim
--                 calTypeIncoming1
--                 calTypeIncoming2
--                 calTypeOutgoing
--           }
--         ]
skeletonToActors lhsId (Dimension width height) (R.ScaleSkel scaleFactorWidth scaleFactorHeight identRhs) dfGraph =
  let bitWidthIncoming =
        (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) dfGraph
      calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
      thisBitWidth =
        ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
           dfGraph)
      calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
      dimension@(Dimension widthPreScale heightPreScale) =
           (fromJust . dim . fromJust . Map.lookup identRhs) dfGraph
  in [ RiplActor
       { package = "cal"
       , actorName = (lhsId)
       , actorAST = scaleActor lhsId scaleFactorWidth scaleFactorHeight widthPreScale calTypeIncoming calTypeOutgoing
       }
     ]
skeletonToActors _ _ rhs _ =
  error ("unsupported rhs in skeletonToActors: " ++ show rhs)

skeletonToIdentityActor :: R.Ident -> Actor
skeletonToIdentityActor ident =
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
