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
import SkeletonTemplates.Common
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
-- import Inference.Dimension
-- import Inference.BitWidth
-- import Inference.Colour
import Pass.Inliner
import Debug.Trace

compile :: R.Program -> Integer -> (CalProject, Dimension, [String],(Int,Int))
compile (R.ProgramC _ functions imRead@(R.ImageInC imReadLhsIdent colourType w h) assignments dataOut@(R.DataOutC outIdent)) numFrames =
  let x = 1
--   let !inlinedStmts = inlineFunctions functions stmts
--       astToIr =
--         inferStreamDimensionsChannelsBitWidth imRead
--         . deriveDataflow imRead outIdent
--       varInfo = astToIr inlinedStmts
      (computeNodes',varInfo) = astToIr imRead dataOut assignments
      computeNodes = sortBy orderComputeNodes computeNodes'
      (unusedActors, actors) = genActors outIdent varInfo computeNodes numFrames
--       showIdent (R.Ident x) = x
      -- network = createDataflowWires varInfo computeNodes actors
      network = dfConnections varInfo computeNodes
      outImageColourChans =
        let (dim,streamMode) = fromJust (Map.lookup outIdent varInfo)
        in case streamMode of
          Sequential -> 1
          Parallel ->
            case dim of
              Dim2{} -> 1 -- grayscale image
              Dim3{} -> 3 -- RGB image
      (outImageDim,_) = --fromJust $ dim $ fromJust $ Map.lookup outIdent varInfo
         fromJust (Map.lookup outIdent varInfo)

--       -- outImageBitWidth =
--       --   fromJust $ maxBitWidth $ fromJust $ Map.lookup outIdent varInfo
--       -- outImageBitWidthUIint = calTypeFromCalBWUInt (correctBW outImageBitWidth)
--   -- in traces (map show inlinedStmts) "Inlined statements" $
  in ( CalProject actors network
     , outImageDim
     -- , outImageBitWidthUIint
     , unusedActors
     , ( case colourType of R.ColourTypeGray -> 1; R.ColourTypeRGB -> 3
       , outImageColourChans)
     )
orderComputeNodes node1 node2 =
  compare (lineNum node1) (lineNum node2)


-- nodesLeftToRight :: R.Ident -> VarInfo -> [(R.Ident, Dimension)]
-- nodesLeftToRight _notUsed mp =
--   let xs = Map.toList mp
--   in sortBy
--        (\(_, ComputeNode _ _ _ _ _ i) (_, ComputeNode _ _ _ _ _ j) ->
--           if i < j
--             then LT
--             else if i == j
--                    then EQ
--                    else GT)
--        xs

astToIr :: R.ImageIn -> R.DataOut -> [R.Assignment] -> ([ComputeNode] , VarInfo)
astToIr (R.ImageInC imReadLhsIdent colourType w h) (R.DataOutC outIdent) computeAssignments =
  let (imReadNode,imReadInfo) = ( ComputeNode
                                  "ImRead"
                                  []
                                  [imReadLhsIdent]
                                  (ImReadRHS (case colourType of
                                                R.ColourTypeRGB -> Dim3 w h 3
                                                R.ColourTypeGray -> Dim2 w h))
                                  True
                                  False
                                  0
                                  -- imReadLhsIdent
                                , Map.singleton imReadLhsIdent
                                  ( case colourType of
                                      R.ColourTypeRGB -> Dim3 w h 3
                                      R.ColourTypeGray -> Dim2 w h
                                  , Parallel ))
      (computeNodes,computeVars,computeNodeCount) =
        foldl' processStmt ([],imReadInfo,1) computeAssignments
      outNode =
        ComputeNode
        "ImWrite"
        [outIdent]
        []
        (ImWriteRHS (fst (fromJust (Map.lookup outIdent computeVars))) outIdent)
        False
        True
        (computeNodeCount + 1)
  in ( imReadNode : outNode : computeNodes
     , computeVars )

processStmt :: ([ComputeNode],VarInfo, Int) -> R.Assignment -> ([ComputeNode],VarInfo, Int)
processStmt (nodes,varInfo,assignNum) (R.AssignSkelC idents skelRhs) =
  let outputNodes = case idents of
                      R.IdentsOneId ident -> [ident]
                      R.IdentsManyIds idents -> idents
      inputNodes =
        trace (show (idsFromRHS skelRhs)) $
        idsFromRHS skelRhs
      node =
        ComputeNode
        ("Node" ++ show assignNum)
        inputNodes
        outputNodes
        (SkelRHS skelRhs)
        False
        False
        assignNum
      newVars =
        map (\(node,i) -> dimensionOfInput skelRhs varInfo node i) (zip outputNodes [0..])

  in (node : nodes
     -- (a -> b -> b) -> b -> [a] -> b
     , foldr (\(ident,dimension) varNodes -> Map.insert ident dimension varNodes) varInfo newVars
     , assignNum + 1
     )


inferStreamDimensionsChannelsBitWidth :: R.ImageIn
                                             -> VarInfo
                                             -> VarInfo
inferStreamDimensionsChannelsBitWidth (R.ImageInC imReadLhsIdent colourType w h) varInfo =
  undefined
  --   inferredNodes
  -- where
  --   -- inferredNodes = bitWidthInferred -- Map.fromList dimensionsInferred
  --   orderedNodesFromImRead =
  --     (imReadLhsIdent, imReadNode imReadLhsIdent colourType w h) :
  --     nodesLeftToRight imReadLhsIdent varInfo :: [(R.Ident, ComputeNode)]
  --   singletonMapWithImRead =
  --     Map.singleton imReadLhsIdent (imReadNode imReadLhsIdent colourType w h)

  --   dimensionsInferred =
  --     foldl
  --       inferDimColour
  --       (singletonMapWithImRead :: VarInfo)
  --       orderedNodesFromImRead

  --   inferDimColour :: VarInfo -> (R.Ident, ComputeNode) -> VarInfo
  --   inferDimColour mp (idLHS, ComputeNode idIdx lhsIdent (SkelRHS rhs) dimn bitW isIn isOut ln chans) =
  --     let idDependedOn = case rhs of
  --           -- the second argument is the image, i.e.
  --           -- zipWith [p..] image1 ...
  --           R.ZipWithScalarSkel{} -> idsFromRHS rhs !! 1
  --           _ -> head (idsFromRHS rhs)
  --         incomingDimension =
  --           case rhs of
  --             (R.FoldSkel _ (R.ExprRangeArray exp) func) ->
  --               dimensionFromTuple exp
  --             _ ->
  --               fromJust $ dim $ fromJust $ Map.lookup idDependedOn mp
  --         incomingColour = chans
  --            -- colourType (fromJust (Map.lookup idDependedOn mp))
  --         -- incomingDirection =
  --         --   fromJust $ direction $ fromJust $ Map.lookup idDependedOn mp
  --     in Map.insert
  --          idLHS
  --          (ComputeNode
  --             idIdx
  --             lhsIdent
  --             (SkelRHS rhs)
  --             (Just
  --                ((inferDimension incomingDimension rhs) !! (idIdx - 1)))
  --             bitW
  --             isIn
  --             isOut
  --             -- ln
  --             -- (inferColour rhs)
  --          )
  --          mp
  --   inferDimColour mp (idLHS, ComputeNode idIdx lhsIdent (ImWriteRHS numColourChans outIdent) dimn bitW isIn isOut ln chans) =
  --     let idDependedOn = outIdent
  --         incomingDimension = dim $ fromJust $ Map.lookup idDependedOn mp
  --     in Map.insert
  --          idLHS
  --          (ComputeNode
  --             idIdx
  --             lhsIdent
  --             (ImWriteRHS numColourChans outIdent)
  --             incomingDimension
  --             bitW
  --             isIn
  --             isOut
  --             ln
  --             chans
  --          )
  --          mp
  --   inferDimColour mp (idLHS, ComputeNode idIdx lhsIdent (ImReadRHS colourChans w h) dim bitW isIn isOut ln chans) =
  --     Map.insert
  --       idLHS
  --       (ComputeNode
  --          idIdx
  --          lhsIdent
  --          (ImReadRHS colourChans w h)
  --          (Just (Dim2 w h))
  --          bitW
  --          isIn
  --          isOut
  --          ln
  --          chans
  --       )
  --       mp

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

rhsSkelToNode :: [R.Ident] -> R.AssignSkelRHS -> Int -> ComputeNode
rhsSkelToNode identsLhs rhs stmtNum =
  ComputeNode
    ("Node" ++ show stmtNum)
    identsLhs
    [undefined] -- TODO
    (SkelRHS rhs)
    -- Nothing
    -- Nothing
    False
    False
    stmtNum
    -- Chan1 -- temporary

imReadNode :: R.Ident -> R.ColourType -> Integer -> Integer -> ComputeNode
imReadNode lhsIdent colourType w h =
  let colourChans =
        case colourType of
          R.ColourTypeGray -> Dim2 w h
          R.ColourTypeRGB -> Dim3 w h 3
  in
  ComputeNode
  "ImRead"
    []
    [lhsIdent]
    (ImReadRHS colourChans)
    -- (Just (Dim2 w h))
    -- (Just 255)
    True
    False
    1
    -- colourChans

outNode outIdent@(R.Ident rhsId) varLookup =
  let dir = Just Rowwise -- TODO implement
      dimension = (Dim2 10 10) -- TODO implement
      bitW = 255
      -- outColourType =
      --   case varRHS (fromJust (Map.lookup outIdent varLookup)) of
      --     SkelRHS rhs -> inferColour rhs
      --     _ ->  Chan3
  in ComputeNode
       "ImWrite"
       [outIdent] -- TODO
       []
       (ImWriteRHS undefined outIdent)
       -- (Just dimension)
       -- (Just 255)
       False
       True
       1000000 -- TODO is this needed anymore?

-- TODO: uncomment and re-implement
-- deriveDataflow :: R.ImageIn -> R.Ident -> [R.Assignment] -> VarInfo
-- deriveDataflow (R.ImageInC imReadLhsIdent colourType w h) outIdent stmts = varMap -- connectedGraph
--   where
--     varMap = Map.insert (R.Ident "out") (outNode outIdent varMap') varMap'
--     varMap' =
--       Map.fromList $
--       [ (imReadLhsIdent, imReadNode imReadLhsIdent colourType w h)
--       ] ++
--       concatMap mkNode (zip [1 ..] stmts)
--     mkNode (stmtNum, R.AssignSkelC (R.IdentsOneId lhsIdent) rhs) =
--       [(lhsIdent, rhsSkelToNode [lhsIdent] rhs stmtNum)]


    -- Note: previous implementation for splitX, splitY. Do not remove
    -- until splitX and splitY works with new scheme
    --
    -- mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs) =
    --   map
    --     (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
    --     (zip idents [1 ..])



    -- mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitXSkel splitFactor identRs)) =
    --   map
    --     (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
    --     (zip idents [1 ..])
    -- mkNode (stmtNum, R.AssignSkelC (R.IdentsManyIds idents) rhs@(R.SplitYSkel splitFactor identRs)) =
    --   map
    --     (\(lhsIdent, i) -> (lhsIdent, rhsSkelToNode i lhsIdent rhs stmtNum))
    --     (zip idents [1 ..])
    -- mkNode x  = error ("mkNode: " ++ show x)

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

dfConnections :: VarInfo -> [ComputeNode] -> Connections
dfConnections varInfo nodes = catMaybes (concatMap createConn nodes)
  where
    createConn :: ComputeNode -> [Maybe Connection]
    createConn node =
      concatMap
      (\(outputIdent,i) ->
         concatMap
         (\anotherNode -> createConn' outputIdent i node anotherNode)
         nodes
      )
      (zip (outputs node) [1..])

    createConn' :: R.Ident -> Int -> ComputeNode -> ComputeNode -> [Maybe Connection]
    createConn' outputIdent outputIdentPort outputNode inputNode =
      concatMap
      (\(inputIdent,i) ->
         if inputIdent == outputIdent
         then
           case (varRhs outputNode,varRhs inputNode) of
             (ImReadRHS inDim,SkelRHS skelRhs) ->
               map
               (\portNum ->
                   Just
                   (Connection
                     { src = Port ("In" ++ show portNum)
                     , dest = Actor (nodeName inputNode) ("In" ++ show portNum)
                     }))
               [1..case inDim of
                     Dim2{} -> 1 -- grayscale image
                     Dim3{} -> 3 -- RGB image
               ]
             (SkelRHS skelOutRhs,SkelRHS skelInRhs) ->
               [ Just $
                 Connection
                 { src = Actor (nodeName outputNode) (idRiplShow outputIdent)
                 , dest = Actor (nodeName inputNode) (idRiplShow inputIdent)
                 }
                 -- Connection
                 -- { src = Actor (nodeName outputNode) ("Out" ++ show i)
                 -- , dest = Actor (nodeName inputNode) ("In" ++ show outputIdentPort)
                 -- }
               ]
             (SkelRHS skelOutRhs,ImWriteRHS outDim _outIdent) ->
               map
               (\portNum ->
                   Just
                   (Connection
                     { src = Actor (nodeName outputNode) (idRiplShow outputIdent)
                     , dest = Port ("Out" ++ show portNum)
                     }))
               [1..case snd (fromJust (Map.lookup outputIdent varInfo)) of
                 Sequential -> 1
                 Parallel   ->
                   case outDim of
                     Dim2{} -> 1
                     Dim3{} -> 3
               ]
               -- [1..case outDim of
               --       Dim2{} -> 1 -- grayscale image
               --       Dim3{} -> 3 -- RGB image
               -- ]

               -- [ Just $
               --   Connection
               --   { src = Actor (nodeName outputNode) ("Out" ++ show i)
               --   , dest = Port (nodeName inputNode) ("In" ++ show outputIdentPort)
               --   }
               -- ]




             -- case varRhs inputNode of
             --   ImReadRHS dim ->
             --     -- case dim of
             --       map
             --       (\portNum ->
             --          Just
             --           (Connection
             --            { src = Port ("In" ++ show portNum)
             --            , dest = Actor (nodeName inputNode) ("In" ++ show i)
             --            }))
             --       [1..case dim of
             --         Dim2{} -> 1 -- grayscale image
             --         Dim3{} -> 3 -- RGB image
             --         ]
             --   ImWriteRHS dim dataOutIdent ->
             --       map
             --       (\portNum ->
             --          trace (show ((Connection
             --            { src = Actor (nodeName outputNode) ("Out" ++ show i)
             --            , dest = Port ("Out" ++ show portNum)
             --            }))) $
             --          Just
             --           (Connection
             --            { src = Actor (nodeName outputNode) ("Out" ++ show i)
             --            , dest = Port ("Out" ++ show portNum)
             --            }))
             --       [1..case dim of
             --         Dim2{} -> 1 -- grayscale image
             --         Dim3{} -> 3 -- RGB image
             --         ]
             --   SkelRHS skelRhs ->
             --     [ Just $
             --       Connection
             --       { src = Actor (nodeName outputNode) ("Out" ++ show i)
             --       , dest = Actor ("rob") ("In" ++ show outputIdentPort)

             --       }
             --     ]

           -- , dest = Actor (nodeName inputNode) ("In" ++ show i)
           -- }
         else [Nothing])
      (zip (inputs inputNode) [1,2..])

  -- foldr (\a b -> b) [] nodes
  -- where
  --   createConn
  --     node

-- TODO: deprecate
-- createDataflowWires ::  VarInfo -> [ComputeNode] -> [Actor] -> Connections
-- createDataflowWires varInfo computeNodes actors =
--   --traces (reverse (map showConn conns)) "deriving connections" conns
--   conns
--   where
--     conns =
--       -- nub $ -- TODO I have removed this, is it needed?
--       concatMap
--         -- (\(lhsIdent, varNode) -> connectDeps lhsIdent varNode)
--       connectDeps
--       computeNodes
--         -- (Map.toList varInfo)
--     connectDeps :: ComputeNode -> Connections
--     connectDeps {- lhsIdent -} (ComputeNode nodeName inputs outputs rhs _ _ _) =
--       case rhs of
--         -- TODO: reimplement these two cases
--         --
--         (ImReadRHS dimension) ->
--           let imReadConns =
--                 map (\portNum ->
--                 Connection
--                 { src = Port ("In" ++ show portNum)
--                 , dest = Actor (idToString (head outputs)) ("In"++show portNum ++ "_" ++ show 1)
--                 })
--                 [1..case dimension of
--                     Dim2{} -> 1 -- grayscale image
--                     Dim3{} -> 3 -- RGB image
--                 ]
--           in imReadConns
--         (ImWriteRHS outputDimension rhsIdent) ->
--           let outConns =
--                 map (\portNum ->
--                 Connection
--                 { src = Actor (idToString (head outputs)) ("Out" ++ show portNum ++ "_" ++ show 1)
--                 , dest = Port ("Out" ++ show portNum)})
--                 [1..case outputDimension of
--                     Dim2{} -> 1 -- grayscale
--                     Dim3{} -> 3 -- RGB
--                 ]
--           in outConns
--         (SkelRHS (R.MapSkel fromIdent fun)) ->
--           mkConn (head outputs) fromIdent -- (fromJust (Map.lookup fromIdent varInfo))
--           -- ++ mkFuncDepConnsElemUnary lhsIdent fun
--         (SkelRHS (R.FoldSkel _ (R.ExprVar (R.VarC fromIdent)) _)) ->
--           mkConn (head outputs) fromIdent
--         -- (SkelRHS (R.ScanSkel (R.Ident fromIdent) _ _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.SplitXSkel _ (R.Ident fromIdent))) ->
--         --   mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitX") colourChans
--         --   ++ mkConn (R.Ident (fromIdent ++ "_splitX")) 1 1 fromIdent colourChans
--         -- (SkelRHS (R.SplitYSkel _ (R.Ident fromIdent))) ->
--         --   mkConn lhsIdent 1 idIdx (fromIdent ++ "_splitY") colourChans
--         --   ++ mkConn (R.Ident (fromIdent ++ "_splitY")) 1 1 fromIdent colourChans
--         -- (SkelRHS (R.FoldSkel _ (R.ExprVar (R.VarC (R.Ident fromIdent))) _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.FoldSkel _ _ _)) ->
--         --   []
--         -- (SkelRHS (R.FoldScalarSkel (R.Ident fromIdent) _ _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.FoldVectorSkel (R.Ident fromIdent) _ _ _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.Stencil1DSkel (R.Ident fromIdent) _ _ _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.Stencil2DSkel (R.Ident fromIdent) _ _ _)) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.ScaleSkel _ _ (R.Ident fromIdent))) ->
--         --   mkConn lhsIdent 1 idIdx fromIdent colourChans
--         -- (SkelRHS (R.ZipWithSkel idents _)) ->
--         --   concatMap
--         --     (\(i, R.IdentSpaceSepC idents) ->
--         --        case idents of
--         --          R.IdentsOneId (R.Ident fromIdent) ->
--         --            mkConn lhsIdent i idIdx fromIdent colourChans
--         --          R.IdentsManyIds idents -> error "error connecting zipWith")
--         --     (zip [1 ..] idents)
--         -- (SkelRHS (R.ZipWithScalarSkel (R.ExprVar (R.VarC id1)) id2 _)) ->
--         --   concatMap
--         --     (\(i, (R.Ident fromIdent)) ->
--         --        mkConn lhsIdent i idIdx fromIdent colourChans)
--         --     (zip [1 ..] [id1,id2])
--         rhs -> error ("unsupported rhs in createDataflowWires: " ++ show rhs)

--     -- Note: shold this just be zipping lhsIds and rhsIds and a
--     -- counter i ?
--     mkConn (R.Ident lhsId) (R.Ident rhsId) =
--       [ Connection
--         { src = Actor rhsId "Out1" -- "Out" ++ show i ?
--         , dest = Actor lhsId "In1"
--         }
--       ]

--     -- TODO: reimplement
--     --
--     -- mkConn (R.Ident lhsIdent) inIdx outIdx (R.Ident fromIdent) colourChans =
--     --   -- TODO: how to deal with naming when multiple inputs,
--     --   -- i.e. zipWith
--     --   -- this was the previous solution:
--     --   -- Connection
--     --   -- { src = Actor fromIdent ("Out" ++ show outIdx)
--     --   -- , dest = Actor lhsIdent ("In" ++ show inIdx)
--     --   -- }
--     --   map (\i ->
--     --   Connection
--     --   { src = Actor fromIdent ("Out" ++ show i ++ "_" ++ show outIdx)
--     --   , dest = Actor lhsIdent ("In"  ++ show i ++ "_" ++ show inIdx)
--     --   })
--     --   [1..colourChans]
--     --
--     -- TODO: what is this for again?
--     -- mkFuncDepConnsElemUnary (R.Ident lhsIdent) fun@(R.OneVarFunC ident exp) =
--     --   let idents = globalIdentsElemUnary fun
--     --   in map (\ident@(R.Ident identStr) ->
--     --             Connection
--     --             { src  = Actor identStr ("Out1")
--     --             , dest = Actor lhsIdent (identStr ++ "Port")
--     --             }
--     --          ) idents

genActors :: R.Ident -> VarInfo -> [ComputeNode] -> Integer -> ([String], [Actor])
genActors outIdent varInfo computeNodes numFrames =
  (unusedVariableActors, nub $ map (\(n,i) -> mkActor n i) (zip computeNodes [1..]))
  where
    -- TODO: change the way that this unit actor is created.
    --       This will involve simplifying VarRHS to just SkelRHS.
    --
    mkActor :: ComputeNode -> Int -> Actor
    mkActor (ComputeNode _ inputs outputs (ImReadRHS dim) _ _ _) _ =
      let (wIn,hIn) = case dim of
                        Dim2 wIn hIn -> (wIn,hIn)
                        Dim3 wIn hIn _ -> (wIn,hIn)
      in RiplUnit
           "std.headers"
           "ParametersIn"
           (inputParametersActor wIn hIn)
    mkActor (ComputeNode _ inputs outputs (ImWriteRHS dim outIdent) _ _ _) _ =
      let (wOut,hOut) = case dim of
                        Dim2 wOut hOut -> (wOut,hOut)
                        Dim3 wOut hOut _ -> (wOut,hOut)
      in RiplUnit
           "std.headers"
           "ParametersOut"
           (outputParametersActor wOut hOut numFrames)
         -- , skeletonToIdentityActor
         --   (head inputs)
         --   (case dim of
         --      Dim1{} -> 1
         --      Dim3{} -> 3)
         -- ]
    -- mkActor (ComputeNode outputs (ImWriteRHS outIdent) _dimension _ _isOut) =
    --   [] -- [skeletonToIdentityActor outIdent]
    mkActor (ComputeNode name inputs outputs (SkelRHS rhs) dimension _ _isOut) actorNum =
      case rhs of
        (R.MapSkel _rhsIdent _anonFun) ->
          RiplActor
          { package = "cal"
          , actorName = name -- "Node" ++ show actorNum
          , actorAST = mapActor name inputs outputs rhs varInfo
          }
        (R.FoldSkel expState expFoldedOver anonFun) ->
          RiplActor
          { package = "cal"
          , actorName = name
          , actorAST =
              foldActor
              name
              outputs
              expState
              expFoldedOver
              anonFun
              varInfo
          }


          -- skeletonToActor outputs (fromJust dimension) rhs varInfo
        -- (R.Stencil1DSkel rhsIdent windowWidth windowHeight kernelValues) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.Stencil2DSkel rhsIdent windowWidth windowHeight kernelValues) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.ScanSkel _ _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.SplitXSkel _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.SplitYSkel _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.FoldSkel _ _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.FoldScalarSkel _ _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.FoldVectorSkel _ _ _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.ZipWithSkel _ _) ->
        --   skeletonToActor lhsIdent rhs varInfo
        -- (R.ZipWithScalarSkel _ _ _) ->
        --   skeletonToActor lhsIdent (fromJust dimension) rhs varInfo
        -- (R.ScaleSkel _ _ _) ->
        --   skeletonToActor lhsIdent (fromJust dimension) rhs varInfo
        _ -> error ("Unsupported rhs in genActors: " ++ show rhs)
    unusedVariableActors =
      let allLhsIdents = Map.keys varInfo
          allRhsIdents =
            concatMap
              (\(ComputeNode _ _ _ rhs _ _ _) ->
                 case rhs of
                   SkelRHS assRHS -> idsFromRHS assRHS
                   _ -> [])
              computeNodes
              -- (Map.elems varInfo) :: [R.Ident]
          unusedIdents =
            filter
              (\ident ->
                 not (elem ident allRhsIdents) &&
                 ident /= outIdent && ident /= (R.Ident "out"))
              (nub allLhsIdents) -- TODO: nub is a hack, why duplicates?
          -- in traces (map show unusedIdents) "Unused variables" $ map (\ident -> varToConsumerActor ident) unusedIdents
      in map (\(R.Ident ident) -> ident) unusedIdents

-- skeletonToActor :: [R.Ident]
--                 -> [R.Ident]
--                 -> R.AssignSkelRHS
--                 -> VarInfo
--                 -> Actor
-- skeletonToActor outputs (R.MapSkel identRhs anonFun) varInfo =

  -- let bitWidthIncoming =
  --       (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
  --     calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
  --     thisBitWidth =
  --       ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
  --          varInfo)
  --     calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
  -- in
    -- RiplActor
    --    { package = "cal"
    --    , actorName = (lhsId)
    --    , actorAST = mapActor lhsId anonFun undefined undefined varInfo
    --    }


-- skeletonToActor lhsId (R.FoldSkel stateExp identExp anonFun) varInfo =
--   let bitWidthIncoming =
--         case identExp of
--           R.ExprVar (R.VarC identRhs) ->
--             (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            foldActor
--              lhsId
--              stateExp
--              identExp
--              anonFun
--              varInfo
--        }
--      ]


-- skeletonToActor lhsId dim@(Dimension width height) (R.Stencil1DSkel identRhs winWidth winHeight userDefinedFunc) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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
-- skeletonToActor lhsId dim@(Dimension width height) (R.Stencil2DSkel identRhs winWidth winHeight userDefinedFunc) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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


-- skeletonToActor lhsId dim@(Dimension width height) (R.ScanSkel identRhs initInt anonFun) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST =
--            scanActor (lhsId) initInt dim anonFun calTypeIncoming calTypeOutgoing
--        }
--      ]
-- skeletonToActor lhsId dim@(Dimension width height) (R.SplitXSkel splitFactor rhsId) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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
-- skeletonToActor lhsId dim@(Dimension width height) (R.SplitYSkel splitFactor rhsId) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup rhsId) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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
-- skeletonToActor lhsId dim@(Dimension width height) (R.FoldScalarSkel identRhs initInt anonFun) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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
-- skeletonToActor lhsId dim@(Dimension width height) (R.FoldVectorSkel identRhs vectorLength initInt anonFun) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
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
-- skeletonToActor lhsId (Dim2 width height) (R.ZipWithSkel identsRhs exp) varInfo =
--   let bitWidthIncoming =
--         let bitWidths =
--               map
--                 (\(R.IdentSpaceSepC x) ->
--                    case x of
--                      R.IdentsOneId identRhs ->
--                        (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
--                        varInfo
--                      R.IdentsManyIds idents ->
--                        error "error inferring bitwidths from zipWith"
--                      )
--                 identsRhs
--         in maximum bitWidths
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST = zipWithActor (lhsId) exp calTypeIncoming calTypeOutgoing
--        }
--      ]

-- skeletonToActor lhsId (Dim2 width height) (R.FoldSkel expState expRhs expFun) varInfo =
--   [ RiplActor
--     { package = "cal"
--     , actorName = lhsId
--     , actorAST = foldActor lhsId expState expRhs expFun varInfo
--     }
--   ]

-- skeletonToActor lhsId dim@(Dimension width height) (R.ZipWithScalarSkel initVarExp ident1 exp) varInfo =
--   let bitWidthIncoming =
--         let bitWidths =
--               map
--                 (\identRhs ->
--                    (fromJust . maxBitWidth . fromJust . Map.lookup identRhs)
--                      varInfo)
--                 [ident1]
--         in maximum bitWidths
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--   in [ RiplActor
--        { package = "cal"
--        , actorName = lhsId
--        , actorAST =
--            zipWithScalarActor (lhsId) exp dim calTypeIncoming calTypeOutgoing
--        }
--      ]

-- skeletonToActor lhsId (Dimension width height) (R.ScaleSkel scaleFactorWidth scaleFactorHeight identRhs) varInfo =
--   let bitWidthIncoming =
--         (fromJust . maxBitWidth . fromJust . Map.lookup identRhs) varInfo
--       calTypeIncoming = calTypeFromCalBW (correctBW bitWidthIncoming)
--       thisBitWidth =
--         ((fromJust . maxBitWidth . fromJust . Map.lookup (R.Ident lhsId))
--            varInfo)
--       calTypeOutgoing = calTypeFromCalBW (correctBW thisBitWidth)
--       dimension@(Dimension widthPreScale heightPreScale) =
--            (fromJust . dim . fromJust . Map.lookup identRhs) varInfo
--   in [ RiplActor
--        { package = "cal"
--        , actorName = (lhsId)
--        , actorAST = scaleActor lhsId scaleFactorWidth scaleFactorHeight widthPreScale calTypeIncoming calTypeOutgoing
--        }
--      ]
-- skeletonToActor _ _ rhs _ =
--   error ("unsupported rhs in skeletonToActors: " ++ show rhs)

skeletonToIdentityActor :: R.Ident -> Int -> Actor
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
