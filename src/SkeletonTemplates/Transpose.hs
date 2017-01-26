module SkeletonTemplates.Transpose
-- transposeActorUsingRepeat
  ( transposeActorUsingAccumulatorActions
  ) where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import SkeletonTemplates.CalTypes

-- Note: skeleton to actor implemenations in:
--       - transposeActorUsingAccumulatorActions
--       - transposeActorUsingRepeat
--
--  are the simplest implementation of transpose.
--  Parallel input versions are commented below, with discussion.
transposeActorUsingAccumulatorActions actorName widthPreTransposed heightPreTransposed incomingType outgoingType =
  actorAST
  where
    actorAST =
      C.ActrSchd
        (C.PathN [C.PNameCons (C.Ident "cal")])
        []
        (C.Ident actorName)
        []
        ioSig
        (globalVars widthPreTransposed heightPreTransposed)
        actions
        fsmSchedule
        [priorityBlock]
    ioSig =
      C.IOSg
        [C.PortDcl inType (C.Ident "In1")]
        [C.PortDcl outType (C.Ident "Out1")]
    inType = incomingType
    outType = outgoingType
    actions =
      [ C.ActionCode getCoeffValue
      , C.ActionCode doneCountX
      , C.ActionCode doneCountY
      , C.ActionCode sendCoeffValue
      ]
    getCoeffValue = C.AnActn (C.ActnTagsStmts tag head stmts)
      where
        tag = C.ActnTagDecl [C.Ident "getCoeffValue"]
        head = C.ActnHead [inPattern] []
        inPattern = C.InPattTagIds (C.Ident "In1") [C.Ident "x"]
        stmts =
          [ C.SemiColonSeparatedStmt
              (C.AssignStt
                 (C.AssStmtIdx
                    (C.Ident "coeffBuffer")
                    (C.Idx
                       [ (C.BExp (C.EIdent (C.Ident "countX")))
                       , (C.BExp (C.EIdent (C.Ident "countY")))
                       ])
                    (C.EIdent (C.Ident "x"))))
          , C.SemiColonSeparatedStmt
              (C.AssignStt
                 (C.AssStmt
                    (C.Ident "countX")
                    (C.BEAdd (identCalExp "countX") (mkInt 1))))
          , C.EndSeparatedStmt
              (C.IfStt
                 (C.IfOneSt
                    (C.BEEQ (identCalExp "countX") (mkInt widthPreTransposed))
                    [ C.SemiColonSeparatedStmt
                        (C.AssignStt
                           (C.AssStmt
                              (C.Ident "countY")
                              (C.BEAdd (identCalExp "countY") (mkInt 1))))
                    , C.SemiColonSeparatedStmt
                        (C.AssignStt (C.AssStmt (C.Ident "countX") (mkInt 0)))
                    ]))
          ]
    doneCountX = C.AnActn (C.ActnTagsStmts tag head stmts)
      where
        tag = C.ActnTagDecl [C.Ident "doneCountX"]
        head = C.ActnHeadGuarded [] [] [guardExp]
        guardExp = C.BEEQ (identCalExp "countX") (mkInt widthPreTransposed)
        stmts =
          [ C.SemiColonSeparatedStmt
              (C.AssignStt (C.AssStmt (C.Ident "countX") (mkInt 0)))
          , C.SemiColonSeparatedStmt
              (C.AssignStt (C.AssStmt (C.Ident "countY") (mkInt 0)))
          ]
    doneCountY = C.AnActn (C.ActnTagsStmts tag head stmts)
      where
        tag = C.ActnTagDecl [C.Ident "doneCountY"]
        head = C.ActnHeadGuarded [] [] [guardExp]
        guardExp = C.BEEQ (identCalExp "countY") (mkInt heightPreTransposed)
        stmts =
          [ C.SemiColonSeparatedStmt
              (C.AssignStt (C.AssStmt (C.Ident "countX") (mkInt 0)))
          , C.SemiColonSeparatedStmt
              (C.AssignStt (C.AssStmt (C.Ident "countY") (mkInt 0)))
          ]
    sendCoeffValue = C.AnActn (C.ActnTagsStmts tag head stmts)
      where
        tag = C.ActnTagDecl [C.Ident "sendCoeffValue"]
        head = C.ActnHeadVars [] [outPattern] localVars
        outPattern =
          C.OutPattTagIds
            (C.Ident "Out1")
            [C.OutTokenExp (identCalExp "outVal")]
        localVars =
          C.LocVarsDecl
            [C.LocVarDecl (C.VDecl (mkIntType 16) (C.Ident "outVal") [])]
        stmts =
          [ C.SemiColonSeparatedStmt
              (C.AssignStt
                 (C.AssStmt
                    (C.Ident "outVal")
                    (C.EIdentArr
                       (C.Ident "coeffBuffer")
                       [ C.BExp (identCalExp "countX")
                       , C.BExp (identCalExp "countY")
                       ])))
          , C.SemiColonSeparatedStmt
              (C.AssignStt
                 (C.AssStmt
                    (C.Ident "countY")
                    (C.BEAdd (identCalExp "countY") (mkInt 1))))
          , C.EndSeparatedStmt
              (C.IfStt
                 (C.IfOneSt
                    (C.BEEQ (identCalExp "countY") (mkInt heightPreTransposed))
                    [ C.SemiColonSeparatedStmt
                        (C.AssignStt
                           (C.AssStmt
                              (C.Ident "countX")
                              (C.BEAdd (identCalExp "countX") (mkInt 1))))
                    , C.SemiColonSeparatedStmt
                        (C.AssignStt (C.AssStmt (C.Ident "countY") (mkInt 0)))
                    ]))
          ]
    fsmSchedule =
      C.SchedfsmFSM
        (C.Ident "s0")
        [ C.StTrans (C.Ident "s0") (C.Ident "getCoeffValue") (C.Ident "s0")
        , C.StTrans (C.Ident "s0") (C.Ident "doneCountY") (C.Ident "s1")
        , C.StTrans (C.Ident "s1") (C.Ident "sendCoeffValue") (C.Ident "s1")
        , C.StTrans (C.Ident "s1") (C.Ident "doneCountX") (C.Ident "s0")
        ]
    priorityBlock =
      C.PriOrd
        [ C.PriInEQ (C.Ident "doneCountY") (C.Ident "getCoeffValue") []
        , C.PriInEQ (C.Ident "doneCountX") (C.Ident "sendCoeffValue") []
        ]
    globalVars width height =
      [ C.GlobVarDecl
          (C.VDeclExpMut (intCalType 16) (C.Ident "countX") [] (mkInt 0))
      , C.GlobVarDecl
          (C.VDeclExpMut (intCalType 16) (C.Ident "countY") [] (mkInt 0))
      , C.GlobVarDecl
          (C.VDecl
             (mkIntType 16)
             (C.Ident "coeffBuffer")
             [C.BExp (mkInt width), C.BExp (mkInt height)])
      ]

transposeActorUsingRepeat actorName width height = actorAST
  where
    numPixels = width * height
    ioSig =
      C.IOSg
        [C.PortDcl inType (C.Ident "In1")]
        [C.PortDcl outType (C.Ident "Out1")]
    inType = intTypeParamSize 8
    outType = intTypeParamSize 8
    inputPattern =
      C.InPattTagIdsRepeat
        (C.Ident "In1")
        [C.Ident "img"]
        (C.RptClause (intCalExp numPixels))
    outputPattern =
      C.OutPattTagIdsRepeat
        (C.Ident "Out1")
        [C.OutTokenExp (C.EIdent (C.Ident "reordered_pixels"))]
        (C.RptClause (intCalExp numPixels))
    localVarsDecl =
      C.LocVarsDecl
        [ C.LocVarDecl
            (C.VDeclExpIMut
               (intTypeParamSize 16)
               (C.Ident "WIDTH")
               []
               (intCalExp width))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intTypeParamSize 16)
               (C.Ident "HEIGHT")
               []
               (intCalExp height))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (C.TypParam
                  C.TList
                  [ C.TypeAttrTypeDf (intTypeParamSize 16)
                  , C.TypeAttrSizeDf (intCalExp (width * height))
                  ])
               (C.Ident "idx")
               []
               idxComprehension)
        , C.LocVarDecl
            (C.VDeclExpIMut
               inType
               (C.Ident "reordered_pixels")
               [C.BExp (intCalExp (width * height))]
               (reorderedComprehension))
        ]
    idxComprehension =
      C.LstExpCons
        (C.ListComp
           (C.ListExpGen
              [ (C.BEAdd
                   (C.BEMult
                      (C.EIdent (C.Ident "WIDTH"))
                      (C.EIdent (C.Ident "h")))
                   (C.EIdent (C.Ident "w")))
              ]
              (C.GenExpr
                 [ C.GeneratorExpr
                     (intTypeParamSize 16)
                     (C.Ident "w")
                     (C.BEList (intCalExp 0) (C.EIdent (C.Ident "WIDTH-1")))
                 , C.GeneratorExpr
                     (intTypeParamSize 16)
                     (C.Ident "h")
                     (C.BEList (intCalExp 0) (C.EIdent (C.Ident "HEIGHT-1")))
                 ])))
    reorderedComprehension =
      C.LstExpCons
        (C.ListComp
           (C.ListExpGen
              [ (C.IndSExpCons
                   (C.IndExpr
                      (C.Ident "img")
                      (C.BExp
                         ((C.IndSExpCons
                             (C.IndExpr
                                (C.Ident "idx")
                                (C.BExp (C.EIdent (C.Ident "i")))))))))
              ]
              (C.GenExpr
                 [ C.GeneratorExpr
                     (intTypeParamSize 16)
                     (C.Ident "i")
                     (C.BEList (intCalExp 0) (intCalExp (width * height - 1)))
                 ])))
    actionHead = C.ActnHeadVars [inputPattern] [outputPattern] localVarsDecl
    action =
      C.ActionCode
        (C.AnActn (C.ActnTags (C.ActnTagDecl [C.Ident "the_action"]) actionHead))
    actorAST =
      C.Actr
        (C.PathN [C.PNameCons (C.Ident "cal")])
        []
        (C.Ident actorName)
        []
        ioSig
        []
        [action]
        []
-- {-
-- NOTE: this idea implementation has not currently been taken forward because
--       of concerns of FPGA resource requirements for supporting parallel ports.
-- Take the iterator composition:
--   img2 = map columnwise img1 (\[x1,x2] -> modify x1 := x2*x1 );
--   img3 = map rowwise    img2 (\[y1,y2] -> modify y1 := y2-y1 );
-- Over the greyscale image:
--      W
--  +------->
--  |1 3 5 7
-- H|2 4 6 8
--  |3 4 5 6
--  v
-- The sequential stream of img1 pixel values were:
-- 1 3 5 7 2 4 6 8 3 4 5 6
-- For the 2nd iterator, we need them to be (read rowwise):
-- 1 2 3 3 4 4 5 6 5 7 8 6
-- So the index into the stream was:
-- 0 1 2 3 4 5 6 7 8 9 10 11
-- For rowwise iteration, we need to index the stream to be:
-- 0 4 8 1 5 9 2 6 10 3 7 11
-- * Option 1
-- A new actor will be injected by the RIPL compiler that rearranges the
-- pixel stream for the rowwise iterator to happen. To hide communication
-- latency to the extreme, we can read all pixels of img2 in one clock
-- cycle, using as many wires as there are pixels, i.e.:
-- action: In0:[i0], In1:[i1], In2:[i2], In3:[i3], In4:[i4], In5:[i5],
--         In6:[i6], In7:[i7], In8:[i8], In9:[i9], In10:[i10], In11:[i1]
-- =>
-- Out:[i0,i4,i8 .. i7,i11]
-- This is a /parallel/ input and sequential output, so 1 clock cycle
-- regardless of the size of img1, though the clock cycles required to
-- pass on img2 to the img3 iterator does depend on the image size.
-- To generate this rowwise output index sequence from the columnwise pixel stream, the RIPL compiler should:
-- list = []
-- for w in width do
--   for h in height do
--       list = list : [(h*width) + w]
-- The limitation of this parallel input idea for stream reordering is
-- hardware. The FIFO for each wire is thankfully synchronous, so that's
-- one register per FIFO. E.g. a 512x512 image requires 262144 registers
-- for the parallel input, in addition to 262144 logic or multiplier
-- operations for the output. This is not achieveable on a Xilinx Zynq,
-- which has around 380 multipliers.. which limits this parallel input
-- idea. Perhaps chunking the input in parallel across 64 ports is a
-- compromise between parallel IO and FPGA register and multiplier
-- resource reuse. What's more, the dependency of multiple ports in an
-- action body will very likely put this stream re-ordering action on the
-- dataflow critical path.
-- * Option 2
-- A compromise between wholly sequential input and wholly parallel input
-- in option 1 is to chunk inputs for parallel input, but not to the
-- extent of the parallel input of an entire image. Why not parallelise
-- the input of an entire image? FPGA registers for the FIFO for each
-- pixel are limited, and FPGA multipliers are limited. Instead, for
-- example, we could parameterise input chunking by a factor of 16
-- (i.e. 16 input ports). Take a conservative chunking of 2, the pixel
-- stream reordering actor would instead look like:
-- /* reordering stream index, computed in option 1 */
-- idx = [0,4,8,1,5,9,2,6,10,3,7,11]
-- /* reordered array, unfortunately constructed in full before emission */
-- int reordered_pixels[12]
-- /* pointer into the reordered array */
-- int i = 0;
-- action: In1:[x1] repeat 6, In2:[x2] repeat 6 ==> Out:[reordered_pixels] repeat 12
-- do
--   reordered_pixels[ idx[i  ] ] := x1;
--   reordered_pixels[ idx[i+1] ] := x2;
--   i := i + 2;
-- end
-- The above action would take two pixels from the column wise stream in
-- parallel, and scatter the values into the correct position in the row
-- wise array to be streamed out. A chunking of 2 (i.e. two input ports)
-- would half the input latency, a chunking of 4 would reduce the input
-- latency to 1/4, and so on.
-- The generate this code in the RIPL compiler, the parameters are
-- derived from:
-- 1. the width and height the incoming image.
-- 2. the chunking factor.
-- - `idx` is computed using the nested for loop described in option 1.
-- - the size of the reordered_pixels array is width * height of the incoming image.
-- - the incoming `repeat` parameter is (width * height) / chunk.
-- - int `i` is incremented by chunk as the last statement in the action.
-- -}
-- {- This represents the first of a few ideas about how to do this in parallel.
-- chunk_size = 32
-- transpositionActor :: R.Ident -> Dimension -> ChunkSize -> Actor
-- transpositionActor assignedVar (Dimension width height) chunkSize =
--     let actorAST = C.Actr (C.PathN [C.PNameCons (C.Ident "cal")]) [] (idRiplToCal assignedVar) actorPar ioSig varDecl [action] priorityBlock
--         priorityBlock = []
--         idx = [h*width + w | w <- [0..(width-1)] , h <- [0..(height-1)] ]
--         varDecl =
--             [ -- uint(size=8) idx[12] = [0,4,8,1,5,9,2,6,10,3,7,11]; -- (for example for a 4x3 image)
--             --  C.VDeclExpIMut inType (C.Ident "idx") [C.BExp (intCalExp (width*height))] (C.LstExpCons (C.ListComp (C.ListExp (map intCalExp idx))))
--               -- uint(size=8) reordered_pixels[12];
--               C.VDecl inType (C.Ident "reordered_pixels") [C.BExp (intCalExp (width*height))]
--               -- uint(size=8) i;
--             , C.VDecl inType  (C.Ident "i") []
--             , C.VDecl inType  (C.Ident "hCount") []
--             , C.VDecl inType  (C.Ident "wCount") []
--             , C.VDecl inType  (C.Ident "a") []
--             ]
--         actorPar = []
--         ioSig = C.IOSg --  uint (size = 8)In1, uint(size=8) In2 , .. (dependent on chunk size)
--                        (map (\i -> C.PortDcl (intCalType 8) (C.Ident ("In1" ++ show i)) ) [1..chunkSize])
--                        --  uint (size = 8)Out
--                        [C.PortDcl (intCalType 8) (C.Ident "Out")]
--         inType  = C.TypParam C.TUint [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit 8)))]
--         outType = C.TypParam C.TUint [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit 8)))]
--         action = C.AnActn (C.ActnTagsStmts (C.ActnTagDecl [C.Ident "the_action"]) actionHead stmts)
--         actionHead = C.ActnHead inputPattern outputPattern
--         upperBound = (floor (fromInteger (width*height) / fromInteger chunkSize))
--         -- e.g. In1:[x1] repeat 6, In2:[x2] repeat 6 (depending on chunk size)
--         inputPattern =
--             map (\i -> C.InPattTagIdsRepeat (C.Ident ("In1" ++ show i)) [C.Ident ("x" ++ show i)] (C.RptClause (intCalExp upperBound))) [1..chunkSize]
--         -- e.g. -- Out:[reordered_pixels] repeat 12
--         outputPattern =
--             [C.OutPattTagIdsRepeat (C.Ident "Out") [C.Ident "reordered_pixels"] (C.RptClause (intCalExp (width*height)))]
--             {- e.g.
-- foreach uint(size=8) j in 0 .. 5 do
--     reordered_pixels[ idx[i]   ] := x1[j];
--     reordered_pixels[ idx[i+1] ] := x2[j];
--     i := i + 2 ; -- depending on chunk size.
--   end
--           -}
--         stmts = []
--             -- [ C.EndSeparatedStmt (C.ForEachStt (C.ForeachStmtsSt
--             --                      [(C.ForeachGen uintType (C.Ident "j") (C.BEList zeroExp (intCalExp (upperBound-1))))]
--             --                      ( map (\i -> C.SemiColonSepartedStmt (C.AssignStt (C.AssStmtIdx (C.Ident "reordered_pixels") (C.Idx (C.BExp (C.EIdentArr (C.Ident "idx") (C.BExp (C.EIdent (C.Ident ("i + " ++ show (i-1)))))))) (C.EIdentArr (C.Ident ("x" ++ show i)) (C.BExp (identCalExp "j")))))) [1..chunkSize]
--             --                      -- semi-colon delimited statements
--             --                      -- ( map (\i -> C.SemiColonSepartedStmt (C.AssignStt (C.AssStmtIdx (C.Ident "reordered_pixels") (C.Idx (C.BExp (C.EIdentArr (C.Ident "idx") (C.BExp (C.EIdent (C.Ident ("i + " ++ show (i-1)))))))) (C.EIdentArr (C.Ident ("x" ++ show i)) (C.BExp (identCalExp "j")))))) [1..chunkSize]
--             --                         -- i := i + chunkSize ;
--             --                         -- ++ [C.SemiColonSepartedStmt (C.AssignStt (C.AssStmt (C.Ident "i") (C.BEAdd (C.EIdent (C.Ident "i")) (intCalExp chunkSize)))) ] )))
--             -- ]
--     in RiplActor "cal" (idRiplShow assignedVar) actorAST
-- -}
-- {-
-- Over the greyscale image:
--      W
--  +------->
--  |1 3 5 7
-- H|2 4 6 8
--  |3 4 5 6
--  v
-- The sequential stream of img1 pixel values were:
-- 1 3 5 7 2 4 6 8 3 4 5 6
-- For the 2nd iterator, we need them to be (read columnwise):
-- 1 2 3 3 4 4 5 6 5 7 8 6
-- So the index into the stream was:
-- 0 1 2 3 4 5 6 7 8 9 10 11
-- For rowwise iteration, we need to index the stream to be:
-- 0 4 8 1 5 9 2 6 10 3 7 11
-- The output pattern with repeat + forall should index into the `img` array using:
-- list = []
-- for w in width do
--   for h in height do
--       list = list : [(h*width) + w]
-- NOTE: reordering
-- tmp: action In:[img] repeat 262144 ==> Out :[ reordered ] repeat 262144
-- var
--   int WIDTH= 512,
--   int HEIGHT = 512,
--   List(type:int(size=16),size=262144) idx = [ (WIDTH*h) + w :  for int w in 0 .. (WIDTH-1) , for int h in 0 .. (HEIGHT-1) ],
--   int (size=8) reordered[262144] = [ img[ idx[i] ] : for int i in 0 .. ((WIDTH*HEIGHT)-1) ]
-- end
-- -}
