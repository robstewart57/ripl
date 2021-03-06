module SkeletonTemplates.Stencil2D
  ( stencil2DActor
  ) where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

stencil2DActor :: String -> Dimension -> R.Stencil2DFun -> C.Type -> C.Type -> C.Actor
stencil2DActor actorName (Dimension width height) anonFun@(R.Stencil2DFunC _ xLocId' yLocId' _) incomingType outgoingType =
  let xLocId = idRiplToCal xLocId'
      yLocId = idRiplToCal yLocId'
      ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = incomingType
      outType = outgoingType
      functions =
        [ C.UnitCode (C.UFunDecl maxFun)
        , C.UnitCode (C.UFunDecl (kernelFun anonFun))
        , C.UnitCode (C.UFunDecl mkModFunctionMod)
        ]
      -- , C.UnitCode (C.UFunDecl mkModFunctionModTwoArgs)]
      actions =
        [ C.ActionCode (populateBufferAction width)
        , C.ActionCode (donePopulateBufferAction width)
        , C.ActionCode (topLeftAction width xLocId)
        , C.ActionCode (topRowAction width xLocId)
        , C.ActionCode (topRightAction width xLocId yLocId)
        , C.ActionCode (midLeftAction width height xLocId)
        , C.ActionCode (midLeftActionNoConsume width height xLocId)
        , C.ActionCode (midAction width height xLocId)
        , C.ActionCode (midActionNoConsume width height xLocId)
        , C.ActionCode (midRightAction width height xLocId yLocId)
        , C.ActionCode (midRightActionNoConsume width height xLocId yLocId)
        , C.ActionCode (bottomLeftActionNoConsume width height xLocId)
        , C.ActionCode (bottomRowActionNoConsume width height xLocId)
        , C.ActionCode (bottomRightActionNoConsume width height xLocId yLocId)
        ]
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       (globalVars width xLocId yLocId)
       (functions ++ actions)
       fsmSchedule
       []

globalVars width xLocId yLocId
           -- uint(size=16) bufferSize = imageWidth * 2 + 3;
 =
  [ C.GlobVarDecl
      (C.VDeclExpIMut
         (uintCalType 32)
         (C.Ident "bufferSize")
         []
         (C.BEAdd (C.BEMult (mkInt width) (mkInt 2)) (mkInt 3)))
    -- uint(size=16) buffer[bufferSize];
  , C.GlobVarDecl
      (C.VDecl
         (intCalType 32)
         xLocId
         [])
  , C.GlobVarDecl
      (C.VDecl
         (intCalType 32)
         yLocId
         [])
  , C.GlobVarDecl
      (C.VDecl
         (intCalType 32)
         (C.Ident "buffer")
         [C.BExp (C.EIdent (C.Ident "bufferSize"))])
    -- uint(size=16) idx := 0;
  , C.GlobVarDecl (C.VDeclExpMut (intCalType 32) (C.Ident "idx") [] (mkInt 0))
    -- uint(size=16) populatePtr := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (intCalType 32) (C.Ident "populatePtr") [] (mkInt 0))
    -- uint(size=16) processedMidRows := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (intCalType 32) (C.Ident "processedRows") [] (mkInt 0))
    -- uint(size=32) consumed := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (uintCalType 32) (C.Ident "consumed") [] (mkInt 0))
  , C.GlobVarDecl
      (C.VDeclExpMut (uintCalType 32) (C.Ident "midPtr") [] (mkInt 0))
  ]

fsmSchedule :: C.ActionSchedule
fsmSchedule =
  C.SchedfsmFSM
    (C.Ident "s0")
    [ C.StTrans (C.Ident "s0") (C.Ident "populateBuffer") (C.Ident "s0")
    , C.StTrans (C.Ident "s0") (C.Ident "donePopulateBuffer") (C.Ident "s1")
    , C.StTrans (C.Ident "s1") (C.Ident "topLeft") (C.Ident "s2")
    , C.StTrans (C.Ident "s2") (C.Ident "topRow") (C.Ident "s2")
    , C.StTrans (C.Ident "s2") (C.Ident "topRight") (C.Ident "s3")
    , C.StTrans (C.Ident "s3") (C.Ident "midLeft") (C.Ident "s4")
    , C.StTrans (C.Ident "s3") (C.Ident "midLeftNoConsume") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "mid") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "midNoConsume") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "midRight") (C.Ident "s5")
    , C.StTrans (C.Ident "s4") (C.Ident "midRightNoConsume") (C.Ident "s5")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeft") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeftNoConsume") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "bottomLeftNoConsume") (C.Ident "s6")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRowNoConsume") (C.Ident "s6")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRightNoConsume") (C.Ident "s0")
    ]

-- priorityBlock :: [C.PriorityBlock]
-- priorityBlock = [ C.PriOrd
--                   [ C.PriInEQ (C.Ident "midLeftNoConsume") (C.Ident "midLeft") []
--                   , C.PriInEQ (C.Ident "midNoConsume") (C.Ident "mid") []
--                   , C.PriInEQ (C.Ident "midRightNoConsume") (C.Ident "midRight") []
--                   ]
--                 ]
maxFun :: C.FunctionDecl
maxFun = C.FDecl (C.Ident "max") args returnType localVars body
  where
    args =
      [ (C.ArgPar (intCalType 32) (C.Ident "i"))
      , (C.ArgPar (intCalType 32) (C.Ident "j"))
      ]
    localVars = C.FNVarDecl
    returnType = intCalType 32
    body =
      C.IfExpCons
        (C.IfExpr
           (C.BEGT (identCalExp "i") (identCalExp "j"))
           (identCalExp "i")
           (identCalExp "j"))

mkModFunctionMod :: C.FunctionDecl
mkModFunctionMod = C.FDecl (C.Ident "myMod") args returnType localVars body
  where
    args = [C.ArgPar (mkIntType 32) (C.Ident "x")]
    localVars = C.FNVarDecl
    returnType = mkIntType 32
    body = C.IfExpCons (C.IfExpr ifCond ifThen ifElse)
    ifCond =
      C.BEGT
        (C.EIdent (C.Ident "x"))
        (C.BENeg (C.BEMult (mkInt 2) (mkVar "bufferSize")) (mkInt 1))
    ifThen = C.BENeg (mkVar "x") (C.BEMult (mkInt 2) (mkVar "bufferSize"))
    ifElse = C.IfExpCons (C.IfExpr elseCond elseThen elseElse)
    elseCond = C.BEGT (mkVar "x") (C.BENeg (mkVar "bufferSize") (mkInt 1))
    elseThen = C.BENeg (mkVar "x") (mkVar "bufferSize")
    elseElse = mkVar "x"

kernelFun :: R.Stencil2DFun -> C.FunctionDecl
kernelFun (R.Stencil2DFunC (R.VarListC lambdaExps) xLoc yLoc userDefinedFunc) =
  C.FDecl (C.Ident "applyKernel") args returnType localVars body
  where
    args =
      map
        -- (\(R.ExpSpaceSepC (R.ExprVar (R.VarC lambdaIdent))) ->
      (\(R.VarC lambdaIdent) ->
           (C.ArgPar (mkIntType 32) (idRiplToCal lambdaIdent)))
        lambdaExps
    localVars = C.FVarDecl [resultAssignment]
    resultAssignment =
      C.LocVarDecl
        (C.VDeclExpIMut
           (intCalType 32)
           (C.Ident "result")
           []
           (expRiplToCal userDefinedFunc))
    returnType = mkIntType 32
    body = C.IdBrSExpCons (C.Ident "max") [mkInt 0, C.EIdent (C.Ident "result")]

populateBufferAction width = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "populateBuffer"]
    head = C.ActnHeadGuarded [inPattern] [] [guardExp]
    inPattern = C.InPattTagIds (C.Ident "In1") [C.Ident "x"]
    guardExp =
      C.BELT
        (C.EIdent (C.Ident "populatePtr"))
        (C.BEAdd (mkInt width) (mkInt 3))
    stmts =
      [ arrayUpdate "buffer" "populatePtr" "x"
      , varIncr "consumed"
      , varIncr "populatePtr"
      ]

donePopulateBufferAction width = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "donePopulateBuffer"]
    head = C.ActnHeadGuarded [] [] [guardExp]
    guardExp =
      C.BEEQ (identCalExp "populatePtr") (C.BEAdd (mkInt width) (mkInt 3))
    stmts = [varSetInt "populatePtr" 0]

-- in1:[token]
streamInPattern = C.InPattTagIds (C.Ident "In1") [C.Ident "token"]

-- out1:[v]
streamOutPattern =
  C.OutPattTagIds (C.Ident "Out1") [C.OutTokenExp (identCalExp "v")]

mkBufferIdxAssignment lhsIdent idxExp =
  C.LocVarDecl
    (C.VDeclExpIMut
       (intCalType 32)
       (C.Ident lhsIdent)
       []
       (C.EIdentArr (C.Ident "buffer") [(C.BExp idxExp)]))

topLeftAction width xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topLeft"]
    head = C.ActnHeadVars [streamInPattern] [streamOutPattern] localVars
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment "p2" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment "p4" (identCalExp "idx")
        , mkBufferIdxAssignment "p5" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BENeg (mkInt width) (mkInt 1))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BENeg (mkInt width) (mkInt 1))))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayUpdate "buffer" "consumed" "token"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr "consumed"
      , varIncr (idCalShow xLocId)
      ]

topRowAction width xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topRow"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern]
        [guardExp]
        localVars
    guardExp = C.BELT (mkVar "midPtr") (mkInt (width - 1))
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 2)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 3)))
        , mkBufferIdxAssignment "p4" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 2)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 3)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEAdd (mkInt width) (mkInt 1))))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEAdd (mkInt width) (mkInt 2))))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayExpUpdate
          "buffer"
          (C.BExp (findIndexFunc (C.BEAdd (mkVar "idx") (mkVar "bufferSize"))))
          ("token")
      , varIncr "consumed"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr "processedRows"
      , varIncr (idCalShow xLocId)
      ]

topRightAction width xLocId yLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topRight"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern]
        [guardExp]
        localVars
    guardExp = C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment "p4" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEAdd (mkInt width) (mkInt 1))))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEAdd (mkInt width) (mkInt 2))))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayExpUpdate
          "buffer"
          (C.BExp (findIndexFunc (C.BEAdd (mkVar "idx") (mkVar "bufferSize"))))
          ("token")
      , varIncr "consumed"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varSetInt "midPtr" 0
      , varSetInt "processedRows" 1
      , varSetInt (idCalShow xLocId) 0
      , varIncr (idCalShow yLocId)
      ]

midLeftAction width height xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "midLeft"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern]
        guardExps
        localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment "p2" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayExpUpdate
          "buffer"
          (C.BExp (findIndexFunc (C.BEAdd (mkVar "idx") (mkVar "bufferSize"))))
          ("token")
      , varIncr "consumed"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

midLeftActionNoConsume width height xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "midLeftNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment "p2" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ C.SemiColonSeparatedStmt
          (C.AssignStt
             (C.AssStmt
                (C.Ident "idx")
                (C.IdBrSExpCons (C.Ident "myMod") [mkVar "consumed"])))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

midAction width height xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "mid"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern]
        guardExps
        localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 2)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 2)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 2)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayExpUpdate
          "buffer"
          (C.BExp (findIndexFunc (C.BEAdd (mkVar "idx") (mkVar "bufferSize"))))
          ("token")
      , varIncr "consumed"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

midActionNoConsume width height xLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "midNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 2)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 2)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 2)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

midRightAction width height xLocId yLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "midRight"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern]
        guardExps
        localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayExpUpdate
          "buffer"
          (C.BExp (findIndexFunc (C.BEAdd (mkVar "idx") (mkVar "bufferSize"))))
          ("token")
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "consumed"
      , varIncr "processedRows"
      , varSetInt "midPtr" 0
      , varSetInt (idCalShow xLocId) 0
      , varIncr (idCalShow yLocId)
      ]

midRightActionNoConsume width height xLocId yLocId = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "midRightNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc
               (C.BEAdd (identCalExp "idx") (C.BEMult (mkInt 2) (mkInt width))))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd
                  (C.BEAdd
                     (identCalExp "idx")
                     (C.BEMult (mkInt 2) (mkInt width)))
                  (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts = [varIncr "processedRows"
            ,varSetInt "midPtr" 0
            , varSetInt (idCalShow xLocId) 0
            , varIncr (idCalShow yLocId)
            ]

bottomLeftActionNoConsume width height xLocId =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "bottomLeftNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BEEQ (mkVar "processedRows") (mkInt (height - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment "p2" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

bottomRowActionNoConsume width height xLocId =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "bottomRowNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 2)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 2)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 2)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr (idCalShow xLocId)
      ]

bottomRightActionNoConsume width height xLocId yLocId =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "bottomRightNoConsume"]
    head = C.ActnHeadGuardedVars [] [streamOutPattern] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      ]
    localVars =
      C.LocVarsDecl
        [ mkBufferIdxAssignment "p1" (identCalExp "idx")
        , mkBufferIdxAssignment
            "p2"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p3"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt 1)))
        , mkBufferIdxAssignment
            "p4"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p5"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p6"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p7"
            (findIndexFunc (C.BEAdd (identCalExp "idx") (mkInt width)))
        , mkBufferIdxAssignment
            "p8"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , mkBufferIdxAssignment
            "p9"
            (findIndexFunc
               (C.BEAdd (C.BEAdd (identCalExp "idx") (mkInt width)) (mkInt 1)))
        , C.LocVarDecl
            (C.VDeclExpIMut
               (intCalType 32)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetInt "processedRows" 0
      , varSetInt "idx" 0
      , varSetInt "midPtr" 0
      , varSetInt "consumed" 0
      , varSetInt (idCalShow xLocId) 0
      , varSetInt (idCalShow yLocId) 0
      ]

findIndexFunc offset = C.IdBrSExpCons (C.Ident "myMod") [offset]
-- mkModFunctionModTwoArgs :: C.FunctionDecl
-- mkModFunctionModTwoArgs = C.FDecl (C.Ident "myModTwoArgs") args returnType localVars body
--   where
--     args = [ C.ArgPar (mkIntType 16) (C.Ident "x")
--            , C.ArgPar (mkIntType 16) (C.Ident "y")
--            ]
--     localVars = C.FNVarDecl
--     returnType = mkIntType 16
--     body = C.IfExpCons
--            (C.IfExpr
--            (C.BEGT (C.EIdent (C.Ident "x")) (C.BENeg (C.EIdent (C.Ident "y")) (mkInt 1)))
--            (C.BENeg (C.EIdent (C.Ident "x")) (C.EIdent (C.Ident "y")))
--            (C.EIdent (C.Ident "x")))
-- -- | not currently used.
-- mkModFunction bufferSize =
--   if bufferSize `elem` map(2^)[1..50]
--   then mkModFunctionBitwise
--   else mkModFunctionMod
-- -- | not currently used.
-- mkModFunctionBitwise :: C.FunctionDecl
-- mkModFunctionBitwise  = C.FDecl (C.Ident "myMod") args returnType localVars body
--   where
--     args = [ C.ArgPar (mkIntType 16) (C.Ident "x")
--            ]
--     localVars = C.FVarDecl [ ]
--     returnType = mkIntType 16
--     body = C.BEBWAnd
--            (C.EIdent (C.Ident "x"))
--            (C.BENeg (C.EIdent (C.Ident "bufferSize")) (mkInt 1))
