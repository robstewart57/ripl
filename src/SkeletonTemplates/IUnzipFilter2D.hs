module SkeletonTemplates.IUnzipFilter2D
  ( iunzipFilter2DActor
  ) where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

iunzipFilter2DActor :: String -> Dimension -> R.AnonFun -> R.AnonFun -> C.Type -> C.Type -> C.Actor
iunzipFilter2DActor actorName (Dimension width height) anonFun1 anonFun2 incomingType outgoingType =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")
          ,C.PortDcl outType (C.Ident "Out2")]
      inType = incomingType
      outType = outgoingType
      functions =
        [ C.UnitCode (C.UFunDecl maxFun)
        , C.UnitCode (C.UFunDecl (kernelFun "applyKernel1" anonFun1))
        , C.UnitCode (C.UFunDecl (kernelFun "applyKernel2" anonFun2))
        , C.UnitCode (C.UFunDecl mkModFunctionMod)
        ]
      -- , C.UnitCode (C.UFunDecl mkModFunctionModTwoArgs)]
      actions =
        [ C.ActionCode (populateBufferAction width)
        , C.ActionCode (donePopulateBufferAction width)
        , C.ActionCode (topLeftAction width)
        , C.ActionCode (topRowAction width)
        , C.ActionCode (topRightAction width)
        , C.ActionCode (midLeftAction1 width height)
        , C.ActionCode (midLeftAction2 width height)
        , C.ActionCode (midLeftActionNoConsume1 width height)
        , C.ActionCode (midLeftActionNoConsume2 width height)
        , C.ActionCode (midAction1 width height)
        , C.ActionCode (midAction2 width height)
        , C.ActionCode (midActionNoConsume1 width height)
        , C.ActionCode (midActionNoConsume2 width height)
        , C.ActionCode (midRightAction1 width height)
        , C.ActionCode (midRightAction2 width height)
        , C.ActionCode (midRightActionNoConsume1 width height)
        , C.ActionCode (midRightActionNoConsume2 width height)
        , C.ActionCode (bottomLeftActionNoConsume1 width height)
        , C.ActionCode (bottomLeftActionNoConsume2 width height)
        , C.ActionCode (bottomRowActionNoConsume1 width height)
        , C.ActionCode (bottomRowActionNoConsume2 width height)
        , C.ActionCode (bottomRightActionNoConsume1 width height)
        , C.ActionCode (bottomRightActionNoConsume2 width height)
        ]
  in C.ActrSchd
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       (globalVars width)
       (functions ++ actions)
       fsmSchedule
       []

globalVars width
           -- uint(size=16) bufferSize = imageWidth * 2 + 3;
 =
  [ C.GlobVarDecl
      (C.VDeclExpIMut
         (uintCalType 16)
         (C.Ident "bufferSize")
         []
         (C.BEAdd (C.BEMult (mkInt width) (mkInt 2)) (mkInt 3)))
    -- uint(size=16) buffer[bufferSize];
  , C.GlobVarDecl
      (C.VDecl
         (intCalType 16)
         (C.Ident "buffer")
         [C.BExp (C.EIdent (C.Ident "bufferSize"))])
    -- uint(size=16) idx := 0;
  , C.GlobVarDecl (C.VDeclExpMut (intCalType 16) (C.Ident "idx") [] (mkInt 0))
    -- uint(size=16) populatePtr := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (intCalType 16) (C.Ident "populatePtr") [] (mkInt 0))
    -- uint(size=16) processedMidRows := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (intCalType 16) (C.Ident "processedRows") [] (mkInt 0))
    -- uint(size=32) consumed := 0;
  , C.GlobVarDecl
      (C.VDeclExpMut (uintCalType 32) (C.Ident "consumed") [] (mkInt 0))
  , C.GlobVarDecl
      (C.VDeclExpMut (uintCalType 16) (C.Ident "midPtr") [] (mkInt 0))
  , C.GlobVarDecl
      (C.VDeclExpMut (boolCalType) (C.Ident "isEven") [] (mkBool True))
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
    , C.StTrans (C.Ident "s3") (C.Ident "midLeft1") (C.Ident "s4")
    , C.StTrans (C.Ident "s3") (C.Ident "midLeft2") (C.Ident "s4")
    , C.StTrans (C.Ident "s3") (C.Ident "midLeftNoConsume1") (C.Ident "s4")
    , C.StTrans (C.Ident "s3") (C.Ident "midLeftNoConsume2") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "mid1") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "mid2") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "midNoConsume1") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "midNoConsume2") (C.Ident "s4")
    , C.StTrans (C.Ident "s4") (C.Ident "midRight1") (C.Ident "s5")
    , C.StTrans (C.Ident "s4") (C.Ident "midRight2") (C.Ident "s5")
    , C.StTrans (C.Ident "s4") (C.Ident "midRightNoConsume1") (C.Ident "s5")
    , C.StTrans (C.Ident "s4") (C.Ident "midRightNoConsume2") (C.Ident "s5")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeft1") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeft2") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeftNoConsume1") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "midLeftNoConsume2") (C.Ident "s4")
    , C.StTrans (C.Ident "s5") (C.Ident "bottomLeftNoConsume1") (C.Ident "s6")
    , C.StTrans (C.Ident "s5") (C.Ident "bottomLeftNoConsume2") (C.Ident "s6")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRowNoConsume1") (C.Ident "s6")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRowNoConsume2") (C.Ident "s6")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRightNoConsume1") (C.Ident "s0")
    , C.StTrans (C.Ident "s6") (C.Ident "bottomRightNoConsume2") (C.Ident "s0")
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
      [ (C.ArgPar (intCalType 16) (C.Ident "i"))
      , (C.ArgPar (intCalType 16) (C.Ident "j"))
      ]
    localVars = C.FNVarDecl
    returnType = intCalType 16
    body =
      C.IfExpCons
        (C.IfExpr
           (C.BEGT (identCalExp "i") (identCalExp "j"))
           (identCalExp "i")
           (identCalExp "j"))

mkModFunctionMod :: C.FunctionDecl
mkModFunctionMod = C.FDecl (C.Ident "myMod") args returnType localVars body
  where
    args = [C.ArgPar (mkIntType 16) (C.Ident "x")]
    localVars = C.FNVarDecl
    returnType = mkIntType 16
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

kernelFun :: String -> R.AnonFun -> C.FunctionDecl
kernelFun kernelName (R.AnonFunC lambdaExps userDefinedFunc) =
  C.FDecl (C.Ident kernelName) args returnType localVars body
  where
    args =
      map
        (\(R.ExpSpaceSepC (R.ExprVar (R.VarC lambdaIdent))) ->
           (C.ArgPar (mkIntType 16) (idRiplToCal lambdaIdent)))
        lambdaExps
    localVars = C.FVarDecl [resultAssignment]
    resultAssignment =
      C.LocVarDecl
        (C.VDeclExpIMut
           (intCalType 16)
           (C.Ident "result")
           []
           (expRiplToCal userDefinedFunc))
    returnType = mkIntType 16
    body = C.IdBrSExpCons (C.Ident "max") [mkInt 0, C.EIdent (C.Ident "result")]

midLeftAction1 width height = midLeftAction (C.EIdent (C.Ident "isEven")) "applyKernel1" "midLeft1" "Out1" width height
midLeftAction2 width height = midLeftAction (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "midLeft2" "Out2" width height
midLeftActionNoConsume1 width height = midLeftActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "midLeftNoConsume1" "Out1" width height
midLeftActionNoConsume2 width height = midLeftActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "midLeftNoConsume2" "Out2" width height
midAction1 width height = midAction (C.EIdent (C.Ident "isEven")) "applyKernel1" "mid1" "Out1" width height
midAction2 width height = midAction (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "mid2" "Out2" width height
midActionNoConsume1 width height = midActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "midNoConsume1" "Out1" width height
midActionNoConsume2 width height = midActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "midNoConsume2" "Out2" width height
midRightAction1 width height = midRightAction (C.EIdent (C.Ident "isEven")) "applyKernel1" "midRight1" "Out1" width height
midRightAction2 width height = midRightAction (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "midRight2" "Out2" width height
midRightActionNoConsume1 width height = midRightActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "midRightNoConsume1" "Out1" width height
midRightActionNoConsume2 width height = midRightActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "midRightNoConsume2" "Out2" width height
bottomLeftActionNoConsume1 width height = bottomLeftActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "bottomLeftNoConsume1" "Out1" width height
bottomLeftActionNoConsume2 width height = bottomLeftActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "bottomLeftNoConsume2" "Out2" width height
bottomRowActionNoConsume1 width height = bottomRowActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "bottomRowNoConsume1" "Out1" width height
bottomRowActionNoConsume2 width height = bottomRowActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "bottomRowNoConsume2" "Out2" width height
bottomRightActionNoConsume1 width height = bottomRightActionNoConsume (C.EIdent (C.Ident "isEven")) "applyKernel1" "bottomRightNoConsume1" "Out1" width height
bottomRightActionNoConsume2 width height = bottomRightActionNoConsume (C.UENot (C.EIdent (C.Ident "isEven"))) "applyKernel2" "bottomRightNoConsume2" "Out2" width height

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

-- out1:[v] or out2:[v]
streamOutPattern portName =
  C.OutPattTagIds (C.Ident portName) [C.OutTokenExp (identCalExp "v")]

mkBufferIdxAssignment lhsIdent idxExp =
  C.LocVarDecl
    (C.VDeclExpIMut
       (intCalType 16)
       (C.Ident lhsIdent)
       []
       (C.EIdentArr (C.Ident "buffer") [(C.BExp idxExp)]))

topLeftAction width = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topLeft"]
    head = C.ActnHeadVars [streamInPattern] [streamOutPattern "Out1"] localVars
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel1")
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ arrayUpdate "buffer" "consumed" "token"
      , varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      , varIncr "consumed"
      ]

topRowAction width = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topRow"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern "Out1"]
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel1")
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
      ]

topRightAction width = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident "topRight"]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern "Out1"]
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident "applyKernel1")
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
      , varNot "isEven"
      ]

midLeftAction predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern outPortName]
        guardExps
        localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
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
      ]

midLeftActionNoConsume predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
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
      ]

midAction predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern outPortName]
        guardExps
        localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BELT (mkVar "processedRows") (mkInt (height - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
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
      ]

midActionNoConsume predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      ]

midRightAction predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head =
      C.ActnHeadGuardedVars
        [streamInPattern]
        [streamOutPattern outPortName]
        guardExps
        localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BELT (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
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
      , varNot "isEven"
      ]

midRightActionNoConsume predicateExp kernelName actionName outPortName width height = C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts = [ varIncr "processedRows"
            , varSetInt "midPtr" 0
            , varNot "isEven"
            ]

bottomLeftActionNoConsume predicateExp kernelName actionName outPortName width height =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt 0)
      , C.BEEQ (mkVar "processedRows") (mkInt (height - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      ]

bottomRowActionNoConsume predicateExp kernelName actionName outPortName width height =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BELT (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetExp "idx" (findIndexFunc (C.BEAdd (mkVar "idx") (mkInt 1)))
      , varIncr "midPtr"
      ]

bottomRightActionNoConsume predicateExp kernelName actionName outPortName width height =
  C.AnActn (C.ActnTagsStmts tag head stmts)
  where
    tag = C.ActnTagDecl [C.Ident actionName]
    head = C.ActnHeadGuardedVars [] [streamOutPattern outPortName] guardExps localVars
    guardExps =
      [ C.BEEQ (mkVar "midPtr") (mkInt (width - 1))
      , C.BEEQ (mkVar "consumed") (mkInt (width * height))
      , predicateExp
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
               (intCalType 16)
               (C.Ident "v")
               []
               (C.IdBrSExpCons
                  (C.Ident kernelName)
                  (map
                     identCalExp
                     ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"])))
        ]
    stmts =
      [ varSetInt "processedRows" 0
      , varSetInt "idx" 0
      , varSetInt "midPtr" 0
      , varSetInt "consumed" 0
      , varNot "isEven"
      ]

findIndexFunc offset = C.IdBrSExpCons (C.Ident "myMod") [offset]

