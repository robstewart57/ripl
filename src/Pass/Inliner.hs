module Pass.Inliner where

import qualified AbsRIPL as R
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import AstMappings
import Debug.Trace
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (replicate)

type FunctionDictionary = Map.Map R.Ident R.Function

inlineFunctions
  :: [R.Function] -- ^ functions in the program
  -> [R.Assignment] -- ^ list of statments in the main function
  -> [R.Assignment] -- ^ list of statements for the main function after inlining
inlineFunctions functions stmts =
  let funLookup =
        Map.fromList $
        map (\f@(R.FunctionC ident _ _ _) -> (ident, f)) functions
      inlinedStmts =
        concatMap
          (\stmt ->
             case stmt of
               (R.AssignSkelC {}) -> [stmt]
               (R.AssignFunCallC _ (R.FunCall functionName _)) ->
                 let function =
                       fromMaybe
                         (error
                            ("undefined function: " ++ idRiplShow functionName))
                         (Map.lookup functionName funLookup)
                 in inlineFunction function stmt funLookup)
          stmts
  in inlinedStmts

{-
 1. create an AST instance of the called function.

 2. for each argument at position N of the function definition,
    replace with idents and constants from the call site.

 3. for the returned ident from the called function, replace it in
    the AST instance with the LHS ident from the call site.

 4. for all ident's in the AST instance that are neither returned
    nor are function arguments, prefix with "<LHS>_".
-}
-- TODO: surely inlining would mean many ass statements?
inlineFunction :: R.Function
               -> R.Assignment
               -> Map R.Ident R.Function
               -> [R.Assignment]
inlineFunction (R.FunctionC funIdent funArgs funStmts returnIdents) (R.AssignFunCallC lhsInProgFun (R.FunCall calledFun passedArgs)) funLookup
                                                                                                                                     -- 2. for each argument at position N of the function definition,
                                                                                                                                     --    replace with idents and constants from the call site.
 =
  let argDict = Map.fromList $ zip funArgs passedArgs
      funStmtsFoldedArgNames =
        map
          (\assignment ->
             case assignment of
               (R.AssignSkelC lhsId skelRHS) ->
                 R.AssignSkelC lhsId (inlineArgNames skelRHS argDict)
               (R.AssignFunCallC lhsId funCallRHS) ->
                 R.AssignFunCallC
                   lhsId
                   (inlineArgNamesToFunCall funCallRHS argDict))
          funStmts
      -- 3. for the returned ident from the called function, replace it in
      --   the AST instance with the LHS ident from the call site.
      newReturnedIdent =
        map
          (\assignment ->
             case assignment of
               (R.AssignSkelC (R.IdentsOneId lhsId) skelRHS)
               -- let returnedIdents =
               --       case returnIdents of
               --         R.IdentsOneId ident -> [ident]
               --         _ -> error ("unexpected ident in inlineFunction: " ++ show returnIdent ++ ", RHS is: " ++ show skelRHS)
                ->
                 let newLhsId =
                       case returnIdents of
                         R.IdentsOneId oneIdent ->
                           if lhsId == oneIdent
                             then lhsInProgFun
                             else R.IdentsOneId lhsId
                         R.IdentsManyIds returnedIds {- firstId secondId -}
                          ->
                           let R.IdentsManyIds idsAtCallSite = lhsInProgFun {- firstAtCallSite secondAtCallSite -}
                               myFun :: (R.Ident, R.Ident) -> R.Ident -> R.Ident
                               myFun (returnedId, idAtCallSite) keptLhsId =
                                 if lhsId == returnedId
                                   then idAtCallSite
                                   else keptLhsId
                               -- (a -> b -> b) -> b -> [a] -> b
                               newLhsId =
                                 foldr
                                   myFun
                                   lhsId
                                   (zip returnedIds idsAtCallSite)
                               {-
                                              newLhsId =
                                                if lhsId == firstId
                                                then firstAtCallSite
                                                else if lhsId == secondId
                                                     then secondAtCallSite
                                                     else secondId
                                              -}
                           in R.IdentsOneId newLhsId
                 in R.AssignSkelC newLhsId skelRHS
               -- for the unzip case, TODO fix: this is a hack for now
               -- i.e. no renaming is performed, meaning that unzip
               -- must not return variables that are directly used
               -- in the return tuple of a function
               ass@(R.AssignSkelC (R.IdentsManyIds lhsIdents) skelRHS) -> ass
               (R.AssignFunCallC (R.IdentsOneId lhsId) funCallRHS) ->
                 let newLhsId =
                       case returnIdents of
                         R.IdentsOneId oneIdent ->
                           if lhsId == oneIdent
                             then lhsInProgFun
                             else R.IdentsOneId lhsId
                 in R.AssignFunCallC newLhsId funCallRHS
               ass ->
                 error ("unexpected assignment in inlineFunction: " ++ show ass))
          funStmtsFoldedArgNames
      -- 4. for all ident's in the AST instance that are neither returned
      --    nor are function arguments, prefix with "<LHS>_".
      -- mainStmtLHSs =
      --   case lhsInProgFun of
      --     R.IdentsOneId mainFunStmtLhs -> [idRiplShow mainFunStmtLhs]
      -- TODO something better
      prefix =
        case lhsInProgFun of
          R.IdentsOneId mainFunStmtLhs -> idRiplShow mainFunStmtLhs
          R.IdentsManyIds mainFunStmtLhss -> idRiplShow (head mainFunStmtLhss)
      prefixedStmts =
        map
          (\ass ->
             replaceLocalIdents ass argDict returnIdents lhsInProgFun prefix)
          newReturnedIdent
      stmtsFoldedConsts =
        concatMap
          (\assignment ->
             case assignment of
               (R.AssignSkelC lhs rhs) ->
                 [R.AssignSkelC lhs (foldConstantArgs rhs argDict)]
               (R.AssignFunCallC lhs funCall@(R.FunCall functionName args)) ->
                 let function =
                       fromMaybe
                         (error
                            ("undefined function: " ++ idRiplShow functionName))
                         (Map.lookup functionName funLookup)
                 in inlineFunction function assignment funLookup)
          prefixedStmts
  in stmtsFoldedConsts

-- inlineFunction :: R.Function -> R.Assignment -> [R.Assignment]
-- | renames LHS variables in the body of a called function, for all
--   variables that are neither an argument of the function or are
--   returned variables from the function. Also rename RHS variables
--   corresponding to the changes to internal LHS variable names.
replaceLocalIdents
  :: R.Assignment
  -> Map R.FunArg R.FunArg
  -> R.Idents
  -> R.Idents
  -> String
  -> R.Assignment
replaceLocalIdents (R.AssignSkelC (R.IdentsOneId lhsId) rhs) boundIdents returnedIdents (R.IdentsOneId lhsCallSiteId) prefix
                                                                                                                      -- replaceLocalIdents' boundIdents lhsId [lhsCallSite] prefix rhs
 = doReplace [lhsCallSiteId] prefix [lhsId] boundIdents rhs
replaceLocalIdents (R.AssignSkelC (R.IdentsOneId lhsId) rhs) boundIdents returnedIdents (R.IdentsManyIds lhsManyCallSiteIds) prefix
                                                                                                                             -- replaceLocalIdents' boundIdents lhsId lhsManyCallSite prefix rhs
 = doReplace lhsManyCallSiteIds prefix [lhsId] boundIdents rhs
replaceLocalIdents rhsFunCall@R.AssignFunCallC {} boundIdents returnedIdent (R.IdentsOneId lhsCallSite) prefix =
  error
    ("replaceLocalIdents doesn't support 1 arity functions in functions: " ++
     show rhsFunCall)
replaceLocalIdents rhsFunCall@R.AssignFunCallC {} boundIdents returnedIdent idents prefix =
  error
    ("replaceLocalIdents doesn't support 2 arity functions in functions: " ++
     show rhsFunCall)
replaceLocalIdents (R.AssignSkelC (R.IdentsManyIds lhsIds) rhs) boundIdents returnedIdents (R.IdentsManyIds lhsManyCallSiteIds) prefix =
  doReplace lhsManyCallSiteIds prefix lhsIds boundIdents rhs
replaceLocalIdents rhs _ _ _ _ =
  error ("Unexpected RHS to replaceLocalIdents: " ++ show rhs)

doReplace lhsCallSiteIds prefixes lhsIds boundIdents rhs =
  let argVarsAtCallSite = Map.elems boundIdents
      newLhsIds =
        map (replaceInternalLHS lhsCallSiteIds prefixes boundIdents) lhsIds
      newRhs =
        renameInternalLhsIdentsInRhs
          argVarsAtCallSite
          lhsCallSiteIds
          prefixes
          rhs
  in case newLhsIds of
       [newLhsId] -> R.AssignSkelC (R.IdentsOneId newLhsId) newRhs
       newLhss -> R.AssignSkelC (R.IdentsManyIds newLhss) newRhs

renameInternalLhsIdentsInRhs argVarsAtCallSite lhsCallSiteIds prefix rhs =
  let newRhs =
        let rhsIds = idsFromRHS rhs
            x = foldr f rhs (zip [0,1 ..] rhsIds)
            f (i, rhsId) rhs
                         -- let rhsId@(R.Ident rhsIdStr) = idFromRHS rhs
             =
              if not
                   (elem
                      (R.FunArgExprC (R.ExprVar (R.VarC rhsId)))
                      argVarsAtCallSite) &&
                 (not (elem rhsId lhsCallSiteIds))
                then replaceIdInRHS
                       i
                       (R.Ident (prefix ++ "_" ++ idRiplShow rhsId))
                       rhs
                else rhs
        in x
  in newRhs

replaceInternalLHS lhsCallSiteIds prefix boundIdents lhsId =
  let argVarsAtCallSite = Map.elems boundIdents
      newLhsId
        | not
           (elem (R.FunArgExprC (R.ExprVar (R.VarC lhsId))) argVarsAtCallSite) &&
            (not (elem lhsId lhsCallSiteIds)) =
          R.Ident (prefix ++ "_" ++ (idRiplShow lhsId))
        | otherwise = lhsId
  in newLhsId

{-
replaceLocalIdents'
  :: Map k R.FunArg
  -> R.Ident
  -> [R.Ident]
  -> String
  -> R.AssignSkelRHS
  -> R.Assignment
replaceLocalIdents' boundIdents lhsId lhsCallSiteIds prefix rhs =
    let argVarsAtCallSite = Map.elems boundIdents
        newLhsId
            | not (elem (R.FunArgExprC (R.ExprVar (R.VarC lhsId))) argVarsAtCallSite)
                   && (not (elem lhsId lhsCallSiteIds)) =
              R.Ident (prefix ++ "_" ++ (idRiplShow lhsId))
            | otherwise = lhsId

        -- (a -> b -> b) -> b -> [a] -> b
        newRhs =
            let rhsIds = idsFromRHS rhs
                x = foldr f rhs (zip [0,1..] rhsIds)
                f (i,rhsId) rhs =
                    -- let rhsId@(R.Ident rhsIdStr) = idFromRHS rhs
                       if not (elem (R.FunArgExprC (R.ExprVar (R.VarC rhsId))) argVarsAtCallSite)
                           && (not (elem rhsId lhsCallSiteIds))
                       then replaceIdInRHS i (R.Ident (prefix ++ "_" ++ idRiplShow rhsId)) rhs
                       else rhs
            in x

    in (R.AssignSkelC (R.IdentsOneId newLhsId) newRhs)
-}
replaceExprs :: [R.Exp] -> Map R.FunArg R.FunArg -> [R.Exp]
replaceExprs exprs renameMap = map replace exprs
  where
    replace (R.ExprVar v) = replaceVar v
    replace e@(R.ExprInt {}) = e
    replace (R.ExprAdd e1 e2) = R.ExprAdd (replace e1) (replace e2)
    replace (R.ExprMinus e1 e2) = R.ExprMinus (replace e1) (replace e2)
    replace (R.ExprMul e1 e2) = R.ExprMul (replace e1) (replace e2)
    replace (R.ExprDiv e1 e2) = R.ExprDiv (replace e1) (replace e2)
    replace (R.ExprShiftR e1 e2) = R.ExprShiftR (replace e1) (replace e2)
    replace (R.ExprShiftL e1 e2) = R.ExprShiftL (replace e1) (replace e2)
    replace (R.ExprMin e1 e2) = R.ExprMin (replace e1) (replace e2)
    replace (R.ExprMax e1 e2) = R.ExprMax (replace e1) (replace e2)
    replace (R.ExprAbs e1) = R.ExprAbs (replace e1)
    replace (R.ExprGT e1 e2) = R.ExprGT (replace e1) (replace e2)
    replace (R.ExprGTE e1 e2) = R.ExprGTE (replace e1) (replace e2)
    replace (R.ExprLT e1 e2) = R.ExprLT (replace e1) (replace e2)
    replace (R.ExprLTE e1 e2) = R.ExprLTE (replace e1) (replace e2)
    replace (R.ExprIfThenElse e1 e2 e3) =
      R.ExprIfThenElse (replace e1) (replace e2) (replace e3)
    replace (R.ExprBracketed e) = R.ExprBracketed (replace e)
    replace R.ExprIndexHere = R.ExprIndexHere
    replace e@R.ExprIndex {} = e
    replace (R.ExprIndexedVector ident e1) =
      let R.ExprVar (R.VarC newIdent) = replace (R.ExprVar (R.VarC ident))
      in R.ExprIndexedVector newIdent (replace e1)
    replace (R.ExprVectorMod ident e1 modifier) =
      let R.ExprVar (R.VarC newIdent) = replace (R.ExprVar (R.VarC ident))
      in R.ExprVectorMod newIdent (replace e1) modifier
    replace e =
      error ("unsupported exp in Inliner.foldConstantArgs: " ++ show e)
    replaceVar (R.VarC varIdent)
      | Map.member (R.FunArgExprC (R.ExprVar (R.VarC varIdent))) renameMap =
        let replacement =
              fromJust $
              Map.lookup (R.FunArgExprC (R.ExprVar (R.VarC varIdent))) renameMap
        in case replacement of
             R.FunArgExprC (R.ExprVar (R.VarC replacementIdent)) ->
               R.ExprVar (R.VarC replacementIdent)
             R.FunArgConstC i -> R.ExprInt i
             R.FunArgExprC (R.ExprInt i) -> R.ExprInt i
             R.FunArgExprC intExpr -> R.ExprInt (riplExpToInt intExpr)
             arg -> error ("uncaught arg in foldConstantArgs: " ++ show arg)
      | otherwise = R.ExprVar (R.VarC varIdent)

foldConstantArgs :: R.AssignSkelRHS -> Map R.FunArg R.FunArg -> R.AssignSkelRHS
foldConstantArgs (R.MapSkel usedId (R.AnonFunDiscreteUnaryC _varList (R.ExprListC exprs))) renameMap =
  let newExprs = replaceExprs exprs renameMap
  in (R.MapSkel usedId (R.AnonFunDiscreteUnaryC _varList (R.ExprListC newExprs)))
foldConstantArgs (R.IUnzipSkel usedId (R.AnonFunIndexedC exprs1) (R.AnonFunIndexedC exprs2)) renameMap =
  let [newExprs1] = replaceExprs [exprs1] renameMap
      [newExprs2] = replaceExprs [exprs2] renameMap
  in R.IUnzipSkel
       usedId
       (R.AnonFunIndexedC newExprs1)
       (R.AnonFunIndexedC newExprs2)
foldConstantArgs skel@R.TransposeSkel {} _ = skel
foldConstantArgs (R.MapSkel usedId (R.AnonFunDiscreteUnaryC _varList (R.ExprRepeatTokensC expN expToken))) renameMap =
  let [R.ExprInt n] = replaceExprs [expN] renameMap
      newExprs = replicate (fromIntegral n) expToken
  in (R.MapSkel usedId (R.AnonFunDiscreteUnaryC _varList (R.ExprListC newExprs)))
foldConstantArgs (R.ZipWithSkel usedIds (R.AnonFunC lambdas exp)) renameMap =
  let [newExp] = replaceExprs [exp] renameMap
  in (R.ZipWithSkel usedIds (R.AnonFunC lambdas newExp))
foldConstantArgs (R.ZipWithScalarSkel usedIds (R.AnonFunC lambdas exp)) renameMap =
  let [newExp] = replaceExprs [exp] renameMap
  in (R.ZipWithScalarSkel usedIds (R.AnonFunC lambdas newExp))
foldConstantArgs skel@(R.ConvolveSkel {}) renameMap = skel
foldConstantArgs (R.FoldScalarSkel usedId initVal (R.AnonFunBinaryC lambda1 lambda2 exp)) renameMap =
  let [newExp] = replaceExprs [exp] renameMap
  in (R.FoldScalarSkel usedId initVal (R.AnonFunBinaryC lambda1 lambda2 newExp))
foldConstantArgs (R.RepeatSkel usedId exp) renameMap =
  let [newExp] = replaceExprs [exp] renameMap
  in R.RepeatSkel usedId newExp
foldConstantArgs skel _ =
  error ("unsupported skel in foldConstantArgs: " ++ show skel)

-- update 2 "foo" $ fromList ["bar", "bar", "bar"]
-- fromList ["bar","bar","foo"]
replaceIdInRHS 0 newId (R.MapSkel id fun) = R.MapSkel newId fun
replaceIdInRHS 0 newId (R.IUnzipSkel id fun1 fun2) =
  R.IUnzipSkel newId fun1 fun2
replaceIdInRHS 0 newId (R.TransposeSkel ident) = R.TransposeSkel newId
replaceIdInRHS 0 newId (R.RepeatSkel id x) = R.RepeatSkel newId x
replaceIdInRHS 0 newId (R.FoldScalarSkel id i fun) =
  R.FoldScalarSkel newId i fun
replaceIdInRHS n newId (R.ZipWithSkel ids fun) =
  let x = R.IdentSpaceSepC newId
      newIds = toList $ Seq.update n x $ Seq.fromList ids
  in R.ZipWithSkel newIds fun
replaceIdInRHS n newId (R.ZipWithScalarSkel ids fun) =
  let x = R.IdentSpaceSepC newId
      newIds = toList $ Seq.update n x $ Seq.fromList ids
  in R.ZipWithScalarSkel newIds fun
-- R.ZipWithSkel id1 newId fun
-- replaceIdInRHS 1 newId (R.ZipWithSkel ids fun) =
--     let x = R.IdentSpaceSepC newId
--     in R.ZipWithSkel (x:tail ids) fun
replaceIdInRHS _ _ skel =
  error ("unsupported skeleton in replaceIdInRHS: " ++ show skel)

inlineArgNamesToFunCall :: R.FunCallRHS -> Map R.FunArg R.FunArg -> R.FunCallRHS
inlineArgNamesToFunCall (R.FunCall functionName funArgs) renameMap =
  let renamedArgs =
        map
          (\arg ->
             case arg of
               R.FunArgConstC {} -> arg
               R.FunArgExprC (R.ExprVar (R.VarC usedIdent)) ->
                 R.FunArgExprC
                   (R.ExprVar (R.VarC (inlineRhsId usedIdent renameMap))))
          funArgs
  in R.FunCall functionName renamedArgs

inlineArgNames :: R.AssignSkelRHS -> Map R.FunArg R.FunArg -> R.AssignSkelRHS
inlineArgNames rhs@(R.MapSkel usedId fun) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.MapSkel newRhsId fun
inlineArgNames rhs@(R.TransposeSkel usedId) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.TransposeSkel newRhsId
inlineArgNames rhs@(R.FoldScalarSkel usedId initVal fun) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.FoldScalarSkel newRhsId initVal fun
inlineArgNames rhs@(R.ConvolveSkel usedId winWidth windHeight kernelValues) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.ConvolveSkel newRhsId winWidth windHeight kernelValues
inlineArgNames rhs@(R.ZipWithSkel usedIds fun) renameMap =
  let newRhsIds =
        map
          (\(R.IdentSpaceSepC ident) ->
             R.IdentSpaceSepC (inlineRhsId ident renameMap))
          usedIds
  in R.ZipWithSkel newRhsIds fun
inlineArgNames rhs@(R.ZipWithScalarSkel usedIds fun) renameMap =
  let newRhsIds =
        map
          (\(R.IdentSpaceSepC ident) ->
             R.IdentSpaceSepC (inlineRhsId ident renameMap))
          usedIds
  in R.ZipWithScalarSkel newRhsIds fun
inlineArgNames rhs@(R.IUnzipSkel usedId fun1 fun2) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.IUnzipSkel newRhsId fun1 fun2
inlineArgNames rhs@(R.RepeatSkel usedId repeatFreq) renameMap =
  let newRhsId = inlineRhsId usedId renameMap
  in R.RepeatSkel newRhsId repeatFreq
inlineArgNames rhs renameMap =
  error ("unsupported RHS in inlineArgNames: " ++ show rhs)

inlineRhsId :: R.Ident -> Map R.FunArg R.FunArg -> R.Ident
inlineRhsId rhsId renameMap
  | Map.member (R.FunArgExprC (R.ExprVar (R.VarC rhsId))) renameMap =
    let R.FunArgExprC (R.ExprVar (R.VarC newRhsId)) =
          fromJust
            (Map.lookup (R.FunArgExprC (R.ExprVar (R.VarC rhsId))) renameMap)
    in newRhsId
  | otherwise = rhsId
