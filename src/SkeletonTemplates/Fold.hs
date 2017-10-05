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
        -- , foldAction
        -- , outputAction
        ]
      preloads = processGlobalVarsTwoVarFunc dataflow fun
      stateVars = genState expState vars1
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

foldAction rhsId = ("fold", action)
  where
    action = undefined

outputAction lhsId = ("output_" ++ lhsId, action)
  where
    action = undefined

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
