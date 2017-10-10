module SkeletonTemplates.Common where

import Types
import qualified AbsCAL as C
import qualified AbsRIPL as R
import AstMappings
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

dimensionOfVar :: R.Ident -> ImplicitDataflow -> Dimension
dimensionOfVar ident dataflow =
  fromJust (dim (fromJust (Map.lookup ident dataflow)))

processGlobalVarsTwoVarProc ::
  ImplicitDataflow ->
  R.TwoVarProc ->
  [(C.GlobalVarDecl -- to contain the data to be preloaded
  , C.PortDecl      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsTwoVarProc dataflow foldExp@(R.TwoVarProcC var1 var2 stmts) =
  let globalIds = newIdentsInStatements foldExp
  in map (processGlobalVar dataflow) globalIds

processGlobalVarsTwoVarFunc ::
  ImplicitDataflow ->
  R.TwoVarFun ->
  [(C.GlobalVarDecl -- to contain the data to be preloaded
  , C.PortDecl      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsTwoVarFunc dataflow fun@(R.TwoVarFunC var1 var2 exp) =
  let globalIds = globalIdentsElemBinary fun
  in map (processGlobalVar dataflow) globalIds

processGlobalVarsOneVarFunc ::
  ImplicitDataflow ->
  R.OneVarFun ->
  [(C.GlobalVarDecl -- to contain the data to be preloaded
  , C.PortDecl      -- the port for the preloaded data to arrive into
  , (String,C.CodeBlock))]     -- the action to load the data in
processGlobalVarsOneVarFunc dataflow fun@(R.OneVarFunC var exp) =
  let globalIds = globalIdentsElemUnary fun
  in map (processGlobalVar dataflow) globalIds



processGlobalVar :: ImplicitDataflow -> R.Ident -> (C.GlobalVarDecl,C.PortDecl,(String,C.CodeBlock))
processGlobalVar varLookup ident@(R.Ident identStr) =
  let varNode = fromJust (Map.lookup ident varLookup)
      Dim2 w h = fromJust (dim varNode)

      varDecl = C.GlobVarDecl (C.VDecl (calIntType 32) (idRiplToCal ident) [(C.BExp (C.LitExpCons (C.IntLitExpr (C.IntegerLit (w*h)))))])
      portDecl = C.PortDcl (calIntType 32) (C.Ident (idRiplShow ident ++ "Port"))
      inputPattern = C.InPattTagIdsRepeat (C.Ident (idRiplShow ident ++ "Port")) [C.Ident ("data_" ++ idRiplShow ident)] (C.RptClause (C.LitExpCons (C.IntLitExpr (C.IntegerLit (w*h)))))
      actionHead = C.ActnHead [inputPattern] []

      consumeLoop =
        C.EndSeparatedStmt
        (C.ForEachStt
         (C.ForeachStmtsSt
         [C.ForeachGen (calIntType 32) (C.Ident "i") (C.BEList (intCalExp 0) (intCalExp ((w*h)-1)))]
           [C.SemiColonSeparatedStmt
            (C.AssignStt
             (C.AssStmtIdx
              (C.Ident identStr)
              (C.Idx [C.BExp (C.EIdent (C.Ident "i"))])
              (C.EIdentArr (C.Ident ("data_" ++ identStr)) [C.BExp (C.EIdent (C.Ident "i"))])))]))

      action =
        ("load_" ++ idRiplShow ident
        , C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident ("load_" ++ "lut")])
                actionHead
                [consumeLoop])))

  in (varDecl,portDecl,action)