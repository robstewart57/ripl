module SkeletonTemplates.Scale where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace
import SkeletonTemplates.CalTypes
import Types

{- This actor scales an image in the X and Y dimensions.

e.g. given this 4x4 image:

1 2 3 4
5 6 7 8
9 9 2 2
8 1 2 4

if the RIPL programs says:

    biggerImage = scale image 4;

then each pixel is first stretched in the X direction, then the row is
outputted by the scale factor, in this case 4. e.g. for the 1st row:

1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4
1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4
1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4
1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4

The actor is implemented as follows:

action in1:[a,b,c,d]
 ==>
 out1:[a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d]
-}

scaleActor :: String -> R.Exp -> Integer -> C.Type -> C.Type -> C.Actor
scaleActor actorName (R.ExprInt scaleFactor) imageWidth calTypeIncoming calTypeOutgoing =
  let ioSig =
        C.IOSg
          [C.PortDcl inType (C.Ident "In1")]
          [C.PortDcl outType (C.Ident "Out1")]
      inType = calTypeIncoming
      outType = calTypeOutgoing
      -- inTokens = map (\i -> C.Ident ("x" ++ show i)) [1..imageWidth]
      inputPattern =
        C.InPattTagIdsRepeat
        (C.Ident "In1")
        [C.Ident "row"]
        (C.RptClause (mkInt imageWidth))
      outputPattern =
        C.OutPattTagIds
        (C.Ident "Out1")
      -- (concatMap
      --   (\i ->
      --       replicate (fromIntegral scaleFactor)
      --   )
      --   )
      --     [0..(imageWidth-1)]        
        (map C.OutTokenExp
        (concat
         (replicate (fromInteger scaleFactor)
          (concatMap (\i -> replicate (fromInteger scaleFactor)
                  (C.IndSExpCons (C.IndExpr (C.Ident "row") (C.BExp (mkInt i))))
               ) [0..(imageWidth-1)])
          )
         )
        )
       
        -- (replicate (fromInteger scaleFactor)
        --  (C.IndSExpCons (C.IndExpr (C.Ident "row") (C.BExp (C.BEDiv (C.EIdent (C.Ident "i")) (mkInt scaleFactor))))))
        -- -- (replicate (fromIntegral scaleFactor)
        -- --  (C.IndSExpCons (C.IndExpr (C.Ident "row") (C.BExp (C.EIdent (C.Ident "i"))))))
        -- (C.GenExpr
        -- [C.GeneratorExpr (mkIntType 16) (C.Ident "i") (C.BEList (mkInt 0) (mkInt ((imageWidth*scaleFactor)-1)))]
        -- ))))]
        -- (concatMap (\token -> replicate (fromInteger scaleFactor) (C.OutTokenExp (C.EIdent token))) inTokens)
        -- (C.RptClause (mkInt (scaleFactor*scaleFactor*imageWidth)))
      actionHead = C.ActnHead [inputPattern] [outputPattern]
      action =
        C.ActionCode
          (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "the_action"])
                actionHead
                []))
  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       []
       [action]
       []
