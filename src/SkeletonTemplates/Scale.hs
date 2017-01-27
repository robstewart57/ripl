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

For the first two rows

[a,b,c,d
 e,f,g,h]

actor is implemented as follows:

action in1:[a,b,c,d]
 ==>
 out1:[a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,a,a,a,a,b,b,b,b,c,c,c,c,d,d,d,d
      ,e,e,e,e,f,f,f,f,g,g,g,g,h,h,h,h
      ,e... ]
-}

{- actor design:

action1:
  - when (x < imageWidth)
    - consume In1:[token]
    - for int i in 1 .. scaleFactor
      - outputRow[i*x] := token
    - produce Out1:[[token : for int i 1 .. <scaleFactor>]] repeat <scaleFactor>
    - x++
action2:
  - when (x == imageWidth && rowRepeated < <scaleFactor>)
    - produce Out1:[outputRow] repeat (imageWidth*scaleFactor)
    - rowRepeated++
action3:
  - when (rowRepeated == <scaleFactor>)
    - rowRepeated := 0;
    - x := 0;
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
      -- inputPattern =
      --   C.InPattTagIdsRepeat
      --   (C.Ident "In1")
      --   [C.Ident "row"]
      --   (C.RptClause (mkInt imageWidth))
      -- outputPattern =
      --   C.OutPattTagIds
      --   (C.Ident "Out1")
      --   (map C.OutTokenExp
      --   (concat
      --    (replicate (fromInteger scaleFactor)
      --     (concatMap (\i -> replicate (fromInteger scaleFactor)
      --             (C.IndSExpCons (C.IndExpr (C.Ident "row") (C.BExp (mkInt i))))
      --          ) [0..(imageWidth-1)])
      --     )
      --    )
      --   )
      -- actionHead = C.ActnHead [inputPattern] [outputPattern]
      action1 =
        (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "consumeRow"])
                (C.ActnHeadGuarded
                [C.InPattTagIds
                 (C.Ident "In1")
                 [C.Ident "token"]]
                [C.OutPattTagIdsRepeat
                 (C.Ident "Out1")
                 [C.OutTokenExp
                  (C.LstExpCons
                   (C.ListComp (C.ListExpGen [mkVar "token"]
                   (C.GenExpr [C.GeneratorExpr (mkIntType 16) (C.Ident "i") (C.BEList (mkInt 1) (mkVar "scaleFactor"))])
                   )))]
                (C.RptClause (mkVar "scaleFactor"))
                ]
                  [C.BELT (mkVar "x") (mkVar "imageWidth")])
                [ C.EndSeparatedStmt
                  (C.ForEachStt
                   (C.ForeachStmtsSt
                   [C.ForeachGen (mkIntType 16) (C.Ident "i") (C.BEList (mkInt 0) (C.BENeg (mkVar "scaleFactor") (mkInt 1)))]
                   [C.SemiColonSeparatedStmt
                    (C.AssignStt
                     (C.AssStmtIdx
                      (C.Ident "outputRow")
                      (C.Idx [C.BExp (C.BEAdd (C.BEMult (mkVar "x") (mkVar "scaleFactor")) (mkVar "i"))])
                      (mkVar "token")))]))
                ,
                 C.SemiColonSeparatedStmt
                 (C.AssignStt (C.AssStmt (C.Ident "x") (C.BEAdd (mkVar "x") (mkInt 1))))

                ]))
      action2 =
        (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "repeatRowExpanded"])
                (C.ActnHeadGuarded
                [ ] -- no input pattern
                [C.OutPattTagIdsRepeat
                  (C.Ident "Out1")
                  [C.OutTokenExp (mkVar "outputRow")]
                  (C.RptClause (C.BEMult (mkVar "imageWidth") (mkVar "scaleFactor"))) 
                ]
                  [ C.BEEQ (mkVar "x") (mkVar "imageWidth")
                  , C.BELT (mkVar "rowRepeated") (mkVar "scaleFactor")
                  ])
                [ C.SemiColonSeparatedStmt
                 (C.AssignStt (C.AssStmt (C.Ident "rowRepeated") (C.BEAdd (mkVar "rowRepeated") (mkInt 1))))
                ]))

      action3 = 
        (C.AnActn
             (C.ActnTagsStmts
                (C.ActnTagDecl [C.Ident "reset"])
                (C.ActnHeadGuarded
                [ ] -- no input pattern
                [ ] -- no output pattern
                  [ C.BEEQ (mkVar "rowRepeated") (mkVar "scaleFactor")
                  ])
                [ C.SemiColonSeparatedStmt
                 (C.AssignStt (C.AssStmt (C.Ident "rowRepeated") (mkInt 1)))
                , C.SemiColonSeparatedStmt
                 (C.AssignStt (C.AssStmt (C.Ident "x") (mkInt 0)))
                ]))

  in C.Actr
       (C.PathN [C.PNameCons (C.Ident "cal")])
       []
       (C.Ident actorName)
       []
       ioSig
       (globalVars imageWidth scaleFactor)
       [C.ActionCode action1
       ,C.ActionCode action2
       ,C.ActionCode action3
       ]
       []

globalVars imageWidth scaleFactor =
    [ -- int x := 0;
      C.GlobVarDecl
      (C.VDeclExpMut
         (uintCalType 16)
         (C.Ident "x")
         []
         (mkInt 0))
    , C.GlobVarDecl
      (C.VDeclExpIMut
         (uintCalType 16)
         (C.Ident "imageWidth")
         []
         (mkInt imageWidth))
    , C.GlobVarDecl
      (C.VDecl
         (uintCalType 16)
         (C.Ident "outputRow")
         [C.BExp (C.BEMult (mkVar "imageWidth") (mkVar "scaleFactor"))])
    , C.GlobVarDecl
      (C.VDeclExpMut
         (uintCalType 16)
         (C.Ident "rowRepeated")
         []
        (mkInt 1))
    , C.GlobVarDecl
      (C.VDeclExpMut
         (uintCalType 16)
         (C.Ident "scaleFactor")
         []
        (mkInt scaleFactor))
    ]
      

{- the CAL code for the above actor will be (with a few values folded in):

actor actorName () int(size=16) In1 ==> int(size=16) Out1 :

	int x := 0;
	int imageWidth = 512;
	int(size=16) outputRow [ imageWidth * scaleFactor ];
	int rowRepeated := 1;
	int scaleFactor = 2;

	action1: action In1:[ token ] ==> Out1:[ [ token : for int i in 1 ..
		scaleFactor ] ] repeat scaleFactor
	guard (x < imageWidth)
	do foreach int i in 0 .. (scaleFactor - 1) do
             outputRow[(x * scaleFactor) + i] := token;
           end
           x := x + 1;
	end

	action2: action ==> Out1:[ outputRow ] repeat (imageWidth * scaleFactor)
	guard (x = imageWidth) , (rowRepeated < scaleFactor)
	do rowRepeated := rowRepeated + 1;
	end

	action3: action ==>
	guard (rowRepeated = scaleFactor)
	do rowRepeated := 1;
           x := 0;
	end
end
-}

-- this version uses a long output pattern, Verilog backend takes a long
-- time to compile long output pattern token vector lengths.
{-
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
        (map C.OutTokenExp
        (concat
         (replicate (fromInteger scaleFactor)
          (concatMap (\i -> replicate (fromInteger scaleFactor)
                  (C.IndSExpCons (C.IndExpr (C.Ident "row") (C.BExp (mkInt i))))
               ) [0..(imageWidth-1)])
          )
         )
        )
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
-}
