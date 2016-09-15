module SkeletonTemplates.Parameters where

import AstMappings
import qualified AbsCAL as C
import qualified AbsRIPL as R
import Debug.Trace

parametersActor :: Integer -> Integer -> Integer -> Integer -> Integer -> C.Unit
parametersActor widthIn heightIn widthOut heightOut frames =
  let widthInDecl =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "IM_WIDTH_IN")
             (C.LitExpCons (C.IntLitExpr (C.IntegerLit widthIn))))
      heightInDecl =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "IM_HEIGHT_IN")
             (C.LitExpCons (C.IntLitExpr (C.IntegerLit heightIn))))
      widthOutDecl =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "IM_WIDTH_OUT")
             (C.LitExpCons (C.IntLitExpr (C.IntegerLit widthOut))))
      heightOutDecl =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "IM_HEIGHT_OUT")
             (C.LitExpCons (C.IntLitExpr (C.IntegerLit heightOut))))
      endOfFrame =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "END_OF_FRAME")
             (identCalExp "0x100"))
      endOfFrames =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "END_OF_FRAMES")
             (identCalExp "0x101"))
      numFrames =
        C.UConstVarDecl
          (C.ConstVarDecl
             (C.TypParam
                C.TUint
                [ C.TypeAttrSizeDf
                    (C.LitExpCons (C.IntLitExpr (C.IntegerLit 16)))
                ])
             (C.Ident "FRAMES")
             (C.LitExpCons (C.IntLitExpr (C.IntegerLit frames))))
  in C.Unt
       (C.Ident "std.headers")
       []
       (C.Ident "Parameters")
       [ widthInDecl
       , heightInDecl
       , heightOutDecl
       , widthOutDecl
       , endOfFrame
       , endOfFrames
       , numFrames
       ]
