module Obsidian.MonadObsidian.Printing where


import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.AbsC


import Control.Monad.State


{- PRINTING OF DEXPS -}

 
pprint (LitInt a) = show a 
pprint (LitFloat a) = show a
pprint (LitBool a) = show a
--pprint (Op2 Rem b c) = "remainderf(" ++ pprint b ++ "," ++ pprint c ++ ")"
pprint (Op2 Pow b c) = "pow((float)" ++ pprint b ++ "," ++ pprint c ++ ")"
pprint (Op2 Powi b c) = " (int)(pow((float)" ++ pprint b ++ "," ++ pprint c ++ "))"
pprint (Op2 Min  a b) = "((" ++ pprint a ++ " < " ++ pprint b ++ ") ? " ++ pprint a ++ " : " ++ pprint b ++ ")"
pprint (Op2 Max  a b) = "((" ++ pprint a ++ " > " ++ pprint b ++ ") ? " ++ pprint a ++ " : " ++ pprint b ++ ")"
pprint (Op2 a b c) = "(" ++ (pprint b) ++ (printOp2 a) ++ (pprint c) ++ ")"
pprint (Op1 a b) = "(" ++ printOp1 a b ++ ")"
pprint (If a b c ) = "(" ++  pprint a ++ " ?\n" ++ pprint b ++ " : \n" ++ pprint c ++ ")"
--pprint (Variable str) = str
pprint (Index a []) = a
pprint (Index a [b]) = a ++ "["++ pprint b ++"]"

printOp1 Not a   = " not " ++ pprint a
printOp1 Exp a   = " exp(" ++ pprint a ++ ")"
printOp1 Log a   = " log(" ++ pprint a ++ ")" 
printOp1 Log2i a = " ((int)log2f(" ++ pprint a ++ "))"
printOp1 Sin a   = " sin(" ++ pprint a ++ ")"
printOp1 Sqrt a  = " sqrt(" ++ pprint a ++ ")"
printOp1 Cos a   = " cos(" ++ pprint a ++ ")"
printOp1 Tan a   = " tan(" ++ pprint a ++ ")"
printOp1 ASin a  = " asin(" ++ pprint a ++ ")"
printOp1 ACos a  = " acos(" ++ pprint a ++ ")"
printOp1 ATan a  = " atan(" ++ pprint a ++ ")"
printOp1 ASinH a = " asinh(" ++ pprint a ++ ")"
printOp1 ACosH a = " acosh(" ++ pprint a ++ ")"
printOp1 ATanH a = " atanh(" ++ pprint a ++ ")"
printOp1 IntToFloat a = " __int2float_rn(" ++ pprint a ++ ")"
printOp1 FloatToInt a = " __float2int_rn(" ++ pprint a ++ ")" 


printOp2 Add = " + "
printOp2 Sub = " - "
printOp2 Div = " / "
printOp2 Mul = " * " 
printOp2 Mod = " % "
printOp2 And = " & "
printOp2 Or  = " | "
printOp2 Xor = " ^ "
printOp2 Lt  = " < "
printOp2 Gt  = " > "
printOp2 Leq = " <= "
printOp2 Geq = " >= "
printOp2 Eq  = " == "
printOp2 Shl = " << "
printOp2 Shr = " >> "


{- printing of AbsC -} 

printAbsC Skip = ";\n"
printAbsC (For init a inc body) = "for (" ++ strip_newline (printAbsC init) ++ 
                              pprint a ++ ";" ++ 
                              (strip_semi . strip_newline) (printAbsC inc) ++ "){\n" ++ printAbsC body ++ "\n}\n"
--printAbsC (NewVar (Variable name) val) = "Type_not_decided " ++ " " ++ name ++ " = " ++ pprint val ++ ";\n" 
--printAbsC (NewVar _ _) = error "incorrect use of NewVar"
printAbsC (a :>>  b) = printAbsC a ++ printAbsC b 
printAbsC Synchronize = "__syncthreads();\n"
printAbsC (a := b)  = pprint a ++ " = " ++ pprint b ++ ";\n"
printAbsC (IfThenElse a b c) = "if ("++pprint a++"){\n" ++ 
                               "  " ++ printAbsC b ++ "\n}\n" ++ 
                               "else {\n" ++ 
                               "  " ++ printAbsC c ++ "\n}\n"
--printAbsC SwapPtrs = " tmp = source;\n" ++
--                     " source = target;\n" ++
--                     " target = tmp;\n"

strip_newline str = if (head str' == '\n') then reverse (tail str')
                                           else str
    where str' = reverse str
strip_semi str = if (head str' == ';') then reverse (tail str')
                                       else str
    where str' = reverse str

printType Int = "int"
printType Float = "float"
printType Bool = "bool"
