
module Obsidian.MonadObsidian.IC where 

import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.Types
import Obsidian.MonadObsidian.Printing 

--------------------------------------------------------------------------------

data Statement -- Skip 
        =  Synchronize 
        | (Name,[DExp]) ::= DExp 
        -- NOTE: Statements within IfThen can not contain Synchronize
        | IfThen BoolE [Statement] 
          deriving (Show,Eq)

type IC = [Statement] 

type GIC = IxExp -> IC
--------------------------------------------------------------------------------
printICKernel  [] = ""
printICKernel (x:xs) =  printStatementK x ++ printICKernel xs
printStatementK stm = printSt' stm 
    where 
   --   printSt' (Skip) = ";\n"
      printSt' (Synchronize) = "__syncthreads();\n "
      --printSt' ((d1 ::= d2)) = pprint d1 ++ " = " ++ pprint d2 ++";\n" 
      printSt' ((nom,[]) ::= d2) = nom ++ " = " ++ pprint d2 ++";\n" 
      printSt' ((nom,[d1]) ::= d2) = 
          nom ++ "[" ++ pprint d1 ++ "]" ++  " = " ++ pprint d2 ++";\n" 
  --    printSt' ((Cond e i))  = "if (" ++ pprint (unE e) ++ ") goto " ++ 
--                               "label" ++ show i ++ ";\n"
--      printSt' (Goto i)      = "goto label" ++ show i ++ ";\n" 
--      printSt' ((Label i ))  = "label" ++ show i ++ ": ;\n"  
      printSt' (IfThen e s1) = 
          "if (" ++ pprint (unE e) ++ "){\n" ++
          printICKernel s1 ++ "}\n"

--------------------------------------------------------------------------------

