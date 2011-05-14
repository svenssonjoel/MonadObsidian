
module Obsidian.MonadObsidian.AbsC where


import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.Types



{- 
   about For constructor. 
   AbsC = initialising (variables) (int i = 0)  
   DExp = Boolean expression (loops as long as it is true)
   AbsC = for example i = i + 1  
   AbsC = the body of the loop
-} 
data AbsC = Skip
          | For AbsC DExp AbsC AbsC 
    --      | NewVar DExp DExp -- first DExp of very special shape 
          | AbsC :>> AbsC 
          | IfThenElse DExp AbsC AbsC
          | Synchronize
          | DExp := DExp 
    --      | SwapPtrs
            deriving Show

--infixl 9 :>> 


infixl 1 =:=
(=:=) :: Exp a -> Exp a -> AbsC 
(=:=) (E a) (E b) =  (:=) a b


forAbsC :: AbsC -> (Exp Bool) -> AbsC -> AbsC -> AbsC 
forAbsC a exp b c  = For a (unE exp) b c 

--declare :: Num (Exp a) => (Exp a) -> (Exp a) -> AbsC 
--declare exp1 exp2 = NewVar (unE exp1) (unE exp2)


type Code = IxExp -> AbsC


