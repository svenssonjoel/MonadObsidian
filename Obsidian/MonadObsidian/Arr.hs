{-# LANGUAGE FlexibleInstances #-} 
module  Obsidian.MonadObsidian.Arr
       ( Arr(Arr),
         GArr, SArr, CArr,
         Global,
         Shared,
         Constant,
         singleton,
         (!),
         mkArray,
         mkArr,
         len, 
        ) where

import Obsidian.MonadObsidian.AbsC
import Obsidian.MonadObsidian.Exp 
import Obsidian.MonadObsidian.GPUMonad

-- -----------------------------------------------------------------------------

data Global = Global deriving Show
data Shared = Shared deriving Show
data Constant = Constant deriving Show           

data Arr s a = Arr (IxExp ->  a, Int)

type GArr a = Arr Global a
type SArr a = Arr Shared a 
type CArr a = Arr Constant a 


instance Show a => Show (IndexE -> a) where 
    show a = show (a (variable "tid"))

instance Show a => Show (Arr s a) where 
    show (Arr (f,i)) = show f ++ " " ++ show i

instance Eq a => Eq (IndexE -> a) where 
    (==) a b = (a (variable "tid")) == (b (variable "tid"))

(!) :: Arr s a-> IndexE -> a
(!) (Arr (ixf,_)) ix = ixf ix

len (Arr (_,n)) = n

mkArray :: (IndexE -> a) -> Int -> (Arr s a)
mkArray ixf n = Arr (ixf,n)  

-- TODO: remove mkArray 
mkArr :: (IndexE -> a) -> Int -> (Arr s a)
mkArr ixf n = Arr (ixf,n)  

singleton a = Arr (\ix -> a, 1)

-- -----------------------------------------------------------------------------
