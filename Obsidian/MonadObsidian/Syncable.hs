{-# OPTIONS -fglasgow-exts #-}

module Obsidian.MonadObsidian.Syncable where

import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.IC
import Obsidian.MonadObsidian.GPUMonad
import Obsidian.MonadObsidian.Arr
import Obsidian.MonadObsidian.Tools

-- import BasicAPI
import Obsidian.MonadObsidian.PureAPI


tid = variable "tid"

class Syncable a where 
    sync :: a -> GPU a
    commit :: a -> GPU a 
    
 
instance TypeOf (Exp a) => Syncable (GArr (Exp a)) where
    sync arr = do
      arr' <- commit arr
      write [Synchronize]
      return arr'
    commit arr = do 
      let n = len arr
      var  <- newGlobalArray (typeOf (arr ! 0))  n 
      write [(var,[unE tid]) ::= unE (arr ! tid)] 
      return $ mkArr (\ix -> index var ix) n

      

instance TypeOf (Exp a) => Syncable (SArr (Exp a)) where
    sync arr = do 
      arr' <- commit arr 
      write [Synchronize]
      return arr'
    commit arr = do 
      let n = len arr
      var  <- newSharedArray (typeOf (arr ! 0))  n 
      write [(var,[unE tid]) ::= unE (arr ! tid)] 
      return $ mkArr (\ix -> index var ix) n
  

instance (Syncable (Arr s a), Syncable (Arr s b)) => Syncable (Arr s (a,b)) where 
    sync arr = do
      arr' <- commit arr 
      write [Synchronize]
      return arr'      
    commit arr = do 
      (a1,a2) <- pure unzipp arr
      a1' <- commit a1
      a2' <- commit a2
      arr' <- pure zipp (a1',a2')
      return arr'


instance Syncable (Arr s a) => Syncable (Arr s [a]) where 
    sync arr = do
      arr' <- commit arr 
      write [Synchronize]
      return arr'      
    commit arr = do 
      xs <- pure guz arr
      xs' <- mapM commit xs
      arr' <- pure gz xs'
      return arr'
  

