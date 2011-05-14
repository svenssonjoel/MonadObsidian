{-# OPTIONS -XFlexibleContexts              
            -XTypeSynonymInstances #-}

module Test where

import Obsidian.MonadObsidian

import Prelude hiding (sum)

import Obsidian.MonadObsidian.IC
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.State

cache :: GArr a -> GPU (SArr a)
cache arr = return $ mkArray (\ix -> arr ! ix) (len arr)
 

wb :: SArr a -> GPU (GArr a)
wb arr  = return $ mkArray (\ix -> arr ! ix) (len arr)


kernel :: Syncable (GArr b) => 
          (SArr a -> GPU (SArr b)) -> GArr a -> GPU (GArr b)
kernel k = cache ->- k ->- wb ->- sync

sum :: SArr IntE -> GPU (SArr IntE)
sum arr | len arr == 1 = return arr 
        | otherwise    = (pure op ->- sync ->- sum) arr
  where
    op  = fmap (uncurry (+)) . pair



syncs :: SArr IntE -> GPU (SArr IntE) 
syncs arr = 
    do 
      a1' <- sync a1
      a2' <- sync a2 
      pure conc (a1',a2') 
    where (a1,a2) = halve arr 


syncs2 :: SArr IntE -> GPU (SArr IntE) 
syncs2 arr = 
    do 
      a1' <- (pure (fmap (+1)) ->- sync) a1
      a2' <- sync a2 
      pure conc (arr,conc (a1',a2')) 
    where (a1,a2) = halve arr 




--------------------------------------------------------------------------------
-- Sklansky
--------------------------------------------------------------------------------
--sklansky :: Int -> SArr IntE -> GPU (SArr IntE) 
--sklansky 0 = pure id
--sklansky n = two (sklansky (n-1)) ->- pure fan ->- sync
    
sklansky :: SArr IntE -> GPU (SArr IntE) 
sklansky arr | len arr == 1 = return arr
             | otherwise = (two sklansky ->- pure fan ->- sync) arr 


fan arr = conc (a1,a2')
    where 
      (a1,a2)= halve arr
      m = len a1
      c = a1 ! (fromIntegral (m-1))
      a2' = fmap (+c) a2
            


--scan_add_kernel :: GArr IntE -> GPU (GArr IntE )
--scan_add_kernel = cache ->- (sklansky 3)  ->- wb ->- sync

two f arr = do 
  (a1,a2) <- pure halve arr 
  a1' <- f a1
  a2' <- f a2 
  pure conc (a1',a2')

strange arr = two f arr
   where f _ = (pure (fmap (+1)) ->- sync) arr

