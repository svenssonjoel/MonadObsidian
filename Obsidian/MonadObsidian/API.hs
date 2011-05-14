{-# LANGUAGE FlexibleContexts #-} 

module  Obsidian.MonadObsidian.API 
        ((->-), 
         (->>-), 
         threadID,
         idM, riffle, unriffle,
         -- oeSplit ,
         -- shuffle,
         rep,   --
         iter,  --
         iter2,
         fmap,
         cache,
         wb,
        
         -- two, ilv, parl,
         -- evens, evens2, odds,
         riffevens2,
         -- initS, endS,
         sort2, cmpSwap, swap,
         mini,maxi
--       module BasicAPI
         ) where

import Obsidian.MonadObsidian.PureAPI
import Obsidian.MonadObsidian.Exp 
import Obsidian.MonadObsidian.Arr
import Obsidian.MonadObsidian.AbsC
import Obsidian.MonadObsidian.Syncable
import Obsidian.MonadObsidian.GPUMonad

import Control.Monad

import Obsidian.MonadObsidian.Tools

-- helper
fromInt = fromInteger . toInteger

-- binding power

instance Functor (Arr s) where 
    fmap f (Arr (ixf,n)) = Arr (f . ixf, n)

infixl 8 ->>-


(->-) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(->-) = (>=>)

cache :: GArr a -> GPU (SArr a)
cache arr = return $ mkArray (\ix -> arr ! ix) (len arr)
 

wb :: SArr a -> GPU (GArr a)
wb arr  = return $ mkArray (\ix -> arr ! ix) (len arr)



threads_maximum = 1024

threadID :: CArr (Exp Int)
threadID = mkArray (\ix -> ix) threads_maximum

-- pure f a = return $ f a

{- 
evens :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> GPU (Arr s a) 
evens f arr = 
    let n = len arr 
    in  return $ mkArray (\ix -> ifThenElse ((modi ix 2) ==* 0)
                          (ifThenElse ((ix + 1) <* (fromInt n))
                                          (fst (f (arr ! ix,arr ! (ix + 1))))
                                          (arr ! ix))
                          (ifThenElse (ix <* (fromInt n))
                                          (snd (f (arr ! (ix - 1),arr ! ix)))

                                          (arr !  ix))) n
-}
-- requires even length, or will produce faulty answers
evens2 :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> GPU (Arr s a) 
evens2 f arr = 
    let n = len arr 
        nhalf = (n `div` 2)
    in  return $ mkArray (\ix -> ifThenElse ((ix `modi` 2) ==* 0)
                                 (fst (f (arr ! ix,arr ! (ix + 1))))
                                 (snd (f (arr ! (ix - 1),arr ! ix)))) n
                          


riffevens2 :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> GPU (Arr s a) 
riffevens2 f arr = 
    let n = len arr 
    in  return $ mkArray (\ix -> ifThenElse ((ix `modi` 2) ==* 0 )
                                 (fst (f (arr ! ix,arr ! ((ix + 1) * 2))))
                                 (fst (f (arr ! (ix `divi` 2),arr ! ix)))) n



                    
{-
odds :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> GPU (Arr s a) 
odds f = endS 1 (pure (evens f))
-} 
{- 
   endS, applies a program to an end segment of an array
   Todo: 
     #2:  look into the type. parhaps its possible to 
          make it more beautiful.
-}

endS :: Choice a => Int -> (Arr s a -> GPU (Arr s a)) -> 
                              Arr s a -> GPU (Arr s a)
endS m p arr =  
    do 
      let n = len arr
      let (a,b) = split m arr
      (arr1,_) <- local (idM a) 
      (arr2,_) <- local (p b) 
      return $ mkArray ((\ix -> ifThenElse (ix <* (fromInt m))
                                (arr1 ! ix) 
                                (arr2 ! (ix - (fromInt m))))) n


{-
  initS, applies a program to a initial segment of an array
-}
initS :: Choice a => Int -> (Arr s a -> GPU (Arr s a)) -> 
                               Arr s a -> GPU (Arr s a)
initS m p arr = 
     do 
       let n = len arr
       let (a,b) = split m arr
       (arr1,_) <- local (p a) 
       (arr2,_) <- local (idM b) 
       return $ mkArray ((\ix -> ifThenElse (ix <* (fromInt m))
                                 (arr1 ! ix) 
                                 (arr2 ! (ix - (fromInt m))))) n
    
-- Longest Even length Initial Segment      
leis :: Choice a => (Arr s a -> GPU (Arr s a)) -> 
                     Arr s a -> GPU (Arr s a)   
leis p arr =
    let n = len arr
        l = if (mod n 2 == 0)
            then n
            else (n-1)
    in initS l p arr


{-
parl :: Choice b => (Arr a -> W (Arr b)) -> 
                    (Arr a -> W (Arr b)) -> 
                     Arr a -> W (Arr b)
parl p1 p2 arr = 
    do 
      (a,b) <- halve arr
      (arr1@(ixf,_),c1)  <- local (p1 a) 
      (arr2@(ixf',_),c2) <- local (p2 b)
      --INSPECT C1 and C2 
      let inspect = (c1 (variable "X" Int),c2 (variable "X" Int))
          n  = len arr1
          n' = len arr2
      case inspect of 
        ([Skip],[Skip]) -> return $ mkArray ((\ix -> ifThenElse (ix <* n)
                                                     (ixf  ix) 
                                                     (ixf' (ix - n)))) (n+n')
        (_,_)       ->  do write (\ix -> [IfThenElse (unE (ix <* n)) (c1 ix) (c2 (ix - n))] 
                           return $ mkArray ((\ix -> ifThenElse (ix <* n)
                                                     (ixf  ix) 
                                                    (ixf' (ix - n)))) (n+n')  
-}
{- 
   The Following functions are an attempt to 
   enable working with Odd lengts (in riffles etc)
-}


                 
riffle :: Choice a => Arr s a ->  GPU (Arr s a) 
riffle = pure (shuffle . halve)  

unriffle :: Choice a => Arr s a -> GPU (Arr s a)
unriffle = pure (conc . oeSplit)

--ilv :: Program (Exp a) (Exp b) -> Program (Exp a) (Exp b)
--ilv f = unriffle ->- two f ->- riffle


idM :: a -> GPU a
idM a = return a

{- 
   CMPSWAP AND SWAP, Used in sorters  
-}
cmpSwap :: Choice (a,a) => (a -> a -> Exp Bool) -> (a,a) -> (a,a)
cmpSwap op (a,b) = ifThenElse (op a b) (a,b) (b,a)
 
mini (x,y) = ifThenElse (x <* y) x y
maxi (x,y) = ifThenElse (x >* y) x y


swap (a,b) = (b,a)
     
{- version of riffle limited to even length arrays -}
riffle2 :: Choice a => Arr s a -> GPU (Arr s a)
riffle2 = leis$ pure (unpair . zipp . halve)

{- version of unriffle limited to even length arrays -}
unriffle2 :: Choice a => Arr s a -> GPU (Arr s a) 
unriffle2 = leis$ pure (conc . unzipp . pair)

{- sort2 operates on the longest even length initial segment -}
sort2 :: Arr s (Exp a) -> GPU (Arr s (Exp a))
sort2 = leis$ pure (unpair . fmap (cmpSwap (<*)) . pair)

--swapPairs :: Choice a => Program a a
swapPairs :: Choice a => Arr s a ->  GPU (Arr s a) 
swapPairs = leis (pure (unpair .  fmap swap . pair))

--two :: Choice b =>  (Arr a -> W (Arr b)) ->  Arr a -> W (Arr b)
--two f = parl f f


{- version of ilv limited to even length arrays -}
--ilv2 :: Program (Exp a) (Exp b) -> Program (Exp a) (Exp b)
--ilv2 f = unriffle2 ->- two f ->- riffle2
    

-- EXPERIMENTAL.. 

(->>-) a b = a ->- sync ->- b




iter :: Int -> (Arr s a -> GPU (Arr s a)) -> Arr s a -> GPU(Arr s a)
iter 0 f arr = return arr
iter n f arr = do arr' <- f arr 
                  iter (n-1) f arr'


iter2 :: Monad m => Int -> (a -> m a) -> a -> m a
iter2 0 f a = return a
iter2 n f a = do a' <- f a 
                 iter2 (n-1) f a'
   
             


rep = iter