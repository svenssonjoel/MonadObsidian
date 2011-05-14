{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts #-}

module Obsidian.MonadObsidian.PureAPI where 

import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.Arr
-- import Bitwise

--import Data.Bits

import Control.Monad

import Data.Foldable

--------------------------------------------------------------------------------
pure f a = return $ f a

--------------------------------------------------------------------------------
(??) b (x,y) = ifThenElse b x y 

intLog 1 = 0 
intLog n = 1 + (intLog (div n 2))

--------------------------------------------------------------------------------
{-
unriffle' :: Arr s a ->  Arr s a 
unriffle' arr | even (len arr) = mkArr (\ix -> arr ! (rotLocalL ix bits) ) n
                   where 
                    n = len arr
                    bits = fromIntegral $ intLog n 
unriffle' _ = error "unriffle' demands even length"
              
riffle' :: Arr s a ->  Arr s a 
riffle' arr | even (len arr) = mkArr (\ix -> arr ! (rotLocalR ix bits) ) n
                  where 
                    n = len arr
                    bits = fromIntegral $ intLog n
riffle' _ = error "riffle' demands even length"
  
-}
                
instance Foldable (Arr s) where 
    foldr f o arr = 
        Prelude.foldr f o [arr ! (fromIntegral ix) | ix <- [0.. (len arr - 1)]]

--------------------------------------------------------------------------------
flatten :: (Arr s (Arr s a)) -> Arr s a 
flatten arrs | len (arrs ! 0) == 1 = 
                 mkArr (\ix -> (arrs ! ix) ! 0) m
                     where 
                       k = len arrs
                       n = len (arrs ! 0)
                       m = k * n
flatten arrs = mkArr (\ix -> (arrs ! (ix `divi` fromIntegral k)) ! (ix `modi` fromIntegral n)) m
      where k = len arrs
            n = len (arrs ! 0)
            m = k*n


restruct :: Int -> Arr s a ->  (Arr s (Arr s a)) 
restruct n arr = mkArr (\ix -> inner ix) n 
    where
      m       = len arr
      k       = m `div` n 
      inner a = mkArr (\ix -> arr  ! ((a * fromIntegral k) + ix) ) k

--------------------------------------------------------------------------------


chopN :: Int -> Arr s a -> Arr s (Arr s a)
chopN n arr = 
    mkArr (\o -> (mkArr (\i -> arr ! ((o * (fromIntegral n)) + i)) n)) (fromIntegral m)
    where 
      m = (len arr) `div` n



nParts :: Int -> Arr s a -> Arr s (Arr s a)
nParts n arr = mkArr (\o -> (mkArr (\i -> arr ! (((fromIntegral m) * o) + i)) m)) (fromIntegral n)
    where 
      m = (len arr) `div` n


unChop :: Arr s (Arr s a) -> Arr s a 
unChop arr = mkArr (\ix -> let x = ix `modi` fromIntegral w
                               y = ix `divi` fromIntegral w
                           in (arr ! y) ! x) newLen
    where
      h      = len arr
      w      = len (arr ! 0)
      newLen = h * w


--------------------------------------------------------------------------------

rev :: Arr s a -> Arr s a 
rev arr  = mkArr ixf n
    where 
        ixf ix = arr ! (fromIntegral (n-1) - ix)
        n = len arr 


--------------------------------------------------------------------------------

split :: Int -> Arr s a -> (Arr s a,Arr s a)
split  m arr = 
    let n  = len arr
        h1 = mkArr (\ix -> arr ! ix)  m
        h2 = mkArr (\ix -> arr ! (ix + (fromIntegral m))) (n-m)
    in  (h1,h2)

halve :: Arr s a -> (Arr s a,Arr s a)
halve arr = split (len arr `div` 2) arr

conc :: Choice a => (Arr s a, Arr s a) -> Arr s a
conc (arr1,arr2) = 
    let (n,n') = (len arr1,len arr2)
    in mkArr (\ix -> ifThenElse (ix <* fromIntegral n) 
                                (arr1 !  ix)
                                (arr2 !  (ix - fromIntegral n))) (n+n')


{- 
   oeSplit. Odd Even Split. used to create 
   a more general riffle 
-}
oeSplit :: Arr s a -> (Arr s a, Arr s a)
oeSplit arr = 
    let n = len arr
        nhalf = div n 2
    in  (mkArray (\ix -> arr ! (2 * ix)) (n - nhalf),
         mkArray (\ix -> arr ! ((2 * ix) + 1)) nhalf) 


{- 
   shuffle, (use with caution) 
-}
shuffle :: Choice a => (Arr s a, Arr s a) -> (Arr s a)
shuffle (arr1,arr2) = 
    let (n1,n2) = (len arr1,len arr2)
    in  (mkArray (\ix -> ifThenElse ((modi ix 2) ==* 0) 
                               (arr1 ! (divi ix 2))
                               (arr2 ! (divi (ix - 1) 2)))
                 (n1 + n2))

--------------------------------------------------------------------------------

 
evens :: Choice a =>  ((a,a) -> (a,a)) -> Arr s a -> Arr s a
evens f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse ((modi ix 2) ==* 0)
                (ifThenElse ((ix + 1) <* (fromIntegral n))
                                (fst (f (arr ! ix,arr ! (ix + 1))))
                                (arr ! ix))
                (ifThenElse (ix <* (fromIntegral n))
                                (snd (f (arr ! (ix - 1),arr ! ix)))
                                (arr !  ix))) n

odds f arr = let (a1,a2) = split 1 arr 
             in  conc (a1,evens f a2)

--------------------------------------------------------------------------------
{-
pair :: Arr s a -> Arr (a,a) 
pair arr | odd (len arr) = error "Pair: Odd n"
         | otherwise = mkArr (\ix -> (arr ! (ix * 2),
                                      arr ! ((ix * 2) + 1))) nhalf
         where 
           n = len arr
           nhalf = div n 2
-}

pair :: Arr s a -> Arr s (a,a) 
pair arr | odd (len arr) = error "Pair: Odd n"
         | otherwise = mkArr (\ix -> (arr ! (ix * 2),
                                      arr ! ((ix * 2 ) + 1))) nhalf
         where 
           n = len arr
           nhalf = div n 2

{-
unpair :: Choice a => Arr (a,a) -> Arr s a
unpair arr = 
    let n = len arr
    in  mkArr (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                    (fst (arr ! (div ix 2)))
                    (snd (arr ! (div (ix-1) 2)))) (2*n)

-}
unpair :: Choice a => Arr s (a,a) -> Arr s a
unpair arr = 
    let n = len arr
    in  mkArr (\ix -> ifThenElse ((modi ix 2) ==* 0) 
                    (fst (arr ! (divi ix 2)))
                    (snd (arr ! (divi ix 2)))) (2*n)


pairwise :: Functor (Arr s) => (a -> a -> b) -> Arr s a -> Arr s b
pairwise f = fmap (uncurry f) . pair

--------------------------------------------------------------------------------

zipp :: (Arr s a, Arr s b) -> Arr s (a,b)
zipp (arr1,arr2) = mkArr (\ix -> (arr1 ! ix,arr2 ! ix)) n
    where n = min (len arr1) (len arr2)

unzipp :: Arr s (a,b) -> (Arr s a, Arr s b) 
unzipp arr =  (mkArr (\ix -> fst (arr ! ix)) n,
               mkArr (\ix -> snd (arr ! ix)) n)
    where n = len arr

--------------------------------------------------------------------------------

evens2 :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> Arr s a
evens2 f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse ((ix `modi` 2) ==* 0)
                    (fst (f (arr ! ix,arr ! (ix + 1))))
                    (snd (f (arr ! (ix - 1),arr ! ix)))) n


odds2 :: Choice a => ((a,a) -> (a,a)) -> Arr s a -> Arr s a
odds2 f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse (((ix - 1) `modi` 2) ==* 0)
                    (fst (f (arr ! ix,arr ! (ix + 1))))
                    (snd (f (arr ! (ix - 1),arr ! ix)))) n
                          

endS :: Int -> Arr s a -> Arr s a
endS n arr = mkArr (\ix -> arr ! (ix + (fromIntegral n)) ) nl
    where l = len arr 
          nl = l - n 


gz :: [Arr s a] -> (Arr s [a])
gz xs = 
    let n1 = len (head xs) -- deal with different lenghts ! 
    in mkArr (\ix -> map (! ix) xs) n1

--gz = undefined 

guz :: Arr s [a] -> [Arr s a] 
guz arr = 
    let n = len arr 
        m = length (arr ! (E (LitInt 0)))
    in [mkArr (\ix -> (arr ! ix) !! i) n | i <- [0..(m-1)]] 