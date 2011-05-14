{-

  MM module. 
  manage memory in Shared and Device Memory.


  Todo: 
    --DONE-- Maintain the Top-of-memory pointer correctly. 
                - Currently it is ever increasing.
    --DONE-- Fuse consecutive free areas into a single free area. 
    Look at corner cases ! 
      - this code is a mess
    Improve memory allocation. 
      - the knowledge to give optimal usage exists 
        but is not exploited. ( requires looking at future allocations
        thus making the algorithm more involved ) 



     
   

-}

module  Obsidian.MonadObsidian.MM 
      (allocate,free,newMem,Memory,Address,Bytes,size) where

import Obsidian.MonadObsidian.Exp

import Data.List

type Address = Int
type Bytes  = Int

data Memory = Memory {freeList  :: [(Address,Bytes)],
                      allocated :: [(Address,Bytes)],
                      size      :: Bytes} --top :: Address 
            deriving Show

memMax :: Int 
memMax    = maxBound

newMem = Memory [(0,memMax)] [] 0

updateMax :: Memory -> Memory 
updateMax mem = let m = maximum [a+b|(a,b) <- allocated mem]
                    m' = max m (size mem)
                in mem {size = m'}
                   
allocate :: Memory -> Bytes -> (Memory,Address)
allocate m b = let adress = filter (\(x,y) -> y >= b) (freeList m) -- get a list of candidates
                   getTop mem = let (a,b)  = case null (allocated m) of 
                                              False -> maximum $ sort (allocated m) 
                                              True  -> (0,0)
                                in a+b
               in case adress of 
                    -- use the first candidate (try better approaches 
                    --  such as searching for best match, so that not to waste memory)
                    ((a,bytes):_)  -> let fl = filter (\(addr,_) -> a /= addr) (freeList m)
                                          fl' = if b < bytes then (a+b,bytes-b):fl
                                                else fl
                                      in  (updateMax (m {freeList = fl', 
                                                         allocated = (a,b):allocated m}) ,a)



free :: Memory -> Address -> Memory
free m a = mem 
    where 
      bytes = lookup a (allocated m)
      al    = filter (\(addr,_) -> a /= addr) (allocated m)
      compress = merge . sort 
      merge [] = [] 
      merge [x] = [x]
      merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                                              else (x,b):merge((y,b2):xs)
      mem   = case bytes of 
                Nothing -> error $ "error: Address " ++ show a ++ " not found in free list"
                Just b -> m {freeList = compress ((a,b):(freeList m)),
                             allocated = al}


compress = merge . sort 
merge [] = [] 
merge [x] = [x]
merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                           else (x,b):merge((y,b2):xs)


free_old :: Memory -> Address -> Memory
free_old m a = mem 
    where 
      bytes = lookup a (allocated m)
      al    = filter (\(addr,_) -> a /= addr) (allocated m)
      mem   = case bytes of 
                Nothing -> error $ "error: Address " ++ show a ++ " not found in free list"
                Just b -> m {freeList = (a,b):(freeList m),
                             allocated = al}