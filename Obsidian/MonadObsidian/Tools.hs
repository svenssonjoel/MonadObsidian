{-# LANGUAGE TypeSynonymInstances, 
             FlexibleInstances, 
             FlexibleContexts #-} 

module Obsidian.MonadObsidian.Tools where

import Control.Monad.State

import Obsidian.MonadObsidian.Exp
import Obsidian.MonadObsidian.IC
import Obsidian.MonadObsidian.Types
import Obsidian.MonadObsidian.GPUMonad
import Obsidian.MonadObsidian.Arr
import Obsidian.MonadObsidian.PureAPI

import qualified Data.Map as Map

type Dim = Int


--------------------------------------------------------------------------------
data SourceState = SourceState {st :: SymbolTable, count :: Int}

type Src a = State SourceState a 

updateSymT :: Name -> Type -> Int -> Src (Name,Int)
updateSymT name t len = do 
  s <- get 
  let c   = count s
      name' = name ++ show c
      st' = Map.insert name' (t,len) (st s)
  put (SourceState {st = st', count = (c + 1)} )
  return (name',len)

--------------------------------------------------------------------------------

class Sources' a where 
    mkSrc :: Name -> Dim -> Src a

instance Sources' (GArr IntE) where 
    mkSrc name x = do 
      (name',len') <- updateSymT name (Global_Array Int) x
      return $ mkArray (\ix -> index name' ix) (len') 


instance Sources' (GArr FloatE) where 
    mkSrc name x = do 
      (name',len') <- updateSymT name (Global_Array Float) x
      return $ mkArray (\ix -> index  name' ix) (len') 

    
instance (Sources' (GArr a), Sources' (GArr b)) => 
    Sources' (GArr (a,b)) where 
        
    mkSrc name x = do 
      a1 <- mkSrc name x 
      a2 <- mkSrc name x 
      return $ (fst . runGPU . (pure zipp)) (a1,a2)

--------------------------------------------------------------------------------


class Sources a where 
    mkT :: Name -> Dim -> SymbolTable -> (SymbolTable, a)

instance Sources (GArr IntE) where 
    mkT name x st = (
                       Map.insert name (Global_Array Int,x) st,
                       mkArray (\ix -> index  name ix) (x) 
                      )

instance Sources (GArr FloatE) where 
    mkT name x st = (
                       Map.insert name (Global_Array Float,x) st,
                       mkArray (\ix -> index name ix) (x) 
                      )

instance Sources (GArr BoolE) where 
    mkT name x st = (
                    Map.insert name (Global_Array Bool,x) st,
                    mkArray (\ix -> index name ix) (x) 
                   )

instance (Sources (GArr a), Sources (GArr b)) => Sources (GArr (a,b)) where 
    mkT name x st = (st'',
                    fst (runGPU ((pure zipp) (arr1,arr2)))
                   )
        where 
          (st' ,arr1) = mkT (name ++ "1") x st
          (st'',arr2) = mkT (name ++ "2") x st' 
      
instance  (Sources (GArr a), Sources (GArr b)) => Sources (GArr a,GArr b) where
    mkT name x st = (
                       st'', 
                       (arr1,arr2)
                      )
        where 
          (st',arr1) = mkT (name ++ "1") (x) st
          (st'',arr2) = mkT (name ++ "2") (x) st' 
      
--------------------------------------------------------------------------------
{- 
getState :: Sources a => (a -> W b) -> S
getState a = runWs (a t) st
    where 
      (st,t) = mkT "source" "n" empty
  

getIC :: Sources a => (a -> W b) -> IC
getIC a = snd (runW (a t)) (variable "tid") 
    where 
      (st,t) = mkT "source" "n" empty

getResult :: Sources a => (a -> W b) -> b
getResult a = fst (runW (a t)) 
    where 
      (st,t) = mkT "source" "n" empty

-}

initState = SourceState {st = Map.empty, count = 0}

getState :: Sources' a => (a -> GPU b) -> Int -> S
getState a n = runGPUs (a t) st'
    where 
      (t,srct) = runState (mkSrc "source" n) initState
      st' = st srct

getIC :: Sources' a => (a -> GPU b) -> Int -> IC
getIC a n = snd (runGPU (a t)) -- (variable "tid") 
    where 
      t = evalState (mkSrc "source" n) initState

getResult :: Sources' a => (a -> GPU b) -> Int -> b
getResult a n = fst (runGPU (a t)) 
    where 
      t = evalState (mkSrc "source" n) initState




--------------------------------------------------------------------------------


class Names a where 
    names :: a -> [Name]
 
instance Names (GArr (Exp a)) where 
    names arr = [name $ unE (arr ! (variable "tid"))]
        where name (Index x _) = x
              name _ = error "In Names (Garr (Exp a))"


instance Names (GArr (Exp a,Exp b)) where 
    names arr = [name $ unE $ fst (arr ! (variable "tid"))] ++
                [name $ unE $ snd (arr ! (variable "tid"))]
        where name (Index x _) = x
              name _ = error "In Names (Garr (Exp a, Exp b))"



{-
getExp :: ObsType a => (a -> W b) -> b
getExp a = (fst (runW (a array))) ! ix 
    where
      array = mkT "source"
      ix    = variable "tid" Int
      
-}
    

 

