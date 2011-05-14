module Obsidian.MonadObsidian.GPUMonad where


import Obsidian.MonadObsidian.IC
import Obsidian.MonadObsidian.Exp 
import Obsidian.MonadObsidian.Types



import Control.Monad.State
import Control.Monad.Writer

import Data.Map

type SymbolTable = Map Name (Type,Int)

data S = S { arrayId :: Int,
             symtab   :: SymbolTable }
       deriving Show

--------------------------------------------------------------------------------
type GPU a = StateT S (Writer IC) a 


runGPU :: GPU a -> (a, IC) 
runGPU a = runWriter (evalStateT a (S 0 empty))

runGPUfs :: GPU a -> ((a,S),IC) 
runGPUfs a = runWriter (runStateT a (S 0 empty))

runGPUc :: GPU a -> IC
runGPUc a = snd $ runGPUfs a

runGPUs a st = snd $ fst $ runWriter (runStateT a (S 0 st)) 

--------------------------------------------------------------------------------

write :: IC -> GPU ()
write c = lift (tell c)


local :: GPU a -> GPU (a,IC)
local a =
  do s <- get
     let ((b,s'),c) = runWriter (runStateT a (S 0 (symtab s))) 
     put s'
     return (b,c)

codeOfNew :: GPU a -> GPU IC
codeOfNew a = do
  (_,c) <- local a
  return c

{- 
newVar :: Type -> GPU (Exp a)
newVar t = do 
  st <- get
  let sb  = symtab st
      i   = scalarId st
      id  = "var" ++ show i
      sb' = insert id (t,1) sb
  put (st {symtab = sb', scalarId = (i + 1)})
  return (variable id)


newLabel :: GPU (Int)
newLabel = do 
  st <- get
  let i = label st
  put (st {label = (i+1)})
  return i
-}

newSharedArray :: Type -> Int -> GPU Name
newSharedArray t len = do 
  st <- get
  let sb  = symtab st
      i   = arrayId st
      id  = "arr" ++ show i
      sb' = insert id (Shared_Array t,len) sb
  put (st {symtab = sb', arrayId = (i + 1)})
  return id

newGlobalArray :: Type -> Int -> GPU Name
newGlobalArray t len = do 
  st <- get
  let sb  = symtab st
      i   = arrayId st
      id  = "arr" ++ show i
      sb' = insert id (Global_Array t,len) sb
  put (st {symtab = sb', arrayId = (i + 1)})
  return id


