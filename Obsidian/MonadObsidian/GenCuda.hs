{-# OPTIONS -fglasgow-exts #-}
-- -----------------------------------------------------------------------------

module Obsidian.MonadObsidian.GenCuda where 

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List
import Control.Monad.State

import Obsidian.MonadObsidian.Types

--import MyWriter
import Obsidian.MonadObsidian.GPUMonad
import Obsidian.MonadObsidian.Exp 

import Obsidian.MonadObsidian.IC

import Obsidian.MonadObsidian.MM

import Obsidian.MonadObsidian.Tools
import Obsidian.MonadObsidian.Printing
import Obsidian.MonadObsidian.Arr


import System.Directory
import System.FilePath
import System.Process


-- -----------------------------------------------------------------------------

type ICLive = [(Statement,Set.Set Name)]

-- -----------------------------------------------------------------------------

-- TODO: rediscover what I am doing here
liveness :: IC -> Set.Set Name -> ICLive
liveness ic names = reverse $ analyze (reverse ic) names 
    where 
      analyze [] _ = [] 
      analyze (x@((nom,_) ::= d2):xs) ns = 
          let ns' = Set.delete nom $ (ns `Set.union` getArrayNames d2 )
          in (x, (Set.insert nom ns')) : analyze xs ns'
      analyze (x:xs) ns = (x,ns) : analyze xs ns
                                 
      

getArrayNames :: DExp -> Set.Set Name
-- NOTE: Only add arrays to the set
getArrayNames (Index x [])  = Set.empty
getArrayNames (Index x ixs) = Set.singleton x
getArrayNames (Op2 _ a b)   = (getArrayNames a `Set.union` getArrayNames b)
getArrayNames (Op1 _ a)     = getArrayNames a
getArrayNames (If a b c)    = (getArrayNames a `Set.union` 
                               getArrayNames b `Set.union` 
                               getArrayNames c)
getArrayNames _ = Set.empty


-- -----------------------------------------------------------------------------

type ICT = [(Statement,Int)]
type MemoryMap =  Map.Map Name (Address,Type)  


sharedMem = newMem 
globalMem = newMem

threads :: Name -> SymbolTable -> Int
threads name table = case Map.lookup name table of 
                     Nothing -> error "error: threads"
                     Just a -> snd a
          
bytes :: Name -> SymbolTable -> Int 
bytes name table = case Map.lookup name table of 
                     Nothing -> error "error: bytes"
                     Just a -> (snd a) * sizeof (elemType (fst a))

typeof :: Name -> Map.Map Name (Type,a) -> Type
typeof name table = case Map.lookup name table of 
                      Nothing -> error ("error: typeof " ++ name)
                      Just a -> fst a

sizeof :: Type -> Int
sizeof Int     = 4
sizeof Float   = 4 
sizeof Bool    = 4 
sizeof _       = error "sizeof: only element types"

name :: DExp -> Name
name (Index x _) = x                               


ptrStr :: Type -> String
ptrStr t = typeStr t ++ " *" 

-- c type string
typeStr :: Type -> String
typeStr Float = "float"
typeStr Int   = "int"
typeStr Bool  = "int"
typeStr _     = error "typeStr: only element types"

-- printf specifier string 
pfSpecStr Float = "%f"
pfSpecStr Int   = "%d"
pfSpecStr Bool  = "%d"
pfSpecStr _     = error "pfSpecPtr: only element types"

-- -----------------------------------------------------------------------------

data MState = MState {shared :: Memory,
                      global :: Memory,
                      ct :: SymbolTable,
                      mm :: MemoryMap}

type GenState a  = State MState a  

-- -----------------------------------------------------------------------------
-- new version of genMemoryMap 

genMemoryMap :: ICLive -> Set.Set Name -> GenState (ICT,MemoryMap) 
genMemoryMap ic set = genMemoryMap' ic set [] 

genMemoryMap' :: ICLive -> Set.Set Name -> ICT -> GenState (ICT,MemoryMap) 
genMemoryMap' [] _ ict = 
    do  
      mstate <- get; 
      return (reverse ict,mm mstate) --REVERSE IT 
genMemoryMap' ((a@((nom,_) ::= d2),ns):xs) prev_names ict = 
    do
      freeDead prev_names ns -- updates memorymap
      allocFor nom           -- updates memorymap
      mstate <- get
      genMemoryMap' xs ns ((a,threads nom (ct mstate)):ict) 
genMemoryMap' ((x,ns):xs) prev_names ict =  
    do 
      freeDead prev_names ns
      genMemoryMap' xs ns ((x,0):ict) 


-- -----------------------------------------------------------------------------
freeDead :: Set.Set Name -> Set.Set Name -> GenState ()
freeDead prev curr = 
    do 
      mstate <- get 
      let (smem,gmem,nt,table) = (shared mstate, 
                                  global mstate, 
                                  mm mstate, 
                                  ct mstate)  
          (smem',gmem') = 
              freeAll smem gmem table nt 
                          (Set.toList (prev `Set.difference` curr))
      put (mstate {shared = smem', global = gmem'})
      return ()
    where 
      freeAll smem gmem _ _ [] = (smem,gmem)
      freeAll smem gmem ct nt (x:xs) = 
          if ("source" `isPrefixOf` x) 
          then freeAll smem gmem ct nt xs
          else freeAll smem' gmem' ct nt xs
          where 
            address = 
                case Map.lookup x nt of 
                  Nothing -> error $ "error: freeDead, "++ 
                                     show x ++ 
                                     "not found in nt"
                  Just a -> fst a
            (smem',gmem') = 
                case typeof x ct of  
                  Global_Array t -> (smem,free gmem address)
                  Shared_Array t -> (free smem address,gmem)
                  Constant_Array t -> error "Impossible to free constant arrays" 
          
-- -----------------------------------------------------------------------------

allocFor :: Name -> GenState () 
allocFor n1 = 
    do 
      mstate <- get
      let nt = mm mstate
          table = ct mstate
          (smem,gmem) = (shared mstate,global mstate)
          --(smem',gmem') = freeDead' smem gmem table nt prev_names ns 
          (smem',gmem',addr) = 
              case typeof n1 table of
                Global_Array _ -> 
                    let (m,a) = allocate gmem (bytes n1 table)
                    in  (smem,m,a)
                Shared_Array _ -> 
                    let (m,a) = allocate smem (bytes n1 table)
                    in  (m,gmem,a)
                Constant_Array _ -> 
                    error "Impossible to allocate constant arrays"
          nt' = Map.insert n1 (addr,typeof n1 table) nt
      put (mstate {shared = smem',global = gmem',mm = nt'})



-- -----------------------------------------------------------------------------

mm_IC :: ICT -> MemoryMap -> ICT
mm_IC [] _ = []
mm_IC (((nom,d1) ::= d2,i):stms) mm = 
    ((rename nom mm,d1) ::= (renameAll d2 mm),i):(mm_IC stms mm) 
--mm_IC ((Cond (E d1) j,i):stms) mm = 
--    ((Cond (E (renameAll d1 mm)) j),i):(mm_IC stms mm)
mm_IC ((x,i):stms) mm = (x,i):(mm_IC stms mm)

-- -----------------------------------------------------------------------------

rename :: Name -> MemoryMap -> Name
rename n mm = if (not ("source" `isPrefixOf` n)) 
                 then n' 
                 else n
    where 
      (j,at) = case Map.lookup n mm of 
               Nothing -> error $ "error: "++ n ++" not found in MemoryMap"  
               Just a -> a 
      n' = case at of 
             Global_Array t ->    
                 "(("++ptrStr t++")(gbase+"++ show j++"))"
             Shared_Array t ->    
                 "(("++ptrStr t++")(sbase+"++ show j++"))"
             Constant_Array t ->   
                 "(("++ptrStr t++")(cbase+"++ show j++"))"

renameAll a@(Index n []) mm = a
renameAll (Index n is) mm = 
    Index (rename n mm) (map (\x -> renameAll x mm) is)

renameAll (Op2 op d1 d2) mm = Op2 op (renameAll d1 mm) (renameAll d2 mm)
renameAll (Op1 op d1)    mm = Op1 op (renameAll d1 mm)
renameAll (If d1 d2 d3)  mm = If (renameAll d1 mm) 
                                 (renameAll d2 mm) 
                                 (renameAll d3 mm)
renameAll x _ = x


threadsIC :: ICT -> Int -> IC
threadsIC [] j = [] 
threadsIC ((d1 ::= d2,i):stms) j = 
    case (compare i j) of 
      LT -> (
             IfThen(E (Op2 Lt (Index "tid" []) (LitInt i))) 
                       [(d1 ::= d2)] 
                       
            ):(threadsIC stms j) 
      EQ -> (d1 ::= d2):(threadsIC stms j) 
      GT -> error "threadsIC: Impossible"
threadsIC ((x,i):stms) j   = x:(threadsIC stms j)
 


-- -----------------------------------------------------------------------------

kernelHead :: Name -> Env -> SymbolTable -> String
kernelHead kname env ct = header 
    where header  = "__global__ static void " ++ kname ++ "(" ++ 
                    inputs ++"char *gbase){\n" ++ 
                    sbase ++ tid ++ lengths
          tid     = "const int tid = threadIdx.x;\n"
          sbase   = "extern __shared__ char sbase[] __attribute__ ((aligned(4)));\n" 
          ls (x,y)= "const int " ++ x ++ 
                    " __attribute__ ((unused)) = " ++ show y ++ ";\n"
          lengths = unwords $ map ls env
          inputs  = unwords (map (\x -> (ptrStr ((elemType . typeof x) ct)) ++ x++",") sources)
          sources = filter (\x -> "source" `isPrefixOf` x) (Map.keys ct) 


genCudaKernel :: Name -> Env -> IC -> GenState String 
genCudaKernel name env ic = do 
  mstate <- get
  return $ (kernelHead name env (ct mstate)) ++ (printICKernel ic) ++ "\n}\n"


-- -----------------------------------------------------------------------------
-- test leading towards an "execute function" 


test :: (Input (Exp a), Sources' (GArr (Exp a))) => 
        (GArr (Exp a) -> GPU (GArr (Exp b))) -> [Exp a] -> String
test inp inputs = 
    let 
        -- generate C style array
        len = length inputs
        cInputs = strListToCArrayStr $ map render inputs
        
        -- names of the result arrays, used to read back result 
        -- the results are also alive when leaving the program
        nms = names $ getResult inp len
        res = Set.fromList nms   
        -- perform liveness analysis. 
        icl = liveness (getIC inp len) res 

        -- evaluate the symbolic table
        env = [("n0",len)]
        st  = symtab $ getState inp len
        concreteT  = st -- evalSymTab env st 
        ((ict,memMap),mstate) = 
            runState (genMemoryMap icl Set.empty) 
                         (MState sharedMem globalMem concreteT Map.empty)

        nthreads = maximum [snd x | x <- ict] -- number of threads needed 
        ic = mm_IC ict memMap 
    
        --ic' = (map fst ic) -- threadsIC ic nthreads
        ic' = threadsIC ic nthreads
        (kernel,mstate') = runState (genCudaKernel "generated" env ic') mstate
        inputType  = elemType $ typeof "source0" concreteT
        outputType = elemType $ typeof (head nms) st
        olen       = threads (head nms) concreteT
                     
        sharedMemoryNeeded = size (shared mstate') 
        globalMemoryNeeded = size (global mstate')

        result_pos = case Map.lookup (head nms) memMap of 
                       Nothing -> error "error"
                       Just a -> "(gbase + " ++ show (fst a) ++ ")"
        
        --generate code that launches kernel     
    in "/*\n" ++ "number of threads:" ++ show nthreads ++ "\n" ++ cInputs ++ "\n" ++
       "*/\n\n\n" ++ includes  ++ kernel ++  
       cmain (
             "  "++ typeStr inputType ++ " values[" ++ show len ++ "] = " ++ cInputs ++ ";\n" ++
             "  "++ typeStr outputType ++" result[" ++ show olen ++ "];\n" ++
             "  char *gbase;\n" ++ 
             "  " ++ typeStr inputType ++" * dvalues;\n" ++
             "  cudaMalloc((void**)&dvalues, sizeof("++typeStr inputType++") * "++ show len ++" ); \n" ++
             "  cudaMemcpy(dvalues, values, sizeof("++typeStr inputType++") * "++show len ++", cudaMemcpyHostToDevice);\n" ++
             "  cudaMalloc((void**)&gbase," ++ show globalMemoryNeeded ++ "); \n" ++
             "  " ++ runKernel "generated" nthreads sharedMemoryNeeded ++ 
             "  cudaMemcpy(result," ++ result_pos ++", sizeof("++typeStr outputType++") * "++ show olen ++" , cudaMemcpyDeviceToHost);\n" ++
             "  cudaFree(dvalues);\n" ++
             "  cudaFree(gbase);\n" ++
             "  for(int i = 0; i < " ++ show olen ++ "; i++){\n" ++ 
             "    printf(\"" ++ pfSpecStr outputType ++ " \",result[i]);\n" ++
             "  }\n" 
            ) 

-- -----------------------------------------------------------------------------       
includes = "#include <stdio.h>\n#include <stdlib.h>\n"
           
           
cmain str = "int main(int argc, char **argv){\n" ++ str ++ "\n}\n"
       

-- -----------------------------------------------------------------------------
runKernel :: Name -> Int -> Int -> String 
runKernel name threads sm = name ++ "<<<1, " ++ show threads ++ 
                            "," ++ show sm ++ ">>>(dvalues,gbase);\n"


-- -----------------------------------------------------------------------------
class Input a where 
    render :: a -> String 

instance Input (Exp Int) where 
    render (E (LitInt a)) = show a

instance Input (Exp Float) where 
    render (E (LitFloat a)) = show a
    render (E(Op2 Sub (LitFloat 0.0) (LitFloat a))) = "-"++show a
                                                     

class Output a where 
    scan :: String -> a 

instance Output (Exp Int) where 
    scan str = (E (LitInt (read str :: Int)))

instance Output (Exp Float) where 
    scan str = (E (LitFloat (read str :: Float)))



strListToCArrayStr :: [String] -> String 
strListToCArrayStr xs = '{':((concat $ Data.List.intersperse "," xs) ++ "}")

listToCArrayStr :: Show a => [a] -> String
listToCArrayStr xs = '{':((concat $ Data.List.intersperse "," (map show xs)) ++ "}")

-- -----------------------------------------------------------------------------

data ExecMode = EMU | GPU

execute :: (Input (Exp a) , Output (Exp b),Sources' (GArr (Exp a))) => 
               (GArr (Exp a) -> GPU (GArr (Exp b))) -> 
                   [Exp a] -> IO [Exp b] 
execute = execute' GPU

execute' :: (Input (Exp a) , Output (Exp b),Sources' (GArr (Exp a))) => 
           ExecMode -> 
               (GArr (Exp a) -> GPU (GArr (Exp b))) -> 
                   [Exp a] -> IO [Exp b] 
execute' _ _ []   = return []  -- ULGY FIX
execute' mode program list = exec
    where 
      exec = do 
        tmp_dir <- getTemporaryDirectory
        let fullpath = tmp_dir ++ pathSeparator : "GPU-HASKELL"
        createDirectoryIfMissing False fullpath
        -- genCudaProjectL n (div n threads) fullpath name program list
        writeFile (fullpath ++ (pathSeparator : name) ++ ".cu") 
                      (test program list) 
        writeFile (fullpath ++ (pathSeparator : "Makefile")) 
                      (genMakefile name)
        working_dir <- getCurrentDirectory 
        setCurrentDirectory fullpath
        pid1 <- case mode of 
                  GPU -> runCommand "make -s 2> messages.txt"
                  EMU -> runCommand "make emulation -s 2> messages.txt"
        waitForProcess pid1 -- make sure the executable is generated
        pid2 <- runCommand (fullpath ++ pathSeparator:name ++ " > output.txt")
        waitForProcess pid2 -- make sure output is generated 
        result <- readOutput (fullpath ++ pathSeparator:"output.txt" )
        setCurrentDirectory working_dir
        --removeDirectoryRecursive fullpath
        return result
      n = length list
      name = "generated"
      readOutput file = 
          do 
            string  <- readFile file 
            let strings = words string 
            return (map scan strings)
-- -----------------------------------------------------------------------------

    --"CUDA_INSTALL_PATH := /usr/local/cuda\n" ++       
    --"CUDA_SDK_PATH := /home/ian/NVIDIA_CUDA_SDK\n" ++ 

-- -----------------------------------------------------------------------------
-- genMakefile now uses env variables CUDA_INSTALL_PATH and 
--   CUDA_SDK_PATH(not used at the moment, maybe remove)

genMakefile :: Name -> String 
genMakefile name = 
    "TARGET := " ++ name ++  "\nCOMMON_PATH := $(CUDA_SDK_PATH)/common\n\n\
    \LIBPATHS := -L$(CUDA_INSTALL_PATH)/lib -L$(COMMON_PATH)/lib -L$(CUDA_\
    \SDK_PATH)/lib\nINCPATHS := -I$(CUDA_INSTALL_PATH)/include -I$(COMMON_\
    \PATH)/inc\nLIBRARIES := -lcuda -lcudart\n\nLIBRARIESEMU :\
    \=-lcudart\n\nNVCC := nvcc \n\nall: $(TARGET)\n$(TARGET): \
    \$(TARGET).cu\n\t$(NVCC) -o $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPA\
    \THS) $(LIBRARIES)\n\nemulation: $(TARGET).cu\n\t$(NVCC) -deviceemu -o\
    \ $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPATHS) $(LIBRARIESEMU)\n\ncl\
    \ean:\n\trm $(TARGET)\n" 
{-
genMakefile :: Name -> String 
genMakefile name = 
    "TARGET := " ++ name ++  "\nCOMMON_PATH := $(CUDA_SDK_PATH)/common\n\n\
    \LIBPATHS := -L$(CUDA_INSTALL_PATH)/lib -L$(COMMON_PATH)/lib -L$(CUDA_\
    \SDK_PATH)/lib\nINCPATHS := -I$(CUDA_INSTALL_PATH)/include -I$(COMMON_\
    \PATH)/inc\nLIBRARIES := -lcuda -lGL -lGLU\n\nLIBRARIESEMU :\
    \=-lGL -lGLU\n\nNVCC := nvcc \n\nall: $(TARGET)\n$(TARGET): \
    \$(TARGET).cu\n\t$(NVCC) -o $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPA\
    \THS) $(LIBRARIES)\n\nemulation: $(TARGET).cu\n\t$(NVCC) -deviceemu -o\
    \ $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPATHS) $(LIBRARIESEMU)\n\ncl\
    \ean:\n\trm $(TARGET)\n"
-}


-- -----------------------------------------------------------------------------
-- EXPERIMENT
-- -----------------------------------------------------------------------------
-- EXPERIMENT 
-- -----------------------------------------------------------------------------
  
executep :: (Input (Exp a) , 
             Input (Exp b) , 
             Output (Exp c),
             Output (Exp d), 
             Sources' (GArr (Exp a)),
             Sources' (GArr (Exp b))) => 
           ExecMode -> 
               (GArr (Exp a,Exp b) -> GPU (GArr (Exp c,Exp d))) -> 
                   [(Exp a,Exp b)] -> IO [(Exp c,Exp d)] 
executep _ _ []   = return []  -- ULGY FIX
executep mode program list = exec
    where 
      exec = do 
        tmp_dir <- getTemporaryDirectory
        let fullpath = tmp_dir ++ pathSeparator : "GPU-HASKELL"
        createDirectoryIfMissing False fullpath
        -- genCudaProjectL n (div n threads) fullpath name program list
        writeFile (fullpath ++ (pathSeparator : name) ++ ".cu") 
                      (test2 program list) 
        writeFile (fullpath ++ (pathSeparator : "Makefile")) 
                      (genMakefile name)
        working_dir <- getCurrentDirectory 
        setCurrentDirectory fullpath
        pid1 <- case mode of 
                  GPU -> runCommand "make -s"
                  EMU -> runCommand "make emulation -s"
        waitForProcess pid1 -- make sure the executable is generated
        pid2 <- runCommand (fullpath ++ pathSeparator:name ++ " > output.txt")
        waitForProcess pid2 -- make sure output is generated 
        result <- readOutput (fullpath ++ pathSeparator:"output.txt" )
        setCurrentDirectory working_dir
        --removeDirectoryRecursive fullpath
        return result
      n = length list
      name = "generated"
      --readOutput :: Name -> IO [(Exp b,Exp c)]
      readOutput file = 
          do 
            string  <- readFile file 
            let strings = words string
                [s1,s2] = splits "SEPARATOR" strings
                eb = map scan (s1)
                ec = map scan (s2) 
            return (zip eb ec)
          

splits sep xs = case eat sep xs [] of 
                  (s,[]) -> [s]
                  (s,ss) -> s : splits sep ss
    where 
      eat sep [] acc = (acc,[])
      eat sep (x:xs) acc | x == sep  = (acc,xs)
                         | otherwise = eat sep xs (acc ++ [x])
               

-- -----------------------------------------------------------------------------



test2 :: (Input (Exp a), 
         Input (Exp b),
         Sources' (GArr (Exp a)),
         Sources' (GArr (Exp b))) => 
        (GArr (Exp a,Exp b) -> GPU (GArr (Exp c,Exp d))) -> [(Exp a,Exp b)] -> String
test2 inp inputs = 
    let 
        -- generate C style array
        len = length inputs
        (i1,i2) = unzip inputs      
        cInputs1 = strListToCArrayStr $ map render i1
        cInputs2 = strListToCArrayStr $ map render i2
        -- names of the result arrays, used to read back result 
        -- the results are also alive when leaving the program
        --nms = names $ getResult inp
        [nm1,nm2] = names $ getResult inp len
        res = Set.fromList [nm1,nm2]   
        -- perform liveness analysis. 
        icl = liveness (getIC inp len) res 

        -- evaluate the symbolic table
        env = [("n0",len),("n1",len)]
        st  = symtab $ getState inp len
        concreteT  = st -- evalSymTab env st 
        ((ict,memMap),mstate) = 
            runState (genMemoryMap icl Set.empty) 
                         (MState sharedMem globalMem concreteT Map.empty)

        nthreads = maximum [snd x | x <- ict] -- number of threads needed 
        ic = mm_IC ict memMap 
    
        --ic' = (map fst ic) -- threadsIC ic nthreads
        ic' = threadsIC ic nthreads
        (kernel,mstate') = runState (genCudaKernel "generated" env ic') mstate
        inputType1  = elemType $ typeof "source0" concreteT
        inputType2  = elemType $ typeof "source1" concreteT
        outputType1 = elemType $ typeof nm1 st
        outputType2 = elemType $ typeof nm2 st
        olen       = threads nm1 concreteT
                     
        sharedMemoryNeeded = size (shared mstate') 
        globalMemoryNeeded = size (global mstate')

        result_pos1 = case Map.lookup nm1 memMap of 
                       Nothing -> error "error"
                       Just a -> "(gbase + " ++ show (fst a) ++ ")"
        result_pos2 = case Map.lookup nm2 memMap of 
                       Nothing -> error "error"
                       Just a -> "(gbase + " ++ show (fst a) ++ ")"


        
        --generate code that launches kernel     
    in "/*\n" ++ "number of threads:" ++ show nthreads ++ "\n" ++ 
       cInputs1 ++ "\n" ++
       cInputs2 ++ "\n" ++
       "*/\n\n\n" ++ includes  ++ kernel ++  
       cmain (
             "  "++ typeStr inputType1 ++ " values1[" ++ show len ++ "] = " ++ cInputs1 ++ ";\n" ++
             "  "++ typeStr inputType2 ++ " values2[" ++ show len ++ "] = " ++ cInputs2 ++ ";\n" ++
             "  "++ typeStr outputType1 ++ " result1[" ++ show olen ++ "];\n" ++
             "  "++ typeStr outputType2 ++ " result2[" ++ show olen ++ "];\n" ++
             "  char *gbase;\n" ++ 
             "  " ++typeStr inputType1 ++" * dvalues1;\n" ++
             "  " ++typeStr inputType2 ++" * dvalues2;\n" ++
             "  cudaMalloc((void**)&dvalues1, sizeof("++typeStr inputType1++") * "++ show len ++" ); \n" ++
             "  cudaMemcpy(dvalues1, values1, sizeof("++typeStr inputType1++") * "++ show len ++", cudaMemcpyHostToDevice);\n" ++
             "  cudaMalloc((void**)&dvalues2, sizeof("++typeStr inputType2++") * "++ show len ++" ); \n" ++
             "  cudaMemcpy(dvalues2, values2, sizeof("++typeStr inputType2++") * "++ show len ++", cudaMemcpyHostToDevice);\n" ++
             "  cudaMalloc((void**)&gbase," ++ show globalMemoryNeeded ++ "); \n" ++
             "  " ++ runKernelp "generated" nthreads sharedMemoryNeeded ++ 
             "  cudaMemcpy(result1," ++ result_pos1 ++", sizeof("++typeStr outputType1++") * "++ show olen ++" , cudaMemcpyDeviceToHost);\n" ++
             "  cudaMemcpy(result2," ++ result_pos2 ++", sizeof("++typeStr outputType2++") * "++ show olen ++" , cudaMemcpyDeviceToHost);\n" ++
             "  cudaFree(dvalues1);\n" ++
             "  cudaFree(dvalues2);\n" ++
             "  cudaFree(gbase);\n" ++
             "  for(int i = 0; i < " ++ show olen ++ "; i++){\n" ++ 
             "    printf(\"" ++ pfSpecStr outputType1 ++ " \",result1[i]);\n" ++
             "  }\n" ++
             "  printf(\"\\nSEPARATOR\\n\");\n" ++ 
             "  for(int i = 0; i < " ++ show olen ++ "; i++){\n" ++ 
             "    printf(\"" ++ pfSpecStr outputType1 ++ " \",result2[i]);\n" ++
             "  }\n" 

            ) 

-- -----------------------------------------------------------------------------       


runKernelp :: Name -> Int -> Int -> String 
runKernelp name threads sm = name ++ "<<<1, " ++ show threads ++ 
                            "," ++ show sm ++ ">>>(dvalues1,dvalues2,gbase);\n"




-- -----------------------------------------------------------------------------       


 