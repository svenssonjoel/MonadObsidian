{-# OPTIONS -fglasgow-exts #-}

module Obsidian.MonadObsidian.Exp where 

import Obsidian.MonadObsidian.Types

import Data.List

data Type = Int | Bool | Float 
          | Shared_Array Type
          | Global_Array Type
          | Constant_Array Type
            deriving(Eq,Show)

elemType :: Type -> Type
elemType (Shared_Array t)   = t
elemType (Global_Array t)   = t
elemType (Constant_Array t) = t
elemType _ = error "elemType: Not an array type"




data DExp 
   = LitInt Int
   | LitBool Bool
   | LitFloat Float 
   | Op2 Op2 DExp DExp 
   | Op1 Op1 DExp 
   | If DExp DExp DExp 
   | Index Name [DExp]
     deriving(Eq,Show,Read)
 
instance Show (Exp a) where 
    show (E (LitInt a)) = show a
    show (E (LitFloat a)) = show a
    show (E (LitBool a)) = if a then "1" else "0"
    show (E a) = "E " ++ show a



data Op1 = Not
         | Exp | Log | Sqrt
         | Log2i
         | Cos | Sin | Tan
         | CosH | SinH | TanH
         | ACos | ASin | ATan
         | ACosH | ASinH | ATanH
         | IntToFloat | FloatToInt
           deriving(Eq,Show,Read)


data Op2 = Add | Sub | Div | Mul | Mod | Pow | Powi
         | And | Or | Xor
         | Lt | Leq | Gt | Geq | Eq 
         | Shl | Shr 
         | Min | Max
           deriving(Eq,Show,Read)

data  Exp a = E DExp 
              deriving(Eq,Read)


-- easy to remember types for expressions
type IxExp  = Exp Int
type IndexE  = Exp Int
type IntE   = Exp Int
type FloatE = Exp Float 
type BoolE  = Exp Bool

 
{- LIFTS ETC-} 

typ1 f (E a) = E (f a)
typ2 f (E a) (E b) = E (f a b)
typ3 f (E a) (E b) (E c) = E (f a b c)

unE (E a) = a

{- CONVERSIONS -}

i2f :: Exp Int -> Exp Float 
i2f = typ1 (Op1 IntToFloat)

f2i :: Exp Float -> Exp Int 
f2i = typ1 (Op1 FloatToInt)


{- INTEGER OPERATIONS -}
divi,modi :: Exp Int -> Exp Int -> Exp Int
divi = typ2 (Op2 Div)
modi = typ2 (Op2 Mod)
powi :: Exp Int -> Exp Int -> Exp Int
powi = typ2 (Op2 Powi)


log2i :: Exp Int -> Exp Int 
log2i = typ1 (Op1 Log2i)

shl,shr :: Exp Int -> Exp Int -> Exp Int
shl = typ2 (Op2 Shl)
shr = typ2 (Op2 Shr)


{- Logical ops -}
andi,ori,xori :: Exp Int -> Exp Int -> Exp Int
andi = typ2 (Op2 And)
ori  = typ2 (Op2 Or)
xori = typ2 (Op2 Xor)

invi :: Exp Int -> Exp Int 
invi = typ1 (Op1 Not)



andE = typ2 (Op2 And)
orE  = typ2 (Op2 Or)

true_ = E (LitBool True)
false_ = E (LitBool False)


{- ARRAY OPERATIONS -}

index :: Name -> IxExp -> Exp a
index nom ix = E (Index nom [unE ix])

variable name  = E (Index name []) -- E (Variable name)


{- Things with Boolean result -}

infixl 8 <*,<=*,>*,>=*
infixl 9 ==*

(<*),(<=*),(>*),(>=*),(==*) :: Exp a -> Exp a -> Exp Bool
(<*) a b  = typ2 (Op2 Lt) a b
(<=*) a b  = typ2 (Op2 Leq) a b
(>*)  = typ2 (Op2 Gt)
(>=*) = typ2 (Op2 Geq)
(==*) = typ2 (Op2 Eq)

neg :: Exp Bool -> Exp Bool
neg = typ1 (Op1 Not)

{- MINIMUM -}
-- Usually in the Ord class but since the Ord class 
-- has all the <,>,>= etc with the fixed Bool result
-- I dont want to touch it 

--minE :: Exp a -> Exp a -> Exp a -- called minE for (min on Exps)
--minE = typ2 (Op2 Min)



  -- ifThenElse (a <=* b) a b


{- CLASSES RELATED TO EXP -} 

class Choice a where 
    ifThenElse :: Exp Bool -> a -> a -> a

{- INSTANCE DECLARATIONS -}

instance Ord (Exp Int) where 
    compare = error "compare not implemented"
    min = typ2 (Op2 Min)
    max = typ2 (Op2 Max)

instance Ord (Exp Float) where 
    compare = error "compare not implemented"
    min = typ2 (Op2 Min)
    max = typ2 (Op2 Max)

instance Num (Exp Int) where

    (+) (E (LitInt 0)) x = x      
    (+) x (E (LitInt 0)) = x 
    (+) a@(E (Op2 Sub x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Add) a b
    (+) x y = typ2 (Op2 Add) x y 

    (-) x (E (LitInt 0)) = x
    (-) a@(E (Op2 Add x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Sub)a b
    (-) x y = typ2 (Op2 Sub) x y 

    (*) = typ2 (Op2 Mul)

    negate a = 0 - a
    signum a = error "not implemented"
    abs a    = error "not implemented"
    fromInteger = (E . LitInt) . fromInteger . toInteger

instance Num (Exp Float) where 
    (+) = typ2 (Op2 Add)
    (-) = typ2 (Op2 Sub)
    (*) = typ2 (Op2 Mul)
    negate a = 0.0 - a
    signum a = error "not implemented"
    abs a    = error "not implemented"
    fromInteger = i2f . fromInteger


instance Fractional (Exp Float) where
    (/) = typ2 (Op2 Div)
    recip a = 1.0 / a
    fromRational = (E . LitFloat) . fromRational

instance Floating (Exp Float) where 
    pi    = (E . LitFloat) pi
    exp   = typ1 (Op1 Exp)
    log   = typ1 (Op1 Log)
    sqrt  = typ1 (Op1 Sqrt)
    (**)  = typ2 (Op2 Pow)
    sin   = typ1 (Op1 Sin)
    cos   = typ1 (Op1 Cos)
    tan   = typ1 (Op1 Tan)
    sinh  = typ1 (Op1 SinH)
    cosh  = typ1 (Op1 CosH)
    tanh  = typ1 (Op1 TanH)
    asin  = typ1 (Op1 ASin)
    acos  = typ1 (Op1 ACos)
    atan  = typ1 (Op1 ATan)
    asinh = typ1 (Op1 ASinH)
    acosh = typ1 (Op1 ACosH)
    atanh = typ1 (Op1 ATanH)


instance Enum (IntE) where 
    succ (E (LitInt a)) =  E (LitInt ((toEnum . (+1) . fromEnum) a))
    succ _ = error "Enum"
    pred (E (LitInt a)) = E (LitInt ((toEnum . (subtract 1) . fromEnum) a)) 
    pred _ = error "Enum"
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x y z = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    fromEnum (E (LitInt a)) = a
    toEnum a = (E (LitInt a))


instance Enum (FloatE) where 
    succ (E (LitFloat a)) =  E (LitFloat (succ a))
    succ _ = error "Enum"
    pred (E (LitFloat a)) = E (LitFloat (pred a)) 
    pred _ = error "Enum"
    enumFrom (E (LitFloat x)) =  map toEnum [fromEnum x ..]
    enumFromTo (E (LitFloat x)) (E (LitFloat  y))   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo (E (LitFloat x)) (E (LitFloat y)) (E (LitFloat z)) = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    fromEnum (E (LitFloat a)) = (fromInteger . truncate) a
    toEnum a = (E (LitFloat (toEnum a)))



instance Choice (Exp a) where 
    ifThenElse (E (LitBool True)) e1 e2  = e1
    ifThenElse (E (LitBool False)) e1 e2  = e2
    ifThenElse b e1 e2 = typ3 If b e1 e2

instance Choice (Exp a,Exp b) where 
    ifThenElse a (b,b') (c,c') = (typ3 If a b c ,typ3 If a b' c')

instance Choice (Exp a,Exp b,Exp c) where 
    ifThenElse a (b,b',b'') (c,c',c'') = 
        ( typ3 If a b c, typ3 If a b' c', typ3 If a b'' c'')



caseof a [(v,x)]    = x
caseof a ((v,x):xs) = ifThenElse (a ==* v) x 
                        (caseof a xs)



{- Limited Evaluator -} 

type Env = [(Name,Int)]


-- Evaluate an IxExp, 
-- evaluate the length of an array for example. 
evalIxExp :: Env -> IxExp -> Int
evalIxExp env (E (LitInt x)) = x
evalIxExp env (E (Op2 Add a b)) = evalIxExp env (E a) + evalIxExp env (E b)
evalIxExp env (E (Op2 Sub a b)) = evalIxExp env (E a) - evalIxExp env (E b)
evalIxExp env (E (Op2 Mul a b)) = evalIxExp env (E a) * evalIxExp env (E b)
evalIxExp env (E (Op2 Div a b)) = div (evalIxExp env (E a))  (evalIxExp env (E b))
evalIxExp env (E (Op2 Min a b)) = let a' = evalIxExp env (E a)
                                      b' = evalIxExp env (E b)
                                  in  if a' < b' then a' else b'
evalIxExp env (E (Op2 Max a b)) = let a' = evalIxExp env (E a)
                                      b' = evalIxExp env (E b)
                                  in  if a' < b' then b' else a'
evalIxExp env (E (Index name [])) = case lookup name env of 
                                      Just i -> i
                                      Nothing -> error "not in env"
evalIxExp env _ = error "evalIxExp: Not supported"
{-
evalIxExp env (E (Variable name)) = case lookup name env of 
                                      Just i -> i
                                      Nothing -> error "not in env"
-}


{- TypeOF class -}
class TypeOf a where
    typeOf :: a -> Type

instance TypeOf IntE where
    typeOf a = Int

instance TypeOf FloatE where
    typeOf a = Float

instance TypeOf BoolE where
    typeOf a = Bool




