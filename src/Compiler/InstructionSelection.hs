{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Instruction Selection
Description : LLVM -> RTL
Maintainer  : santiago@galois.com
Stability   : prototype

This module compiles LLVM to RTL.

Our instruciton selection is linear. There are no complex instructions
so selection is pretty siple.

If we add complex instructions we might want to build the DAG and do
instruction selection there.

As we do the instructino selections WE KEEP THE DAG INFORMATION by
annotating each block with all the blocks it can jump to. This could
be reversed (annotate blocks by those blocks that jump to it) in
a single pass. 

-}

module Compiler.InstructionSelection
    ( instrSelect,
    ) where


import Data.ByteString.Short
import Control.Monad.State.Lazy

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.ParameterAttribute as ParamAtt

import Compiler.CompileErrors
import Compiler.IRs

import MicroRAM.MicroRAM (Operand'(..), MAOperand) 
import qualified MicroRAM.MicroRAM as MRAM

{-| Notes on this instruction generation :

   TO DOs:
   1. Check exception handeling, I'm  not sure I'm translating that correctly.
      Particulary, do we include exeption jumpin in DAGS? Right now we don't
   

-}

-- ** Translation between LLVM and RTL "things"
any2short :: Show a => a -> ShortByteString
any2short n = toShort $ read $ show $ n

name2name (LLVM.Name s) = return $ Name s
name2name (LLVM.UnName n) = return $ Name $ any2short n

--  implError "Unnamed opareands not supported yet. TODO soon... "


toType :: LLVM.Type -> Ty
toType _ = Tint


wrd2integer:: Word -> Integer
wrd2integer x = fromIntegral x

integer2wrd:: Integer -> Hopefully $ Word
integer2wrd x
  | x >= (wrd2integer minBound) && x <= (wrd2integer maxBound) = return $ fromInteger x
  | otherwise = otherError $ "Literal out of bounds: " ++ (show x) ++ ". Bounds " ++ (show (wrd2integer minBound, wrd2integer maxBound)) 
  
getConstant :: LLVM.Constant.Constant -> Hopefully $ Word
getConstant (LLVM.Constant.Int _ val) = integer2wrd val
getConstant (LLVM.Constant.Undef typ) = return $ 0 -- Concretising values is allways allowed TODO: Why are there undefined values, can't we remove this?
getConstant consT = otherError $
  "Illegal constant. Maybe you used an unsuported type (e.g. float) or you forgot to run constant propagation (i.e. constant expresions in instructions)" ++ (show consT)


operand2operand :: LLVM.Operand -> Hopefully $ MAOperand VReg Word
operand2operand (LLVM.ConstantOperand c) = Const <$> getConstant c
operand2operand (LLVM.LocalReference _ name) = do
  name' <- (name2name name)
  return $ Reg name'
operand2operand _ = implError "operand, probably metadata"

name2Operand :: LLVM.Name -> Hopefully $ MRAM.MAOperand VReg Word
name2Operand (LLVM.Name name) = return $ Reg $ Name name
name2Operand (LLVM.UnName number) = return $ Reg $ Name $ any2short number
--name2Operand _ = assumptError "Unnamed name passed. Unnammed things should not be called."


type2type (LLVM.IntegerType n) = return Tint -- FIXME check size! 
type2type (LLVM.PointerType t _) = return Tint -- FIXME enrich typed!
type2type (LLVM.FunctionType _ _ _) = return Tint -- FIXME enrich typed!
type2type t = implError $ "Type: " ++ (show t)


-- | toRTL lifts simple MicroRAM instruction into RTL.
toRTL :: [MRAM.MAInstruction VReg Word] -> [RTLInstr () Word]
toRTL = map  $ \x -> MRI x ()

toStateRTL x = toState $ toRTL <$> x

returnRTL :: Monad m => [MRAM.MAInstruction VReg Word] -> m [RTLInstr () Word]
returnRTL = return . toRTL

returnStateRTL x = toState  $ returnRTL x

-- ** State
-- We create a state to create new variables
type Statefully = StateT Word Hopefully

initState :: Word
initState = 2 -- Leave space for ESP and EBP

freshName :: Statefully Name
freshName = do
  n <- get
  put (n + 1)
  return $ NewName n

evalStatefully :: Statefully t -> Hopefully t
evalStatefully ts = evalStateT ts initState

toState :: Hopefully a -> Statefully a
toState (Left x) = StateT (\_ -> Left x)
toState (Right x) = StateT (\s -> Right (x,s))

-- ** Instruction selection

-- | Instruction Generation
-- We mostly generate MAInstructions and then lift them to RTL. The only exception is
-- function call

--isInstrs = undefined

-- |I
type BinopInstruction = VReg -> VReg -> MRAM.MAOperand VReg Word -> MRAM.MAInstruction VReg Word
isBinop ::
  Maybe VReg
  -> LLVM.Operand
  -> LLVM.Operand
  -> BinopInstruction
  -> Hopefully $ [MRAM.MAInstruction VReg Word]
isBinop Nothing _ _ _ = Right $ [] --  without return is a noop
isBinop _ (LLVM.ConstantOperand _) (LLVM.ConstantOperand _) _ =
  assumptError $
  "Two constants in a binop. Did you forget to run constant propagation?"
isBinop (Just ret) (LLVM.LocalReference _ name1) op2 bop = do
  r1 <- name2name name1
  a <- operand2operand op2
  return $ [bop ret r1 a]
isBinop ret op1 op2 _ = implError $
  "Binary operation with operands other than (register,register) or (register,constant). Tried compiling: " ++ (show (ret,op1,op2)) ++ ". Maybe you tried compiling a term of the form '3*r0'. That's not supported yet. Try 'r0*3'"

-- ** Comparisons
{- Unfortunately MRAM always puts a comparisons in the "flag"
   So if we want the value we need to do 2 operations to get the value.
   In the future, we can reserve one "register" to work as the flag, then a smart -}

-- There are two ways to move the result from the flag.
-- The straight worfward cmptTail_pos and the negated one cmptTail_neg

cmptTail_pos ret = return [MRAM.Imov ret (Const 0), MRAM.Icmov ret (Const 1)] -- moving the flag to the register... super wasteful! write a better register allocater

cmptTail_neg ret = return [MRAM.Imov ret (Const 1), MRAM.Icmov ret (Const 0)] -- Neg. the result && mov the flag to the register... wasteful! 

-- | cmptTail:
-- Describe if the comparison is computed directly or it's negation 
cmptTail :: IntPred.IntegerPredicate -> VReg -> Hopefully [MRAM.MAInstruction VReg Word]
cmptTail IntPred.EQ = cmptTail_pos -- This values allow to flip the result
cmptTail IntPred.NE = cmptTail_neg 
cmptTail IntPred.UGT = cmptTail_pos
cmptTail IntPred.UGE = cmptTail_pos
cmptTail IntPred.ULT = cmptTail_neg
cmptTail IntPred.ULE = cmptTail_neg
cmptTail IntPred.SGT = cmptTail_pos
cmptTail IntPred.SGE = cmptTail_pos
cmptTail IntPred.SLT = cmptTail_neg
cmptTail IntPred.SLE = cmptTail_neg

-- | Instruction selection for comparisons
isCompare pred (Const lhs) _ =
  assumptError
  "Comparing left hand side constants (expected a register). Did you forget to do constant propagation?"
isCompare IntPred.EQ (Reg lhs) rhs = return $ MRAM.Icmpe lhs rhs
isCompare IntPred.NE (Reg lhs) rhs = return $ MRAM.Icmpe lhs rhs
-- Unsigned
isCompare IntPred.UGT (Reg lhs) rhs = return $ MRAM.Icmpa lhs rhs
isCompare IntPred.UGE (Reg lhs) rhs = return $ MRAM.Icmpae lhs rhs
isCompare IntPred.ULT (Reg lhs) rhs = return $ MRAM.Icmpae lhs rhs
isCompare IntPred.ULE (Reg lhs) rhs = return $ MRAM.Icmpa lhs rhs
-- Signed
isCompare IntPred.SGT (Reg lhs) rhs = return $ MRAM.Icmpg lhs rhs
isCompare IntPred.SGE (Reg lhs) rhs = return $ MRAM.Icmpge lhs rhs
isCompare IntPred.SLT (Reg lhs) rhs = return $ MRAM.Icmpge lhs rhs
isCompare IntPred.SLE (Reg lhs) rhs = return $ MRAM.Icmpg lhs rhs
isCompare pred _ _ = implError $ "Unsigned comparisons: " ++ show pred


fError = implError "Floatin point arithmetic"
uError = implError "unsigned operations"


constzero = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 0) 0)
constOne = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 1) 1)

(<:>):: Applicative f => f a -> f [a] -> f [a]
a <:> b = (:) <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a]  -> f [a] 
a <++> b = (++) <$> a <*> b

-- *** Trtanslating Function parameters and types

function2function (Left _ ) = implError $ "Inlined assembly not supported"
function2function (Right (LLVM.LocalReference ty nm)) = do
  nm' <- name2name nm
  (retT', paramT') <- functionTypes ty
  return (nm',retT',paramT')
  where functionTypes (LLVM.FunctionType retTy argTys False) = do
          retT' <- type2type retTy
          paramT' <- mapM type2type argTys
          return (retT',paramT')
        functionTypes (LLVM.FunctionType  _ _ True) =
          implError $ "Variable parameters (isVarArg in function call)."
        functionTypes ty =  assumptError $ "Function type expected found " ++ show ty ++ " instead."

-- | Process parameters into RTL format
-- WE dump the attributes

params2params params paramsT = do
  params' <- mapM (operand2operand . fst) params -- fst dumps the attributes
  return params' 

-- | Storing memory
-- MicroRAM does not suppor storing constants.
-- For storing a constant we need to move it to a register and then store.
storer loc cont'@(Reg reg)  =
  return [MRAM.Istore loc reg]
storer loc cont'@(Const val) = do
  temp <- freshName
  return [MRAM.Imov temp cont', MRAM.Istore loc temp]


-- | Instruction Selection for single LLVM instructions 
isInstruction :: Maybe VReg -> LLVM.Instruction -> Statefully $ [RTLInstr () Word]
-- *** Arithmetic

-- Add
isInstruction ret (LLVM.Add _ _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Iadd
-- Sub
isInstruction ret (LLVM.Sub _ _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Isub
-- Mul
isInstruction ret (LLVM.Mul _ _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Imull
-- SDiv
isInstruction ret (LLVM.SDiv _ _ o1 o2 ) = toState $ implError "Signed division ius hard! SDiv"
-- SRem
isInstruction ret (LLVM.SRem o1 o2 _) = toState $ implError "Signed division ius hard! SRem"



-- *** Floating Point 
-- FAdd
isInstruction ret (LLVM.FAdd _ o1 o2 _) = toState $ implError "Fast Multiplication FMul"
-- FSub
isInstruction ret (LLVM.FSub _ o1 o2 _) =  toState $ implError "Fast Multiplication FMul"
-- FMul
isInstruction ret (LLVM.FMul _ o1 o2 _) =  toState $ implError "Fast Multiplication FMul"
-- FDiv
isInstruction ret (LLVM.FDiv _ o1 o2 _) =  toState $ implError "Fast Division FDiv"
-- FRem
isInstruction ret (LLVM.FRem _ o1 o2 _) = toState $ fError

-- *** Unsigned operations
-- UDiv
isInstruction ret (LLVM.UDiv _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Iudiv -- this is easy
-- URem
isInstruction ret (LLVM.URem o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Iumod -- this is eay


-- *** Shift operations
-- Shl
isInstruction ret (LLVM.Shl _ _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Ishl
-- LShr
isInstruction ret (LLVM.LShr _ o1 o2 _) = toState $ toRTL <$> isBinop ret o1 o2 MRAM.Ishr
-- AShr
isInstruction ret (LLVM.AShr _ o1 o2 _) =  toState $ implError "Arithmetic shift right AShr"

-- *** Logical
--And
isInstruction ret (LLVM.And o1 o2 _) =  toState $ toRTL <$> isBinop ret o1 o2 MRAM.Iand
--Or
isInstruction ret (LLVM.Or o1 o2 _) =  toState $ toRTL <$> isBinop ret o1 o2 MRAM.Ior
--Xor
isInstruction ret (LLVM.Xor o1 o2 _) =  toState $ toRTL <$> isBinop ret o1 o2 MRAM.Ixor

-- *** Memory operations
-- Alloca
{- we ignore type, alignment and metadata and assume we are storing integers,
   we only look at the numElements.
   In fact (currently) we dont check  stack overflow or any similar safety check
   Also the current granularity of our memory is per word so ptr arithmetic and alignment are trivial. -}
isInstruction ret (LLVM.Alloca a Nothing b c) =
  isInstruction ret (LLVM.Alloca a (Just constOne) b c) --NumElements is defaulted to be one. 
isInstruction ret (LLVM.Alloca ty (Just size) _ _) = toState $ do
  ty' <- type2type ty
  size' <- operand2operand size
  return [IRI (RAlloc ret ty' size') ()]



-- Load
isInstruction Nothing (LLVM.Load _ _ _ _ _) = return [] -- Optimization reconside if we care about atomics
isInstruction (Just ret) (LLVM.Load _ n _ _ _) = toState $ do 
  a <- operand2operand n
  returnRTL $ (MRAM.Iload ret a) : []
    
-- | Store
{- Store yields void so we can will ignore the return location -}
{- FIXME: MicroRAM makes it hard to store constants. That's because the first operand must be a register.
   There are two solutions
   1. Add a state to produce fresh `Vreg`s so we can store the constant ina a temp register.
   2. Change MicroRAM: swap the operands in stores so we can store constants but not store to constant addresses.
      Why would you store to a constant address anyways! Isn't that undefined?
-}

isInstruction _ (LLVM.Store _ adr cont _ _ _) = do
  cont' <- toState $ operand2operand cont
  adr' <- toState $ operand2operand adr
  ret <- storer adr' cont'
  returnRTL ret


-- *** Compare
{- Unfortunately MRAM always puts a comparisons in the "flag"
   So if we want the value we need to do 2 operations to get the value.
   In the future, we can reserve one "register" to work as the flag, then a smart
   register allocator can actually use the flag as it was intended...
-}

isInstruction Nothing (LLVM.ICmp pred op1 op2 _) =  toState $ return [] -- Optimization
isInstruction (Just ret) (LLVM.ICmp pred op1 op2 _) = toState $ do
  lhs <- operand2operand op1
  rhs <- operand2operand op2
  comp' <- isCompare pred lhs rhs -- Do the comparison
  compTail <- cmptTail pred ret -- Put the comparison in the ret register
  returnRTL $ comp': compTail

                        
-- *** Function Call 
isInstruction ret (LLVM.Call _ _ _ f args _ _ ) = toState $  do
  (f',retT,paramT) <- function2function f
  args' <- params2params args paramT
  return [IRI (RCall retT ret (Reg f') paramT args') ()]

-- *** Phi
isInstruction Nothing (LLVM.Phi _ _ _)  = return [] -- Phi without a name is useless
isInstruction (Just ret) (LLVM.Phi typ ins _)  =  toState $ do
  ins' <- mapM convertPhiInput ins
  return $ [IRI (RPhi ret ins') ()]

isInstruction Nothing (LLVM.Select _ _ _ _)  = return [] -- Select without a name is useless
isInstruction (Just ret) (LLVM.Select cond op1 op2 _)  =  toStateRTL $ do
   cond' <- operand2operand op1
   op1' <- operand2operand op1 
   op2' <- operand2operand op2 
   return $ case cond' of
     Reg r -> [MRAM.Icmpe r (Const 1), MRAM.Imov ret op2', MRAM.Icmov ret op1']
     Const c -> -- compiler optimization should take care of this case. but just in case...
       [if c == 1 then MRAM.Imov ret op1' else MRAM.Imov ret op2']
       

-- *** Not supprted instructions (return meaningfull error)
isInstruction _ instr =  toState $ implError $ "Instruction: " ++ (show instr)

convertPhiInput :: (LLVM.Operand, LLVM.Name) -> Hopefully $ (MAOperand VReg Word, Name)
convertPhiInput (op, name) = do
  op' <- operand2operand op
  name' <- name2name name
  return (op', name')


-- ** Named instructions and instructions lists

isNInstruction :: LLVM.Named LLVM.Instruction -> Statefully $ [RTLInstr () Word]
isNInstruction (LLVM.Do instr) = isInstruction Nothing instr
isNInstruction (name LLVM.:= instr) = let ret = name2name name in
                                                  (\ret -> isInstruction (Just ret) instr) =<< ret
isInstrs
  :: [LLVM.Named LLVM.Instruction]
     -> Statefully $ [RTLInstr () Word]
isInstrs [] = return []
isInstrs instrs = do
  instrs' <- mapM isNInstruction instrs
  return $ concat instrs'






-- -----------------------------------------------






-- ** Selection for Terminator

-- | Instruction Generation for terminators
-- We ignore the name of terminators
mri instr = MRI instr ()  

isTerminator :: LLVM.Named LLVM.Terminator -> Hopefully $ [RTLInstr () Word]
isTerminator (name LLVM.:= term) = do
  termInstr <- isTerminator' term
  return $ termInstr
isTerminator (LLVM.Do term) = do
  termInstr <- isTerminator' term
  return $ termInstr
  
-- Branching

isTerminator' :: LLVM.Terminator -> Hopefully $ [RTLInstr () Word]
isTerminator' (LLVM.Br name _) = do
  name' <- name2name name
  returnRTL $ [MRAM.Ijmp $ Reg name']
isTerminator' (LLVM.CondBr (LLVM.LocalReference _ name) name1 name2 _) = do
  r1 <- name2name name
  loc1 <- name2Operand name1
  loc2 <- name2Operand name2
  returnRTL $ [MRAM.Icmpe r1 (Const 1),
                    MRAM.Icjmp loc1,
                    MRAM.Ijmp loc2]
isTerminator' (LLVM.CondBr _ name1 name2 _) =
  assumptError "conditional branching must depend on a register. If you passed a constant prhaps you forgot to run constant propagation. Can't branch on Metadata."

-- Possible optimisation:
-- Add just one return block, and have all others jump there.
isTerminator' (LLVM.Ret (Just ret) md) = do
  ret' <- operand2operand ret
  return $ [IRI (RRet $ Just ret') ()] 
isTerminator' (LLVM.Ret Nothing _) =
  return $ [IRI (RRet Nothing) ()]

isTerminator' term = implError $ "Terminator not yet supported" ++ (show term)


-- | blockJumpsTo : Calculates all the blocks that this block might jump to.
--  By convention the only jumps happen at the end, so they can
--  be easily calculated, but much easier to "keep calculated"
blockJumpsTo' :: LLVM.Terminator -> Hopefully [LLVM.Name]
blockJumpsTo' (LLVM.Ret _ _) = return []
blockJumpsTo' (LLVM.CondBr _ trueDest falseDest _ ) =
  return [trueDest,falseDest]
blockJumpsTo' (LLVM.Br dest _) = return  [dest]
blockJumpsTo' (LLVM.Switch _ defaultDest dests _) = return $ defaultDest : (map snd dests)
blockJumpsTo' (LLVM.IndirectBr _ dests _) = return dests
-- We ignore function calls!!! Only care what block it returns to
-- we also ignore the exeption handling.
blockJumpsTo' (LLVM.Invoke _ _ _ _ _ retDest exepcDest _ ) = return [retDest]
blockJumpsTo' (LLVM.Resume _ _ ) = return [] -- exception propagation
blockJumpsTo' (LLVM.Unreachable _ ) = return [] -- unreachable
blockJumpsTo' (LLVM.CleanupRet _ _ _) = return [] -- exception propagation
blockJumpsTo' (LLVM.CatchRet _ _ _) = return [] -- exception propagation
blockJumpsTo' (LLVM.CatchSwitch _ _ _ _) = return [] -- exception propagation





dumpName :: (a -> b) -> LLVM.Named a -> b
dumpName f (_ LLVM.:= a) = f a 
dumpName f (LLVM.Do a) = f a 


blockJumpsTo :: LLVM.Named LLVM.Terminator -> Hopefully [Name]
blockJumpsTo term = do
  dests <- (dumpName blockJumpsTo' term)
  mapM name2name dests 


-- instruction selection for blocks
isBlock:: LLVM.BasicBlock -> Statefully (BB $ RTLInstr () Word)
isBlock  (LLVM.BasicBlock name instrs term) = do
  body <- isInstrs instrs
  end <- toState $ isTerminator term
  jumpsTo <- toState $ blockJumpsTo term
  name' <- toState $ name2name name
  return $ BB name' body jumpsTo


  
isBlocks :: [LLVM.BasicBlock] -> Statefully [BB $ RTLInstr () Word]
isBlocks = mapM isBlock

processParams :: ([LLVM.Parameter], Bool) -> [Ty]
processParams (params, _) = map (\_ -> Tint) params

-- | Instruction generation for Functions

isFunction :: LLVM.Definition -> Hopefully $ RFunction () Word
isFunction (LLVM.GlobalDefinition (LLVM.Function _ _ _ _ _ retT name params _ _ _ _ _ _ code _ _)) =
  do
    body <- evalStateT (isBlocks code) initState
    params' <- return $ processParams params
    name' <- name2name name
    return $ Function name' (toType retT) params' body
isFunction other = unreachableError $ show other -- Shoudl be filtered out 

-- | Instruction Selection for all definitions
-- We create filters to separate the definitions into categories.
-- Then process each category of definition separatedly

-- | Filters
itIsFunc, itIsGlobVar, itIsTypeDef, itIsMetaData :: LLVM.Definition -> Bool
itIsFunc (LLVM.GlobalDefinition (LLVM.Function  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ )) = True
itIsFunc _ = False

itIsFuncAttr (LLVM.FunctionAttributes _ _) = True
itIsFuncAttr _ = False

itIsGlobVar (LLVM.GlobalDefinition (LLVM.GlobalVariable name _ _ _ _ _ _ _ _ _ _ _ _ _)) = True
itIsGlobVar _ = False

itIsTypeDef (LLVM.TypeDefinition _ _) = True
itIsTypeDef _ = False

itIsMetaData (LLVM.MetadataNodeDefinition _ _) = True
itIsMetaData (LLVM.NamedMetadataDefinition _ _) = True
itIsMetaData _ = False

unreachableError what = otherError $ "This is akward. This error should be unreachable. You called a function that should only be called on a list after filtering, to avoid this error. Here is the info: " ++ what

-- ** Instruction selection for each of those filtered definitions

-- | We check that we are not discarding anything we care about
-- We allow discarding metadata for now...
fOr ::  [a -> Bool] -> (a -> Bool)
fOr fs a = or $ map (\f -> f a) fs 

acceptedDef = fOr [itIsFunc, itIsFuncAttr, itIsGlobVar, itIsTypeDef, itIsMetaData]

checkDiscardedDef :: LLVM.Definition -> Hopefully ()
checkDiscardedDef def = if acceptedDef def
  then return ()
  else implError $ "Definition: " ++ (show def) ++ ".\n While checking discarded defs "
  
checkDiscardedDefs :: [LLVM.Definition] -> Hopefully ()
checkDiscardedDefs defs = do
  mapM checkDiscardedDef defs
  return ()

isTypeDefs :: [LLVM.Definition] -> Hopefully $ TypeEnv
isTypeDefs _ = return ()


isGlobVars :: [LLVM.Definition] -> Hopefully $ GEnv Word
isGlobVars _ = return []

isFuncAttributes :: [LLVM.Definition] -> Hopefully $ () -- TODO can we use this attributes?
isFuncAttributes _ = return () 

isDefs :: [LLVM.Definition] -> Hopefully $ Rprog () Word
isDefs defs = do
  typeDefs <- isTypeDefs $ filter itIsTypeDef defs
  globVars <- isGlobVars $ filter itIsGlobVar defs
  funcAttr <- isFuncAttributes $ filter itIsFuncAttr defs
  funcs <- mapM isFunction $ filter itIsFunc defs
  checkDiscardedDefs defs -- Make sure we dont drop something important
  return $ IRprog typeDefs globVars funcs
  
-- | Instruction selection generates an RTL Program
instrSelect :: LLVM.Module -> Hopefully $ Rprog () Word
instrSelect (LLVM.Module _ _ _ _ defs) = isDefs defs

