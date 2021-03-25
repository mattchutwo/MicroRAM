{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Intermediate Representation Languages
Description : Intermediate representations between LLVM and MicroRAM
Maintainer  : santiago@galois.com
Stability   : Prototype

Intermedaiate representations are Languages taking all the instructions from MicroRAM
and adding some functionality such as function call, Stack locations, etc.


-}
module Compiler.IRs(
  -- * Backend languages
  -- ** MicroAssembly
  -- $MA
  MAOperand(..),
  MAProgram,
  AnnotatedProgram,
  MA2Instruction,
  MAInstruction,
  NamedBlock(..),
  
  -- ** Generic IR
  -- $GIR
  IRprog(..), Function(..), IRFunction, BB(..),
  IRInstruction(..),
  
  VReg, DAGinfo,
  -- Utilities
  traverseOpBB, traverseOpIRInstr,  

  -- ** MicroIR
  -- $MIR
  MIRprog, MIRFunction, MIRInstruction(..), MIRInstr,
  

  -- ** RTL
  -- $RTL
  Rprog,RFunction, RTLInstr'(..), RTLInstr, 
  -- RTL utility
  traverseOpRTLInstr,

  -- ** LTL
  -- $LTL
  Lprog, LFunction(..), LTLInstr'(..), LTLInstr,
  Slot(..),
  -- LTL utility
  traverseOpLTLInstr, traverseOpLFun,traverseOpLprog,

  -- * Translation RTL->LTL
  rtlToLtl,
  ) where


import qualified MicroRAM as MRAM

import Compiler.LazyConstants

import Compiler.Errors
import Compiler.Common (Name, Ty, TypeEnv, GEnv)
import Util.Util

-- ** Operands


-- TO BE MOVED TO ITS OWN MODULE

-- type GEnv = String -> Word -- FIXME

data MAOperand regT wrdT where
  AReg :: regT -> MAOperand regT wrdT    -- ^ Assembly register 
  LImm :: LazyConst String wrdT -> MAOperand regT wrdT    -- ^ lazy immidiates
  Label :: String -> MAOperand regT wrdT -- 
  Glob ::  Name -> MAOperand regT wrdT
  HereLabel :: MAOperand regT wrdT
  deriving (Show)



-- ** MicroAssembly
-- $MA MicroAssembly is the lowes level of intermediate language and it's buildnig blocks are
-- reused by other IRs to avoid duplication (and easier compilation).
-- It comes in two flavors:
-- * __Two operands__ : Is more expresive, for higher level IRs it supports immidiates on the left of opperations.
-- * __One operands__ : Closer to machine language `MicroRAM`, left operand is allways a register.
   
-- | Two operands MicroAssembly
type MA2Instruction regT wrdT = MRAM.Instruction' regT (MAOperand regT wrdT) (MAOperand regT wrdT)
-- | One oprand MicroAssembly
type MAInstruction regT wrdT = MRAM.Instruction' regT regT (MAOperand regT wrdT)

data NamedBlock md r w = NBlock (Maybe String) [(MAInstruction r w, md)]
  deriving (Show)
type MAProgram md r w = [NamedBlock md r w] -- These are MicroASM programs
type AnnotatedProgram md r w = [(MRAM.Instruction r w, md)]


-- ** Generic low-level IR (Transfer languages)
-- $GIR All IRs are made of standard 'MicroRAM' instructions plus some new ones. This
-- makes compilation significantly easier and reduces duplication.

data IRInstruction metadata regT wrdT irinst =
   MRI (MAInstruction regT wrdT) metadata
  | IRI irinst metadata
  deriving (Show,Functor, Foldable, Traversable)
data Function nameT paramT blockT = Function
  { funcName :: nameT      -- ^ Function identifier
  , funcRetTy :: paramT    -- ^ Return type
  , funcArgTys :: [paramT] -- ^ Types of arguments
  , funcBlocks :: [blockT] -- ^ Function code as a list of blocks
  , funcNextReg :: Word    -- ^ The index of the next unused register
  }
  deriving (Show, Functor)

-- | Traverse the IR instruction changing operands  
traverseOpIRInstr :: (Traversable irinst, Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT'))
  -> IRInstruction metadata regT wrdT (irinst $ MAOperand regT wrdT)
  -> f (IRInstruction metadata regT wrdT' (irinst $ MAOperand regT wrdT'))
traverseOpIRInstr fop (MRI maInstr metadata) = 
  MRI <$> (traverse fop maInstr) <*> (pure metadata) 
traverseOpIRInstr fop (IRI irinst metadata) = 
  IRI <$> (traverse fop irinst) <*> (pure metadata)


-- | The possible successors of a block.  Used in register allocation.
type DAGinfo name = [name]

-- | Basic blocks:
--  List of instructions + marked with all the blocks that it can jump to.
--  It keeps separated the body from the instructions of the terminator,
-- This allows for easy `phi` lifting.
data BB name instrT = BB name [instrT] [instrT] (DAGinfo name)
  deriving (Show,Functor, Foldable, Traversable)

-- | Traverse the Basic Blocks changing operands  
traverseOpBB :: (Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT))
  -> BB name $ LTLInstr mdata regT wrdT
  -> f (BB name $ LTLInstr mdata regT wrdT)
traverseOpBB fop = traverse (traverseOpLTLInstr fop)  

type IRFunction mdata regT wrdT irinstr =
  Function Name Ty (BB Name $ IRInstruction mdata regT wrdT irinstr)

-- | Virtual registers
type VReg = Name

-- | Programs in the backend.
data IRprog mdata wrdT funcT = IRprog
  { typeEnv :: TypeEnv
  , globals :: GEnv wrdT
  , code :: [funcT]
  } deriving (Show, Functor, Foldable, Traversable)




-- ** MicroIR
-- $MIR
-- High-level IR based on MicroRAM.  Includes MicroRAM instructions with
-- support for extended operand kinds and two non-register operands per
-- instruction (normal MicroRAM requires one operand to be a register), as well
-- as extended high-level instructions (`RTLInstr'`).

data MIRInstruction metadata regT wrdT =
  MirM (MA2Instruction regT wrdT) metadata
  | MirI (RTLInstr' (MAOperand regT wrdT)) metadata
  deriving (Show)

type MIRInstr metadata wrdT = MIRInstruction metadata VReg wrdT

type MIRFunction metadata wrdT =
  Function Name Ty (BB Name $ MIRInstr metadata wrdT)

type MIRprog metadata wrdT =
  IRprog metadata wrdT (MIRFunction metadata wrdT)


-- -------------------------------
-- ** Register Transfer language (RTL)
-- -------------------------------
-- $RTL
-- Register Transfer language (RTL) uses infinite virtual registers,
-- function calls, Stack allocation and regular MicroRAM instructions. 

-- | Instructions for the RTL language
data RTLInstr' operand =
    RCall
      Ty           -- ^ return type
      (Maybe VReg) -- ^ return register
      operand      -- ^ function
      [Ty]         -- ^ types of parameters 
      [operand]    -- ^ arguments
  | RRet (Maybe operand) -- ^ return this value
  | RAlloc
    (Maybe VReg) -- ^ return register (gives location)
    MRAM.MWord    -- ^ size of the allocated thing
    operand -- ^ number of things allocated
  | RPhi VReg [(operand,Name)] -- ^ Static Single Assignment function `phi`
  deriving (Show, Functor, Foldable, Traversable)
    
    
type RTLInstr mdata wrdT = IRInstruction mdata VReg wrdT (RTLInstr' $ MAOperand VReg wrdT)

-- |  Traverse the RTL instruction changing operands  
traverseOpRTLInstr :: (Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT))
  -> LTLInstr metadata regT wrdT
  -> f (LTLInstr metadata regT wrdT)
traverseOpRTLInstr = traverseOpIRInstr

type RFunction mdata wrdT =
  IRFunction mdata VReg wrdT (RTLInstr' $ MAOperand VReg wrdT)
  
type Rprog mdata wrdT = IRprog mdata wrdT $ RFunction mdata wrdT









-- -------------------------------
-- ** Location Transfer Language
-- -------------------------------

-- $LTL 
-- Location Transfer Language (LTL) is the target language for register allocation: Close to RTL but uses machine registers and stack slots instead of virtual registers.

-- | Slots are abstract representation of locations in the activation record and come in three kinds
data Slot =
    Local     -- ^ Used by register allocation to spill pseudo-registers to the stack
  | Incoming  -- ^ Stores parameters of the current function
--  | Outgoing  -- ^ Stores arguments to the called function that cannot be in registers.
  deriving (Eq, Read, Show)

-- | Locations are the disjoint union of machine registers and stack loctions
{-data Loc mreg where
  R :: mreg -> Loc mreg
  L :: Slot -> Int -> Ty -> Loc mregm
  deriving (Show) -}

-- | LTL unique instrustions
-- JP: wrdT is unused. Drop?
-- SC: Should be wrdT instead of Word everywhere. FIXME
data LTLInstr' mreg wrdT operand =
    Lgetstack Slot MRAM.MWord Ty mreg -- load from the stack into a register
  | Lsetstack mreg Slot MRAM.MWord Ty -- store into the stack from a register
  | LCall
      Ty
      (Maybe mreg) -- ^ return register
      operand -- ^ function
      [Ty]         -- ^ types of parameters 
      [operand] -- ^ arguments
  | LRet (Maybe operand) -- ^ return this value
  | LAlloc
    (Maybe mreg) -- ^ return register (gives location)
    MRAM.MWord    -- ^ size of the allocated thing
    operand -- ^ number of things allocated
  deriving (Show, Functor, Foldable, Traversable)
  
type LTLInstr mdata mreg wrdT =
  IRInstruction mdata mreg wrdT (LTLInstr' mreg wrdT $ MAOperand mreg wrdT)
  


-- |  Traverse the LTL instruction changing operands  
traverseOpLTLInstr :: (Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT))
  -> LTLInstr metadata regT wrdT
  -> f (LTLInstr metadata regT wrdT)
traverseOpLTLInstr = traverseOpIRInstr


data LFunction mdata mreg wrdT = LFunction {
    funName :: String -- should this be a special label?
  , retType :: Ty
  , paramTypes :: [Ty]
  , stackSize :: MRAM.MWord
  , funBody:: [BB Name $ LTLInstr mdata mreg wrdT]
  } deriving (Show)

-- | Traverse the LTL functions and replacing operands 
traverseOpLFun :: (Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT))
  -> LFunction mdata regT wrdT
  -> f $ LFunction mdata regT wrdT
traverseOpLFun fop lf = (\body -> lf {funBody = body}) <$>
                        traverseOpBBs fop (funBody lf)
  where traverseOpBBs fop = traverse (traverseOpBB fop)

type Lprog mdata mreg wrdT = IRprog mdata wrdT $ LFunction mdata mreg wrdT


-- | Traverse the LTL Program and replacing operands
traverseOpLprog :: (Applicative f) =>
  (MAOperand regT wrdT -> f (MAOperand regT wrdT))
  -> Lprog mdata regT wrdT
  -> f $ Lprog mdata regT wrdT
traverseOpLprog fop = traverse (traverseOpLFun fop)


-- Converts a RTL program to a LTL program.
rtlToLtl :: forall mdata wrdT . Rprog mdata wrdT -> Hopefully $ Lprog mdata VReg wrdT
rtlToLtl (IRprog tenv globals code) = do
  code' <- mapM convertFunc code
  return $ IRprog tenv globals code'
  where
   convertFunc :: RFunction mdata wrdT -> Hopefully $ LFunction mdata VReg wrdT
   convertFunc (Function name retType paramTypes body _nextReg) = do
     -- JP: Where should we get the metadata and stack size from?
     let stackSize = 0 -- Since nothing is spilled 0
     let name' = show name
     body' <- mapM convertBasicBlock body
     return $ LFunction name' retType paramTypes stackSize body' 

   convertBasicBlock :: BB name (RTLInstr mdata wrdT) -> Hopefully $ BB name (LTLInstr mdata VReg wrdT)
   convertBasicBlock (BB name instrs term dag) = do
     instrs' <- mapM convertIRInstruction instrs
     term' <- mapM convertIRInstruction term
     return $ BB name instrs' term' dag

   convertIRInstruction :: RTLInstr mdata wrdT -> Hopefully $ LTLInstr mdata VReg wrdT
   convertIRInstruction (MRI inst mdata) = return $ MRI inst mdata
   convertIRInstruction (IRI inst mdata) = do
     inst' <- convertInstruction inst
     return $ IRI inst' mdata

   convertInstruction ::
     RTLInstr' (MAOperand VReg wrdT)
     -> Hopefully $ LTLInstr' VReg wrdT (MAOperand VReg wrdT)
   convertInstruction (RCall t mr f ts as) = return $ LCall t mr f ts as
   convertInstruction (RRet mo) = return $ LRet mo
   convertInstruction (RAlloc mr s o) = return $ LAlloc mr s o
   convertInstruction (RPhi _ _) = implError "Phi. Not implemented in the trivial Register allocation."
   
