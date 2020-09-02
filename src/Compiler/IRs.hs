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
  -- * Types
  -- $types
  Ty(..), tySize, TypeEnv,
  
  -- * Backend languages
  -- ** MicroAssembly
  -- $MA
  MAOperand(..),
  MAProgram,
  MA2Instruction,
  MAInstruction,
  NamedBlock(..),
  
  -- ** Generic IR
  -- $GIR
  IRprog(..), Function(..), IRFunction, BB(..),
  IRInstruction(..),
  
  GlobalVariable(..), GEnv,
  Name(..), VReg, DAGinfo,
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

import MicroRAM(MWord)
import qualified MicroRAM as MRAM

import Data.ByteString.Short

import qualified Data.Map as Map

import Compiler.Registers
import Compiler.Errors
import Util.Util

-- ** Types
-- $types
-- This type 'system' is used by the compiler backend. Pointer types don't expose
-- the type of the reference since we mostly just care about the size and
-- all pointers have the same size.
--
-- All the types here should be finite even though that is not enforced by the Haskell
-- Datatype.
-- LLVM's mutually recursive types must have a computable sizes, so they allways
-- "pass" through a pointer before a recursive referece. Thus, all those recursive
-- referece disappear in the backend and we allways get finite types.

-- | The type of something in the stack. Used to calculate
-- stack offsets for stack layout.
data Ty =
   Tint
  | Tptr 
  | Tarray MWord Ty 
  | Tstruct [Ty]
  deriving (Show)

-- | Determines the relative size of types (relative to a 32bit integer/64bit)
tySize ::  Ty -> MWord
tySize (Tarray length subTyp) = length * (tySize subTyp)
tySize (Tstruct tys) = sum $ map tySize tys   
tySize _ = 1 -- Pointers have the same sizer as Tint


type TypeEnv = Map.Map Name Ty

-- ** Operands

data MAOperand regT wrdT where
  AReg :: regT -> MAOperand regT wrdT    -- ^ Assembly register 
  LConst :: wrdT -> MAOperand regT wrdT  -- ^ Name foreshadows the use of lazy constants
  Label :: String -> MAOperand regT wrdT -- 
  Glob ::  String -> MAOperand regT wrdT
  HereLabel :: MAOperand regT wrdT
  deriving (Eq,Ord,Read,Show)

-- ** MicroAssembly
-- $MA MicroAssembly is the lowes level of intermediate language and it's buildnig blocks are
-- reused by other IRs to avoid duplication (and easier compilation).
-- It comes in two flavors:
-- * __Two operands__ : Is more expresive, for higher level IRs it supports immidiates on the left of opperations.
-- * __One operands__ : Closer to machine language `MicroRAM`, left operand is allways a register.
   
-- | Two operands MicroAssembly
type MA2Instruction regT wrdT = MRAM.Instruction' regT (MAOperand regT wrdT) (MAOperand regT wrdT)
-- | One operand MicroAssembly
type MAInstruction regT wrdT = MRAM.Instruction' regT regT (MAOperand regT wrdT)

data NamedBlock r w = NBlock (Maybe String) [MAInstruction r w]
  deriving (Eq, Ord, Read, Show)
type MAProgram r w = [NamedBlock r w] -- These are MicroASM programs



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
  , funcNextReg :: Word    -- ^ 
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


-- | Marks a block to know what blocks can jump to it
-- Seems like it's not used. **FIXME:** REMOVE?
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
 
-- | These names are an extension to LLVM's register names.
-- It includes a `NewName` to produce temporary registers that
-- don't intefere with existing ones. 
data Name =
  Name ShortByteString   -- ^ we keep the LLVM names
  | NewName Word         -- ^ and add some new ones
  deriving (Eq, Ord, Read, Show)

-- | Virtual registers
type VReg = Name

instance Regs Name where
  sp = NewName 0
  bp = NewName 1
  ax = NewName 2
  -- argc = Name "0" -- Where the first arguemtns to main is passed
  -- argv = Name "1" -- Where the second arguemtns to main is passed
  fromWord w      -- FIXME this is terribled: depends on read and show! Ugh!
    | w == 1 = Name "0"
    | even w = NewName $ w `div` 2
    | otherwise = Name $ pack $ read $ show $ digits ((w-1) `div` 2)
  toWord (NewName x) = 2*x
  toWord (Name sh) = 1 + (2 * (read $ read $ show sh))
  
-- Produces the digits, shifted by 48 (ie. the ASCII representation)
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10 + 48] -- ASCII 0 = 0


-- | This is the representation of global variables until they are
-- set in memory and translated to constant pointers. 
data GlobalVariable wrdT = GlobalVariable
  { name :: String -- Optimize?
  , isConstant :: Bool
  , gType :: Ty
  , initializer :: Maybe [wrdT]
  , secret :: Bool
  } deriving (Show)
type GEnv wrdT = [GlobalVariable wrdT] -- Maybe better as a map:: Name -> "gvar description"

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
    Ty   -- ^ type of the allocated thing
    operand -- ^ number of things allocated
  | RPhi VReg [(operand,Name)] -- ^ Static Single Assignment function `phi`
  deriving (Show)
    
    
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
    Lgetstack Slot Word Ty mreg -- load from the stack into a register
  | Lsetstack mreg Slot Word Ty -- store into the stack from a register
  | LCall
      Ty
      (Maybe mreg) -- ^ return register
      operand -- ^ function
      [Ty]         -- ^ types of parameters 
      [operand] -- ^ arguments
  | LRet (Maybe operand) -- ^ return this value
  | LAlloc
    (Maybe mreg) -- ^ return register (gives location)
    Ty   -- ^ type of the allocated thing
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
  , funMetadata :: mdata
  , retType :: Ty
  , paramTypes :: [Ty]
  , stackSize :: Word
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
rtlToLtl :: forall mdata wrdT . Monoid mdata => Rprog mdata wrdT -> Hopefully $ Lprog mdata VReg wrdT
rtlToLtl (IRprog tenv globals code) = do
  code' <- mapM convertFunc code
  return $ IRprog tenv globals code'
  where
   convertFunc :: RFunction mdata wrdT -> Hopefully $ LFunction mdata VReg wrdT
   convertFunc (Function name retType paramTypes body _nextReg) = do
     -- JP: Where should we get the metadata and stack size from?
     let mdata = mempty
     let stackSize = 0 -- Since nothing is spilled 0
     let name' = show name
     body' <- mapM convertBasicBlock body
     return $ LFunction name' mdata retType paramTypes stackSize body' 

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
   convertInstruction (RAlloc mr t o) = return $ LAlloc mr t o
   convertInstruction (RPhi _ _) = implError "Phi. Not implemented in the trivial Register allocation."
   
