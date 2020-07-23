{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Compiler.IRs where

import MicroRAM.MicroRAM(MAOperand)
import qualified MicroRAM.MicroRAM as MRAM
import Data.ByteString.Short

import Compiler.CompileErrors

type ($) a b = a b

{-|
Module      : Irs
Description : Several intermediate representations between LLVM and MicroRAM
Maintainer  : santiago@galois.com
Stability   : Prototype

Intermedaiate representations are Languages taking all the instructions from MicroRAM
and adding some functionality such as functions Stack locations etc.

-}

-- ** Generic IR
-- An IR is made of MRAM instructions plus some new ones
data IRInstruction metadata regT wrdT irinst =
   MRI (MRAM.MAInstruction regT wrdT) metadata
  | IRI irinst metadata
  deriving (Show)

data Function nameT paramT blockT =
  Function nameT paramT [paramT] [blockT] deriving (Functor)

type DAGinfo name = [name]
-- | Basic blocks:
-- | it's a list of instructions + all the blocks that it can jump to
data BB name instrT = BB name [instrT] (DAGinfo name)
  deriving (Show, Functor)

type IRFunction mdata regT wrdT irinstr =
  Function Name Ty (BB Name $ IRInstruction mdata regT wrdT irinstr)

data Name =
  Name ShortByteString -- | we keep the LLVM names
  | NewName Word         -- | and add some new ones
  deriving (Eq, Ord, Read, Show)

type TypeEnv = () -- TODO

data GlobalVariable wrdT = GlobalVariable
  { name :: Name
  , isConstant :: Bool
  , gType :: Ty
  , initializer :: Maybe wrdT
  }
type GEnv wrdT = [GlobalVariable wrdT] -- Maybe better as a map:: Name -> "gvar description"
data IRprog mdata wrdT funcT = IRprog
  { typeEnv :: TypeEnv
  , globals :: GEnv wrdT
  , irProgCode :: [funcT]
  } deriving (Functor)




-- -------------------------------
-- ** Register Transfer language (RTL)
-- -------------------------------

-- RTL uses infinite registers, function calls and regular MRAM instructions for the rest.
data CallInstrs operand = 
   ICall operand -- ^ function
        [operand] -- ^ arguments
  | IRet (Maybe operand) -- ^ return this value
        

-- | Virtual registers
type VReg = Name 

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
  | RPhi VReg [(operand,Name)]
    
    
type RTLInstr mdata wrdT = IRInstruction mdata VReg wrdT (RTLInstr' $ MAOperand VReg wrdT)


type RFunction mdata wrdT =
  IRFunction mdata VReg wrdT (RTLInstr' $ MAOperand VReg wrdT)
  
type Rprog mdata wrdT = IRprog mdata wrdT $ RFunction mdata wrdT









-- -------------------------------
-- ** Location Transfer Language
-- -------------------------------

-- It's the target language for register allocation: Close to RTL but uses machine registers and stack slots instead of virtual registers.

-- | Ty determines the type of something in the stack. Helps us calculate
-- stack offsets for stack layout
-- FIXME: For now we assume everything is an int, but the code should be
--  written genrically over this type so it's easy to change
data Ty = Tint
  deriving Show

-- Determines the relative size of types (relative to a 32bit integer)
tySize :: Ty -> Word
tySize _ = 1


-- | Slots are abstract representation of locations in the activation record and come in three kinds
data Slot =
    Local     -- ^ Used by register allocation to spill pseudo-registers to the stack
  | Incoming  -- ^ Stores parameters of the current function
  | Outgoing  -- ^ Stores arguments to the called function that cannot be in registers.
  deriving (Eq, Read, Show)

-- | Locations are the disjoint union of machine registers and stack loctions
data Loc mreg where
  R :: mreg -> Loc mreg
  L :: Slot -> Int -> Ty -> Loc mregm

-- | LTL unique instrustions
-- JP: wrdT is unused. Drop?
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
  deriving Show
  
type LTLInstr mdata mreg wrdT =
  IRInstruction mdata mreg wrdT (LTLInstr' mreg wrdT $ MAOperand mreg wrdT)
  

-- data Function nameT paramT blockT =
--  Function nameT paramT [paramT] [blockT]
data LFunction mdata mreg wrdT = LFunction {
  funName :: String -- should this be a special label?
  , funMetadata :: mdata
  , retType :: Ty
  , paramTypes :: [Ty]
  , stackSize :: Word
  , funBody:: [BB Name $ LTLInstr mdata mreg wrdT]
}

type Lprog mdata mreg wrdT = IRprog mdata wrdT $ LFunction mdata mreg wrdT


-- Converts a RTL program to a LTL program.
rtlToLtl :: forall mdata wrdT . Monoid mdata => Rprog mdata wrdT -> Hopefully $ Lprog mdata VReg wrdT
rtlToLtl (IRprog tenv globals code) = do
  code' <- mapM convertFunc code
  return $ IRprog tenv globals code'
  where
   convertFunc :: RFunction mdata wrdT -> Hopefully $ LFunction mdata VReg wrdT
   convertFunc (Function name retType paramTypes body) = 
     -- JP: Where should we get the metadata and stack size from?
     let mdata = mempty in
     let stackSize = 0 in
     let name' = show name in
       do
         body' <- mapM convertBasicBlock body
         return $ LFunction name' mdata retType paramTypes stackSize body' 

   convertBasicBlock :: BB name (RTLInstr mdata wrdT) -> Hopefully $ BB name (LTLInstr mdata VReg wrdT)
   convertBasicBlock (BB name instrs dag) = do
     instrs' <- mapM convertIRInstruction instrs
     return $ BB name instrs' dag

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
   
