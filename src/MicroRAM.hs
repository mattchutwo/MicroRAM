{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : MicroRAM
Description : ADT for MicroRAM instructions and programs
Maintainer  : santiago@galois.com
Stability   : experimental

This module describe two (closely related) languages designed for zero knowledge
execution of programs: MicroRAM and MicroASM. These languages are part of the
cheescloth compiler.
 
= MicroRAM
A simple machine language designed for zero knowledge
execution of programs. The instructions (inspired by the TinyRAM language
(<https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf>) are as follows:

+--------+---------+----------------------------------------------+
|  Instr | operands|                   effects                    |
+========+=========+==============================================+
| and    | ri rj A | bitwise AND of [rj] and [A] and store in ri  |
+--------+---------+----------------------------------------------+
| or     | ri rj A | bitwise OR of [rj] and [A] and store in ri   |
+--------+---------+----------------------------------------------+
| xor    | ri rj A | bitwise XOR of [rj] and [A] and store in ri  |
+--------+---------+----------------------------------------------+
| not    | ri A    | bitwise NOT of [A] and store result in ri    |
+--------+---------+----------------------------------------------+
| add    | ri rj A | [rj]u + [A]u and store result in ri          |
+--------+---------+----------------------------------------------+
| sub    | ri rj A | [rj]u − [A]u and store result in ri          |
+--------+---------+----------------------------------------------+
| mull   | ri rj A | [rj]u × [A]u, store least sign. bits in ri   |
+--------+---------+----------------------------------------------+
| umulh  | ri rj A | [rj]u × [A]u, store most sign. bits in ri    |
+--------+---------+----------------------------------------------+
| smulh  | ri rj A | [rj]s × [A]s, store most sign. bits in ri    |
+--------+---------+----------------------------------------------+
| udiv   | ri rj A | quotient of [rj]u/[A]u and store in ri       |
+--------+---------+----------------------------------------------+
| umod   | ri rj A | remainder of [rj]u/[A]u and store in ri      |
+--------+---------+----------------------------------------------+
| shl    | ri rj A | shift [rj] by [A]u bits left, store in ri    |
+--------+---------+----------------------------------------------+
| shr    | ri rj A | shift [rj] by [A]u bits right, store in ri   |
+--------+---------+----------------------------------------------+
| cmpe   | ri rj A | compare [rj] == [A], store the result in ri  |
+--------+---------+----------------------------------------------+
| cmpa   | ri rj A | compare [rj] >  [A]u store the result in ri  |
+--------+---------+----------------------------------------------+
| cmpae  | ri rj A | compare [rj] >= [A]u store the result in ri  |
+--------+---------+----------------------------------------------+
| cmpg   | ri rj A | compare [rj] >  [A], store the result in ri  |
+--------+---------+----------------------------------------------+
| cmpge  | ri rj A | compare [rj] >= [A], store the result in ri  |
+--------+---------+----------------------------------------------+
| mov    | ri A    | store [A] in ri                              |
+--------+---------+----------------------------------------------+
| cmov   | ri rj A | if rj <> 0, store [A] in ri                  |
+--------+---------+----------------------------------------------+
| jmp    | A       | set pc to [A]                                |
+--------+---------+----------------------------------------------+
| cjmp   | ri A    | if ri <> 0, set pc to [A] (else pc++)        |
+--------+---------+----------------------------------------------+
| cnjmp  | ri A    | if ri = 0, set pc to [A] (else pc++)         |
+--------+---------+----------------------------------------------+
| storeN | A ri    | store N bytes of [ri] at memory address [A]u |
+--------+---------+----------------------------------------------+
| loadN  | ri A    | store N bytes of mem address [A]u in ri      |
+--------+---------+----------------------------------------------+
| answer | A       | stall or halt (ret. value is [A]u)           |
+-----------------------------------------------------------------+
| New instructions not present in TinyRAM:                        |
+-----------------------------------------------------------------+
| poisonN | ri A   | store N bytes of [ri] at address [A]u and poison them |
+-----------------------------------------------------------------+
| advise | ri A    | Receive advice in the range [0..A] into ri   |
+------------------------------------------------------------------
| sink   | rj A    | Signifies rj is written to a sink of label A |
+------------------------------------------------------------------
| taint  | rj A    | Taints rj with label A                       |
+-----------------------------------------------------------------+
| ext    | ??      | extensions                                   |
+-----------------------------------------------------------------+
| (answer) answer causes a stall (i.e. not increment pc) or a halt|
+-----------------------------------------------------------------+

= MicroASM: 
  Represents the MicroRAM assembly langues. It enhances MicroRAM with support for
  global variables and code labels.

-}
module MicroRAM
( -- * MicroRAM
  Instruction'(..),
  Instruction,
--  NamedBlock(NBlock),
  Program,
  Operand(..),

  MemWidth(..),
  pattern WWord,
  widthInt,

  ExtInstr(..),
  ExtValInstr(..),
  ExtAdviseInstr(..),

  pattern IstoreW,
  pattern IloadW,
  pattern IpoisonW,

  -- * MicroASM
  {-MAInstruction,
  MAProgram,
  MAOperand,

  -- * MicroIR
  MA2Instruction, -}

  -- * Words
  MWord,
  MInt,
  logWordBytes,
  wordBytes,
  wordBits,

  -- * Mappiung and Folding
  mapInstr, mapProg, mapInstrM,
  foldInstr,
  ) where

import Control.Monad.Identity
import Data.Bifunctor
import Data.Bits
import Data.Foldable
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics -- Helps testing

-- * The MicroRAM language(s)

-- | Phase: This language can be instantiated at different levels:
-- Pre: Indicates MicroAssembly where the program is made of
-- labeled blocks and Operands can be labels
-- Post: indicates Pure MicroRAM where the program is a list of instructions
-- at this level, labels have been removed.
-- FIXME: Post is now used in RTL and LTL, so change names like Pre->"labeled" and Post->"simple"
data Phase = Pre | Post
  deriving (Eq, Ord, Read, Show, Generic)


-- | Operands
-- TinyRAM instructions take immidiate values (constants) and registers
-- when an instruction allows either we denote it a (|A| in the paper).



-- Note that some code depends on these being in increasing order, via the
-- `Enum` instance.
data MemWidth = W1 | W2 | W4 | W8
    deriving (Eq, Ord, Enum, Read, Show)

widthInt :: MemWidth -> Int
widthInt w = case w of
    W1 -> 1
    W2 -> 2
    W4 -> 4
    W8 -> 8

data Operand regT wrdT where
  Reg :: regT -> Operand regT wrdT
  Const :: wrdT -> Operand regT wrdT
  deriving (Eq,Ord,Read,Show)

instance Bifunctor Operand where
  bimap regF _ (Reg r) = Reg $ regF r
  bimap _ conF (Const c) = Const $ conF c

data ExtInstr operand2 =
    XTrace Text [operand2]
  -- ^ Print the `Text` and all operand values.
  | XTraceStr operand2
  -- ^ Read a string from the pointer operand and print it.
  | XTraceExec operand2 [operand2]
  -- ^ Read a string from the first pointer operand, and print it along with
  -- the values of the remaining operands.
  | XFree operand2
  -- ^ Free the memory at the pointer operand.
  | XAccessValid operand2 operand2
  -- ^ Mark memory between the two operands as valid to access.
  | XAccessInvalid operand2 operand2
  -- ^ Mark memory between the two operands as invalid to access.
  | XStoreUnchecked operand2 operand2
  -- ^ Store the second operand at the address given by the first operand,
  -- bypassing memory safety checks.
  | XSnapshot
  -- ^ Save a copy of the entire machine state.
  | XCheck operand2 -- The PC of the instruction.
  -- ^ Take the latest snapshot (saved by Xrvsnapshot), run the RISC-V instruction pc, and check that the simulator's result matches the MicroRAM machine state. Notice that pc can be a program counter or directly the RISC-V instruction or both, whatever is easier.
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

data ExtValInstr operand2 =
    XLoadUnchecked operand2
  -- ^ Load a word from the pointer operand, bypassing memory safety checks.
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

data ExtAdviseInstr operand2 =
    XMalloc operand2
  -- ^ Allocate N bytes, returning the address of the new allocation.
  | XAdvisePoison operand2 operand2
  -- ^ Choose an address to poison between the two operands.
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

-- | TinyRAM Instructions
data Instruction' regT operand1 operand2 =
   -- Bit Operations
  Iand regT operand1 operand2     -- ^ compute bitwise AND of [rj] and [A] and store result in ri
  | Ior regT operand1 operand2    -- ^ compute bitwise OR of [rj] and [A] and store result in ri
  | Ixor regT operand1 operand2   -- ^ compute bitwise XOR of [rj] and [A] and store result in ri
  | Inot regT operand2        -- ^ compute bitwise NOT of [A] and store result in ri
  -- Integer Operations
  | Iadd regT operand1 operand2   -- ^ compute [rj]u + [A]u and store result in ri
  | Isub regT operand1 operand2   -- ^ compute [rj]u − [A]u and store result in ri
  | Imull regT operand1 operand2  -- ^ compute [rj]u × [A]u and store least significant bits of result in ri
  | Iumulh regT operand1 operand2 -- ^ compute [rj]u × [A]u and store most significant bits of result in ri
  | Ismulh regT operand1 operand2 -- ^ compute [rj]s × [A]s and store most significant bits of result in ri 
  | Iudiv regT operand1 operand2  -- ^ compute quotient of [rj ]u /[A]u and store result in ri
  | Iumod regT operand1 operand2  -- ^ compute remainder of [rj ]u /[A]u and store result in ri
  -- Shift operations
  | Ishl regT operand1 operand2   -- ^ shift [rj] by [A]u bits to the left and store result in ri
  | Ishr regT operand1 operand2   -- ^ shift [rj] by [A]u bits to the right and store result in ri
  -- Compare Operations          
  | Icmpe regT operand1 operand2       -- ^ “compare equal”
  | Icmpa regT operand1 operand2       -- ^ “compare above”, unsigned
  | Icmpae regT operand1 operand2      -- ^ “compare above or equal”, unsigned
  | Icmpg regT operand1 operand2       -- ^ “compare greater”, signed
  | Icmpge regT operand1 operand2      -- ^ “compare greater or equal”, signed
  -- Move operations             
  | Imov regT operand2                 -- ^  store [A] in ri
  | Icmov regT operand1 operand2       -- ^  iff rj<>0, store [A] in ri
  -- Jump operations             
  | Ijmp operand2                      -- ^  set pc to [A]
  | Icjmp operand1 operand2            -- ^  if rj<>0, set pc to [A] (else increment pc as usual)
  | Icnjmp operand1 operand2           -- ^  if rj = 0, set pc to [A] (else increment pc as usual)
  -- Memory operations           
  | Istore MemWidth operand2 operand1 -- ^  store [ri] at memory address [A]u
  | Iload MemWidth regT operand2 -- ^  store the content of memory address [A]u into ri 
  | Iread regT operand2       -- ^  if the [A]u-th tape has remaining words then consume the next word,
                              --  store it in ri, and set flag = 0; otherwise store 0W in ri and set flag = 1.
                              --  __To be removed__
  | Ianswer operand2          -- ^  stall or halt (and the return value is [A]u)
  -- Advice
  | Iadvise regT operand2     -- ^ load nondeterministic advice into ri
  -- Poison
  | Ipoison MemWidth operand2 operand1
  -- Dynamic taint tracking operations
  | Isink MemWidth operand1 operand2
  | Itaint MemWidth regT operand2
  -- Extensions
  | Iext (ExtInstr operand2)                -- ^ Custom instruction with no return value
  | Iextval regT (ExtValInstr operand2)     -- ^ Custom instruction, returning a value
  | Iextadvise regT operand2 (ExtAdviseInstr operand2) -- ^ Like `Iextval`, but gets serialized as `Iadvise`
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

pattern IstoreW :: operand2 -> operand1 -> Instruction' regT operand1 operand2
pattern IstoreW a b = Istore WWord a b
pattern IloadW :: regT -> operand2 -> Instruction' regT operand1 operand2
pattern IloadW a b = Iload WWord a b
pattern IpoisonW :: operand2 -> operand1 -> Instruction' regT operand1 operand2
pattern IpoisonW a b = Ipoison WWord a b

  
-- ** MicroRAM
type Instruction regT wrdT = Instruction' regT regT (Operand regT wrdT)

type Program r w = [Instruction r w]



-- * Instances and classes

-- ** Generics 

-- We derive generics for testing (to get Series in Smallcheck)
{- The following instance was generated by defining
@
data Operand'' regT wrdT = 
  Reg' regT
  | Const' wrd
 deriving (Generic)
@
and then running `ghc -ddump-deriv src/MicroRAM/MicroRAM.hs` plus the standard cleanup.
-}
instance Generic (Operand regT wrdT) where
  type Rep (Operand regT wrdT) =
      D1 ('MetaData
           "Operand"
           "MicroRAM.MicroRAM"
           "main"
           'False)
          (C1 ('MetaCons
                "Reg"
                'PrefixI
                'False)
               (S1 ('MetaSel
                     'Nothing
                     'NoSourceUnpackedness
                     'NoSourceStrictness
                     'DecidedLazy)
                    (Rec0 regT))
            :+:
            C1 ('MetaCons
                 "Const"
                 'PrefixI
                 'False)
               (S1
                 ('MetaSel
                   'Nothing
                   'NoSourceUnpackedness
                   'NoSourceStrictness
                   'DecidedLazy)
                 (Rec0 wrdT)))
  from x =
    M1 (case x of
          Reg g1 -> L1 (M1 (M1 (K1 g1)))
          Const g1 -> R1 (M1 (M1 (K1 g1))))
  to (M1 x) =
    case x of
      (L1 (M1 (M1 (K1 g1)))) -> Reg g1
      (R1 (M1 (M1 (K1 g1)))) -> Const g1
  

-- | MicroRAM machine word.  Some places still use a hardcoded word size
-- instead of being parametric.  In those places, we use `MWord` to distinguish
-- it from a general unsigned integer, and to avoid depending on the host
-- architecture's word size.
--
-- Note that the rest of the MicroRAM module *has* been parameterized, so
-- `MWord` should not be used here.
type MWord = Word64
type MInt = Int64

pattern WWord :: MemWidth
pattern WWord = W8

logWordBytes :: Int
logWordBytes = 3
wordBytes :: Int
wordBytes = 1 `shiftL` logWordBytes
wordBits :: Int
wordBits = wordBytes * 8


-- Mapping and traversing instructions
mapInstr ::  
  (regT -> regT')
  -> (operand1 -> operand1')
  -> (operand2 -> operand2')
  -> Instruction' regT operand1 operand2
  -> Instruction' regT' operand1' operand2'
mapInstr regF opF1 opF2 instr =
  runIdentity $ mapInstrM (lift regF) (lift opF1) (lift opF2) instr
  where lift f x = return $ f x

mapProg ::  
  (r1 -> r2)
  -> (w1 -> w2)
  -> Program r1 w1
  -> Program r2 w2
mapProg regF wF prog =
  (mapInstr regF regF opF) <$> prog
  where opF = bimap regF wF

mapInstrM :: Monad m =>  
  (regT -> m regT')
  -> (operand1 -> m operand1')
  -> (operand2 -> m operand2')
  -> Instruction' regT operand1 operand2
  -> m (Instruction' regT' operand1' operand2')
mapInstrM regF opF1 opF2 instr =
  case instr of             
  -- Bit Operations             
  Iand r1 op1 op2        -> Iand <$>  (regF r1) <*> (opF1 op1) <*> (opF2 op2)          
  Ior r1 op1 op2         -> Ior <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)     
  Ixor r1 op1 op2        -> Ixor <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
  Inot r1 op2            -> Inot <$> (regF r1) <*> (opF2 op2)             
  -- Integer Operations         
  Iadd r1 op1 op2        -> Iadd <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
  Isub r1 op1 op2        -> Isub <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
  Imull r1 op1 op2       -> Imull <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)   
  Iumulh r1 op1 op2      -> Iumulh <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)  
  Ismulh r1 op1 op2      -> Ismulh <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)  
  Iudiv r1 op1 op2       -> Iudiv <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)   
  Iumod r1 op1 op2       -> Iumod <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)   
  -- Shift operations                 
  Ishl r1 op1 op2        -> Ishl <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
  Ishr r1 op1 op2        -> Ishr <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
   -- Compare Operations           
  Icmpe r1 op1 op2       -> Icmpe <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)     
  Icmpa r1 op1 op2       -> Icmpa <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)     
  Icmpae r1 op1 op2      -> Icmpae <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
  Icmpg r1 op1 op2       -> Icmpg <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)     
  Icmpge r1 op1 op2      -> Icmpge <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)    
   -- Move operations              ->  -- Move operations              
  Imov r1 op2            -> Imov <$>  (regF r1) <*> (opF2 op2)               
  Icmov r1 op1 op2       -> Icmov <$> (regF r1) <*> (opF1 op1) <*> (opF2 op2)     
   -- Jump operations              ->  -- Jump operations              
  Ijmp op2               -> Ijmp <$>  (opF2 op2)                    
  Icjmp op1 op2          -> Icjmp <$>  (opF1 op1) <*> (opF2 op2)          
  Icnjmp op1 op2         -> Icnjmp <$> (opF1 op1) <*> (opF2 op2)         
  -- Memory operations             -> -- Memory operations            
  Istore w op2 op1       -> Istore w <$>  (opF2 op2) <*> (opF1 op1)         
  Iload w r1 op2         -> Iload w <$>  (regF r1) <*> (opF2 op2)              
  Iread r1 op2           -> Iread <$>  (regF r1) <*> (opF2 op2)              
  Ianswer op2            -> Ianswer <$>  (opF2 op2)                 
  -- Taint operations
  Isink wd r2 l          -> Isink wd <$> opF1 r2 <*> opF2 l
  Itaint wd r2 l         -> Itaint wd <$> regF r2 <*> opF2 l
  -- Advice                                    
  Iadvise r1 op2         -> Iadvise <$> (regF r1) <*> (opF2 op2)
  -- Poison                                    
  Ipoison w op2 op1      -> Ipoison w <$>  (opF2 op2) <*> (opF1 op1)      
  -- Extensions                            
  Iext ext               -> Iext <$> (mapM opF2 ext)
  Iextval dest ext       -> Iextval <$> (regF dest) <*> (mapM opF2 ext)
  Iextadvise dest op2 ext -> Iextadvise <$> (regF dest) <*> (opF2 op2) <*> (mapM opF2 ext)


foldInstr :: Monoid a =>
  (regT -> a)
  -> (operand1 -> a)
  -> (operand2 -> a)
  -> Instruction' regT operand1 operand2
  -> a
foldInstr regF opF1 opF2 instr = aggregateOps $ mapInstr regF opF1 opF2 instr

aggregateOps:: Monoid a => Instruction' a a a -> a 
aggregateOps instr =
  case instr of
    Iand r1 op1 op2        -> r1 <> op1 <> op2 
    Ior r1 op1 op2         -> r1 <> op1 <> op2
    Ixor r1 op1 op2        -> r1 <> op1 <> op2
    Inot r1 op2            -> r1        <> op2
    Iadd r1 op1 op2        -> r1 <> op1 <> op2
    Isub r1 op1 op2        -> r1 <> op1 <> op2
    Imull r1 op1 op2       -> r1 <> op1 <> op2
    Iumulh r1 op1 op2      -> r1 <> op1 <> op2
    Ismulh r1 op1 op2      -> r1 <> op1 <> op2
    Iudiv r1 op1 op2       -> r1 <> op1 <> op2
    Iumod r1 op1 op2       -> r1 <> op1 <> op2
    Ishl r1 op1 op2        -> r1 <> op1 <> op2
    Ishr r1 op1 op2        -> r1 <> op1 <> op2
    Icmpe r1 op1 op2       -> r1 <> op1 <> op2
    Icmpa r1 op1 op2       -> r1 <> op1 <> op2
    Icmpae r1 op1 op2      -> r1 <> op1 <> op2
    Icmpg r1 op1 op2       -> r1 <> op1 <> op2
    Icmpge r1 op1 op2      -> r1 <> op1 <> op2
    Imov r1 op2            -> r1        <> op2
    Icmov r1 op1 op2       -> r1 <> op1 <> op2
    Ijmp op2               ->              op2
    Icjmp op1 op2          ->       op1 <> op2
    Icnjmp op1 op2         ->       op1 <> op2
    Istore _ op2 op1       ->       op1 <> op2
    Iload _ r1 op2         -> r1        <> op2
    Iread r1 op2           -> r1        <> op2
    Ianswer op2            ->              op2
    Isink _ r2 l           -> r2 <> l
    Itaint _ r2 l          -> r2 <> l
    Iadvise r1 op2         -> r1        <> op2
    Ipoison _ op2 op1      ->       op1 <> op2
    Iext ext               -> fold ext
    Iextval dest ext       -> dest <> fold ext
    Iextadvise dest op2 ext -> dest <> op2 <> fold ext
