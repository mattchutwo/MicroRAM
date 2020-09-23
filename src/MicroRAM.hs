{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

+--------+---------+----------------------------------------------+--------------+
|  Instr | operands|                   effects                    |     flag     |
+========+=========+==============================================+==============+
| and    | ri rj A | bitwise AND of [rj] and [A] and store in ri  | result is 0W |
+--------+---------+----------------------------------------------+--------------+
| or     | ri rj A | bitwise OR of [rj] and [A] and store in ri   | result is 0W |
+--------+---------+----------------------------------------------+--------------+
| xor    | ri rj A | bitwise XOR of [rj] and [A] and store in ri  | result is 0W |
+--------+---------+----------------------------------------------+--------------+
| not    | ri A    | bitwise NOT of [A] and store result in ri    | result is 0W |
+--------+---------+----------------------------------------------+--------------+
| add    | ri rj A | [rj]u + [A]u and store result in ri          | overflow     |
+--------+---------+----------------------------------------------+--------------+
| sub    | ri rj A | [rj]u − [A]u and store result in ri          | borrow       |
+--------+---------+----------------------------------------------+--------------+
| mull   | ri rj A | [rj]u × [A]u, store least sign. bits in ri   | overflow     |
+--------+---------+----------------------------------------------+--------------+
| umulh  | ri rj A | [rj]u × [A]u, store most sign. bits in ri    | overflow     |
+--------+---------+----------------------------------------------+--------------+
| smulh  | ri rj A | [rj]s × [A]s, store most sign. bits in ri    | over/underf. |
+--------+---------+----------------------------------------------+--------------+
| udiv   | ri rj A | quotient of [rj]u/[A]u and store in ri       | [A]u = 0     |
+--------+---------+----------------------------------------------+--------------+
| umod   | ri rj A | remainder of [rj]u/[A]u and store in ri      | [A]u = 0     |
+--------+---------+----------------------------------------------+--------------+
| shl    | ri rj A | shift [rj] by [A]u bits left, store in ri    | MSB of [rj]  |
+--------+---------+----------------------------------------------+--------------+
| shr    | ri rj A | shift [rj] by [A]u bits right, store in ri   | LSB of [rj]  |
+--------+---------+----------------------------------------------+--------------+
| cmpe   | ri A    | none (“compare equal”)                       | [ri] = [A]   |
+--------+---------+----------------------------------------------+--------------+
| cmpa   | ri A    | none (“compare above”, unsigned)             | [ri]u > [A]u |
+--------+---------+----------------------------------------------+--------------+
| cmpae  | ri A    | none (“compare above or equal”, unsigned)    | [ri]u ≥ [A]u |
+--------+---------+----------------------------------------------+--------------+
| cmpg   | ri A    | none (“compare greater”, signed)             | [ri]s > [A]s |
+--------+---------+----------------------------------------------+--------------+
| cmpge  | ri A    | none (“compare greater or equal”, signed)    | [ri]s ≥ [A]s |
+--------+---------+----------------------------------------------+--------------+
| mov    | ri A    | store [A] in ri                              |              |
+--------+---------+----------------------------------------------+--------------+
| cmov   | ri A    | if flag = 1, store [A] in ri                 |              |
+--------+---------+----------------------------------------------+--------------+
| jmp    | A       | set pc to [A]                                |              |
+--------+---------+----------------------------------------------+--------------+
| cjmp   | A       | if flag = 1, set pc to [A] (else pc++)       |              |
+--------+---------+----------------------------------------------+--------------+
| cnjmp  | A       | if flag = 0, set pc to [A] (else pc++)       |              |
+--------+---------+----------------------------------------------+--------------+
| store  | A ri    | store [ri] at memory address [A]u            |              |
+--------+---------+----------------------------------------------+--------------+
| load   | ri A    | store content of mem address [A]u in ri      |              |
+--------+---------+----------------------------------------------+--------------+
| answer | A       | stall or halt (ret. value is [A]u)           | (2)          |
+--------------------------------------------------------------------------------+
| New instructions not present in TinyRAM:                                       |
+--------------------------------------------------------------------------------+
| poison | ri A    | store [ri] at address [A]u and poison it     |              |
+--------------------------------------------------------------------------------+
| advice | ri      | Receive advice to ri                         |              |
+--------------------------------------------------------------------------------+
| ext    | ??      | extensions                                   |              |
+--------------------------------------------------------------------------------+
| (answer) answer causes a stall (i.e., not increment pc) or a halt              |
|         (i.e., the computation stops); the choice between the two is undefined.|
+--------------------------------------------------------------------------------+

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

  -- * MicroASM
  {-MAInstruction,
  MAProgram,
  MAOperand,

  -- * MicroIR
  MA2Instruction, -}

  -- * Words
  MWord,
  ) where

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



data Operand regT wrdT where
  Reg :: regT -> Operand regT wrdT
  Const :: wrdT -> Operand regT wrdT
  deriving (Eq,Ord,Read,Show)

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
  | Icmpe operand1 operand2       -- ^ none (“compare equal”)
  | Icmpa operand1 operand2       -- ^ none (“compare above”, unsigned)
  | Icmpae operand1 operand2      -- ^ none (“compare above or equal”, unsigned)
  | Icmpg operand1 operand2       -- ^ none (“compare greater”, signed)
  | Icmpge operand1 operand2      -- ^ none (“compare greater or equal”, signed)
  -- Move operations             
  | Imov regT operand2        -- ^  store [A] in ri
  | Icmov regT operand2       -- ^  iff lag=1, store [A] in ri
  -- Jump operations             
  | Ijmp operand2             -- ^  set pc to [A]
  | Icjmp operand2            -- ^  if flag = 1, set pc to [A] (else increment pc as usual)
  | Icnjmp operand2           -- ^  if flag = 0, set pc to [A] (else increment pc as usual)
  -- Memory operations           
  | Istore operand2 operand1      -- ^  store [ri] at memory address [A]u
  | Iload regT operand2       -- ^  store the content of memory address [A]u into ri 
  | Iread regT operand2       -- ^  if the [A]u-th tape has remaining words then consume the next word,
                              --  store it in ri, and set flag = 0; otherwise store 0W in ri and set flag = 1.
                              --  __To be removed__
  | Ianswer operand2          -- ^  stall or halt (and the return value is [A]u)
  -- Advice
  | Iadvise regT              -- ^ load nondeterministic advice into ri
  -- Poison
  | Ipoison operand2 operand1
  -- Extensions
  | Iext Text [operand2]      -- ^ Custom instruction with no return value
  | Iextval Text regT [operand2] -- ^ Custom instruction, returning a value
  | Iextadvise Text regT [operand2] -- ^ Like `Iextval`, but gets serialized as `Iadvise`
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

  
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
