module RiscV.RiscVAsm
  ( 
  -- * Operators
  -- ** Immediates
  -- $immediate
    Imm (..)
  , ImmOp (..)
  , Modifier (..)
  -- ** Offsets
  , Offset
  -- ** Registers
  , Reg (..)

  -- * Risc V Assembly
  -- $instr
  , LineOfRiscV(..)
  , Instr (..)

  -- ** RV32I
  -- $rv32i
  , BranchCond (..)
  , Binop32I(..)
  , Binop32(..)
  , MemOp32(..)
  , SetOrdering(..)
  , InstrRV32I(..)

  -- ** RV64I
  -- $rv64i
  , MemOp64(..)
  , Binop64I(..)
  , Binop64(..)
  , InstrRV64I(..)

  -- ** RV32M
  -- $ext32M
  , InstrExt32M(..)

  -- ** RV64M
  -- $ext64M
  , InstrExt64M(..)

  -- ** Pseudoinstructions
  -- $pseudo
  , AbsolutePseudo(..)
  , MemOpKind(..)
  , UnaryPseudo(..)
  , CMovPseudo(..)
  , BranchZPseudo(..)
  , BranchPseudo(..)
  , JumpPseudo(..)
  , PseudoInstr(..)
   ) where

-- import Data.Bits
import Data.Word (Word64)
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

{- | The RISC-V assembler supports following modifiers for relocatable
   addresses used in RISC-V instruction operands. These expressions
   should be resolved during linking. [Reference](https://sourceware.org/binutils/docs/as/RISC_002dV_002dModifiers.html)
-}
data Modifier =
  ModLo                -- ^ @%lo(symbol)@ The low 12 bits of absolute
                       -- address for symbol.
  | ModHi              -- ^ @%hi(symbol)@ The high 20 bits of absolute
                       -- address for symbol.
  | ModPcrel_lo        -- ^ @%pcrel_lo(label)@ The low 12 bits of
                       -- relative address between pc and symbol. The
                       -- symbol is related to the high part instruction
                       -- which is marked by label.
  | ModPcrel_hi        -- ^ @%pcrel_hi(symbol)@ The high 20 bits of
                       -- relative address between pc and symbol.
  | ModGot_pcrel_hi    -- ^ @%got_pcrel_hi(symbol)@ The high 20 bits of
                       -- relative address between pc and the GOT entry
                       -- of symbol.
  | ModTprel_add       -- ^ @%tprel_add(symbol)@ This is used purely to
                       -- associate the R_RISCV_TPREL_ADD relocation for
                       -- TLS relaxation. This one is only valid as the
                       -- fourth operand to the normally 3 operand add
                       -- instruction.
  | ModTprel_lo        -- ^ @%tprel_lo(symbol)@ The low 12 bits of
                       -- relative address between tp and symbol.
  | ModTprel_hi        -- ^ @%tprel_hi(symbol)@ The high 20 bits of
                       -- relative address between tp and symbol.
  | ModTls_ie_pcrel_hi -- ^ @%tls_ie_pcrel_hi(symbol)@ The high 20
                       -- bits of relative address between pc and GOT
                       -- entry.
  | ModTls_gd_pcrel_hi -- ^ @%tls_gd_pcrel_hi(symbol)@ The high 20
                       -- bits of relative address between pc and GOT
                       -- entry.
  deriving (Show, Eq, Ord)

instance Arbitrary Modifier where
  arbitrary =
    oneof $ pure <$>
      [ ModLo
      , ModHi
      , ModPcrel_lo
      , ModPcrel_hi
      , ModGot_pcrel_hi
      , ModTprel_add
      , ModTprel_lo
      , ModTprel_hi
      , ModTls_ie_pcrel_hi
      , ModTls_gd_pcrel_hi]


{- $immediate
   RiscV Assembly accepts some operations on immediates to be
   resolved at link time. As far as I can tell, these expressions are
   not well documented. Normally these expressions are small with, at
   most, a modifier, a binary operation a symbol and a constant:
   e.g. @%hi(__iob+8)@.
-}

{- | We allow arbitrary expressions for Immediates, including any number
   of modifiers and binary operators constants and variables. This is
   more permissive than most programs I have seen, but sice I haven't
   found standards for these expressions i chose to do the most
   general.
-}
data Imm =
  ImmNumber Word64
  | ImmSymbol String
  | ImmMod Modifier Imm
  | ImmBinOp ImmOp Imm Imm 
  deriving (Show, Eq, Ord)

data ImmOp =
  ImmAnd     
  | ImmOr
  | ImmAdd
  | ImmMinus
  --- | ImmMult
  --- | ImmDiv
  deriving (Show, Eq, Ord)

instance Arbitrary ImmOp where
  arbitrary =
    oneof $ pure <$>
    [ImmAnd
    , ImmOr
    , ImmAdd
    , ImmMinus
    -- , ImmMult
    -- , ImmDiv
    ]
  


instance Arbitrary Imm where
  arbitrary = oneof $ 
    [ ImmNumber  <$> arbitrary
    , ImmSymbol  <$> arbitrary
    , ImmMod     <$> arbitrary <*> arbitrary
    , ImmBinOp   <$> arbitrary <*> arbitrary <*> arbitrary ]
  
{- |
Registers:

+----------+------------------------------------+--------+
| Register |            Description             | Saver  |
+==========+====================================+========+
| x0       | hardwired zero                     | -      |
+----------+------------------------------------+--------+
| x1       | return address                     | Caller |
+----------+------------------------------------+--------+
| x2       | stack pointer                      | Callee |
+----------+------------------------------------+--------+
| x3       | global pointer                     | -      |
+----------+------------------------------------+--------+
| x4       | thread pointer                     | -      |
+----------+------------------------------------+--------+
| x5-7     | temporary registers                | Caller |
+----------+------------------------------------+--------+
| x8       | saved register / frame pointer     | Callee |
+----------+------------------------------------+--------+
| x9       | saved register                     | Callee |
+----------+------------------------------------+--------+
| x10-11   | function arguments / return values | Caller |
+----------+------------------------------------+--------+
| x12-17   | function arguments                 | Caller |
+----------+------------------------------------+--------+
| x18-27   | saved registers                    | Callee |
+----------+------------------------------------+--------+
| x28-31   | temporary registers                | Caller |
+----------+------------------------------------+--------+
-}
data Reg
  = X0   -- ^ hardwired zero     
  | X1   -- ^ return address     
  | X2   -- ^ stack pointer      
  | X3   -- ^ global pointer     
  | X4   -- ^ thread pointer     
  | X5   -- ^ temporary registers
  | X6
  | X7     
  | X8   -- ^ frame pointer / saved register (s0=fp)            
  | X9                                          
  | X10  -- ^ function arguments / return values
  | X11                                         
  | X12  -- ^ function arguments                
  | X13                                         
  | X14                                         
  | X15                                         
  | X16                                         
  | X17                                         
  | X18  -- ^ saved registers                   
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28  -- ^ temporary registers
  | X29
  | X30
  | X31
  deriving (Show, Eq, Ord)

instance Arbitrary Reg where
  arbitrary =
    oneof $ pure <$>
    [X0, X1 , X2 , X3 , X4 , X5 , X6,X7 , X8 , X9 , X10, X11,
     X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22,
     X23, X24, X25, X26, X27, X28, X29, X30, X31 ]


{- | Offsets are just a wrapper for `Imm`. However, in RiscV, they are
   used in different contexts and have different sizes. For example,
   some instructions accept offsets and others immediates. We don't
   check for the size difference and only enforce the context by using
   a type synonym.
-}
type Offset = Imm



{- $instr
The RiscV Assembly Language presented here supports the following modules:

1. 32bit Base Integer Instruction Set                      
2. 64bit Base Integer Instruction Set                      
3. RV32M Standard Extension for Integer Multiply and Divide
4. RV64M Standard Extension for Integer Multiply and Divide

We also support all pseudoinstructions defined in the Set Manual.

The RISC-V Instruction Set Manual ( [Version 20191213, December 13, 2019]("https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf") ).

-}

{- | An RiscV assembly file contains labels, directives and instructions. We ignore comments and empty lines.

-}
data LineOfRiscV =
    Label       String
  | Directive   String
  | Instruction Instr
  deriving (Show, Eq, Ord)


data Instr
  = Instr32I    InstrRV32I   -- ^ 32bit Base Integer Instruction Set
  | Instr64I    InstrRV64I   -- ^ 64bit Base Integer Instruction Set
  | Instr32M    InstrExt32M  -- ^ RV32M Standard Extension for Integer Multiply and Divide
  | Instr64M    InstrExt64M  -- ^ RV64M Standard Extension for Integer Multiply and Divide
  | InstrPseudo PseudoInstr  -- ^ Pseudoinstructions
  deriving (Show, Eq, Ord)




{- $rv32i
RV32I 32 bit Base Integer Instruction Set, Version 2.1

-}

{- | Branch conditions

+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| BEQ rs1,rs2,off     | Branch Equal          | if rs1 = rs2 : pc ← pc + off |
+---------------------+-----------------------+------------------------------+
| BNE rs1,rs2,off     | Branch Not Equal      | if rs1 ≠ rs2 : pc ← pc + off |
+---------------------+-----------------------+------------------------------+
| BLT rs1,rs2,off     | Branch Less Than      | if rs1 < rs2 : pc ← pc + off |
+---------------------+-----------------------+------------------------------+
| BGE rs1,rs2,off     | Branch Greater        | if rs1 ≥ rs2 : pc ← pc + off |
| .                   | than or Equal         |                              |
+---------------------+-----------------------+------------------------------+
| BLTU rs1,rs2,off    | Branch Less Than      | if rs1 < rs2 : pc ← pc + off |
| .                   | Unsigned              |                              |
+---------------------+-----------------------+------------------------------+
| BGEU rs1,rs2,offset | Branch Greater than   | if rs1 ≥ rs2 : pc ← pc + off |
| .                   | or Equal Unsigned     |                              |
+---------------------+-----------------------+------------------------------+
-}
data BranchCond 
  = BEQ 
  | BNE 
  | BLT 
  | BGE 
  | BLTU
  | BGEU
  deriving (Show, Eq, Ord)
instance Arbitrary BranchCond where
  arbitrary = 
    oneof $ pure <$>
    [ BEQ 
    , BNE 
    , BLT 
    , BGE 
    , BLTU
    , BGEU]


{- | Binary operations with immediates. Has no @SUB@. For substraction,
 add a negative immediate.

Integer Register-Immediate Instructions

+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| ADDI rd,rs1,imm     | Add Immediate         | rd ← rs1 + sx(imm)           |
+---------------------+-----------------------+------------------------------+
| SLTI rd,rs1,imm     | Set Less Than Imm.    | rd ← sx(rs1) < sx(imm)       |
+---------------------+-----------------------+------------------------------+
| SLTIU rd,rs1,imm    | Set Less Than Imm.    | rd ← ux(rs1) < ux(imm)       |
|                     | Unsigned              |                              |
+---------------------+-----------------------+------------------------------+
| XORI rd,rs1,imm     | Xor Immediate         | rd ← ux(rs1) ⊕ ux(imm)       |
+---------------------+-----------------------+------------------------------+
| ORI rd,rs1,imm      | Or Immediate          | rd ← ux(rs1) ∨ ux(imm)       |
+---------------------+-----------------------+------------------------------+
| ANDI rd,rs1,imm     | And Immediate         | rd ← ux(rs1) ∧ ux(imm)       |
+---------------------+-----------------------+------------------------------+
| SLLI rd,rs1,imm     | Shift Left Log. Imm   | rd ← ux(rs1) « ux(imm)       |
+---------------------+-----------------------+------------------------------+
| SRLI rd,rs1,imm     | Shift Right Log. Imm  | rd ← ux(rs1) » ux(imm)       |
+---------------------+-----------------------+------------------------------+
| SRAI rd,rs1,imm     | Shift Right Arith Imm | rd ← sx(rs1) » ux(imm)       |
+---------------------+-----------------------+------------------------------+

* @sx@ = Signed Extension

* @ux@ = Unsigned Extension

* @imm@ are immediate

* @rs1@, @rs2@ are read registers

* @rd@ is the return register

@LUI@ and @AUIPC@ are not binary and are defined directly in 'InstrRV32I'

-}
data Binop32I
  = ADDI       
  | SLTI       
  | SLTIU      
  | XORI       
  | ORI        
  | ANDI       
  | SLLI       
  | SRLI       
  | SRAI
  deriving (Show, Eq, Ord)

  
{- | Binary Register-Register Instructions

+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| ADD rd,rs1,rs2      | Add                   | rd ← sx(rs1) + sx(rs2)       |
+---------------------+-----------------------+------------------------------+
| SUB rd,rs1,rs2      | Subtract              | rd ← sx(rs1) - sx(rs2)       |
+---------------------+-----------------------+------------------------------+
| SLL rd,rs1,rs2      | Shift Left Logical    | rd ← ux(rs1) « rs2           |
+---------------------+-----------------------+------------------------------+
| SLT rd,rs1,rs2      | Set Less Than         | rd ← sx(rs1) < sx(rs2)       |
+---------------------+-----------------------+------------------------------+
| SLTU rd,rs1,rs2     | Set Less Than Unsig.  | rd ← ux(rs1) < ux(rs2)       |
+---------------------+-----------------------+------------------------------+
| XOR rd,rs1,rs2      | Xor                   | rd ← ux(rs1) ⊕ ux(rs2)       |
+---------------------+-----------------------+------------------------------+
| SRL rd,rs1,rs2      | Shift Right Logical   | rd ← ux(rs1) » rs2           |
+---------------------+-----------------------+------------------------------+
| SRA rd,rs1,rs2      | Shift Right Arith.    | rd ← sx(rs1) » rs2           |
+---------------------+-----------------------+------------------------------+
| OR rd,rs1,rs2       | Or                    | rd ← ux(rs1) ∨ ux(rs2)       |
+---------------------+-----------------------+------------------------------+
| AND rd,rs1,rs2      | And                   | rd ← ux(rs1) ∧ ux(rs2)       |
+---------------------+-----------------------+------------------------------+

-}
data Binop32
  = ADD 
  | SUB 
  | SLL 
  | SLT 
  | SLTU
  | XOR 
  | SRL 
  | SRA 
  | OR
  | AND 
  deriving (Show, Eq, Ord)
{- | Memory operations 32I

+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| LB rd,offset(rs1)   | Load Byte             | rd ← s8[rs1 + offset]        |
+---------------------+-----------------------+------------------------------+
| LH rd,offset(rs1)   | Load Half             | rd ← s16[rs1 + offset]       |
+---------------------+-----------------------+------------------------------+
| LW rd,offset(rs1)   | Load Word             | rd ← s32[rs1 + offset]       |
+---------------------+-----------------------+------------------------------+
| LBU rd,offset(rs1)  | Load Byte Unsigned    | rd ← u8[rs1 + offset]        |
+---------------------+-----------------------+------------------------------+
| LHU rd,offset(rs1)  | Load Half Unsigned    | rd ← u16[rs1 + offset]       |
+---------------------+-----------------------+------------------------------+
| SB rs2,offset(rs1)  | Store Byte            | u8[rs1 + offset] ← rs2       |
+---------------------+-----------------------+------------------------------+
| SH rs2,offset(rs1)  | Store Half            | u16[rs1 + offset] ← rs2      |
+---------------------+-----------------------+------------------------------+
| SW rs2,offset(rs1)  | Store Word            | u32[rs1 + offset] ← rs2      |
+---------------------+-----------------------+------------------------------+

-}
data MemOp32
  = LB 
  | LH 
  | LW 
  | LBU
  | LHU
  | SB 
  | SH 
  | SW
  deriving (Show, Eq, Ord)


{- | 

The entire list of instructions can be found in this [reference](https://mark.theis.site/riscv/)


=== Jump Instructions
+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| JAL rd,offset       | Jump and Link         | "rd ← pc + length(inst)      |
| .                   | .                     | pc ← pc + offset"            |
+---------------------+-----------------------+------------------------------+
| JALR rd,rs1,offset  | Jump and Link Reg     | "rd ← pc + length(inst)      |
| .                   | .                     | pc ← (rs1 + offset) ∧ -2"    |
+---------------------+-----------------------+------------------------------+

=== (Unary) Integer Register-Immediate Instructions
+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| LUI rd,imm          | Load Upper Immediate  | rd ← imm                     |
+---------------------+-----------------------+------------------------------+
| AUIPC rd,offset     | Add Upper Imm. to PC  | rd ← pc + offset             |
+---------------------+-----------------------+------------------------------+

=== Fences
+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| FENCE pred,succ     | Fence                 |                              |
+---------------------+-----------------------+------------------------------+
| FENCE.I             | Fence Instruction     |                              |
+---------------------+-----------------------+------------------------------+

-}

data InstrRV32I =
    -- | Jump Instructions
    JAL  Reg Offset
  | JALR Reg Reg Offset
  
     -- | Branch Instructions
  | BranchInstr BranchCond Reg Reg Offset
  
    -- | Memory Instructions
  | MemInstr32 MemOp32 Reg Offset Reg

    -- | Integer Register-Immediate Instructions
  | LUI   Reg Imm           
  | AUIPC Reg Offset
  | ImmBinop32 Binop32I Reg Reg Imm
  
    -- | Integer Register-Register Instructions
  | RegBinop32 Binop32 Reg Reg Reg
  
    -- | Synchronisation Instructions (Fences)
  | FENCE SetOrdering
  | FENCEI            
  deriving (Show, Eq, Ord)

{- | Memory operations 64I

+--------------------+------------------------------+--------------------------+
|       Format       |             Name             |        Pseudocode        |
+====================+==============================+==========================+
| LWU rd,offset(rs1) | Load Word Unsigned           | rd ← u32[rs1 + offset]   |
+--------------------+------------------------------+--------------------------+
| LD rd,offset(rs1)  | Load Double                  | rd ← u64[rs1 + offset]   |
+--------------------+------------------------------+--------------------------+
| SD rs2,offset(rs1) | Store Double                 | u64[rs1 + offset] ← rs2  |
+--------------------+------------------------------+--------------------------+

-}


{- | Fences can optionally further restrict the predecessor set and/or
the successor set to a smaller set of memory accesses in order to
provide some speedup. Specifically, fences have PR, PW, SR, and SW
bits which restrict the predecessor and/or successor sets. The
predecessor set includes loads (resp. stores) if and only if PR
(resp. PW) is set.
-}
data SetOrdering = SetOrdering
  { predRead  :: Bool
  , predWrite :: Bool
  , succRead  :: Bool
  , succWrite :: Bool
  } deriving (Show, Eq, Ord)


{- $rv64i
RV64I Base Integer Instruction Set
-}


data MemOp64
 = LWU | LD | SD
 deriving (Show, Eq, Ord)

{- |  Integer Register-Immediate Instructions 64I

+------------------+---------------------------------------+--------------------------+
|      Format      |                 Name                  |        Pseudocode        |
+==================+=======================================+==========================+
| ADDIW rd,rs1,imm | Add Immediate Word                    | rd ← s32(rs1) + imm      |
+------------------+---------------------------------------+--------------------------+
| SLLIW rd,rs1,imm | Shift Left Logical Immediate Word     | rd ← s32(u32(rs1) « imm) |
+------------------+---------------------------------------+--------------------------+
| SRLIW rd,rs1,imm | Shift Right Logical Immediate Word    | rd ← s32(u32(rs1) » imm) |
+------------------+---------------------------------------+--------------------------+
| SRAIW rd,rs1,imm | Shift Right Arithmetic Immediate Word | rd ← s32(rs1) » imm      |
+------------------+---------------------------------------+--------------------------+

The codes @SLLI@, @SRLI@ and @SRAI@ are ommited since they are already
covered in RV32I. The context will make clear the semantics of the
instruction.
-}

data Binop64I
  =  ADDIW   
  | SLLIW   
  | SRLIW   
  | SRAIW
  deriving (Show, Eq, Ord)
{- | Integer Register-Register Instructions 64I

+--------------------+------------------------------+--------------------------+
|       Format       |             Name             |        Pseudocode        |
+====================+==============================+==========================+
| ADDW rd,rs1,rs2    | Add Word                     | rd ← s32(rs1) + s32(rs2) |
+--------------------+------------------------------+--------------------------+
| SUBW rd,rs1,rs2    | Subtract Word                | rd ← s32(rs1) - s32(rs2) |
+--------------------+------------------------------+--------------------------+
| SLLW rd,rs1,rs2    | Shift Left Log Word          | rd ← s32(u32(rs1) « rs2) |
+--------------------+------------------------------+--------------------------+
| SRLW rd,rs1,rs2    | Shift Right Log Word         | rd ← s32(u32(rs1) » rs2) |
+--------------------+------------------------------+--------------------------+
| SRAW rd,rs1,rs2    | Shift Right Arith. Word      | rd ← s32(rs1) » rs2      |
+--------------------+------------------------------+--------------------------+
-}
 

data Binop64
  = ADDW       
  | SUBW     
  | SLLW     
  | SRLW     
  | SRAW    
  deriving (Show, Eq, Ord)

{- | $RV64I
64bit Base Integer Instruction Set, Version 2.1
-}
data InstrRV64I 
  = MemInstr64 MemOp64 Reg Offset Reg -- ^ Memory operations
  | ImmBinop64 Binop64I Reg Reg Imm  -- ^ Integer Register-Immediate Instructions
  | RegBinop64 Binop64 Reg Reg Reg -- ^ Integer Register-Register Instructions
  deriving (Show, Eq, Ord)

{- $ext32M
RV32M Standard Extension for Integer Multiply and Divide (Version 2.0)

+-------------------+-----------------------+---------------------------------+
|      Format       |         Name          |           Pseudocode            |
+===================+=======================+=================================+
| MUL rd,rs1,rs2    | Mult                  | rd ← ux(rs1) × ux(rs2)          |
+-------------------+-----------------------+---------------------------------+
| MULH rd,rs1,rs2   | Mult High Sig. Sig.   | rd ← (sx(rs1) × sx(rs2)) » xlen |
+-------------------+-----------------------+---------------------------------+
| MULHSU rd,rs1,rs2 | Mult High Sig. Unsig  | rd ← (sx(rs1) × ux(rs2)) » xlen |
+-------------------+-----------------------+---------------------------------+
| MULHU rd,rs1,rs2  | Mult High Unsig Unsig | rd ← (ux(rs1) × ux(rs2)) » xlen |
+-------------------+-----------------------+---------------------------------+
| DIV rd,rs1,rs2    | Divide Signed         | rd ← sx(rs1) ÷ sx(rs2)          |
+-------------------+-----------------------+---------------------------------+
| DIVU rd,rs1,rs2   | Divide Unsigned       | rd ← ux(rs1) ÷ ux(rs2)          |
+-------------------+-----------------------+---------------------------------+
| REM rd,rs1,rs2    | Remainder Signed      | rd ← sx(rs1) mod sx(rs2)        |
+-------------------+-----------------------+---------------------------------+
| REMU rd,rs1,rs2   | Remainder Unsigned    | rd ← ux(rs1) mod ux(rs2)        |
+-------------------+-----------------------+---------------------------------+

-}
  
data InstrExt32M
  = MUL    Reg Reg Reg
  | MULH   Reg Reg Reg
  | MULHSU Reg Reg Reg
  | MULHU  Reg Reg Reg
  | DIV    Reg Reg Reg
  | DIVU   Reg Reg Reg
  | REM    Reg Reg Reg
  | REMU   Reg Reg Reg
  deriving (Show, Eq, Ord)
{- Is Instr Ext Mult better defined as folows?
data MulOp
  = MUL    
  | MULH   
  | MULHSU 
  | MULHU  
  | DIV    
  | DIVU   
  | REM    
  | REMU

data InstrExtM = InstrExtM MulOp Reg Reg Reg

-}


{- $ext64M
RV64M Standard Extension for Integer Multiply and Divide

+------------------+-------------------------+----------------------------+
|      Format      |          Name           |         Pseudocode         |
+==================+=========================+============================+
| MULW rd,rs1,rs2  | Multiple Word           | rd ← u32(rs1) × u32(rs2)   |
+------------------+-------------------------+----------------------------+
| DIVW rd,rs1,rs2  | Divide Signed Word      | rd ← s32(rs1) ÷ s32(rs2)   |
+------------------+-------------------------+----------------------------+
| DIVUW rd,rs1,rs2 | Divide Unsigned Word    | rd ← u32(rs1) ÷ u32(rs2)   |
+------------------+-------------------------+----------------------------+
| REMW rd,rs1,rs2  | Remainder Signed Word   | rd ← s32(rs1) mod s32(rs2) |
+------------------+-------------------------+----------------------------+
| REMUW rd,rs1,rs2 | Remainder Unsigned Word | rd ← u32(rs1) mod u32(rs2) |
+------------------+-------------------------+----------------------------+

-}

data InstrExt64M
  = MULW    Reg Reg Reg
  | DIVW    Reg Reg Reg
  | DIVUW   Reg Reg Reg
  | REMW    Reg Reg Reg
  | REMUW   Reg Reg Reg
  deriving (Show, Eq, Ord)



{- $pseudo

The tables show most the pseudoinstructions documented in the Instruction Set Manual, and marks the ones we support. 
We do not yet support pseudoinstructions for control and status
register (CSR) or single-precision operations over floats

-}

{- |

Absolute instructions. The base instructions use pc-relative addressing, so the linker subtracts pc from symbol to get delta. The linker adds delta[11] to the 20-bit high part, counteracting sign extension of the 12-bit low part.

+-----------------------------+----------------------------------------+--------------------------------+-----------+
|      pseudoinstruction      |           Base Instruction(s)          |             Meaning            | Supported |
+=============================+========================================+================================+===========+
| @la rd, symbol (non-PIC)@   | @auipc rd, delta[31 : 12] + delta[11]@ | Load absolute address,         |           |
|                             +----------------------------------------+                                |           |
|                             | @addi rd, rd, delta[11:0]            @ | where delta = symbol − pc      |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @la rd, symbol (PIC)@       | @auipc rd, delta[31 : 12] + delta[11]@ | Load absolute address,         |           |
|                             +----------------------------------------+                                |           |
| .                           | @l{w|d} rd, rd, delta[11:0]          @ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @lla rd, symbol@            | @auipc rd, delta[31 : 12] + delta[11]@ | Load local address,            |           |
|                             +----------------------------------------+                                |           |
| .                           | @addi rd, rd, delta[11:0]            @ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @l{b|h|w|d} rd, symbol@     | @auipc rd, delta[31 : 12] + delta[11]@ | Load global                    |           |
|                             +----------------------------------------+                                |           |
| .                           | @l{b|h|w|d} rd, delta[11:0](rd)      @ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @s{b|h|w|d} rd, symbol, rt@ | @s{b|h|w|d} rd, delta[11:0](rt)@       | Store global                   |           |
|                             +----------------------------------------+                                |           |
| .                           | @auipc rt, delta[31 : 12] + delta[11]@ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @fl{w|d} rd, symbol, rt@    | @auipc rt, delta[31 : 12] + delta[11]@ | Floating-point load global     |   No      |
|                             +----------------------------------------+                                |           |
| .                           | @fl{w|d} rd, delta[11:0](rt)         @ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
| @fs{w|d} rd, symbol, rt@    | @auipc rt, delta[31 : 12] + delta[11]@ | Floating-point store global    |   No      |
|                             +----------------------------------------+                                |           |
|                             | @fs{w|d} rd, delta[11:0](rt)         @ |                                |           |
+-----------------------------+----------------------------------------+--------------------------------+-----------+
-}

data AbsolutePseudo
  = PseudoLA Reg Imm
  | PseudoLLA Reg Imm
  | PseudoLoad MemOpKind Reg Imm 
  | PseudoStore MemOpKind Reg Imm Reg
  deriving (Show, Eq, Ord)

data MemOpKind = MemByte | MemHalf | MemWord | MemDouble
  deriving (Show, Eq, Ord)

{- |
Unary register operations and @nop@.

+---------------------+---------------------+----------------------------+-----------+
|  pseudoinstruction  | Base Instruction(s) |          Meaning           | Supported |
+=====================+=====================+============================+===========+
| mv rd, rs           | addi rd, rs, 0      | Copy register              |           |
+---------------------+---------------------+----------------------------+-----------+
| not rd, rs          | xori rd, rs, -1     | One’s complement           |           |
+---------------------+---------------------+----------------------------+-----------+
| neg rd, rs          | sub rd, x0, rs      | Two’s complement           |           |
+---------------------+---------------------+----------------------------+-----------+
| negw rd, rs         | subw rd, x0, rs     | Two’s complement word      |           |
+---------------------+---------------------+----------------------------+-----------+
| sext.w rd, rs       | addiw rd, rs, 0     | Sign extend word           |           |
+---------------------+---------------------+----------------------------+-----------+

-}

data UnaryPseudo
  = MOV 
  | NOT
  | NEG
  | NEGW
  | SEXTW
  deriving (Show, Eq, Ord)

{- |
Conditional move instructions

+---------------------+---------------------+----------------------------+-----------+
|  pseudoinstruction  | Base Instruction(s) |          Meaning           | Supported |
+=====================+=====================+============================+===========+
| seqz rd, rs         | sltiu rd, rs, 1     | Set if = zero              |           |
+---------------------+---------------------+----------------------------+-----------+
| snez rd, rs         | sltu rd, x0, rs     | Set if <> zero             |           |
+---------------------+---------------------+----------------------------+-----------+
| sltz rd, rs         | slt rd, rs, x0      | Set if < zero              |           |
+---------------------+---------------------+----------------------------+-----------+
| sgtz rd, rs         | slt rd, x0, rs      | Set if > zero              |           |
+---------------------+---------------------+----------------------------+-----------+

-}


data CMovPseudo
  = SEQZ -- ^ @sltiu rd, rs, 1@      Set if = zero 
  | SNEZ -- ^ @sltu rd, x0, rs@      Set if <> zero
  | SLTZ -- ^ @slt rd, rs, x0@       Set if < zero 
  | SGTZ -- ^ @slt rd, x0, rs@       Set if > zero 
  deriving (Show, Eq, Ord)

{- |
Alternative Branching with zero

+---------------------+---------------------+----------------------------+-----------+
|  pseudoinstruction  | Base Instruction(s) |          Meaning           | Supported |
+=====================+=====================+============================+===========+
| beqz rs, offset     | beq rs, x0, offset  | Branch if = zero           |           |
+---------------------+---------------------+----------------------------+-----------+
| bnez rs, offset     | bne rs, x0, offset  | Branch if <> zero          |           |
+---------------------+---------------------+----------------------------+-----------+
| blez rs, offset     | bge x0, rs, offset  | Branch if ≤ zero           |           |
+---------------------+---------------------+----------------------------+-----------+
| bgez rs, offset     | bge rs, x0, offset  | Branch if ≥ zero           |           |
+---------------------+---------------------+----------------------------+-----------+
| bltz rs, offset     | blt rs, x0, offset  | Branch if < zero           |           |
+---------------------+---------------------+----------------------------+-----------+
| bgtz rs, offset     | blt x0, rs, offset  | Branch if > zero           |           |
+---------------------+---------------------+----------------------------+-----------+

-}


data BranchZPseudo
  = BEQZ -- ^ @beq rs, x0, offset @   Branch if = zero        
  | BNEZ -- ^ @bne rs, x0, offset @    Branch if <> zero      
  | BLEZ -- ^ @bge x0, rs, offset @    Branch if ≤ zero       
  | BGEZ -- ^ @bge rs, x0, offset @    Branch if ≥ zero       
  | BLTZ -- ^ @blt rs, x0, offset @    Branch if < zero       
  | BGTZ -- ^ @blt x0, rs, offset @    Branch if > zero
  deriving (Show, Eq, Ord)

{- |
Alternative Branching

+---------------------+---------------------+----------------------------+-----------+
|  pseudoinstruction  | Base Instruction(s) |          Meaning           | Supported |
+=====================+=====================+============================+===========+
| bgt rs, rt, offset  | blt rt, rs, offset  | Branch if >                |           |
+---------------------+---------------------+----------------------------+-----------+
| ble rs, rt, offset  | bge rt, rs, offset  | Branch if ≤                |           |
+---------------------+---------------------+----------------------------+-----------+
| bgtu rs, rt, offset | bltu rt, rs, offset | Branch if >, unsigned      |           |
+---------------------+---------------------+----------------------------+-----------+
| bleu rs, rt, offset | bgeu rt, rs, offset | Branch if ≤, unsigned      |           |
+---------------------+---------------------+----------------------------+-----------+

-}
  
data BranchPseudo
  = BGT  -- ^ @blt rt, rs, offset @    Branch if >            
  | BLE  -- ^ @bge rt, rs, offset @    Branch if ≤            
  | BGTU -- ^ @bltu rt, rs, offset@    Branch if >, unsigned  
  | BLEU -- ^ @bgeu rt, rs, offset@    Branch if ≤, unsigned  
  deriving (Show, Eq, Ord)


{- | Alternative jump instructions. We only distinguish jump and link or
just jump. Both variants are implemented in `PseudoInstr` @ret@,
@call@ and @tail@ are implemented directly in `PseudoInstr`

+-------------------+-----------------------------------------+------------------------------------+-----------+
| pseudoinstruction | Base Instruction(s)                     | Meaning                            | Supported |
+===================+=========================================+====================================+===========+
| j offset          | jal x0, offset                          | Jump                               |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| jal offset        | jal x1, offset                          | Jump and link                      |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| jr rs             | jalr x0, 0(rs)                          | Jump register                      |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| jalr rs           | jalr x1, 0(rs)                          | Jump and link register             |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+


-}

data JumpPseudo
  = JPseudo       -- ^ Just jump
  | JLinkPseudo   -- ^ Jump and link
  
  deriving (Show, Eq, Ord)

{- |

Function call/return, total fence, @nop@ and unary immediate

+-------------------+-----------------------------------------+------------------------------------+-----------+
| pseudoinstruction | Base Instruction(s)                     | Meaning                            | Supported |
+===================+=========================================+====================================+===========+
| ret               | jalr x0, 0(x1)                          | Return from subroutine             |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
|                   | auipc x1, offset[31 : 12] + offset[11]  |                                    |           |
|                   +-----------------------------------------+                                    +-----------+
| call offset       | jalr x1, offset[11:0](x1)               | Call far-away subroutine           |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
|                   | auipc x6, offset[31 : 12] + offset[11]  | Tail call far-away subroutine      |           |
|                   +-----------------------------------------+                                    +-----------+
| tail offset       | jalr x0, offset[11:0](x6)               |                                    |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fence             | fence iorw, iorw                        | Fence on all memory and I/O        |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| nop               | addi x0, x0, 0                          | No operation                       |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| li rd, immediate  | Myriad sequences [sic]                  | Load immediate                     |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+

-}

data PseudoInstr
  -- | Function call/return instructions
  = RetPI
  | CallPI Offset
  | TailPI Offset
  -- | Total Fence
  | FencePI
  -- | Unary immediate
  | LiPI Reg Imm
  -- | nop
  | NopPI
  -- | Absolute load/store
  | AbsolutePI AbsolutePseudo
  -- | Unary register Pseudoinstructions
  | UnaryPI UnaryPseudo Reg Reg
  -- | Conditional Moves
  | CMovPI CMovPseudo Reg Reg
  -- | Alternative branches
  | BranchZPI BranchZPseudo Reg Offset
  | BranchPI BranchPseudo Reg Reg Offset
  -- | Alternative Jumps
  | JmpImmPI JumpPseudo Offset
  | JmpRegPI JumpPseudo Reg
  deriving (Show, Eq, Ord)




{-

Tables for pseudoinstructions not supported.

+-------------------+-----------------------------------------+------------------------------------+-----------+
| pseudoinstruction | Base Instruction(s)                     | Meaning                            | Supported |
+===================+=========================================+====================================+===========+
| rdinstret[h] rd   | csrrs rd, instret[h], x0                | Read instructions-retired counter  |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| rdcycle[h] rd     | csrrs rd, cycle[h], x0                  | Read cycle counter                 |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| rdtime[h] rd      | csrrs rd, time[h], x0                   | Read real-time clock               |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrr rd, csr      | csrrs rd, csr, x0                       | Read CSR                           |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrw csr, rs      | csrrw x0, csr, rs                       | Write CSR                          |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrs csr, rs      | csrrs x0, csr, rs                       | Set bits in CSR                    |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrc csr, rs      | csrrc x0, csr, rs                       | Clear bits in CSR                  |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrwi csr, imm    | csrrwi x0, csr, imm                     | Write CSR, immediate               |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrsi csr, imm    | csrrsi x0, csr, imm                     | Set bits in CSR, immediate         |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| csrci csr, imm    | csrrci x0, csr, imm                     | Clear bits in CSR, immediate       |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| frcsr rd          | csrrs rd, fcsr, x0                      | Read FP control/status register    |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fscsr rd, rs      | csrrw rd, fcsr, rs                      | Swap FP control/status register    |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fscsr rs          | csrrw x0, fcsr, rs                      | Write FP control/status register   |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| frrm rd           | csrrs rd, frm, x0                       | Read FP rounding mode              |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fsrm rd, rs       | csrrw rd, frm, rs                       | Swap FP rounding mode              |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fsrm rs           | csrrw x0, frm, rs                       | Write FP rounding mode             |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| frflags rd        | csrrs rd, fflags,                       | Read FP exception flags            |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fsflags rd, rs    | csrrw rd, fflags,                       | Swap FP exception flags            |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+
| fsflags rs        | csrrw x0, fflags,                       | Write FP exception flags           |           |
+-------------------+-----------------------------------------+------------------------------------+-----------+


+---------------------+---------------------+----------------------------+-----------+
|  pseudoinstruction  | Base Instruction(s) |          Meaning           | Supported |
+=====================+=====================+============================+===========+
| fmv.s rd, rs        | fsgnj.s rd, rs,     | Copy single-precision reg  |           |
+---------------------+---------------------+----------------------------+-----------+
| fabs.s rd, rs       | fsgnjx.s rd, rs, rs | Single-precision abs value |           |
+---------------------+---------------------+----------------------------+-----------+
| fneg.s rd, rs       | fsgnjn.s rd, rs, rs | Single-precision negate    |           |
+---------------------+---------------------+----------------------------+-----------+
| fmv.d rd, rs        | fsgnj.d rd, rs, rs  | Copy double-precision reg  |           |
+---------------------+---------------------+----------------------------+-----------+
| fabs.d rd, rs       | fsgnjx.d rd, rs, rs | Double-precision abs value |           |
+---------------------+---------------------+----------------------------+-----------+
| fneg.d rd, rs       | fsgnjn.d rd, rs, rs | Double-precision negate    |           |
+---------------------+---------------------+----------------------------+-----------+

-}
