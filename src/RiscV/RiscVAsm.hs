module RiscV.RiscVAsm where

-- import Data.Bits
import Data.Word (Word64)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, elements)


data Instr
  = Instr32 InstrRV32I -- ^ 32bit Base Integer Instruction Set
  | Instr64 InstrRV64I -- ^ 64bit Base Integer Instruction Set
  | InstrM  InstrExtM  -- ^ Standard Extension for Integer Multiplication and Division


{-- $immediates

--}


{- | Modifiers: The RISC-V assembler supports following modifiers for relocatable addresses used in RISC-V instruction operands. These expressions should be resolved during linking.
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

{-- RiscV Assembly accepts arbitrary operations on immediates, to be resolved at link time. Normally these expressions are small with, at most, a modifier, a binary operation a symbola and a constant: e.g. @%hi(__iob+8)@

--}
data ImmOp =
  ImmAnd
  | ImmOr
  | ImmAdd
  | ImmMinus
  | ImmMult
  | ImmDiv
  deriving (Show, Eq, Ord)

instance Arbitrary ImmOp where
  arbitrary =
    oneof $ pure <$>
    [ImmAnd
    , ImmOr
    , ImmAdd
    , ImmMinus
    , ImmMult
    , ImmDiv]
  
data Imm =
  ImmNumber Word64
  | ImmSymbol String
  | ImmMod Modifier Imm
  | ImmBinOp Imm ImmOp Imm 
  deriving (Show, Eq, Ord)

instance Arbitrary Imm where
  arbitrary = oneof $ 
    [ ImmNumber  <$> arbitrary
    , ImmSymbol  <$> arbitrary
    , ImmMod     <$> arbitrary <*> arbitrary
    , ImmBinOp   <$> arbitrary <*> arbitrary <*> arbitrary ]
  
{-- $regs
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
--}
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

{-- $RV32I
32 bit Base Integer Instruction Set, Version 2.1
--}

{- | Branch conditions

+---------------------+---------------------------+------------------------------------+
|       Format        |           Name            |             Pseudocode             |
+=====================+===========================+====================================+
| BEQ rs1,rs2,offset  | Branch Equal              | if rs1 = rs2 then pc ← pc + offset |
+---------------------+---------------------------+------------------------------------+
| BNE rs1,rs2,offset  | Branch Not Equal          | if rs1 ≠ rs2 then pc ← pc + offset |
+---------------------+---------------------------+------------------------------------+
| BLT rs1,rs2,offset  | Branch Less Than          | if rs1 < rs2 then pc ← pc + offset |
+---------------------+---------------------------+------------------------------------+
| BGE rs1,rs2,offset  | Branch Greater than Equal | if rs1 ≥ rs2 then pc ← pc + offset |
+---------------------+---------------------------+------------------------------------+
| BLTU rs1,rs2,offset | Branch Less Than Unsigned | if rs1 < rs2 then pc ← pc + offset |
+---------------------+---------------------------+------------------------------------+
| BGEU rs1,rs2,offset | Branch Greater than Equal | if rs1 ≥ rs2 then pc ← pc + offset |
|                     | Unsigned                  |                                    |
+---------------------+---------------------------+------------------------------------+
-}
data BranchCond 
  = BEQ 
  | BNE 
  | BLT 
  | BGE 
  | BLTU
  | BGEU

instance Arbitrary BranchCond where
  arbitrary = 
    oneof $ pure <$>
    [ BEQ 
    , BNE 
    , BLT 
    , BGE 
    , BLTU
    , BGEU]


{- | BinopI: Binary operations with immediates. Has no Sub. For substraction,
 add a negative immediate. 
-}
data BinopI
  = ADDI       
  | SLTI       
  | SLTIU      
  | XORI       
  | ORI        
  | ANDI       
  | SLLI       
  | SRLI       
  | SRAI
  
-- | Binop: Binary operations with two registers.
data Binop
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
               
data MemOp
  = LB 
  | LH 
  | LW 
  | LBU
  | LHU
  | SB 
  | SH 
  | SW
  
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

type Offset = Word

{- | InstrRV32I

[Reference](https://mark.theis.site/riscv/)

+---------------------+-----------------------+------------------------------+
|       Format        |         Name          |          Pseudocode          |
+=====================+=======================+==============================+
| LUI rd,imm          | Load Upper Immediate  | rd ← imm                     |
+---------------------+-----------------------+------------------------------+
| AUIPC rd,offset     | Add Upper Imm. to PC  | rd ← pc + offset             |
+---------------------+-----------------------+------------------------------+
| JAL rd,offset       | Jump and Link         | "rd ← pc + length(inst)      |
| .                   | .                     | pc ← pc + offset"            |
+---------------------+-----------------------+------------------------------+
| JALR rd,rs1,offset  | Jump and Link Reg     | "rd ← pc + length(inst)      |
| .                   | .                     | pc ← (rs1 + offset) ∧ -2"    |
+---------------------+-----------------------+------------------------------+
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
| ADDI rd,rs1,imm     | Add Immediate         | rd ← rs1 + sx(imm)           |
+---------------------+-----------------------+------------------------------+
| SLTI rd,rs1,imm     | Set Less Than Imm.    | rd ← sx(rs1) < sx(imm)       |
+---------------------+-----------------------+------------------------------+
| SLTIU rd,rs1,imm    | Set Less Than Imm.    | rd ← ux(rs1) < ux(imm)       |
| .                   | Unsigned              |                              |
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

    -- | Integer Register-Immediate Instructions
  | LUI   Reg Imm           
  | AUIPC Reg Offset
  | ImmBinop BinopI Reg Reg Imm
    -- | Memory Instructions
  | MemInstr MemOp Reg Offset Reg
  
    -- | Integer Register-Register Instructions
  | RegBinop Binop Reg Reg Reg
  
    -- | Synchronisation Instructions (Fences)
  | FENCE SetOrdering
  | FENCEI            

{-- $RV64I
64bit Base Integer Instruction Set, Version 2.1

--}
  
data InstrRV64I = Bogus64


{-- $ExtM
Standard Extension for Integer Multiplication and Division (Version 2.0)

--}
  
data InstrExtM = BogusM
