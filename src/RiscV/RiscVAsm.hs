{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RiscV.RiscVAsm
  ( 
  -- * Operands
  -- $operands
    
  -- ** Immediates
  -- $immediate
    
    Imm (..)
  , ImmOp (..)
  , Modifier (..)
  -- ** Offsets
  , Offset
  -- ** Registers
  , Reg (..), newReg

  
  -- * Assembler Directives
  -- ** Sections
  -- $section
  , Flag(..)
  , SectionType(..)
  , FlagArg

  -- ** Directives
  -- $directives
  , Directive(..)
  , VisibilityDir(..)
  , DirTypes(..)
  , Option(..)
  , AttTag(..)
  , CFIsectionOpt(..)
  , Opcodes(..)
  , CFIDirectives(..)
  , EmitDir(..)
  
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

  -- ** Aliases
  -- $alias
  , AliasInstr(..)

  -- ** Traverseble
  , TraversableOp
  , traverseOp
  , instrTraverseImmM
  , instrTraverseImm
  , instrTraverseRegM
   ) where

-- import Data.Bits
import Data.Word (Word8, Word16, Word64)
import Data.Functor.Identity (runIdentity, Identity(..))
import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Compiler.LazyConstants (LazyConst(..))

import Debug.Trace

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

{- $operands

   An expression specifies an address or numeric value using
   operands. Whitespace may precede and/or follow an expression.

   The result of an expression must be an absolute number, or else an
   offset into a particular section. If an expression is not absolute,
   and there is not enough information when as sees the expression to
   know its section, a second pass over the source program might be
   necessary to interpret the expression—but the second pass is
   currently not implemented. as aborts with an error message in this
   situation.

-}

{- $immediate

   Immediates are expressions that can be resolved to
   constatnts after linking
-}

{- | RiscV Assembly accepts some operations. Normally these expressions
   are small with, at most, a modifier, a binary operation a symbol
   and a constant: e.g. @%hi(__iob+8)@.

   We will allow arbitrary expressions for Immediates, including any
   number of modifiers and binary operands constants and
   variables. This is more permissive than most programs I have seen,
   but sice I haven't found standards for these expressions, we might
   as well support all of them

-}

data Imm =
  ImmNumber Word64
  | ImmSymbol String
  | ImmLazy (LazyConst Word64) -- Only to go inside Xrvcheck
  | ImmMod Modifier Imm
  | ImmBinOp ImmOp Imm Imm 
  deriving (Show, Eq)

data ImmOp =
  ImmAnd     
  | ImmOr
  | ImmAdd
  | ImmMinus
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

{- $registers

RISC-V has 32 (or 16 in the embedded variant) integer registers. For
now, we only support the 32 variant, but expect to support the 16
variant and, perhaps, a variable option soon.

The first integer register is a zero register, and the remainder are
general-purpose registers. A store to the zero register has no effect,
and a read always provides 0. Using the zero register as a placeholder
makes for a simpler instruction set.

Control and status registers exist, but user-mode programs can access
only those used for performance measurement and floating-point
management.

Register table:

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
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Arbitrary Reg where
  arbitrary =
    oneof $ pure <$>
    [X0, X1 , X2 , X3 , X4 , X5 , X6,X7 , X8 , X9 , X10, X11,
     X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22,
     X23, X24, X25, X26, X27, X28, X29, X30, X31 ]

-- We need an extra register, to translate RISCV to MRAMAsm
newReg :: Int
newReg = 1 + (fromEnum $ (maxBound::Reg)::Int)

{- | Offsets are just a wrapper for `Imm`. However, in RiscV, they are
   used in different contexts and have different sizes. For example,
   some instructions accept offsets and others immediates, some take
   one byte, others can take full words. We don't check for the size
   difference and only enforce the context by using a type synonym.
-}
type Offset = Imm



{- $directives

Assembler directives are directions to the assembler to take some action or change a setting

+--------------+--------------------------------+------------------------------------+
| Directive    | Arguments                      | Description                        |
+==============+================================+====================================+
| .align       | integer                        | align to power of 2                |
|              |                                | (alias for .p2align)               |
+--------------+--------------------------------+------------------------------------+
| .file        | filename                       | emit filename FILE                 |
|              |                                | LOCAL symbol table                 |
+--------------+--------------------------------+------------------------------------+
| .comm        | symbol_name,size,align         | emit common object to .bss section |
+--------------+--------------------------------+------------------------------------+
| .common      | symbol_name,size,align         | emit common object to .bss section |
+--------------+--------------------------------+------------------------------------+
| .ident       | string                         | accepted for source compatibility  |
+--------------+--------------------------------+------------------------------------+
| .section     | name [flags] [type][flag_args] | emit section (if not present,      |
|              |                                | default .text) and make current    |
|              |                                |(format specific to ELF Version)    |
+--------------+--------------------------------+------------------------------------+
| .size        | symbol, expression             | accepted for source compatibility  |
+--------------+--------------------------------+------------------------------------+
| .text        |                                | emit .text section (if not         |
|              |                                | present) and make current          |
+--------------+--------------------------------+------------------------------------+
| .data        |                                | emit .data section (if not         |
|              |                                | present) and make current          |
+--------------+--------------------------------+------------------------------------+
| .rodata      |                                | emit .rodata section (if not       |
|              |                                | present) and make current          |
+--------------+--------------------------------+------------------------------------+
| .bss         |                                | emit .bss section (if not          |
|              |                                | present) and make current          |
+--------------+--------------------------------+------------------------------------+
| .string      | string                         | emit string                        |
+--------------+--------------------------------+------------------------------------+
| .asciz       | string                         | emit string (alias for .string)    |
+--------------+--------------------------------+------------------------------------+
| .equ         | name, value                    | constant definition                |
+--------------+--------------------------------+------------------------------------+
| .macro       | name arg1 [, argn]             | begin macro definition             |
|              |                                | \argname to substitute             |
+--------------+--------------------------------+------------------------------------+
| .endm        |                                | end macro definition               |
+--------------+--------------------------------+------------------------------------+
| .type        | symbol, @function              | accepted for source compat.        |
+--------------+--------------------------------+------------------------------------+
| .option      | {rvc,norvc,pic,nopic,push,pop} | RISC-V options                     |
+--------------+--------------------------------+------------------------------------+
| .p2align     | p2,[pad_val=0],max             | align to power of 2                |
+--------------+--------------------------------+------------------------------------+
| .balign      | b,[pad_val=0]                  | byte align                         |
+--------------+--------------------------------+------------------------------------+
| .zero        | integer                        | zero bytes                         |
+--------------+--------------------------------+------------------------------------+
| .variant_cc  | symbol_name                    | annotate the symbol with           |
|              |                                | variant calling convention         |
+--------------+--------------------------------+------------------------------------+
| .attribute   | tag, vaslue                    | Set the object attribute           |
|              |                                | tag to value.                      |
+--------------+--------------------------------+------------------------------------+
| .sleb128     | expression                     | signed little endian base 128      |
|              |                                | , DWARF                            |
+--------------+--------------------------------+------------------------------------+
| .uleb128     | expression                     | unsigned little endian             |
|              |                                | base 128, DWARF                    |
+--------------+--------------------------------+------------------------------------+

-}

{- |

The directive @type@ is always followed by "@function" or "@object"

-}

data DirTypes
  = DTFUNCTION
  | DTOBJECT
  deriving (Show, Eq, Ord)




data Directive
  = ALIGN       Integer                 -- ^ Align to power of 2 (alias for .p2align)
  | FILE        String                  -- ^ Emit filename FILE LOCAL symbol table
  | Visibility  VisibilityDir String    -- ^ Emit symbol_name to symbol table or overrides it's visibility
  | COMM        String Integer Integer  -- ^ Emit common object to .bss section
  | COMMON      String Integer Integer  -- ^ Emit common object to .bss section
  | IDENT       String                  -- ^ Accepted for source compatibility
  | SIZE        String Imm              -- ^ Accepted for source compatibility
  | TEXT                                -- ^ Emit .text section (if not present) and make current
  | DATA                                -- ^ Emit .data section (if not present) and make current
  | RODATA                              -- ^ Emit .rodata section (if not present) and make current
  | BSS                                 -- ^ Emit .bss section (if not present) and make current
  | STRING      String                  -- ^ Emit string
  | ASCIZ       String                  -- ^ Emit string (alias for .string)
  | EQU         String Word             -- ^ Constant definition
  | TYPE        String DirTypes         -- ^ Accepted for source compatibility
  | OPTION      Option                  -- ^ RISC-V options
  | BALIGN      Integer (Maybe Integer) -- ^ Byte align
  | ZERO        Integer                 -- ^ Zero bytes
  | VARIANT_CC  String                  -- ^ Annotate the symbol with variant calling convention
  | SLEB128     Imm                     -- ^ signed little endian base 128, DWARF
  | ULEB128     Imm                     -- ^ unsigned little endian base 128, DWARF
  | MACRO       String String [String]  -- ^ Begin macro definition \argname to substitute
  | ENDM                                -- ^ End macro definition
  | ADDRSIG                             -- ^ This section is used to
                                        --   mark symbols as
                                        --   address-significant. [See
                                        --   reference](https://llvm.org/docs/Extensions.html#sht-llvm-addrsig-section-address-significance-table)
  | ADDRSIG_SYM Imm
  | ATTRIBUTE   AttTag  (Either Integer String) 
  | P2ALIGN
    Integer                             -- ^ Align to this power of 2
    (Maybe Integer)                     -- ^ Padding value (default 0 or nop)
    (Maybe Integer)                     -- ^ Max number of padding (no padding if exceeded)
  
  | SECTION                             -- ^ Emit section (if not present, default .text) and make current
    String
    [Flag]
    (Maybe SectionType)
    [FlagArg]
  | DirEmit EmitDir [Imm]               -- ^ Emits a value at the current position
  | CFIDirectives CFIDirectives         -- ^ Control Flow Integrity
  deriving (Show, Eq)


data AttTag
  = Tag_RISCV_arch
  | Tag_RISCV_stack_align
  | Tag_RISCV_unaligned_access
  | Tag_RISCV_priv_spec
  | Tag_RISCV_priv_spec_minor
  | Tag_RISCV_priv_spec_revision
  | Tag_number Integer
  deriving (Show, Eq, Ord)


{- | Visibility directives

+--------------+--------------------------------+------------------------------------+
| Directive    | Arguments                      | Description                        |
+==============+================================+====================================+
| .globl       | symbol_name                    | emit symbol_name to symbol         |
|              |                                | table (scope GLOBAL)               |
+--------------+--------------------------------+------------------------------------+
| .local       | symbol_name                    | emit symbol_name to symbol         |
|              |                                | table (scope LOCAL)                |
+--------------+--------------------------------+------------------------------------+
| .weak        | symbol_name                    | emit symbol_name to symbol         |
|              |                                | table (visibility WEAK)            |
+--------------+--------------------------------+------------------------------------+
| .hidden      | symbol_name                    | This directive overrides the       |
|              |                                | named symbols default visibility   |
+--------------+--------------------------------+------------------------------------+
| .internal    | symbol_name                    | This directive overrides the       |
|              |                                | named symbols default visibility   |
+--------------+--------------------------------+------------------------------------+
| .protected   | symbol_name                    | This directive overrides the       |
|              |                                | named symbols default visibility   |
+--------------+--------------------------------+------------------------------------+

-}

data VisibilityDir
  = GLOBL    
  | LOCAL    
  | WEAK     
  | HIDDEN   
  | INTERNAL 
  | PROTECTED
 deriving (Show, Eq, Ord)


-- | Modifies RISC-V specific assembler options inline with the
-- assembly code. This is used when particular instruction sequences
-- must be assembled with a specific set of options.
data Option 
  -- | Enables or disables the generation of compressed
  -- instructions. Instructions are opportunistically compressed by
  -- the RISC-V assembler when possible, but sometimes this behavior
  -- is not desirable, especially when handling alignments.
  = RVC | NORVC

  -- | Enables or disables position-independent code
  -- generation. Unless you really know what you’re doing, this should
  -- only be at the top of a file.
  | PIC | NOPIC
  
  -- | Pushes or pops the current option stack. These should be used
  -- whenever changing an option in line with assembly code in order
  -- to ensure the user’s command-line options are respected for the
  -- bulk of the file being assembled.
  | PUSH | POP
  
  -- | Enables or disables relaxation. The RISC-V assembler and linker
  -- opportunistically relax some code sequences, but sometimes this
  -- behavior is not desirable.
  | RELAX | NORELAX
  deriving (Show, Eq, Ord)

{- $section
   Roughly, a section is a range of addresses, with no gaps;
   all data “in” those addresses is treated the same for some
   particular purpose. For example there may be a “read only” section.
   
   The linker @ld@ reads many object files (partial programs) and
   combines their contents to form a runnable program. When as emits
   an object file, the partial program is assumed to start at address
   0. ld assigns the final addresses for the partial program, so that
   different partial programs do not overlap. This is actually an
   oversimplification, but it suffices to explain how as uses
   sections.
   
   @ld@ moves blocks of bytes of your program to their run-time
   addresses. These blocks slide to their run-time addresses as rigid
   units; their length does not change and neither does the order of
   bytes within them. Such a rigid unit is called a section. Assigning
   run-time addresses to sections is called relocation. It includes
   the task of adjusting mentions of object-file addresses so they
   refer to the proper run-time addresses. For the H8/300, and for the
   Renesas / SuperH SH, as pads sections if needed to ensure they end
   on a word (sixteen bit) boundary.
   
   An object file written by as has at least three sections, any of
   which may be empty. These are named text, data and bss sections.

   Use the .section directive to assemble the following code into a
   section named name. This directive is only supported for targets
   that actually support arbitrarily named sections; on a.out targets,
   for example, it is not accepted, even with a standard a.out section
   name.
-}

data Flag
  = Flag_a   -- ^ is allocatable
  | Flag_d   -- ^ is a GNU_MBIND section
  | Flag_e   -- ^ is excluded from executable and shared library.
  | Flag_o   -- ^ references a symbol defined in another section (the linked-to section) in the same file.
  | Flag_w   -- ^ is writable
  | Flag_x   -- ^ is executable
  | Flag_M   -- ^ is mergeable
  | Flag_S   -- ^ contains zero terminated strings
  | Flag_G   -- ^ is a member of a section group
  | Flag_T   -- ^ is used for thread-local-storage
  | Flag_QM  -- ^ is a member of the previously-current section’s group, if any
  | Flag_R   -- ^ retained section (apply SHF_GNU_RETAIN to prevent linker garbage collection, GNU ELF extension)
  | Flag_number Word8 -- ^ a numeric value indicating the bits to be set in the ELF section header’s flags field. Note - if one or more of the alphabetic characters described above is also included in the flags field, their bit values will be ORed into the resulting value.

  -- - |Flat_target TargetFlag -- ^ some targets extend this list with their own flag characters
  deriving (Show, Eq, Ord)


{- | The optional type argument may contain one of the following constants:
-}
data SectionType
  = PROGBITS              -- ^ contains data
  | NOBITS                -- ^ does not contain data (i.e., section only occupies space)
  | NOTE                  -- ^ contains data which is used by things other than the program
  | INIT_ARRAY            -- ^ contains an array of pointers to init functions
  | FINI_ARRAY            -- ^ contains an array of pointers to finish functions
  | PREINIT_ARRAY         -- ^ contains an array of pointers to pre-init functions
  | TypeNum Word8         -- ^ a numeric value to be set as the ELF section header’s type field.
  
  -- -| TargetType TargetType -- ^ some targets extend this list with their own types
  deriving (Show, Eq, Ord)

type FlagArg = Either Word16 String
  
-- | If section_list is .eh_frame, .eh_frame is emitted, if
-- section_list is .debug_frame, .debug_frame is emitted. To emit both
-- use .eh_frame, .debug_frame. The default if this directive is not
-- used is .cfi_sections .eh_frame.
data CFIsectionOpt = CFIsectionOpt
  { eh_frame :: Bool, debug_frame :: Bool }
  deriving (Show, Eq, Ord)

-- | Compact unwind opcodes to be used for the current function
data Opcodes = OCNotSupported
  deriving (Show, Eq, Ord)

{- | CFI directives

These directives manage the Control Flow Integrity. Clang and GCC insert these directives often (e.g. I see them in grit)

Reference : [CFI directives](https://sourceware.org/binutils/docs/as/CFI-directives.html)
-}
data CFIDirectives =
  CFI_SECTIONS CFIsectionOpt
    -- ^ .cfi_sections may be used to specify whether CFI directives
    -- should emit .eh_frame section and/or .debug_frame section. If
    -- section_list is .eh_frame, .eh_frame is emitted, if section_list is
    -- .debug_frame, .debug_frame is emitted. To emit both use .eh_frame,
    -- .debug_frame. The default if this directive is not used is
    -- .cfi_sections .eh_frame.
    --
    --On targets that support compact unwinding tables these can be
    --generated by specifying .eh_frame_entry instead of .eh_frame.
    --
    -- Some targets may support an additional name, such as .c6xabi.exidx
    -- which is used by the target.
    --
    -- The .cfi_sections directive can be repeated, with the same or
    -- different arguments, provided that CFI generation has not yet
    -- started. Once CFI generation has started however the section list
    -- is fixed and any attempts to redefine it will result in an error.

  | CFI_STARTPROC Bool
    -- ^ .cfi_startproc is used at the beginning of each function that
    -- should have an entry in .eh_frame. It initializes some internal
    -- data structures. Don’t forget to close the function by
    -- .cfi_endproc.  Unless .cfi_startproc is used along with parameter
    -- simple it also emits some architecture dependent initial CFI
    -- instructions.
  
  | CFI_ENDPROC
    -- ^ .cfi_endproc is used at the end of a function where it closes
    -- its unwind entry previously opened by .cfi_startproc, and emits it
    -- to .eh_frame.
  
  | CFI_PERSONALITY Word8 (Either Word8 String)
    -- ^ .cfi_personality defines personality routine and its
    -- encoding. encoding must be a constant determining how the
    -- personality should be encoded. If it is 255 (DW_EH_PE_omit),
    -- second argument is not present, otherwise second argument should
    -- be a constant or a symbol name. When using indirect encodings,
    -- the symbol provided should be the location where personality can
    -- be loaded from, not the personality routine itself. The default
    -- after .cfi_startproc is .cfi_personality 0xff, no personality
    -- routine.
    
  | CFI_PERSONALITY_ID String
    -- ^ cfi_personality_id defines a personality routine by its index as
    -- defined in a compact unwinding format. Only valid when generating
    -- compact EH frames (i.e. with .cfi_sections eh_frame_entry.
    
  | CFI_FDE_DATA [Opcodes]
    -- ^ cfi_fde_data is used to describe the compact unwind opcodes to be
    -- used for the current function. These are emitted inline in the
    -- .eh_frame_entry section if small enough and there is no LSDA, or in
    -- the .gnu.extab section otherwise. Only valid when generating
    -- compact EH frames (i.e. with .cfi_sections eh_frame_entry.
  
  | CFI_LSDA Word8 (Either Int String)
    -- ^ .cfi_lsda defines LSDA and its encoding. encoding must be a
    -- constant determining how the LSDA should be encoded. If it is 255
    -- (DW_EH_PE_omit), the second argument is not present, otherwise the
    -- second argument should be a constant or a symbol name. The default
    -- after .cfi_startproc is .cfi_lsda 0xff, meaning that no LSDA is
    -- present.
  
  | CFI_INLINE_LSDA Int
    -- ^ .cfi_inline_lsda marks the start of a LSDA data section and
    -- switches to the corresponding .gnu.extab section. Must be preceded
    -- by a CFI block containing a .cfi_lsda directive. Only valid when
    -- generating compact EH frames (i.e. with .cfi_sections
    -- eh_frame_entry.  The table header and unwinding opcodes will be
    -- generated at this point, so that they are immediately followed by
    -- the LSDA data. The symbol referenced by the .cfi_lsda directive
    -- should still be defined in case a fallback FDE based encoding is
    -- used. The LSDA data is terminated by a section directive.  The
    -- optional align argument specifies the alignment required. The
    -- alignment is specified as a power of two, as with the .p2align
    -- directive.
    
  | CFI_DEF_CFA Reg Word8
    -- ^ .cfi_def_cfa defines a rule for computing CFA as: take address
    -- from register and add offset to it.

  | CFI_DEF_CFA_REGISTER Reg
    -- ^ .cfi_def_cfa_register modifies a rule for computing CFA. From now
    -- on register will be used instead of the old one. Offset remains the
    -- same.

  | CFI_DEF_CFA_OFFSET Word8
    -- ^ .cfi_def_cfa_offset modifies a rule for computing CFA. Register
    -- remains the same, but offset is new. Note that it is the absolute
    -- offset that will be added to a defined register to compute CFA
    -- address.

  | CFI_ADJUST_CFA_OFFSET Word8
    -- ^ Same as .cfi_def_cfa_offset but offset is a relative value that
    -- is added/subtracted from the previous offset.

  | CFI_OFFSET Reg Word8
    -- ^ Previous value of register is saved at offset offset from CFA.

  | CFI_VAL_OFFSET Reg Word8
    -- ^ Previous value of register is CFA + offset.

  | CFI_REL_OFFSET Reg Word8
    -- ^ Previous value of register is saved at offset offset from the
    -- current CFA register. This is transformed to .cfi_offset using the
    -- known displacement of the CFA register from the CFA. This is often
    -- easier to use, because the number will match the code it’s
    -- annotating.

  | CFI_REGISTER Reg Reg
    -- ^ Previous value of register1 is saved in register register2.

  | CFI_RESTORE Reg
    -- ^ .cfi_restore says that the rule for register is now the same as
    -- it was at the beginning of the function, after all initial
    -- instruction added by .cfi_startproc were executed.

  | CFI_UNDEFINED Reg
    -- ^ From now on the previous value of register can’t be restored anymore.

  | CFI_SAME_VALUE Reg
    -- ^ Current value of register is the same like in the previous frame,
    -- i.e. no restoration needed.

  | CFI_REMEMBER_STATE
  | CFI_RESTORE_STATE
    -- ^ .cfi_remember_state pushes the set of rules for every register
    -- onto an implicit stack, while .cfi_restore_state pops them off the
    -- stack and places them in the current row. This is useful for
    -- situations where you have multiple .cfi_* directives that need to
    -- be undone due to the control flow of the program.

  | CFI_RETURN_COLUMN Reg
    -- ^ Change return column register, i.e. the return address is either
    -- directly in register or can be accessed by rules for register.

  | CFI_SIGNAL_FRAME
    -- ^ Mark current function as signal trampoline.

  | CFI_WINDOW_SAVE
    -- ^ SPARC register window has been saved.

  | CFI_ESCAPE (Either Int String)
    -- ^ Allows the user to add arbitrary bytes to the unwind info. One
    -- might use this to add OS-specific CFI opcodes, or generic CFI
    -- opcodes that GAS does not yet support.

  | CFI_VAL_ENCODED_ADDR Reg (Either Int String) String
    -- ^ The current value of register is label. The value of label will
    -- be encoded in the output file according to encoding; see the
    -- description of .cfi_personality for details on this encoding.  The
    -- usefulness of equating a register to a fixed label is probably
    -- limited to the return address register. Here, it can be useful to
    -- mark a code segment that has only one return address which is
    -- reached by a direct branch and no copy of the return address exists
    -- in memory or another register.
  deriving (Show, Eq, Ord)





{- | Emits a value at the current position.

+--------------+--------------------------------+------------------------------------+
| Directive    | Arguments                      | Description                        |
+==============+================================+====================================+
| .byte        | expression [, expression]*     | 8-bit comma separated words        |
+--------------+--------------------------------+------------------------------------+
| .2byte       | expression [, expression]*     | 16-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .half        | expression [, expression]*     | 16-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .short       | expression [, expression]*     | 16-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .4byte       | expression [, expression]*     | 32-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .word        | expression [, expression]*     | 32-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .long        | expression [, expression]*     | 32-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .8byte       | expression [, expression]*     | 64-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .dword       | expression [, expression]*     | 64-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .quad        | expression [, expression]*     | 64-bit comma separated words       |
+--------------+--------------------------------+------------------------------------+
| .dtprelword  | expression [, expression]*     | 32-bit thread local word           |
+--------------+--------------------------------+------------------------------------+
| .dtpreldword | expression [, expression]*     | 64-bit thread local word           |
+--------------+--------------------------------+------------------------------------+

-}

data EmitDir
  = BYTE        -- ^  8-bit comma separated words
  | BYTE2       -- ^  16-bit comma separated words
  | HALF        -- ^  16-bit comma separated words
  | SHORT       -- ^  16-bit comma separated words
  | BYTE4       -- ^  32-bit comma separated words
  | WORD        -- ^  32-bit comma separated words
  | LONG        -- ^  32-bit comma separated words
  | BYTE8       -- ^  64-bit comma separated words
  | DWORD       -- ^  64-bit comma separated words
  | QUAD        -- ^  64-bit comma separated words
  | DTPRELWORD  -- ^  32-bit thread local word
  | DTPRELDWORD -- ^  64-bit thread local word
  deriving (Show, Eq, Ord)

    







{- $instr
The RiscV Assembly Language presented here supports the following modules:

1. 32bit Base Integer Instruction Set                      
2. 64bit Base Integer Instruction Set                      
3. RV32M Standard Extension for Integer Multiply and Divide
4. RV64M Standard Extension for Integer Multiply and Divide

We also support all pseudoinstructions defined in the Set Manual.

Refrences: 
* [The RISC-V Assembly Programmer's Manual](https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md#risc-v-assembly-programmers-manual) (Recovered Feb 2022)
* The RISC-V Instruction Set Manual ( [Version 20191213, December 13, 2019]("https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf") ).

-}

{- | An RiscV assembly file contains labels, directives and instructions. We ignore comments and empty lines.

Strictly speaking, any statement can begin with a label ( [See documentation](https://sourceware.org/binutils/docs/as/Statements.html#Statements) ) . However, as far as I can tell, Clang always sets labels in separated lines

-}
data LineOfRiscV =
    LabelLn       String
  | Directive   Directive
  | Instruction Instr
  deriving (Show, Eq)


data Instr
  = Instr32I    InstrRV32I   -- ^ 32bit Base Integer Instruction Set
  | Instr64I    InstrRV64I   -- ^ 64bit Base Integer Instruction Set
  | Instr32M    InstrExt32M  -- ^ RV32M Standard Extension for Integer Multiply and Divide
  | Instr64M    InstrExt64M  -- ^ RV64M Standard Extension for Integer Multiply and Divide
  | InstrPseudo PseudoInstr  -- ^ Pseudoinstructions
  | InstrAlias  AliasInstr   -- ^ Instruction Aliases
  deriving (Show, Eq)


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
| LUI rd, imm         | Load Upper Immediate  | rd ← imm                     |
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
  deriving (Show, Eq)

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
  = ADDIW   
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
  deriving (Show, Eq)

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

The tables show most the pseudoinstructions documented in the
Instruction Set Manual, and marks the ones we support.  We do not yet
support pseudoinstructions for control and status register (CSR) or
single-precision operations over floats

-}

{- |

Absolute instructions. The base instructions use pc-relative
addressing, so the linker subtracts pc from symbol to get delta. The
linker adds delta[11] to the 20-bit high part, counteracting sign
extension of the 12-bit low part.

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
| @l{b|h|w|d} rd, symbol(r1)@ | @auipc rd, delta[31 : 12] + delta[11]@ | Load global                    |           |
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

* Note: in the reference, @l{b|h|w|d}@ was only applied to a register and a symbol when it should be applied to a register, and offset and a register @l{b|h|w|d} rd, symbol(r1)@. Perhaps their interpretation of symbol is differetn, but @s{b|h|w|d}@ are correct.

-}

data AbsolutePseudo
  = PseudoLA Reg Imm
  | PseudoLLA Reg Imm
  | PseudoLoad MemOpKind Reg Imm Reg 
  | PseudoStore MemOpKind Reg Imm Reg
  deriving (Show, Eq)

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
|                   | ????                                    | Call far-away subroutine           |           |
|                   +-----------------------------------------+                                    +-----------+
| call rd offset    | ????                                    |  With return                       |           |
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

@call rd offset@ is defined in the [RiscV Asm Manual](https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md#function-calls) , but no semantics are given. 

-}

data PseudoInstr
  -- | Function call/return instructions
  = RetPI
  | CallPI (Maybe Reg) Offset
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
  deriving (Show, Eq)




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



{- $alias

When a program flow reaches an unexpected location, you can use
@unimp@ to signal an unreachable program instruction.  the @UNIMP@
pseudo-instruction, should trap in nearly all systems. The de facto
standard implementation of this instruction is:

* @C.UNIMP@: @0000@. The all-zeroes pattern is not a valid
  instruction. Any system which traps on invalid instructions will
  thus trap on this UNIMP instruction form.

* @UNIMP@ : @C0001073@. This is an alias for @CSRRW x0, cycle,
  x0@. Since cycle is a read-only CSR, this instruction should trap
  (even is CSR is not implemented). This 32-bit form of UNIMP is
  emitted when targeting a system without the C extension, or when the
  .option norvc directive is used.

Reference: [Instruction Aliases (RISC-V Assembly Programmer's
Manual)](https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md#instruction-aliases)

-}

data AliasInstr
  = UNIMPC | UNIMP
  deriving (Show, Eq, Ord)

-- The following class allows us to ReplaceLabels With concrete values inside Native Instructions.
-- But it does restrict Native architectures to ones using Imm and Reg, which is very restrictive!
-- I would like to remove this restriction.
class TraversableOp instr where
  traverseOp :: Applicative m => (Imm -> m Imm) -> (Reg -> m Reg) -> instr -> m instr

instrTraverseImmM :: (Applicative m, TraversableOp instr) => (Imm -> m Imm) -> instr -> m instr
instrTraverseImmM fi i = traverseOp fi pure i

instrTraverseImm :: (Show instr, TraversableOp instr) => (Imm -> Imm) -> instr -> instr
instrTraverseImm fi i = runIdentity $ instrTraverseImmM (Identity . fi) i

instrTraverseRegM :: (Applicative m, TraversableOp instr) => (Reg -> m Reg) -> instr -> m instr
instrTraverseRegM fr = traverseOp pure fr

instance TraversableOp Instr where
  traverseOp fi fr instr =
    case instr of
      Instr32I    instr' -> Instr32I    <$> traverseOp fi fr instr' 
      Instr64I    instr' -> Instr64I    <$> traverseOp fi fr instr' 
      Instr32M    instr' -> Instr32M    <$> traverseOp fi fr instr' 
      Instr64M    instr' -> Instr64M    <$> traverseOp fi fr instr' 
      InstrPseudo instr' -> InstrPseudo <$> traverseOp fi fr instr' 
      InstrAlias  instr' -> InstrAlias  <$> traverseOp fi fr instr'

instance TraversableOp InstrRV32I  where
  traverseOp fi fr instr =
    case instr of
      JAL  r imm               -> JAL            <$> fr r  <*> fi imm                       
      JALR r1 r2 imm           -> JALR           <$> fr r1 <*> fr r2 <*> fi imm                  
      BranchInstr bc r1 r2 imm -> BranchInstr bc <$> fr r1 <*> fr r2 <*> fi imm
      MemInstr32 mop r1 imm r2 -> MemInstr32 mop <$> fr r1  <*> fi imm <*> fr r2    
      LUI   r imm              -> LUI            <$> fr r  <*> fi imm                     
      AUIPC r imm              -> AUIPC          <$> fr r  <*> fi imm                     
      ImmBinop32 bop r1 r2 imm -> ImmBinop32 bop <$> fr r1 <*> fr r2 <*> fi imm   
      RegBinop32 bop r1 r2 r3  -> RegBinop32 bop <$> fr r1 <*> fr r2 <*> fr r3     
      FENCE sord               -> pure $ FENCE sord               
      FENCEI                   -> pure $ FENCEI                          
  
instance TraversableOp InstrRV64I  where
  traverseOp fi fr instr = 
    case instr of
      MemInstr64 mop r1 imm r2 -> MemInstr64 mop <$> fr r1 <*> fi imm <*> fr r2 
      ImmBinop64 bop r1 r2 imm -> ImmBinop64 bop <$> fr r1 <*> fr r2 <*> fi imm
      RegBinop64 bop r1 r2 r3   -> RegBinop64 bop <$> fr r1 <*> fr r2 <*> fr r3
instance TraversableOp InstrExt32M where
  traverseOp _ fr instr =
    case instr of
      MUL    r1 r2 r3 -> MUL    <$> fr r1 <*> fr r2 <*> fr r3    
      MULH   r1 r2 r3 -> MULH   <$> fr r1 <*> fr r2 <*> fr r3  
      MULHSU r1 r2 r3 -> MULHSU <$> fr r1 <*> fr r2 <*> fr r3
      MULHU  r1 r2 r3 -> MULHU  <$> fr r1 <*> fr r2 <*> fr r3 
      DIV    r1 r2 r3 -> DIV    <$> fr r1 <*> fr r2 <*> fr r3   
      DIVU   r1 r2 r3 -> DIVU   <$> fr r1 <*> fr r2 <*> fr r3  
      REM    r1 r2 r3 -> REM    <$> fr r1 <*> fr r2 <*> fr r3   
      REMU   r1 r2 r3 -> REMU   <$> fr r1 <*> fr r2 <*> fr r3
      
instance TraversableOp InstrExt64M where
  traverseOp _ fr instr =
    case instr of
      MULW  r1 r2 r3 -> MULW  <$> fr r1  <*> fr r2 <*> fr r3
      DIVW  r1 r2 r3 -> DIVW  <$> fr r1  <*> fr r2 <*> fr r3
      DIVUW r1 r2 r3 -> DIVUW <$> fr r1  <*> fr r2 <*> fr r3
      REMW  r1 r2 r3 -> REMW  <$> fr r1  <*> fr r2 <*> fr r3
      REMUW r1 r2 r3 -> REMUW <$> fr r1  <*> fr r2 <*> fr r3
      
instance TraversableOp PseudoInstr where
  traverseOp fi fr instr =
    case instr of
      RetPI         -> pure RetPI
      CallPI mr imm -> CallPI <$> (traverse fr mr) <*> fi imm 
      TailPI imm    -> TailPI <$> fi imm
      FencePI       -> pure FencePI
      LiPI r imm    -> LiPI <$> fr r <*> fi imm
      NopPI         -> pure NopPI
      AbsolutePI ap -> pure $ AbsolutePI ap 
      UnaryPI up r1 r2      -> UnaryPI up    <$> fr r1  <*> fr r2     
      CMovPI cmp r1 r2      -> CMovPI cmp    <$> fr r1  <*> fr r2
      BranchZPI bzp r imm   -> BranchZPI bzp <$> fr r   <*> fi imm
      BranchPI bp r1 r2 imm -> BranchPI bp   <$> fr r1  <*> fr r2 <*> fi imm
      JmpImmPI jp imm       -> JmpImmPI jp   <$> fi imm    
      JmpRegPI jp r         -> JmpRegPI jp   <$> fr r
      
instance TraversableOp AliasInstr  where
  traverseOp _ _ instr = pure instr
