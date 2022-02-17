{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module RiscV.Parser (
  riscvParseFile, riscvParser
  )where

-- import Data.Bits
--import Data.Word (Word64)
import RiscV.RiscVAsm

-- import Data.Char (Char)
import Text.Parsec
-- import Text.Parsec.Combinator (sepBy, sepBy1)
import qualified Text.Parsec.Language as Lang 
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tokens
import Data.Maybe (catMaybes)
import Control.Monad (mzero, when)
import Data.Functor.Identity (Identity)

--import Debug.Trace

-- Borrow some definitions from haskell
riscVLang :: Tokens.GenTokenParser String u Identity
riscVLang = Tokens.makeTokenParser riscVLangDef

{- | The RiscV Assembly calls "Symbols" any identifier. 

* Symbol names begin with a letter or with one of ‘._’. On most
  machines, you can also use $ in symbol names. Symbol names do not
  start with a digit. An exception to this rule is made for Local
  Labels, but we don't yet support it.

* That character may be followed by any string of digits, letters,
  dollar signs (unless otherwise noted for a particular target
  machine), and underscores.  We do not yet support qutoed symbols
  names by ‘"’ or multibyte characters. Although, not documented,
  symbold can also have internal periods "."

* Symbol names may also be enclosed in double quote " characters. In
  such cases any characters are allowed, except for the NUL
  character. If a double quote character is to be included in the
  symbol name it must be preceded by a backslash \ character. For
  these quoted characters we can use Tokens.stringLiteral.

From this point on, we will use identifiers, and reserve "symbols" for
things like "+" or "<$>" as it is common in Haskell.

-}
riscVLangDef :: Lang.LanguageDef u
riscVLangDef = Tokens.LanguageDef
  {
    -- RiscV Assembly has no multiline comment 
    Tokens.commentStart = ""
  , Tokens.commentEnd = ""
  , Tokens.commentLine = "#"
  , Tokens.nestedComments = False,

  -- Symbol names begin with a letter or with one of ‘._’. On most
  -- machines, you can also use $ in symbol names; exceptions are
  -- noted in Machine Dependencies. Symbol names do not start with a
  -- digit. An exception to this rule is made for Local Labels, but we
  -- don't yet support it.
  Tokens.identStart = letter <|> oneOf "._$"

  -- That character may be followed by any string of digits,
  -- letters, dollar signs (unless otherwise noted for a particular
  -- target machine), and underscores.  We do not yet support qutoed
  -- symbols names by ‘"’ or multibyte characters.
  , Tokens.identLetter = alphaNum <|> oneOf "._$"

  -- No operators in this language
  , Tokens.opStart        = parserFail "Attempt to read an operands"
  , Tokens.opLetter       = parserFail "Attempt to read an operands"

  -- No reserved names 
  , Tokens.reservedNames   = []
  , Tokens.reservedOpNames = []
  , Tokens.caseSensitive   = True
  }
  
parens, lexeme
  :: Parsec String u a
     -> Parsec String u a
-- Lexeme parser parens p parses p enclosed in parenthesis, returning the value of p.
parens = Tokens.parens riscVLang
-- lexeme p first applies parser p and then the whiteSpace parser, returning the value of p.
lexeme = Tokens.lexeme riscVLang
lexchar :: Char -> Parsec String u Char
lexchar c = lexeme $ char c

-- This lexeme parser parses an integer (a whole number). 
integerParser :: Parsec String u Integer
integerParser = Tokens.integer riscVLang
-- | Lexeme parser symbol s parses string s and skips trailing white space.
-- Here symbol refers to things like "+" or "<$>"
symbolParser
  :: String -> Parsec String u String
symbolParser = Tokens.symbol riscVLang

-- | Identifiers are the RiscV names for variables, sections, etc They
-- can be alphanumeric strings that include "." and "_" or they cna
-- be arbitrary strings if they are quoted.
identifier, textParser
  :: Parsec String u String
textParser = Tokens.stringLiteral riscVLang
identifier = Tokens.identifier riscVLang <|>
             Tokens.stringLiteral riscVLang

whiteSpace :: Parsec String u ()
whiteSpace = Tokens.whiteSpace riscVLang
comma :: Parsec String u String
comma = Tokens.comma riscVLang
integer :: Parsec String u Integer
integer = Tokens.integer riscVLang
intParser  :: Parsec String u Int
intParser = fromInteger <$> integer 
numParser  :: Num n => Parsec String u n
numParser = fromInteger <$> integer 
numStrParser :: Num n =>  Parsec String st (Either n String)
numStrParser = try (Right  <$>     identifier)
                     <|> try (Left   <$> numParser)
                     <?> "encoding"
               
-- | Read file
readFileRV :: FilePath -> IO String
readFileRV file = do
  contents <- readFile file
  return contents

{- | Parse RiscV directly from file 
-} 

riscvParseFile :: String -> IO (Either ParseError [LineOfRiscV])
riscvParseFile fileName = do
  fileContent <- readFileRV fileName
  return $ riscvParser fileName fileContent 

-- | Parse RiscV given the name of the file (only used for errors) and
-- a RiscV assembly program.
riscvParser :: String -> String -> Either ParseError [LineOfRiscV]
riscvParser rvFileName rvFile = catMaybes <$> mapM (parse riscvLnParser rvFileName) (lines rvFile)


{- RiscV assembly file contains labels, directives and instructions. We
   ignore comments and empty lines. Strictly speaking, any statement
   can begin with a label ([See
   documentation](https://sourceware.org/binutils/docs/as/Statements.html#Statements))
   . However, as far as I can tell, Clang always sets labels in
   separated lines

   The Tokens library in Parsec doesn't deal well with end of
   lines. Most commands (all the `lexeme` ones) consume "empty" space,
   but the end of line is considered empty space and there is no way
   to change that. I found it easier to just parse line by line. Since
   there are no multi level comments (or anything really) this is ok.

   We lose the traceback location of errors, the parser will always
   report an error in line 1. Later, with some unwrapping of the error
   we can fix that.

-}

riscvLnParser :: Parsec String st (Maybe LineOfRiscV)
riscvLnParser = (try labelLnParse
                <|> try drctvLnParse
                <|> try instrLnParse
                <|> try emptyLnParse 
                <?> "Line of RiscV Assembly") <* eof
  where
    emptyLnParse,instrLnParse,labelLnParse:: Parsec String st (Maybe LineOfRiscV)
    -- Empty line, possibly with comments and or spaces/tabs, etc.  
    emptyLnParse = whiteSpace >> eof *> return Nothing

    drctvLnParse   = Just . Directive   <$> (tabParse *> char '.' *> directiveParse)
    instrLnParse   = Just . Instruction <$> (tabParse *> instrParser)
    labelLnParse   = Just . Label       <$> identifier <* lexchar ':'

    tabParse     = string "    " <|> string "\t" <?> "alignemnt"

choiceTry :: [ParsecT s u m a] -> ParsecT s u m a
choiceTry ps           = foldr ((<|>) . try) mzero ps


{- Register ABI names
+----------+----------+------------------------------------+
| Register | ABI Name |            Description             |
+----------+----------+------------------------------------+
| x0       | zero     | hardwired zero                     |
| x1       | ra       | return address                     |
| x2       | sp       | stack pointer                      |
| x3       | gp       | global pointer                     |
| x4       | tp       | thread pointer                     |
| x5-7     | t0-2     | temporary registers                |
| x8       | s0 / fp  | saved register / frame pointer     |
| x9       | s1       | saved register                     |
| x10-11   | a0-1     | function arguments / return values |
| x12-17   | a2-7     | function arguments                 |
| x18-27   | s2-11    | saved registers                    |
| x28-31   | t3-6     | temporary registers                |
+----------+----------+------------------------------------+

-}

parseFromPairs ::  [(String,b)] -> Parsec String st b
parseFromPairs pairs = choiceTry $ parseFromPair <$> pairs
  where
    parseFromPair ::  (String,b) -> Parsec String st b
    parseFromPair (a,b) = string a *> pure b

-- The list has to start with the longer registers so they will be
-- parsed first. For example "s10" has to appear before "s1" otherwise
-- "s10" will be parsed as "s1" and and the parser will choke on the
-- trailing "0".
regParser :: Parsec String st Reg
regParser = parseFromPairs registerABInames <?> "a register"
  where registerABInames =
          [("t6",   X31) -- temporary registers
          ,("t5",   X30)
          ,("t4",   X29)
          ,("t3",   X28)
          ,("s11",  X27) -- saved registers
          ,("s10",  X26)
          ,("s9",   X25)
          ,("s8",   X24)
          ,("s7",   X23)
          ,("s6",   X22)
          ,("s5",   X21)
          ,("s4",   X20)
          ,("s3",   X19)
          ,("s2",   X18)
          ,("a7",   X17) -- function arguments
          ,("a6",   X16)
          ,("a5",   X15)
          ,("a4",   X14)
          ,("a3",   X13)
          ,("a2",   X12)
          ,("a1",   X11) -- function arguments / return values
          ,("a0",   X10) 
          ,("s1",   X9)
          ,("s0",   X8)  -- saved register (s0=fp)
          ,("fp",   X8)  -- frame pointer (same as s0)
          ,("t2",   X7)  -- temporary registers
          ,("t1",   X6)
          ,("t0",   X5)  
          ,("tp",   X4)  -- thread pointer
          ,("gp",   X3)  -- global pointer
          ,("sp",   X2)  -- stack pointer
          ,("ra",   X1)  -- return address
          ,("zero",  X0)  -- hardwired zero
          ]

{- | Symbols:
     Symbol names begin with a letter or with one of ‘._’. On
     most machines, you can also use $ in symbol names; exceptions are
     noted in Machine Dependencies. That character may be followed by
     any string of digits, letters, dollar signs (unless otherwise
     noted for a particular target machine), and underscores.

     We do not yet support qutoed symbols  names by ‘"’ or multibyte characters.

     Symbol names do not start with a digit. An exception to this rule
     is made for Local Labels, but we don't yet support it.
-}
instrParser :: Parsec String st Instr
instrParser = Instr32I <$> parse32I
              <|> Instr64I <$> parse64I
              <|> Instr32M <$> parse32M
              <|> Instr64M <$> parse64M
              <|> InstrAlias <$> parseAlias
              <|> InstrPseudo <$> parsePseudo
              <?> "an instruction"

  
{- | Immediates can be:
   1. A number
   2. A symbol (e.g. global constant)
   3. An Assembler Modifier (and a symbol)
   4. En constant expression (Found in the wild, but haven't
      seen this in a compiler output, so we don't support them)

  Also, afaik a modifier can must directly modify a symbol.
  So two modifiers are not allowed, and modifiers to constants
  are just constant expressions, which we don't support
 -}
  
immediateParser :: ParsecT String u Identity Imm
immediateParser = Expr.buildExpressionParser operands immTerm <?> "an immediate"
  where
    immTerm = parens immediateParser
              <|> ImmSymbol <$> lexeme identifier
              <|> ImmNumber <$> fromInteger <$> lexeme integerParser
              <|> modParse <*> parens immediateParser
    operands =
      [ [infix_ "&" (ImmBinOp ImmAnd  ) ]
      , [infix_ "|" (ImmBinOp ImmOr   ) ]
      , [infix_ "+" (ImmBinOp ImmAdd  ) ]
      , [infix_ "-" (ImmBinOp ImmMinus) ]
      , [Expr.Prefix modParse]]
    modParse :: Parsec String st (Imm -> Imm)
    modParse = lexeme $
               char '%' *> pure ImmMod
               <*> parseFromPairs mods
    mods = [
      ("hi", ModHi)
      , ("lo", ModLo)
      ]
           
    infix_ :: String -> (a -> a -> a) -> Expr.Operator String u Identity a
    infix_ operator func =
      Expr.Infix (symbolParser operator >> return func) Expr.AssocLeft
    
{- | Offsets.  Offsets are differnt from Immediates. First of all,
bounded by a smaller number (2^12 I think?). Second, they accept no
symbols or modifiers (I think).

We distinguish them at the type level, but we don't add bound checks.
 -}
offsetParser :: Parsec String st Offset
offsetParser = immediateParser -- read <$> many digit 

{- | Notation.

  This is my attempt at making the parser easier to read and mantain.

-}
infixl 9 ==>
(==>) :: String -> a -> Parsec String st a
label ==> x = lexeme (string label *> pure x)

{- Would be better if I could use `<,>` but Haskell won't let me use commas! -}
infixl 4 <:>
(<:>) :: forall st a b. Parsec String st (a -> b) -> Parsec String st a -> Parsec String st b
(<:>) p1 p2 = p1 <* comma <*> p2


parse32I :: Parsec String st InstrRV32I
parse32I = choiceTry
      [
        -- Jump instructions
        "jal"    ==> JAL    <*> regParser <:> offsetParser
      , "jalr"   ==> JALR   <*> regParser <:> regParser <:> offsetParser
        -- Branch instructions @br r1, r2, offset@
      , "beq"  ==> BranchInstr BEQ  <*> regParser <:> regParser <:> offsetParser
      , "bne"  ==> BranchInstr BNE  <*> regParser <:> regParser <:> offsetParser
      , "blt"  ==> BranchInstr BLT  <*> regParser <:> regParser <:> offsetParser
      , "bge"  ==> BranchInstr BGE  <*> regParser <:> regParser <:> offsetParser
      , "bltu" ==> BranchInstr BLTU <*> regParser <:> regParser <:> offsetParser
      , "bgeu" ==> BranchInstr BGEU <*> regParser <:> regParser <:> offsetParser
      -- Memory instructions @memop r1, offset(r2)@
      , "lb"   ==> MemInstr32 LB  <*> regParser <:> offsetParser <*> parens regParser
      , "lh"   ==> MemInstr32 LH  <*> regParser <:> offsetParser <*> parens regParser
      , "lw"   ==> MemInstr32 LW  <*> regParser <:> offsetParser <*> parens regParser
      , "lbu"  ==> MemInstr32 LBU <*> regParser <:> offsetParser <*> parens regParser
      , "lhu"  ==> MemInstr32 LHU <*> regParser <:> offsetParser <*> parens regParser
      , "sb"   ==> MemInstr32 SB  <*> regParser <:> offsetParser <*> parens regParser
      , "sh"   ==> MemInstr32 SH  <*> regParser <:> offsetParser <*> parens regParser
      , "sw"   ==> MemInstr32 SW  <*> regParser <:> offsetParser <*> parens regParser
      -- unary instructions
      , "lui"   ==> LUI    <*> regParser <:> immediateParser
      , "auipc" ==> AUIPC  <*> regParser <:> immediateParser
      -- Binary Integer Register-Immediate Instructions
      , "addi"  ==> ImmBinop32 ADDI  <*> regParser <:> regParser <:> immediateParser       
      , "slti"  ==> ImmBinop32 SLTI  <*> regParser <:> regParser <:> immediateParser      
      , "sltiu" ==> ImmBinop32 SLTIU <*> regParser <:> regParser <:> immediateParser      
      , "xori"  ==> ImmBinop32 XORI  <*> regParser <:> regParser <:> immediateParser      
      , "ori"   ==> ImmBinop32 ORI   <*> regParser <:> regParser <:> immediateParser      
      , "andi"  ==> ImmBinop32 ANDI  <*> regParser <:> regParser <:> immediateParser      
      , "slli"  ==> ImmBinop32 SLLI  <*> regParser <:> regParser <:> immediateParser      
      , "srli"  ==> ImmBinop32 SRLI  <*> regParser <:> regParser <:> immediateParser      
      , "srai"  ==> ImmBinop32 SRAI  <*> regParser <:> regParser <:> immediateParser      
      -- Integer Register-Register Instructions
      , "add"   ==> RegBinop32 ADD  <*> regParser <:> regParser <:> regParser
      , "sub"   ==> RegBinop32 SUB  <*> regParser <:> regParser <:> regParser
      , "sll"   ==> RegBinop32 SLL  <*> regParser <:> regParser <:> regParser
      , "slt"   ==> RegBinop32 SLT  <*> regParser <:> regParser <:> regParser
      , "sltu"  ==> RegBinop32 SLTU <*> regParser <:> regParser <:> regParser
      , "xor"   ==> RegBinop32 XOR  <*> regParser <:> regParser <:> regParser
      , "srl"   ==> RegBinop32 SRL  <*> regParser <:> regParser <:> regParser
      , "sra"   ==> RegBinop32 SRA  <*> regParser <:> regParser <:> regParser
      , "or"    ==> RegBinop32 OR   <*> regParser <:> regParser <:> regParser
      , "and"   ==> RegBinop32 AND  <*> regParser <:> regParser <:> regParser
      -- fence instructions
      , "fance"    ==> FENCE    <*> orderingParser
      , "fence.i"  ==> FENCEI ]
    

-- | Memory ordering for fences
orderingParser :: Parsec String st SetOrdering
orderingParser = undefined -- not needed?

parse64I :: Parsec String st InstrRV64I
parse64I = choiceTry
      [
        -- Mem instructions
        "lwu"    ==> MemInstr64 LWU <*> regParser <:> offsetParser <*> parens regParser
      , "ld"     ==> MemInstr64 LD  <*> regParser <:> offsetParser <*> parens regParser
      , "sd"     ==> MemInstr64 SD  <*> regParser <:> offsetParser <*> parens regParser
        -- Integer Register-Immediate Instructions 
      , "addiw"  ==> ImmBinop64 ADDIW <*> regParser <:> regParser <:> immediateParser
      , "slliw"  ==> ImmBinop64 SLLIW <*> regParser <:> regParser <:> immediateParser
      , "srliw"  ==> ImmBinop64 SRLIW <*> regParser <:> regParser <:> immediateParser
      , "sraiw"  ==> ImmBinop64 SRAIW <*> regParser <:> regParser <:> immediateParser
      -- Integer Register-Register Instructions
      , "addw"  ==> RegBinop64 ADDW <*> regParser <:> regParser <:> regParser
      , "subw"  ==> RegBinop64 SUBW <*> regParser <:> regParser <:> regParser
      , "sllw"  ==> RegBinop64 SLLW <*> regParser <:> regParser <:> regParser
      , "srlw"  ==> RegBinop64 SRLW <*> regParser <:> regParser <:> regParser
      , "sraw"  ==> RegBinop64 SRAW <*> regParser <:> regParser <:> regParser
      ]




parse32M :: Parsec String st InstrExt32M
parse32M = choiceTry
      [ "mul"    ==> MUL    <*> regParser <:> regParser <:> regParser
      , "mulh"   ==> MULH   <*> regParser <:> regParser <:> regParser
      , "mulhsu" ==> MULHSU <*> regParser <:> regParser <:> regParser
      , "mulhu " ==> MULHU  <*> regParser <:> regParser <:> regParser
      , "div"    ==> DIV    <*> regParser <:> regParser <:> regParser
      , "divu"   ==> DIVU   <*> regParser <:> regParser <:> regParser
      , "rem"    ==> REM    <*> regParser <:> regParser <:> regParser
      , "remu"   ==> REMU   <*> regParser <:> regParser <:> regParser
      ]






parse64M :: Parsec String st InstrExt64M
parse64M = choiceTry
      [ "mulw"  ==> MULW  <*> regParser <:> regParser <:> regParser
      , "divw"  ==> DIVW  <*> regParser <:> regParser <:> regParser
      , "divuw" ==> DIVUW <*> regParser <:> regParser <:> regParser
      , "remw"  ==> REMW  <*> regParser <:> regParser <:> regParser
      , "remuw" ==> REMUW <*> regParser <:> regParser <:> regParser
      ]





parseAlias :: Parsec String st AliasInstr
parseAlias = choiceTry
      [ 
       "unimp.c"    ==> UNIMPC
      , "unimp"   ==> UNIMP
      ]



-- For some reason the parser is haveing trouble with these, reporting
-- "Defined but not used:" when in use
(∘∘) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f ∘∘ g) x y = f (g x y)
(∘∘∘) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(f ∘∘∘ g) x y z = f (g x y z)

parsePseudo :: Parsec String st PseudoInstr
parsePseudo = choiceTry
      [ 
      -- Function call/return instructions
       "ret"    ==> RetPI
      , "call"   ==> CallPI <*> (Just <$> regParser) <:> immediateParser
      , "call"   ==> CallPI     Nothing              <*> offsetParser
      , "tail"   ==> TailPI <*> offsetParser
      -- Total Fence
      , "fence"  ==> FencePI
      -- Unary immediate
      , "li"     ==> LiPI <*> regParser <:> immediateParser
      -- nop
      , "nop"    ==> NopPI
      -- Absolute load/store
      , "la"     ==> (AbsolutePI ∘∘ PseudoLA) <*> regParser <:> immediateParser
      , "lla"    ==> (AbsolutePI ∘∘ PseudoLLA) <*> regParser <:> immediateParser
      , "lb"    ==> (AbsolutePI ∘∘∘ PseudoLoad MemByte)   <*> regParser <:> offsetParser <*> parens regParser
      , "lh"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemHalf)   <*> regParser <:> offsetParser <*> parens regParser
      , "lw"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemWord)   <*> regParser <:> offsetParser <*> parens regParser
      , "ld"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemDouble) <*> regParser <:> offsetParser <*> parens regParser
      , "sb"     ==> (AbsolutePI ∘∘∘ PseudoStore MemByte)   <*> regParser <:> immediateParser <:> regParser
      , "sh"     ==> (AbsolutePI ∘∘∘ PseudoStore MemHalf)   <*> regParser <:> immediateParser <:> regParser
      , "sw"     ==> (AbsolutePI ∘∘∘ PseudoStore MemWord)   <*> regParser <:> immediateParser <:> regParser
      , "sd"     ==> (AbsolutePI ∘∘∘ PseudoStore MemDouble) <*> regParser <:> immediateParser <:> regParser
      -- Unary register Pseudoinstructions
      , "mv"    ==> UnaryPI MOV   <*> regParser <:> regParser
      , "not"    ==> UnaryPI NOT   <*> regParser <:> regParser
      , "neg"    ==> UnaryPI NEG   <*> regParser <:> regParser
      , "negw"   ==> UnaryPI NEGW  <*> regParser <:> regParser
      , "sext.w"  ==> UnaryPI SEXTW <*> regParser <:> regParser
      -- Conditional Moves
      , "seqz"   ==> CMovPI SEQZ <*> regParser <:> regParser
      , "snez"   ==> CMovPI SNEZ <*> regParser <:> regParser
      , "sltz"   ==> CMovPI SLTZ <*> regParser <:> regParser
      , "sgtz"   ==> CMovPI SGTZ <*> regParser <:> regParser
      -- Alternative branches ZERO
      , "beqz"   ==> BranchZPI BEQZ <*> regParser <:> offsetParser
      , "bnez"   ==> BranchZPI BNEZ <*> regParser <:> offsetParser
      , "blez"   ==> BranchZPI BLEZ <*> regParser <:> offsetParser
      , "bgez"   ==> BranchZPI BGEZ <*> regParser <:> offsetParser
      , "bltz"   ==> BranchZPI BLTZ <*> regParser <:> offsetParser
      , "bgtz"   ==> BranchZPI BGTZ <*> regParser <:> offsetParser
      -- Alternative branches ZERO
      , "bgt"    ==> BranchPI BGT  <*> regParser <:> regParser <:> offsetParser
      , "ble"    ==> BranchPI BLE  <*> regParser <:> regParser <:> offsetParser
      , "bgtu"   ==> BranchPI BGTU <*> regParser <:> regParser <:> offsetParser
      , "bleu"   ==> BranchPI BLEU <*> regParser <:> regParser <:> offsetParser
      --  Alternative Jumps
      , "j"      ==> JmpImmPI JPseudo     <*> immediateParser
      , "jal"    ==> JmpImmPI JLinkPseudo <*> immediateParser
      , "jr"     ==> JmpRegPI JPseudo     <*> regParser
      , "jalr"   ==> JmpRegPI JLinkPseudo <*> regParser
      ]




{- | This is one of the ELF section stack manipulation directives. The
   others are .subsection (see SubSection), .pushsection (see
   PushSection), .popsection (see PopSection), and .previous (see
   Previous).

   For ELF targets, the .section directive is used like this:

   @.section name [, "flags"[, @type[,flag_specific_arguments]]]@

   The documentation claims "iIf one or more of the alphabetic
   characters described above is also included in the flags field,
   their bit values will be ORed into the resulting value."
   I'm not sure if that has implications for parsing. Does it?

   For some reason clang emits instructions with the name in quotes
   and sometimes without the quotes.
   @@
	.section	.gcc_except_table,"a",@progbits
   @@

   or

   @@
	.section	".note.GNU-stack","",@progbits
   @@

   The latter is used when the name is not a valid identifier name
   (e.g. ".note.GNU-stack" is not valid because identifiers can't have
   dashes "-")

-}
sectionParser :: Parsec String st Directive
sectionParser = SECTION
  <$> identifier
  <*> (try (comma *> flags)            <|> pure [])
  <*> (try (comma *> (Just <$> secType))  <|> pure Nothing)
  <*> (try (comma *> flagArgs)         <|> pure [])
  where
    -- a string with all the flags
    flags :: Parsec String st [Flag]
    flags = quoted . many $
            (parseFromPairs
             [ ("a" , Flag_a)   
             , ("d" , Flag_d)   
             , ("e" , Flag_e)   
             , ("o" , Flag_o)   
             , ("w" , Flag_w)   
             , ("x" , Flag_x)   
             , ("M" , Flag_M)   
             , ("S" , Flag_S)   
             , ("G" , Flag_G)   
             , ("T" , Flag_T)   
             , ("?" , Flag_QM)  
             , ("R" , Flag_R) ]
             <|> try (Flag_number <$> numParser))

    quoted :: Parsec String st a -> Parsec String st a
    quoted p = (char '"' *> p <* char '"')

    -- Section types
    secType ::  Parsec String st SectionType
    secType =
      -- Parse a numeric section header
      try (char '@' >> TypeNum <$> numParser) <|>
      parseFromPairs
      [ ("@progbits"      , PROGBITS     )   
      , ("@nobits"        , NOBITS       )   
      , ("@note"          , NOTE         )   
      , ("@init_array"    , INIT_ARRAY   )   
      , ("@fini_array"    , FINI_ARRAY   )   
      , ("@preinit_array" , PREINIT_ARRAY)
      ]

    flagArgs :: Parsec String st [FlagArg]
    flagArgs = (lexeme numStrParser) `sepBy` comma
      
      

directiveParse :: Parsec String st Directive
directiveParse = try (CFIDirectives <$> directiveCFIParse) <|>
  choiceTry 
      [ "align"         ==> ALIGN      <*> integer
      , "file"          ==> FILE       <*> textParser
      -- Symbol tables and visibility
      , "globl"         ==> Visibility GLOBL     <*> identifier
      , "local"         ==> Visibility LOCAL     <*> identifier
      , "weak"          ==> Visibility WEAK      <*> identifier
      , "hidden"        ==> Visibility HIDDEN    <*> identifier
      , "internal"      ==> Visibility INTERNAL  <*> identifier
      , "protected"     ==> Visibility PROTECTED <*> identifier
      , "comm"          ==> COMM       <*> identifier <:> integer   <:> integer
      , "common"        ==> COMMON     <*> identifier <:> integer   <:> integer
      , "ident"         ==> IDENT      <*> textParser
      , lexeme(lexeme (string "section") >> sectionParser)
      , "size"          ==> SIZE       <*> identifier <:> immediateParser
      , "text"          ==> TEXT   
      , "data"          ==> DATA   
      , "rodata"        ==> RODATA 
      , "bss"           ==> BSS    
      , "string"        ==> STRING     <*> textParser
      , "asciz"         ==> ASCIZ      <*> textParser
      , "equ"           ==> EQU        <*> identifier <:> (fromInteger <$> integer)
      , "type"          ==> TYPE       <*> identifier <:> typeParser
      , "option"        ==> OPTION     <*> optParser
      , "balign"        ==> BALIGN     <*> integer <:> (Just <$> integer)
      , "balign"        ==> BALIGN     <*> integer <:> (return Nothing)
      , "zero"          ==> ZERO       <*> integer
      , "variant_cc"    ==> VARIANT_CC <*> identifier
      , "sleb128"       ==> SLEB128    <*> immediateParser
      , "uleb128"       ==> ULEB128    <*> immediateParser
      , "macro"         ==> MACRO      <*> identifier <:> identifier <:> (return [])
      , "endm"          ==> ENDM

      , "attribute"       ==> ATTRIBUTE    <*> tagParser    <:> (Right  <$> textParse )
      , "attribute"       ==> ATTRIBUTE    <*> tagParser    <:> (Left   <$> integer   )
      
      , "p2align"       ==> P2ALIGN    <*> integer    <:> (Just <$> integer) <:> (Just <$> integer)
      , "p2align"       ==> P2ALIGN    <*> integer    <:> (return Nothing)   <:> (Just <$> integer)
      , "p2align"       ==> P2ALIGN    <*> integer    <:> (Just <$> integer) <*> (return Nothing)
      , "p2align"       ==> P2ALIGN    <*> integer    <*> (return Nothing)   <*> (return Nothing)
      
      -- Emit
      , "byte"              ==> DirEmit BYTE        <*> immList
      , "byte2"             ==> DirEmit BYTE2       <*> immList
      , "half"              ==> DirEmit HALF        <*> immList
      , "short"             ==> DirEmit SHORT       <*> immList
      , "byte4"             ==> DirEmit BYTE4       <*> immList
      , "word"              ==> DirEmit WORD        <*> immList
      , "long"              ==> DirEmit LONG        <*> immList
      , "byte8"             ==> DirEmit BYTE8       <*> immList
      , "dword"             ==> DirEmit DWORD       <*> immList
      , "quad"              ==> DirEmit QUAD        <*> immList
      , "dtprelword"        ==> DirEmit DTPRELWORD  <*> immList
      , "dtpreldword"       ==> DirEmit DTPRELDWORD <*> immList
      ]
  where
    -- Parses a non-empty list of comma separated immediates
    immList :: Parsec String st [Imm]
    immList = immediateParser `sepBy1` comma
  
    -- Types are @function and @object
    typeParser :: Parsec String st DirTypes
    typeParser  = lexeme $ char '@' *>
                  parseFromPairs
                  [ ("function", DTFUNCTION)
                  , ("object",   DTOBJECT)
                  ]
    --
    optParser    = parseFromPairs
      [ ("rvc",    RVC    )
      , ("norvc",  NORVC  )  
      , ("pic",    PIC    )
      , ("nopic",  NOPIC  )  
      , ("push",   PUSH   )
      , ("pop",    POP    )
      , ("relax",  RELAX  )
      , ("norelax",NORELAX)
      ]

    tagParser = parseFromPairs
      [ ( "Tag_RISCV_arch"               , Tag_RISCV_arch               )
      , ( "Tag_RISCV_stack_align"        , Tag_RISCV_stack_align        )
      , ( "Tag_RISCV_unaligned_access"   , Tag_RISCV_unaligned_access   )
      , ( "Tag_RISCV_priv_spec"          , Tag_RISCV_priv_spec          )
      , ( "Tag_RISCV_priv_spec_minor"    , Tag_RISCV_priv_spec_minor    )
      , ( "Tag_RISCV_priv_spec_revision" , Tag_RISCV_priv_spec_revision )
      ] <|> (Tag_number <$> integer)

-- Doesn't admit escaped quotations. Everything insie the two quotations is the text
textParse :: Parsec String u String
textParse    = char '"' *> many (noneOf ['"']) <* char '"'
    


directiveCFIParse :: Parsec String st CFIDirectives
directiveCFIParse =
  choiceTry 
  [ "cfi_sections"          ==> CFI_SECTIONS  <*> scfiSecOptParse
  , "cfi_startproc"         ==> CFI_STARTPROC <*> (try (string "simpl" >> pure True) <|> pure False )
  , "cfi_endproc"           ==> CFI_ENDPROC
  , "cfi_personality"       ==> CFI_PERSONALITY <*> numParser <:> encodingParser
  , "cfi_personality_id"    ==> CFI_PERSONALITY_ID <*>identifier
  , "cfi_fde_data"          ==> CFI_FDE_DATA    <*> pure [] -- Not supported yet
  , "cfi_lsda"              ==> CFI_LSDA        <*> numParser  <:> encodingParser
  , "cfi_inline_lsda"       ==> CFI_INLINE_LSDA <*> intParser
  , "cfi_def_cfa"           ==> CFI_DEF_CFA <*> regParser <:> numParser 
  , "cfi_def_cfa_register"  ==> CFI_DEF_CFA_REGISTER <*> regParser
  , "cfi_def_cfa_offset"    ==> CFI_DEF_CFA_OFFSET <*> numParser
  , "cfi_adjust_cfa_offset" ==> CFI_ADJUST_CFA_OFFSET <*> numParser
  , "cfi_offset"            ==> CFI_OFFSET        <*> regParser <:> numParser
  , "cfi_val_offset"        ==> CFI_VAL_OFFSET    <*> regParser <:> numParser
  , "cfi_rel_offset"        ==> CFI_REL_OFFSET    <*> regParser <:> numParser
  , "cfi_register"          ==> CFI_REGISTER      <*> regParser <:> regParser
  , "cfi_restore"           ==> CFI_RESTORE       <*> regParser
  , "cfi_undefined"         ==> CFI_UNDEFINED     <*> regParser
  , "cfi_same_value"        ==> CFI_SAME_VALUE    <*> regParser
  , "cfi_remember_state"    ==> CFI_REMEMBER_STATE
  , "cfi_restore_state"     ==> CFI_RESTORE_STATE
  , "cfi_return_column"     ==> CFI_RETURN_COLUMN <*> regParser
  , "cfi_signal_frame"      ==> CFI_SIGNAL_FRAME
  , "cfi_window_save"       ==> CFI_WINDOW_SAVE
  , "cfi_escape"            ==> CFI_ESCAPE        <*> encodingParser
  , "cfi_val_encoded_addr"  ==> CFI_VAL_ENCODED_ADDR <*> regParser <:> encodingParser <:> identifier
  ]
  where
    encodingParser :: Num n =>  Parsec String st (Either n String)
    encodingParser = numStrParser
                     
    --opcodesParser :: Parsec String st [Opcodes]
    --opcodesParser  = pure [OCNotSupported]

    scfiSecOptParse :: Parsec String st CFIsectionOpt
    scfiSecOptParse =
      try     (string ".eh_frame" *> comma *> string ".debug_frame" *> pure (CFIsectionOpt True True  ))
      <|> try (string ".eh_frame" >>                                   pure (CFIsectionOpt True False ))
      <|> try (                               string ".debug_frame" >> pure (CFIsectionOpt False True ))
      <|>     (                                                        pure (CFIsectionOpt False False))
      






_filterDirs :: [LineOfRiscV] -> [Directive]
_filterDirs instrs = catMaybes $ filterDir <$> instrs
  where
    filterDir :: LineOfRiscV -> Maybe Directive
    filterDir (Directive dir) = Just dir
    filterDir _ = Nothing

_test :: Int -> IO ()
_test n = do
  code <- readFileRV "src/RiscV/grit-rv64-20211105.s"
  let codeLns = if n>0 then
                  take n $ lines code
                else
                  lines code
  let enumLn = zip codeLns [1..]
  _mapUntilM testLn $ enumLn
  where
    testLn :: (String, Int) -> IO Bool
    testLn (ln, lnN) = do
      case parse riscvLnParser "" $ ln of
        Right (Just e) -> do
          when (n>0) $ putStrLn $ show lnN <> ". " <> show e
          return True
        Right Nothing -> return True
        Left e  -> do
          putStrLn $ "Line number " <> show lnN <> " : " <> show e
          return False

_mapUntilM :: (Monad m) => (a -> m Bool) -> [a] -> m ()
_mapUntilM f ls =
  case ls of
    [] -> return ()
    x:ls' -> do
      result <- f x
      if result then _mapUntilM f ls' else return ()
