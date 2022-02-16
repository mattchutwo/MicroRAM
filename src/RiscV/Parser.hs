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
import qualified Text.Parsec.Language as Lang 
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tokens
import Data.Maybe (catMaybes)
import Control.Monad (mzero)
import Data.Functor.Identity (Identity)

--import Debug.Trace

-- Borrow some definitions from haskell
riscVLang :: Tokens.GenTokenParser String u Identity
riscVLang = Tokens.makeTokenParser riscVLangDef

{- | The RiscV Assembly has the follwoing properties:

* Symbol names begin with a letter or with one of ‘._’. On most
machines, you can also use $ in symbol names; exceptions are
noted in Machine Dependencies. Symbol names do not start with a
digit. An exception to this rule is made for Local Labels, but we
don't yet support it.

* That character may be followed by any string of digits, letters,
dollar signs (unless otherwise noted for a particular target machine),
and underscores.  We do not yet support qutoed symbols names by ‘"’ or
multibyte characters. Although, not documented, symbold can also have
internal periods "." and dashes "-"

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
  , Tokens.identLetter = alphaNum <|> oneOf "._$-"

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
symbolParser
  :: String -> Parsec String u String
symbolParser = Tokens.symbol riscVLang
identifier
  :: Parsec String u String
identifier = Tokens.identifier riscVLang
whiteSpace :: Parsec String u ()
whiteSpace = Tokens.whiteSpace riscVLang
comma :: Parsec String u String
comma = Tokens.comma riscVLang
integer :: Parsec String u Integer
integer = Tokens.integer riscVLang

-- | Read file
readFileRV :: FilePath -> IO String
readFileRV file = do
  contents <- readFile file
  return contents

-- | Parse RiscV directly from file
riscvParseFile :: String -> IO (Either ParseError [LineOfRiscV])
riscvParseFile fileName = do
  fileContent <- readFileRV fileName
  return $ riscvParser fileName fileContent 

-- | Parse RiscV given the name of the file (only used for errors) and
-- a RiscV assembly program.
riscvParser :: String -> String -> Either ParseError [LineOfRiscV]
riscvParser rvFileName rvFile = catMaybes <$> mapM (parse riscvLnParser rvFileName) (lines rvFile)

-- The Tokens library in Parsec doesn't deal well with end of
-- lines. Most commands (all the `lexeme` ones) consume "empty" space,
-- but the end of line is considered empty space and there is no way
-- to change that. I found it easier to just parse line by line. Since
-- there are no multi level comments (or anything really) this is ok.
--
-- We lose the traceback location of errors, the parser will always
-- report an error in line 1. Later, with some unwrapping of the error
-- we can fix that.


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
infixl 4 <.>
(<.>) :: forall st a b. Parsec String st (a -> b) -> Parsec String st a -> Parsec String st b
(<.>) p1 p2 = p1 <* comma <*> p2


parse32I :: Parsec String st InstrRV32I
parse32I = choiceTry
      [
        -- Jump instructions
        "jal"    ==> JAL    <*> regParser <.> offsetParser
      , "jalr"   ==> JALR   <*> regParser <.> regParser <.> offsetParser
        -- Branch instructions @br r1, r2, offset@
      , "beq"  ==> BranchInstr BEQ  <*> regParser <.> regParser <.> offsetParser
      , "bne"  ==> BranchInstr BNE  <*> regParser <.> regParser <.> offsetParser
      , "blt"  ==> BranchInstr BLT  <*> regParser <.> regParser <.> offsetParser
      , "bge"  ==> BranchInstr BGE  <*> regParser <.> regParser <.> offsetParser
      , "bltu" ==> BranchInstr BLTU <*> regParser <.> regParser <.> offsetParser
      , "bgeu" ==> BranchInstr BGEU <*> regParser <.> regParser <.> offsetParser
      -- Memory instructions @memop r1, offset(r2)@
      , "lb"   ==> MemInstr32 LB  <*> regParser <.> offsetParser <*> parens regParser
      , "lh"   ==> MemInstr32 LH  <*> regParser <.> offsetParser <*> parens regParser
      , "lw"   ==> MemInstr32 LW  <*> regParser <.> offsetParser <*> parens regParser
      , "lbu"  ==> MemInstr32 LBU <*> regParser <.> offsetParser <*> parens regParser
      , "lhu"  ==> MemInstr32 LHU <*> regParser <.> offsetParser <*> parens regParser
      , "sb"   ==> MemInstr32 SB  <*> regParser <.> offsetParser <*> parens regParser
      , "sh"   ==> MemInstr32 SH  <*> regParser <.> offsetParser <*> parens regParser
      , "sw"   ==> MemInstr32 SW  <*> regParser <.> offsetParser <*> parens regParser
      -- unary instructions
      , "lui"   ==> LUI    <*> regParser <.> immediateParser
      , "auipc" ==> AUIPC  <*> regParser <.> immediateParser
      -- Binary Integer Register-Immediate Instructions
      , "addi"  ==> ImmBinop32 ADDI  <*> regParser <.> regParser <.> immediateParser       
      , "slti"  ==> ImmBinop32 SLTI  <*> regParser <.> regParser <.> immediateParser      
      , "sltiu" ==> ImmBinop32 SLTIU <*> regParser <.> regParser <.> immediateParser      
      , "xori"  ==> ImmBinop32 XORI  <*> regParser <.> regParser <.> immediateParser      
      , "ori"   ==> ImmBinop32 ORI   <*> regParser <.> regParser <.> immediateParser      
      , "andi"  ==> ImmBinop32 ANDI  <*> regParser <.> regParser <.> immediateParser      
      , "slli"  ==> ImmBinop32 SLLI  <*> regParser <.> regParser <.> immediateParser      
      , "srli"  ==> ImmBinop32 SRLI  <*> regParser <.> regParser <.> immediateParser      
      , "srai"  ==> ImmBinop32 SRAI  <*> regParser <.> regParser <.> immediateParser      
      -- Integer Register-Register Instructions
      , "add"   ==> RegBinop32 ADD  <*> regParser <.> regParser <.> regParser
      , "sub"   ==> RegBinop32 SUB  <*> regParser <.> regParser <.> regParser
      , "sll"   ==> RegBinop32 SLL  <*> regParser <.> regParser <.> regParser
      , "slt"   ==> RegBinop32 SLT  <*> regParser <.> regParser <.> regParser
      , "sltu"  ==> RegBinop32 SLTU <*> regParser <.> regParser <.> regParser
      , "xor"   ==> RegBinop32 XOR  <*> regParser <.> regParser <.> regParser
      , "srl"   ==> RegBinop32 SRL  <*> regParser <.> regParser <.> regParser
      , "sra"   ==> RegBinop32 SRA  <*> regParser <.> regParser <.> regParser
      , "or"    ==> RegBinop32 OR   <*> regParser <.> regParser <.> regParser
      , "and"   ==> RegBinop32 AND  <*> regParser <.> regParser <.> regParser
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
        "lwu"    ==> MemInstr64 LWU <*> regParser <.> offsetParser <*> parens regParser
      , "ld"     ==> MemInstr64 LD  <*> regParser <.> offsetParser <*> parens regParser
      , "sd"     ==> MemInstr64 SD  <*> regParser <.> offsetParser <*> parens regParser
        -- Integer Register-Immediate Instructions 
      , "addiw"  ==> ImmBinop64 ADDIW <*> regParser <.> regParser <.> immediateParser
      , "slliw"  ==> ImmBinop64 SLLIW <*> regParser <.> regParser <.> immediateParser
      , "srliw"  ==> ImmBinop64 SRLIW <*> regParser <.> regParser <.> immediateParser
      , "sraiw"  ==> ImmBinop64 SRAIW <*> regParser <.> regParser <.> immediateParser
      -- Integer Register-Register Instructions
      , "addw"  ==> RegBinop64 ADDW <*> regParser <.> regParser <.> regParser
      , "subw"  ==> RegBinop64 SUBW <*> regParser <.> regParser <.> regParser
      , "sllw"  ==> RegBinop64 SLLW <*> regParser <.> regParser <.> regParser
      , "srlw"  ==> RegBinop64 SRLW <*> regParser <.> regParser <.> regParser
      , "sraw"  ==> RegBinop64 SRAW <*> regParser <.> regParser <.> regParser
      ]




parse32M :: Parsec String st InstrExt32M
parse32M = choiceTry
      [ "mul"    ==> MUL    <*> regParser <.> regParser <.> regParser
      , "mulh"   ==> MULH   <*> regParser <.> regParser <.> regParser
      , "mulhsu" ==> MULHSU <*> regParser <.> regParser <.> regParser
      , "mulhu " ==> MULHU  <*> regParser <.> regParser <.> regParser
      , "div"    ==> DIV    <*> regParser <.> regParser <.> regParser
      , "divu"   ==> DIVU   <*> regParser <.> regParser <.> regParser
      , "rem"    ==> REM    <*> regParser <.> regParser <.> regParser
      , "remu"   ==> REMU   <*> regParser <.> regParser <.> regParser
      ]






parse64M :: Parsec String st InstrExt64M
parse64M = choiceTry
      [ "mulw"  ==> MULW  <*> regParser <.> regParser <.> regParser
      , "divw"  ==> DIVW  <*> regParser <.> regParser <.> regParser
      , "divuw" ==> DIVUW <*> regParser <.> regParser <.> regParser
      , "remw"  ==> REMW  <*> regParser <.> regParser <.> regParser
      , "remuw" ==> REMUW <*> regParser <.> regParser <.> regParser
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
      , "call"   ==> CallPI <*> (Just <$> regParser) <.> immediateParser
      , "call"   ==> CallPI     Nothing              <*> offsetParser
      , "tail"   ==> TailPI <*> offsetParser
      -- Total Fence
      , "fence"  ==> FencePI
      -- Unary immediate
      , "li"     ==> LiPI <*> regParser <.> immediateParser
      -- nop
      , "nop"    ==> NopPI
      -- Absolute load/store
      , "la"     ==> (AbsolutePI ∘∘ PseudoLA) <*> regParser <.> immediateParser
      , "lla"    ==> (AbsolutePI ∘∘ PseudoLLA) <*> regParser <.> immediateParser
      , "lb"    ==> (AbsolutePI ∘∘∘ PseudoLoad MemByte)   <*> regParser <.> offsetParser <*> parens regParser
      , "lh"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemHalf)   <*> regParser <.> offsetParser <*> parens regParser
      , "lw"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemWord)   <*> regParser <.> offsetParser <*> parens regParser
      , "ld"     ==> (AbsolutePI ∘∘∘ PseudoLoad MemDouble) <*> regParser <.> offsetParser <*> parens regParser
      , "sb"     ==> (AbsolutePI ∘∘∘ PseudoStore MemByte)   <*> regParser <.> immediateParser <.> regParser
      , "sh"     ==> (AbsolutePI ∘∘∘ PseudoStore MemHalf)   <*> regParser <.> immediateParser <.> regParser
      , "sw"     ==> (AbsolutePI ∘∘∘ PseudoStore MemWord)   <*> regParser <.> immediateParser <.> regParser
      , "sd"     ==> (AbsolutePI ∘∘∘ PseudoStore MemDouble) <*> regParser <.> immediateParser <.> regParser
      -- Unary register Pseudoinstructions
      , "mv"    ==> UnaryPI MOV   <*> regParser <.> regParser
      , "not"    ==> UnaryPI NOT   <*> regParser <.> regParser
      , "neg"    ==> UnaryPI NEG   <*> regParser <.> regParser
      , "negw"   ==> UnaryPI NEGW  <*> regParser <.> regParser
      , "sext.w"  ==> UnaryPI SEXTW <*> regParser <.> regParser
      -- Conditional Moves
      , "seqz"   ==> CMovPI SEQZ <*> regParser <.> regParser
      , "snez"   ==> CMovPI SNEZ <*> regParser <.> regParser
      , "sltz"   ==> CMovPI SLTZ <*> regParser <.> regParser
      , "sgtz"   ==> CMovPI SGTZ <*> regParser <.> regParser
      -- Alternative branches ZERO
      , "beqz"   ==> BranchZPI BEQZ <*> regParser <.> offsetParser
      , "bnez"   ==> BranchZPI BNEZ <*> regParser <.> offsetParser
      , "blez"   ==> BranchZPI BLEZ <*> regParser <.> offsetParser
      , "bgez"   ==> BranchZPI BGEZ <*> regParser <.> offsetParser
      , "bltz"   ==> BranchZPI BLTZ <*> regParser <.> offsetParser
      , "bgtz"   ==> BranchZPI BGTZ <*> regParser <.> offsetParser
      -- Alternative branches ZERO
      , "bgt"    ==> BranchPI BGT  <*> regParser <.> regParser <.> offsetParser
      , "ble"    ==> BranchPI BLE  <*> regParser <.> regParser <.> offsetParser
      , "bgtu"   ==> BranchPI BGTU <*> regParser <.> regParser <.> offsetParser
      , "bleu"   ==> BranchPI BLEU <*> regParser <.> regParser <.> offsetParser
      --  Alternative Jumps
      , "j"      ==> JmpImmPI JPseudo     <*> immediateParser
      , "jal"    ==> JmpImmPI JLinkPseudo <*> immediateParser
      , "jr"     ==> JmpRegPI JPseudo     <*> regParser
      , "jalr"   ==> JmpRegPI JLinkPseudo <*> regParser
      ]

{- Some directives we are not supporting yet:

      , "section"       ==> SECTION    <*> textParse <.> textParse


-}
directiveParse :: Parsec String st Directive
directiveParse = choiceTry 
      -- Function call/return instructions
      [ "align"         ==> ALIGN      <*> integer
      , "file"          ==> FILE       <*> textParse
      , "globl"         ==> GLOBL      <*> identifier
      , "local"         ==> LOCAL      <*> textParse
      , "comm"          ==> COMM       <*> textParse <.> integer   <.> integer
      , "common"        ==> COMMON     <*> textParse <.> integer   <.> integer
      , "ident"         ==> IDENT      <*> textParse
      , "size"          ==> SIZE       <*> identifier <.> identifier
      , "text"          ==> TEXT   
      , "data"          ==> DATA   
      , "rodata"        ==> RODATA 
      , "bss"           ==> BSS    
      , "string"        ==> STRING     <*> textParse
      , "asciz"         ==> ASCIZ      <*> textParse
      , "equ"           ==> EQU        <*> identifier <.> (fromInteger <$> integer)
      , "type"          ==> TYPE       <*> identifier <* comma <* string "@function"
      , "option"        ==> OPTION     <*> optParser
      , "balign"        ==> BALIGN     <*> integer <.> (Just <$> integer)
      , "balign"        ==> BALIGN     <*> integer <.> (return Nothing)
      , "zero"          ==> ZERO       <*> integer
      , "variant_cc"    ==> VARIANT_CC <*> identifier
      , "macro"         ==> MACRO      <*> identifier <.> identifier <.> (return [])
      , "endm"          ==> ENDM

      , "attribute"       ==> ATTRIBUTE    <*> tagParser    <.> (Right  <$> textParse )
      , "attribute"       ==> ATTRIBUTE    <*> tagParser    <.> (Left   <$> integer   )
      
      , "p2align"       ==> P2ALIGN    <*> integer    <.> (Just <$> integer) <.> (Just <$> integer)
      , "p2align"       ==> P2ALIGN    <*> integer    <.> (return Nothing)   <.> (Just <$> integer)
      , "p2align"       ==> P2ALIGN    <*> integer    <.> (Just <$> integer) <*> (return Nothing)
      , "p2align"       ==> P2ALIGN    <*> integer    <*> (return Nothing)   <*> (return Nothing)
      ]
  where 
    -- Doesn't admit escaped quotations. Everything insie the two quotations is the text
    textParse    = char '"' *> many (noneOf ['"']) <* char '"'
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



-- filterDirs :: [LineOfRiscV] -> [String]
-- filterDirs instrs = catMaybes $ filterDir <$> instrs
--   where
--     filterDir :: LineOfRiscV -> Maybe String
--     filterDir (Directive dir) = Just dir
--     filterDir _ = Nothing

-- test = do
--   code <- riscvParseFile "src/RiscV/grit-rv64-20211105.s"
--   let dirs = filterDirs <$> code
--   case dirs of
--     Left e -> putStr $ show e
--     Right dirs' -> do
--       let dirs_first = (head . words) <$> dirs'
--       mapM_ putStrLn dirs_first
