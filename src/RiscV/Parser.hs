{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module RiscV.Parser where

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

import Debug.Trace

-- Borrow some definitions from haskell
riscVLang :: Tokens.GenTokenParser String u Identity
riscVLang = Tokens.makeTokenParser riscVLangDef

riscVLangDef :: Lang.LanguageDef u
riscVLangDef = Tokens.LanguageDef
  {
    -- RiscV Assembly has no multiline comment 
    Tokens.commentStart = ""
  , Tokens.commentEnd = ""
  , Tokens.commentLine = "#"
  , Tokens.nestedComments = False
  -- | Symbol names begin with a letter or with one of ‘._’. On most
  -- machines, you can also use $ in symbol names; exceptions are
  -- noted in Machine Dependencies. Symbol names do not start with a
  -- digit. An exception to this rule is made for Local Labels, but we
  -- don't yet support it.
  , Tokens.identStart = letter <|> oneOf "._$"
  -- | That character may be followed by any string of digits,
  -- letters, dollar signs (unless otherwise noted for a particular
  -- target machine), and underscores.  We do not yet support qutoed
  -- symbols names by ‘"’ or multibyte characters.
  , Tokens.identLetter = alphaNum <|> oneOf "$_"

  -- | No operators in this language
  , Tokens.opStart        = parserFail "Attempt to read an operands"
  , Tokens.opLetter       = parserFail "Attempt to read an operands"

  -- | No reserved names 
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


-- parsedInput = parse someParser "source name" "some input"
test :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
test p = parse p ""
wordParser :: Parsec String st String
wordParser = many $ noneOf [' ']

-- | Read file
readFileRV :: FilePath -> IO String
readFileRV file = do
  contents <- readFile file
  return contents

-- Risk Parser

data LineOfRiskV =
    Comment     String
  | EmptyLn
  | LabelLn     String
  | Directive   String
  | Instruction Instr
  deriving (Show, Eq, Ord)

riscvParser :: String -> String -> Either ParseError [LineOfRiskV]
riscvParser rvFileName rvFile = catMaybes <$> mapM (parse riscvLnParser rvFileName) (lines rvFile)

riscvLnParser :: Parsec String st (Maybe LineOfRiskV)
riscvLnParser = try labelLnParse
                <|> try drctvLnParse
                <|> try instrLnParse
                <|> try emptyLnParse 
                <?> "Line of RiscV Assembly"
  where
    emptyLnParse,instrLnParse,labelLnParse:: Parsec String st (Maybe LineOfRiskV)
    -- Empty line, possibly with comments and or spaces/tabs, etc.  
    emptyLnParse = whiteSpace >> eof *> return Nothing

    drctvLnParse   = Just . Directive   <$> (tabParse *> char '.' *> textParse)
    instrLnParse   = Just . Instruction <$> (tabParse *> instrParser)
    labelLnParse   = Just . LabelLn     <$> identifier

    textParse    = many (noneOf ['\n'])
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

                              
regParser :: Parsec String st Reg
regParser = (parseFromPairs registerABInames) <?> "a register"
  where registerABInames = [
          ("zero",  X0)  -- ^ hardwired zero      
          ,("ra",   X1)  -- ^ return address     
          ,("sp",   X2)  -- ^ stack pointer      
          ,("gp",   X3)  -- ^ global pointer     
          ,("tp",   X4)  -- ^ thread pointer     
          ,("t0",   X5)  -- ^ temporary registers
          ,("t1",   X6)  
          ,("t2",   X7)
          ,("fp",   X8)  -- ^ frame pointer (same as s0)
          ,("s0",   X8)  -- ^ saved register (s0=fp)
          ,("s1",   X9)
          ,("a0",   X10) -- ^ function arguments / return values
          ,("a1",   X11)
          ,("a2",   X12) -- ^ function arguments
          ,("a3",   X13)
          ,("a4",   X14)
          ,("a5",   X15)
          ,("a6",   X16)
          ,("a7",   X17)
          ,("s2",   X18) -- ^ saved registers
          ,("s3",   X19)
          ,("s4",   X20)
          ,("s5",   X21)
          ,("s6",   X22)
          ,("s7",   X23)
          ,("s8",   X24)
          ,("s9",   X25)
          ,("s10",  X26)
          ,("s11",  X27)
          ,("t3",   X28) -- ^ temporary registers
          ,("t4",   X29)
          ,("t5",   X30)
          ,("t6",   X31)
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
symbolParser' :: Parsec String st String
symbolParser' =
  -- | Names start with a letter, underscore or period
  (letter <|> oneOf "._$")
  -- | Then any letter, number or underscore
  <:>  many (alphaNum <|> oneOf "$_")
  where
    (<:>) :: Applicative f => f a -> f [a] -> f [a]
    (<:>) a b = (:) <$> a <*> b

newtype Label = Label String
labelParser :: Parsec String st Label
labelParser =  Label <$> symbolParser' -- should it be symbolParser, without '

instrParser :: Parsec String st Instr
instrParser = Instr32I <$> parse32I

  
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
  
-- Infix parser.

infix_ :: String -> (a -> a -> a) -> Expr.Operator String u Identity a
infix_ operator func =
  Expr.Infix (symbolParser operator >> return func) Expr.AssocLeft



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
               
    
  
immediateParser' :: Parsec String st Imm
immediateParser' = 
  (modParse
  <|> trace "binop" (try binopParse)
  <|> trace "symbol" (ImmSymbol <$> identifier)
  <|> trace "number" (ImmNumber <$> toEnum <$> intParse)
  <?> "an immediate")
  where
    binopParse :: Parsec String st Imm
    binopParse = undefined
      -- ImmBinOp <$>
      -- immediateParser' <*>
      -- (spaces *> opParse <*) spaces <*>
      -- immediateParser
  
    modParse :: Parsec String st Imm
    modParse = char '%' *> pure ImmMod
               <*> parseFromPairs mods
               <* char '('
               <*> immediateParser
               <* char ')'
    intParse :: Parsec String st Int
    intParse = read <$> many digit
    mods :: [(String, Modifier)]
    mods = [
      ("hi", ModHi)
      , ("lo", ModLo)
      ]
    
{- | Offsets.  Offsets are differnt from Immediates. First of all,
bounded by a smaller number (2^12 I think?). Second, they accept no
symbols or modifiers (I think).
 -}
offsetParser :: Parsec String st Offset
offsetParser = immediateParser -- read <$> many digit 

{- | Notation.

  This is my attempt at making the parser easier to read and mantain.

-}
infixl 9 ==>
(==>) :: String -> a -> Parsec String st a
label ==> x = lexeme (string label *> pure x)

{- Why can't I use a comma instead? -}
infixl 4 <.>
(<.>) :: forall st a b. Parsec String st (a -> b) -> Parsec String st a -> Parsec String st b
(<.>) p1 p2 = p1 <* string ", " <* spaces <*> p2

-- slliParser :: Parsec String st RegisterImmediateInstr
-- slliParser = "slli" ==> (ShiftInstr SLLI) <*> regParser <.> regParser <.> immediateParser

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
      , "auipc" ==> AUIPC  <*> regParser <.> offsetParser
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







rotate :: [Char]
rotate =
  "# Collection of rotation examples\n#\n# Shows how to bit-rotate registers in the absence of RISC-V Bitmanip (\"B\")\n# extension.\n#\n# As of 2020, the \"B\" extension has draft status. However, it already\n# includes the ror/rol/rori/rorw/rolw/roriw instructions.\n#\n# cf. https://stackoverflow.com/a/60138854/427158\n#\n# 2020, Georg Sauthoff <mail@gms.tf>\n\n    .text\n    .balign 4\n    .global rotl3\nrotl3:\n    slli a2, a0,  3\n    srli a3, a0, 61\n    or   a0, a2, a3\n    ret\n    .global rotr3\nrotr3:\n    srli a2, a0,  3\n    slli a3, a0, 61\n    or   a0, a2, a3\n    ret\n    .global rotl\nrotl:\n    sll  a2,   a0, a1\n    sub  a4, zero, a1\n    srl  a3,   a0, a4\n    or   a0,   a2, a3\n    ret\n    .global rotr\nrotr:\n    srl  a2,   a0, a1\n    sub  a4, zero, a1\n    sll  a3,   a0, a4\n    or   a0,   a2, a3\n    ret\n"


