{-|
Module      : Pretty Print MicroRAM programs
Description :  
Maintainer  : santiago@galois.com
Stability   : prototype

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MicroRAM.PrettyPrint (
  microPrint,
  pprintInst,
  pprintReg,
  pprintOp,
  pprintConst) where

import MicroRAM
import Compiler.Common
import Compiler.IRs
import Compiler.LazyConstants
import Compiler.Metadata
import Compiler.Registers

import Control.Monad.State.Lazy
import Data.Bifunctor (first)
import Data.ByteString.Short (fromShort)
import Data.List
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Util.Util

type MetaState = State Metadata

microPrint :: (Pretty wrd, Bounded wrd, Integral wrd, Show wrd, Show reg, Pretty (PrettyPrintWrapper reg)) => AnnotatedProgram Metadata reg wrd -> String
microPrint = show . prettyAnn

prettyAnn :: forall reg wrd a . (Show reg, Show wrd, Bounded wrd, Integral wrd, Pretty wrd, Pretty (PrettyPrintWrapper reg)) => AnnotatedProgram Metadata reg wrd -> Doc a
prettyAnn prog = mconcat $ evalState (mapM prettyPrintInstr prog) $ defaultMetadata{mdLine = -1} 
  where prettyPrintInstr :: (Instruction reg wrd, Metadata) -> MetaState (Doc a)
        prettyPrintInstr (instr, md') = do
          md <- get
          let outString =
                functionString md md'     <>
                blockString md md'        <>
                pretty instr              <>
                lineString md md'         <>
                "\n"
          put md'
          return outString

        functionString md md' = if (mdFunction md == mdFunction md') then "" else
          "\n\n// Define " <> (cleanName $ mdFunction md') <> ": \n"
        blockString md md'    = if (mdBlock md == mdBlock md') then "" else 
          "\n// " <> (cleanName $ mdBlock md') <> ": \n"
        lineString md md' = if (mdLine md == mdLine md') then "" else 
          "\t \t // Line " <> (viaShow $ mdLine md')

cleanName :: String -> Doc a
cleanName st =
  pretty $ filter (\l -> l /= '\"') $
  if "Name " `isPrefixOf` st then drop 5 st else st

pprintInst :: (Pretty wrd, Bounded wrd, Integral wrd, Show wrd) => Instruction' Int Int (Operand Int wrd) -> String
pprintInst = show . pretty

instance Pretty func => Pretty (IRprog mdata wrd func) where
  pretty (IRprog tenv globals code) =
    -- TODO: tenv + globals
    vsep $ map pretty code

instance Pretty Name where
  pretty (Name s) = pretty (decodeUtf8 $ fromShort s) <> "_"
  pretty (NewName n) = pretty n
  
instance Pretty Ty where
  pretty = viaShow -- TODO: improve this

-- TODO: This currently requires UndecidableInstances. Can we remove this requirement?
instance (Pretty (PrettyPrintWrapper op)) => Pretty (RTLInstr' op) where
  pretty (RRet opM) = "ret" <> maybe "" ((" " <>) . pretty . PPW) opM
  pretty (RAlloc opM s n) = maybe "" (\op -> pretty (PPW op) <> " = ") opM <> "alloc(" <> pretty (PPW n) <> " * " <> pretty s <> ")"
  pretty (RPhi reg srcs) = pretty (PPW reg) <> " = phi(" <> concatWith (surround ",") (map (pretty . first PPW) srcs) <> ")"
  pretty (RCall _retTy regM f _argTys args) = maybe "" (\r -> pretty (PPW r) <> " = ") regM <> pretty (PPW f) <> "(" <> concatWith (surround ",") (map (pretty . PPW) args) <> ")"

instance Pretty wrd => Pretty (LazyConst l wrd) where
  pretty (SConst s) = pretty s
  pretty (LConst _) = "lazy_constant"

instance (Pretty (PrettyPrintWrapper reg), Pretty wrd) => Pretty (PrettyPrintWrapper (MAOperand reg wrd)) where
  pretty (PPW (AReg reg)) = pretty $ PPW reg
  pretty (PPW (LImm lc)) = pretty lc
  pretty (PPW (Label l)) = "@" <> cleanName l
  pretty (PPW (Glob n)) = pretty n
  pretty (PPW (HereLabel)) = "@here"

instance (Show reg, Show wrd, Pretty wrd, Pretty (PrettyPrintWrapper reg)) => Pretty (MIRInstruction mdata reg wrd) where
  pretty (MirM i _mdata) = pretty i -- <> " // " <> pretty mdata
  pretty (MirI i _mdata) = pretty i -- <> " // " <> pretty mdata

instance (Pretty name, Pretty inst) => Pretty (BB name inst) where
  pretty (BB name inst inst' _cfg) = 
    vsep [ "/// Block " <> pretty name
         , vsep (map pretty inst)
         , vsep (map pretty inst')
         , line
         ]

instance (Pretty name, Pretty param, Pretty block) => Pretty (Function name param block) where
  pretty (Function name retTy argTys blocks _nextReg) =
    vsep [ "// " <> pretty name <> " :: " <> prettyArgs argTys <> " -> " <> pretty retTy
         , vsep (map pretty blocks)
         , line
         ]
    where
      prettyArgs []     = "()"
      prettyArgs argTys = concatWith (surround " -> ") (map pretty argTys)

instance (Show reg, Show op1, Show op2, Pretty (PrettyPrintWrapper reg), Pretty (PrettyPrintWrapper op1), Pretty (PrettyPrintWrapper op2)) => Pretty (Instruction' reg op1 op2) where
  pretty (Iand r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <> " && "<> pretty (PPW op)
  pretty (Ior r1 r2 op) = pretty (PPW r1) <>" = "<>  pretty (PPW r2) <>" || "<> pretty (PPW op)
  pretty (Ixor r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" ^ "<> pretty (PPW op)
  pretty (Inot r1 op) = pretty (PPW r1) <>" = ! "<>  pretty (PPW op)
  pretty (Iadd r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" + "<> pretty (PPW op)
  pretty (Isub r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" - "<> pretty (PPW op)
  pretty (Imull r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" * "<> pretty (PPW op)
  pretty (Iumulh r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" * "<> pretty (PPW op)
  pretty (Ismulh r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" * "<> pretty (PPW op)
  pretty (Iudiv r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" / "<> pretty (PPW op)
  pretty (Iumod r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" % "<> pretty (PPW op)
  pretty (Ishl r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" << "<>  pretty (PPW op)
  pretty (Ishr r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" >> "<>  pretty (PPW op)
  pretty (Icmpe r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" == "<>  pretty (PPW op)
  pretty (Icmpa r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" >u "<>  pretty (PPW op)
  pretty (Icmpae r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" >=u "<>  pretty (PPW op)
  pretty (Icmpg r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" > "<>  pretty (PPW op)
  pretty (Icmpge r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <>" >= "<>  pretty (PPW op)
  pretty (Imov r1 op) = pretty (PPW r1) <>" = "<> pretty (PPW op)
  pretty (Icmov r1 r2 op) = pretty (PPW r1) <>" = "<> pretty (PPW r2) <> "  ? "<> pretty (PPW op) <>" : "<> pretty (PPW r1) 
  pretty (Ijmp op) = "jmp "<> pretty (PPW op)
  pretty (Icjmp r2 op) = "if " <> pretty (PPW r2) <> " jmp "<> pretty (PPW op)
  pretty (Icnjmp r2 op) = "if not" <> pretty (PPW r2) <> " jmp "<> pretty (PPW op)
  pretty (Istore _ op r1) = "*("<> pretty (PPW op) <>") = "<> pretty (PPW r1)
  pretty (Iload _ r1 op) = pretty (PPW r1) <>" = *("<> pretty (PPW op) <> ")"
  -- pretty (Iread r1 op) = pretty (PPW r1) pretty (PPW op)
  pretty (Ianswer op) = "ans "<> pretty (PPW op)
  pretty i = viaShow i -- TODO

-- Newtype wrappers to call registers + operands.
newtype PrettyPrintWrapper a = PPW a

-- Name used as register.
instance Pretty (PrettyPrintWrapper Name) where
  pretty (PPW n) = "%" <> pretty n

-- Int used as register.
instance Pretty (PrettyPrintWrapper Int) where
  pretty (PPW r) | r == ax = "%ax"
  pretty (PPW r) | r == bp = "%bp"
  pretty (PPW r) | r == sp = "%sp"
  pretty (PPW r)           = "%" <> pretty r

pprintReg :: Int -> String
pprintReg = show . pretty . PPW

pprintOp  :: (Bounded a, Show a, Integral a, Pretty a) => Operand Int a -> String
pprintOp = show . pretty . PPW

instance (Bounded a, Show a, Integral a, Pretty a, Pretty (PrettyPrintWrapper r)) => Pretty (PrettyPrintWrapper (Operand r a)) where
  pretty (PPW (Reg r)) = pretty $ PPW r
  pretty (PPW (Const c)) = unAnnotate $ prettyConst c
  
pprintConst :: (Bounded a, Show a, Integral a, Pretty a) => a -> String
pprintConst = show . prettyConst

prettyConst :: (Bounded a, Show a, Integral a, Pretty a) => a -> Doc a
prettyConst c | c > (maxBound - 100) = "-" <> pretty (maxBound - c +1)
prettyConst c | c < 100 = pretty c
prettyConst c = pretty $ showHex c
