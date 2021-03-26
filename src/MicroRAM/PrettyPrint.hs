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

module MicroRAM.PrettyPrint (
  microPrint,
  pprintInst,
  pprintReg,
  pprintOp,
  pprintConst) where

import MicroRAM
import Compiler.IRs
import Compiler.Metadata
import Compiler.Registers

import Control.Monad.State.Lazy
import Data.List
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
  pretty (Icjmp r2 op) = "if " <> pretty (PPW r2) <> "jmp "<> pretty (PPW op)
  pretty (Icnjmp r2 op) = "if not" <> pretty (PPW r2) <> "jmp "<> pretty (PPW op)
  pretty (Istore _ op r1) = "*("<> pretty (PPW op) <>") = "<> pretty (PPW r1)
  pretty (Iload _ r1 op) = pretty (PPW r1) <>" = *("<> pretty (PPW op) <> ")"
  -- pretty (Iread r1 op) = pretty (PPW r1) pretty (PPW op)
  pretty (Ianswer op) = "ans "<> pretty (PPW op)
  pretty i = viaShow i -- TODO

-- Newtype wrappers to call registers + operands.
newtype PrettyPrintWrapper a = PPW a

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
