{-|
Module      : Pretty Print MicroRAM programs
Description :  
Maintainer  : santiago@galois.com
Stability   : prototype

-}

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
import Util.Util

type MetaState = State Metadata

microPrint :: (Bounded wrd, Integral wrd, Show wrd) => AnnotatedProgram Metadata Int wrd -> String
microPrint prog = concat $ evalState (mapM prettyPrintInstr prog) $ defaultMetadata{mdLine = -1} 
  where prettyPrintInstr :: (Bounded wrd, Integral wrd, Show wrd) => (Instruction Int wrd, Metadata) -> MetaState String
        prettyPrintInstr (instr, md') = do
          md <- get
          let outString =
                functionString md md'     ++
                blockString md md'        ++
                (pprintInst instr) ++
                lineString md md' ++
                "\n"
          put md'
          return outString

        functionString md md' = if (mdFunction md == mdFunction md') then "" else
          "\n\n// Define " <> (cleanName $ mdFunction md') <> ": \n"
        blockString md md'    = if (mdBlock md == mdBlock md') then "" else 
          "\n// " <> (cleanName $ mdBlock md') <> ": \n"
        lineString md md' = if (mdLine md == mdLine md') then "" else 
          "\t \t // Line " <> (show $ mdLine md')

        cleanName :: String -> String
        cleanName st =
          filter (\l -> l /= '\"') $
          if "Name " `isPrefixOf` st then drop 5 st else st


pprintInst :: (Bounded wrd, Integral wrd, Show wrd) => Instruction' Int Int (Operand Int wrd) -> String
pprintInst (Iand r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <> " && "<> pprintOp op
pprintInst (Ior r1 r2 op) = pprintReg r1 <>" = "<>  pprintReg r2 <>" || "<> pprintOp op
pprintInst (Ixor r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" ^ "<> pprintOp op
pprintInst (Inot r1 op) = pprintReg r1 <>" = ! "<>  pprintOp op
pprintInst (Iadd r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" + "<> pprintOp op
pprintInst (Isub r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" - "<> pprintOp op
pprintInst (Imull r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" * "<> pprintOp op
pprintInst (Iumulh r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" * "<> pprintOp op
pprintInst (Ismulh r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" * "<> pprintOp op
pprintInst (Iudiv r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" / "<> pprintOp op
pprintInst (Iumod r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" % "<> pprintOp op
pprintInst (Ishl r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" << "<>  pprintOp op
pprintInst (Ishr r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" >> "<>  pprintOp op
pprintInst (Icmpe r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" == "<>  pprintOp op
pprintInst (Icmpa r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" >u "<>  pprintOp op
pprintInst (Icmpae r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" >=u "<>  pprintOp op
pprintInst (Icmpg r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" > "<>  pprintOp op
pprintInst (Icmpge r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <>" >= "<>  pprintOp op
pprintInst (Imov r1 op) = pprintReg r1 <>" = "<> pprintOp op
pprintInst (Icmov r1 r2 op) = pprintReg r1 <>" = "<> pprintReg r2 <> "  ? "<> pprintOp op <>" : "<> pprintReg r1 
pprintInst (Ijmp op) = "jmp "<> pprintOp op
pprintInst (Icjmp r2 op) = "if " <> pprintReg r2 <> "jmp "<> pprintOp op
pprintInst (Icnjmp r2 op) = "if not" <> pprintReg r2 <> "jmp "<> pprintOp op
pprintInst (Istore _ op r1) = "*("<> pprintOp op <>") = "<> pprintReg r1
pprintInst (Iload _ r1 op) = pprintReg r1 <>" = *("<> pprintOp op <> ")"
-- pprintInst (Iread r1 op) = pprintReg r1 pprintOp op
pprintInst (Ianswer op) = "ans "<> pprintOp op
pprintInst i = show i -- TODO


pprintReg :: Int -> String
pprintReg r | r == ax = "%ax"
pprintReg r | r == bp = "%bp"
pprintReg r | r == sp = "%sp"
pprintReg r = "%" <> show r
--pprintReg r = show r

pprintOp  :: (Bounded a, Show a, Integral a) => Operand Int a -> String
pprintOp (Reg r) = pprintReg r
pprintOp (Const c) = pprintConst c              

pprintConst :: (Bounded a, Show a, Integral a) => a -> String
pprintConst c | c > (maxBound - 100) = "-" ++ show (maxBound - c +1)
pprintConst c | c < 100 = show c
pprintConst c = showHex c
