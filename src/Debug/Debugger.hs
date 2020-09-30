{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Debugger
Description : Suit of tools for easy testing and debugging.
Maintainer  : santiago@galois.com
Stability   : prototype

Provide following tools:

* Summary: Pretty prints summary of an execution.


-}
module Debug.Debugger where

import qualified GHC.Generics as G
import Data.Data
--import Data.Default

import Text.PrettyPrint.Tabulate
import Text.PrettyPrint.Boxes hiding ((<>))
import Text.Printf



import Util.Util
-- LLVM
import qualified LLVM.AST as LLVM

-- Local 
import Compiler
--import Compiler.CallingConvention
import Compiler.CompilationUnit
--import Compiler.InstructionSelection
--import Compiler.IRs
--import Compiler.Legalize
--import Compiler.RegisterAlloc
import Compiler.Registers
--import Compiler.RemoveLabels
--import Compiler.Stacking

import qualified Data.Set as Set

import MicroRAM.MRAMInterpreter
import MicroRAM
import LLVMutil.LLVMIO


-- * Summary: Pretty prints summary of an execution.
{- We print a summary of an execution.
   It will only display some registers and some memory.
   The summary can be easily customized witha CustomSummary,
   record.

-}


data CustomSummary mreg = CS
  { showPC :: Bool
  , showRegs :: Bool
  , theseRegs :: Maybe [mreg] -- ^ Show these registers. Default is sp, bp, ax.
  , showMem :: Bool
  , theseMem :: [MWord] -- ^ Print this memory locations. Defualt is [0,1,2,3,4]
  , showFlag :: Bool
  , showAnswer :: Bool
  , showBug :: Bool
  , showPoison :: Bool
  , showAdvice :: Bool
  }


memSummarySize :: Integer
memSummarySize = 5

toSummaryMem :: [MWord] -> Mem' -> [MWord]
toSummaryMem theseLocations m =
  map (\loc -> load loc m) theseLocations
  
defaultSummary :: CustomSummary mreg
defaultSummary = CS True True Nothing True [0..4] True True True False False
  
defaultCSInt :: CustomSummary Int 
defaultCSInt = defaultSummary

defaultCSName :: CustomSummary AReg 
defaultCSName = defaultSummary


-- | Reserved regs
data MaybeWord = JustW MWord | NoW
  deriving (G.Generic, Data)

instance Show MaybeWord where
  show (JustW w) = show w
  show NoW = "-"

lookupReg' :: Regs a => a -> RegBank a MWord -> MaybeWord
lookupReg' a bank =
  case lookupReg a bank of
    Just w -> JustW w
    Nothing -> NoW


data ResRegs = RRs
  { esp :: MaybeWord
  , ebp :: MaybeWord
  , eax :: MaybeWord }
  deriving (Show, G.Generic, Data)
instance Tabulate ResRegs ExpandWhenNested
instance CellValueFormatter ResRegs

data SummaryState = SState {
  pc_ :: MWord
  , flag_ :: MWord
  , bug_ :: MWord
  , answer_ :: MWord
  , regs_ :: ResRegs
  , registers_ :: [MaybeWord] -- Custom regs
  , mem_ :: [MWord]
  , psn_ :: [MWord]
  , advc_ :: String
      }
  deriving (Show, G.Generic, Data)
instance Tabulate (SummaryState ) ExpandWhenNested

instance CellValueFormatter Word
instance CellValueFormatter [Word]
instance CellValueFormatter MWord
instance CellValueFormatter [MWord]
instance CellValueFormatter MaybeWord
instance CellValueFormatter [MaybeWord]


toSummaryRegs :: Regs mreg => RegBank mreg MWord -> ResRegs
toSummaryRegs rmap =
  RRs
  (lookupReg' sp rmap)
  (lookupReg' bp rmap)
  (lookupReg' ax rmap)

toSummaryRegsCustom :: Regs mreg => RegBank mreg MWord -> Maybe [mreg] -> [MaybeWord]
toSummaryRegsCustom _rmap Nothing = []
toSummaryRegsCustom rmap (Just regs) =
  map (\r -> lookupReg' r rmap) regs


toSummary :: Regs mreg => Maybe [mreg] -> [MWord] -> ExecutionState mreg -> SummaryState
toSummary theseRegs theseMems st  =
  SState 
  { pc_ = pc st
  , regs_ = toSummaryRegs $ regs st
  , registers_ = toSummaryRegsCustom (regs st) theseRegs
  , mem_ = toSummaryMem theseMems (mem st) 
  , psn_ = Set.toList (psn st) 
  , flag_ = bool2word $ flag st
  , bug_ = bool2word $ bug_flag st
  , answer_ = answer st
  , advc_ = renderAdvc $ advice st
  } where bool2word True = 1
          bool2word False = 0
summary :: Regs mreg =>  Maybe [mreg] -> [MWord] -> Trace mreg -> [SummaryState]
summary theseRegs theseMems t =  map customSummary t
  where customSummary = toSummary theseRegs theseMems 


renderSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> Box
renderSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer sBug sPoison sAdvice) t n =
  renderTableWithFlds flds $ take n $ summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
  where flds = fldDepends sPC pc_ ++
               (fldDepends (sRegs && sCustRegs) registers_) ++
               (fldDepends (sRegs && not sCustRegs) regs_) ++  -- Shows default regs or custom ones. if sCustRegs then registers_ else regs_)
               (fldDepends sMem mem_) ++ 
               (fldDepends sFlag flag_)  ++ 
               (fldDepends sAnswer answer_) ++
               (fldDepends sBug bug_) ++ 
               (fldDepends sPoison psn_) ++ 
               (fldDepends sAdvice advc_)
        fldDepends cond ret = if cond then [DFld ret] else []
        sCustRegs = case custRegs of
                      Just _ -> True
                      Nothing -> False

-- | Pretty prints summary of an execution.                      
printSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> IO ()
printSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer sBug sPoison sAdvice) t n =do
  printf fldsTxt         -- Hack to get headers (check my issue on the Tabular package!)
  printTableWithFlds flds $ take n $ theSummary
  where theSummary = summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
        flds = fldDepends sPC [DFld pc_]
               ++ (fldDepends sFlag [DFld flag_]) 
               ++ (fldDepends sBug [DFld bug_]) 
               ++ (fldDepends sAnswer [DFld answer_])
               ++ (fldDepends sPoison [DFld psn_])
               ++ (fldDepends (sRegs && sCustRegs) [DFld registers_]) 
               ++ (fldDepends (sRegs && not sCustRegs) [DFld regs_])   -- Shows default regs or custom ones. if sCustRegs then registers_ else regs_)
               ++ (fldDepends sMem [DFld mem_])
               ++ (fldDepends sAdvice [DFld advc_ ])
        fldDepends cond ret = if cond then ret else []
        sCustRegs = case custRegs of
                      Just _ -> True
                      Nothing -> False
        fldsTxt = fldDepends sPC "PC    "                 -- Hack to get Headers
                  ++ (fldDepends sFlag "Flag   ")   
                  ++ (fldDepends sBug "Bug   ")   
                  ++ (fldDepends sAnswer "Ansr    ") 
                  ++ (fldDepends sPoison "Poisons")
                  ++ (fldDepends sRegs "Regs")
                  ++ filler
                  ++ (fldDepends sMem "Mem \t")
                  ++  "\n"
        filler = take (regMemTextDistance + 1) $ cycle " "  -- super hack to align stuff (otherwise I'll have to rewrite Tabular)
        regMemTextDistance =  length $ regs0text
        regs0text = if sCustRegs then show $ registers_ (theSummary !! 0) else show $ regs_ (theSummary !! 0)
        


-- Example
{- | Example
@
t::Trace AReg
t = [init_state [] [],init_state [] [] ]
@

Then run:
λ> printSummary defaultSummary t 2
PC   Flag  Ansr   Regs                                Mem 	
0     0     0     RRs {esp = 0, ebp = 0, eax = 0}     [0,0,0,0,0]
0     0     0     RRs {esp = 0, ebp = 0, eax = 0}     [0,0,0,0,0]
@
-}

-- * Loading and saving
{- Tools to load and store programs (at any level) and execution traces.

-}
fromLLVMFile :: FilePath -> IO LLVM.Module
fromLLVMFile = llvmParse

fromMRAMFile :: FilePath
             -> IO $ CompilationResult (Program AReg MWord)
fromMRAMFile file = do
  contents <- readFile file
  return $ read contents
  
runFromFile  :: FilePath -> IO (Trace AReg)
runFromFile file = do
  prog <- fromMRAMFile file
  return $ run prog
  
summaryFromFile ::
  FilePath ->
  CustomSummary AReg ->
  Int -> IO ()
summaryFromFile file cs length = do
  trace <- runFromFile file
  printSummary cs trace length
  



-- * Pretty printing

pprint :: CompilationResult (Program Int MWord) -> String
pprint compUnit =
  let prog = lowProg $ programCU compUnit in
    concat $ map (\(n,inst) -> show (n::Integer) ++ ". " ++ pprintInst inst ++ "\n") $ enumerate prog

pprintInst :: Instruction' AReg AReg (Operand AReg MWord) -> String
pprintInst (Iand r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" && "<> (pprintOp op)
pprintInst (Ior r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" || "<> (pprintOp op)
pprintInst (Ixor r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" ^ "<> (pprintOp op)
pprintInst (Inot r1 op) = (pprintReg r1) <>" = ! "<> (pprintOp op)
pprintInst (Iadd r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" + "<> (pprintOp op)
pprintInst (Isub r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" - "<> (pprintOp op)
pprintInst (Imull r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" * "<> (pprintOp op)
pprintInst (Iumulh r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" * "<> (pprintOp op)
pprintInst (Ismulh r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" * "<> (pprintOp op)
pprintInst (Iudiv r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" / "<> (pprintOp op)
pprintInst (Iumod r1 r2 op) = (pprintReg r1) <>" = "<> (pprintReg r2) <>" % "<> (pprintOp op)
-- pprintInst (Ishl r1 r2 op) = (pprintReg r1) (pprintReg r2) (pprintOp op)
-- pprintInst (Ishr r1 r2 op) = (pprintReg r1) (pprintReg r2) (pprintOp op)
-- pprintInst (Icmpe r1 op) = (pprintReg r1) (pprintOp op)
-- pprintInst (Icmpa r1 op) = (pprintReg r1) (pprintOp op)
-- pprintInst (Icmpae r1 op) = (pprintReg r1) (pprintOp op)
-- pprintInst (Icmpg r1 op) = (pprintReg r1) (pprintOp op)
-- pprintInst (Icmpge r1 op) = (pprintReg r1) (pprintOp op)
pprintInst (Imov r1 op) = (pprintReg r1) <>" = "<> (pprintOp op)
pprintInst (Icmov r1 op) = (pprintReg r1) <>" = flag ? "<> (pprintOp op) <>" : "<> (pprintReg r1) 
pprintInst (Ijmp op) = "jmp "<> (pprintOp op)
pprintInst (Icjmp op) = "cjmp "<> (pprintOp op)
pprintInst (Icnjmp op) = "cnjmp "<> (pprintOp op)
pprintInst (Istore op r1) = "*("<> (pprintOp op) <>") = "<> (pprintReg r1)
pprintInst (Iload r1 op) = (pprintReg r1) <>" = *("<> (pprintOp op) <> ")"
-- pprintInst (Iread r1 op) = (pprintReg r1) (pprintOp op)
pprintInst (Ianswer op) = "ans "<> (pprintOp op)
pprintInst i = show i -- TODO

pprintReg :: AReg -> String
pprintReg r | r == ax = "%ax"
pprintReg r | r == bp = "%bp"
pprintReg r | r == sp = "%sp"
pprintReg r = "%" <> show r
--pprintReg r = show r

pprintOp :: Show a => Operand AReg a -> String
pprintOp (Reg r) = pprintReg r
pprintOp (Const c) = show c

pprintFromFile :: FilePath -> IO ()
pprintFromFile file = do
  prog <- fromMRAMFile file
  putStr $ pprint prog

readProg :: String -> Program AReg MWord
readProg = read

firstRegs :: Word -> [AReg]
firstRegs bound = map fromWord $ map (2*) [0..bound] 

myCS :: CustomSummary AReg
myCS = defaultCSName
  {theseRegs = Just $ [0..2]
  ,showMem = False
  ,theseMem = [0..15]
  ,showAdvice = True
  , showPoison = True}



-- TESTING GROUNDS
fromAscii :: Int -> Char
fromAscii = toEnum



-- Example
myfile, myllvmfile:: FilePath
myfile = "test/programs/MallocOOB/mallocOOB.c.micro" -- "programs/returnInput.micro"
myllvmfile = "programs/returnInput.ll"

pprintMyFile :: IO ()
pprintMyFile = pprintFromFile myfile

mram :: IO $ CompilationResult (Program AReg MWord)
mram =  fromMRAMFile "test/return42.micro"

{- | Example

summaryFromFile myfile myCS 300
-}

-- jpProgComp :: Word -> IO (Program VReg MWord)
jpProgComp :: Word -> IO (CompilationResult (Program AReg MWord))
jpProgComp len = do
    m <- fromLLVMFile "programs/driver-link.ll"
    return $ either undefined id $
      compile len m

{- SC: Broken after resgiter allocation was moved to
   work on compilation units, not just programs.
jpProg :: IO (Program VReg MWord)
jpProg = do
    m <- fromLLVMFile "test/programs/fibSlow.ll"
    return $ either undefined id $
      instrSelect m
      >>= legalize
      >>= registerAlloc def
      >>= callingConvention
      >>= stacking
      >>= removeLabelsProg 
-}

cs :: CustomSummary mreg
cs = defaultSummary {theseMem = [0..27]}
--inp :: [MWord]
--inp = buildInitMem ["one","two", "three"]

-- m' <- prog2unit 200 <$> jpProg
-- putStrLn $ pprint m'
-- printSummary cs (run m') 28

