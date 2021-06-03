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
import Compiler.Analysis
import Compiler.BlockCleanup
import Compiler.CallingConvention
import Compiler.CompilationUnit
import Compiler.Globals
import Compiler.InstructionSelection
import Compiler.Intrinsics
--import Compiler.IRs
import Compiler.Legalize
import Compiler.LocalizeLabels
import Compiler.Metadata
import Compiler.IRs
import Compiler.RegisterAlloc
import Compiler.Registers
import Compiler.RemoveLabels
import Compiler.RemovePhi
import Compiler.Stacking
import Compiler.UndefinedFunctions
import Sparsity.Sparsity

import Data.Default
import qualified Data.Set as Set

import Debug.PrettyPrint
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
  show (JustW w) = pprintConst w
  show NoW = "-"

-- | Just words with automatic pretty printing
data PrintableWords = PW MWord
  deriving (Eq, Ord)
instance Show PrintableWords where
  show (PW w) = pprintConst w
    
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
  , bug_ :: MWord
  , answer_ :: MWord
  , regs_ :: ResRegs
  , registers_ :: [MaybeWord] -- Custom regs
  , mem_ :: [PrintableWords]
  , psn_ :: [PrintableWords]
  , advc_ :: String
      }
  deriving (Show, G.Generic)
instance Tabulate (SummaryState ) ExpandWhenNested

instance CellValueFormatter Word
instance CellValueFormatter [Word]
instance CellValueFormatter MWord
instance CellValueFormatter [MWord]
instance CellValueFormatter MaybeWord
instance CellValueFormatter [MaybeWord]
instance CellValueFormatter [PrintableWords]


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
  , mem_ = map PW $ toSummaryMem theseMems (mem st) 
  , psn_ = map PW $ Set.toList (psn st) 
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
renderSummary (CS sPC sRegs custRegs sMem theseMem _sFlag sAnswer sBug sPoison sAdvice) t n =
  renderTableWithFlds flds $ take n $ summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
  where flds = fldDepends sPC pc_ ++
               (fldDepends (sRegs && sCustRegs) registers_) ++
               (fldDepends (sRegs && not sCustRegs) regs_) ++  -- Shows default regs or custom ones. if sCustRegs then registers_ else regs_)
               (fldDepends sMem mem_) ++ 
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
                  ++ (fldDepends sPoison "Poisons    ")
                  ++ (fldDepends sRegs "Regs    ")
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
  let prog = pmProg $ lowProg $ programCU compUnit in
    concat $ map (\(n,inst) -> show (n::Integer) ++ ". " ++ pprintInst inst ++ "\n") $ enumerate prog

-- | Large numbers are shown in hex 

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
  {theseRegs = Just [] -- Just $ [0..8]
  ,showMem = False
  ,showFlag = False
  ,theseMem = [0..15]
  ,showAdvice = False
  , showPoison = False}



-- TESTING GROUNDS
fromAscii :: Int -> Char
fromAscii = toEnum



-- Example
myfile, myllvmfile:: FilePath
myfile = "test/programs/fibSlow.micro" -- "programs/returnInput.micro"
myllvmfile = "programs/returnInput.ll"

pprintMyFile :: IO ()
pprintMyFile = pprintFromFile myfile

mram :: IO $ CompilationResult (Program AReg MWord)
mram =  fromMRAMFile "test/return42.micro"

{- | Example
*******************************
summaryFromFile myfile myCS 300
*******************************
-}

-- jpProgComp :: Word -> IO (CompilationResult (AnnotatedProgram Metadata AReg MWord))
jpProgComp len = do
  m <- fromLLVMFile "test/programs/funcPointer.ll"
  -- return m
  return $ either (error . show) id $
        (justCompileWithNames instrSelect) (prog2unit len m)
    >>= (justCompile renameLLVMIntrinsicImpls)
    >>= (justCompile lowerIntrinsics)
    >>= (justCompile (catchUndefinedFunctions allowUndefFun))
    >>= (justCompile legalize)
    >>= (justCompile localizeLabels)

    >>= (justCompileWithNames edgeSplit)
    >>= (justCompileWithNames removePhi)
    >>= (registerAlloc def)
    >>= (justCompile callingConvention)
    >>= (replaceGlobals)
    >>= (justCompileWithNames stacking)
    >>= (justAnalyse (return . SparsityData . (forceSparsity spars))) 
    >>= (blockCleanup)
    >>= (removeLabels)
  where
    allowUndefFun = False
    spars = Nothing

    -- return $ either undefined id $
    --   compile False len m Nothing
-- p <- jpProgComp 2000
-- putStr $ microPrint $ lowProg $ programCU p
--
-- do {p <- jpProgComp 200; print $ pretty $ map fst $ programCU p}

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



{-
aa = CompUnit {programCU = [Imov (NewName 2) (Const 97),Istore (Reg (NewName 0)) (NewName 2),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 1),Imov (NewName 1) (Reg (NewName 0)),Ijmp (Const 6),Iadd (NewName 0) (NewName 0) (Const 4),Iadd (NewName 1) (NewName 1) (Const 2),Istore (Reg (NewName 1)) (NewName 3),Isub (NewName 1) (NewName 1) (Const 2),Iadd (NewName 1) (NewName 1) (Const 3),Istore (Reg (NewName 1)) (NewName 4),Isub (NewName 1) (NewName 1) (Const 3),Imov (NewName 3) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 5) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 6) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 7) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 8) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Imov (NewName 4) (Reg (NewName 0)),Iadd (NewName 0) (NewName 0) (Const 1),Iadd (NewName 1) (NewName 1) (Const 1),Istore (Reg (NewName 1)) (NewName 4),Isub (NewName 1) (NewName 1) (Const 1),Imov (NewName 4) (Const 0),Istore (Reg (NewName 3)) (NewName 4),Imov (NewName 3) (Const 0),Istore (Reg (NewName 6)) (NewName 3),Imov (NewName 3) (Const 1),Istore (Reg (NewName 7)) (NewName 3),Iload (NewName 3) (Const 0),Istore (Reg (NewName 5)) (NewName 3),Imov (NewName 3) (Const 0),Iadd (NewName 4) (NewName 1) (Const 1),Iload (NewName 4) (Reg (NewName 4)),Istore (Reg (NewName 4)) (NewName 3),Ijmp (Const 41),Iadd (NewName 3) (NewName 1) (Const 1),Iload (NewName 3) (Reg (NewName 3)),Iload (NewName 4) (Reg (NewName 3)),Iload (NewName 3) (Reg (NewName 5)),Iand (NewName 4) (NewName 4) (Const 4294967295),Iand (NewName 3) (NewName 3) (Const 4294967295),Icmpg (NewName 4) (Reg (NewName 3)),Imov (NewName 3) (Const 1),Icmov (NewName 3) (Const 0),Icmpe (NewName 3) (Const 1),Icjmp (Const 53),Ijmp (Const 88),Iadd (NewName 3) (NewName 1) (Const 1),Iload (NewName 3) (Reg (NewName 3)),Iload (NewName 3) (Reg (NewName 3)),Iand (NewName 4) (NewName 3) (Const 4294967295),Imov (NewName 3) (Const 1),Iand (NewName 3) (NewName 3) (Const 4294967295),Icmpg (NewName 4) (Reg (NewName 3)),Imov (NewName 3) (Const 1),Icmov (NewName 3) (Const 0),Icmpe (NewName 3) (Const 1),Icjmp (Const 65),Ijmp (Const 70),Iadd (NewName 3) (NewName 1) (Const 1),Iload (NewName 3) (Reg (NewName 3)),Iload (NewName 3) (Reg (NewName 3)),Istore (Reg (NewName 8)) (NewName 3),Ijmp (Const 79),Iload (NewName 3) (Reg (NewName 6)),Iload (NewName 4) (Reg (NewName 7)),Iadd (NewName 3) (NewName 4) (Reg (NewName 3)),Istore (Reg (NewName 8)) (NewName 3),Iload (NewName 3) (Reg (NewName 7)),Istore (Reg (NewName 6)) (NewName 3),Iload (NewName 3) (Reg (NewName 8)),Istore (Reg (NewName 7)) (NewName 3),Ijmp (Const 79),Ijmp (Const 80),Iadd (NewName 3) (NewName 1) (Const 1),Iload (NewName 3) (Reg (NewName 3)),Iload (NewName 3) (Reg (NewName 3)),Iadd (NewName 3) (NewName 3) (Const 1),Iadd (NewName 4) (NewName 1) (Const 1),Iload (NewName 4) (Reg (NewName 4)),Istore (Reg (NewName 4)) (NewName 3),Ijmp (Const 41),Iload (NewName 3) (Reg (NewName 8)),Imov (NewName 2) (Reg (NewName 3)),Iadd (NewName 3) (NewName 1) (Const 2),Iload (NewName 3) (Reg (NewName 3)),Iadd (NewName 4) (NewName 1) (Const 3),Iload (NewName 4) (Reg (NewName 4)),Isub (NewName 0) (NewName 1) (Const 1),Iload (NewName 0) (Reg (NewName 0)),Ijmp (Reg (NewName 0)),Ianswer (Reg (NewName 2))], traceLen = 550, regData = InfinityRegs, aData = [SparsityData (fromList [(Kand,1),(Kadd,1),(Ksub,3),(Kcmpe,10),(Kcmpg,10),(Kmov,1),(Kcmov,10),(Kstore,2),(Kload,1),(Kanswer,1),(KmemOp,1),(Kalu,1)])], initM = [InitMemSegment {isSecret = True, isReadOnly = False, location = 0, segmentLen = 1, content = Just [10]}], intermediateInfo = ()}
-}
