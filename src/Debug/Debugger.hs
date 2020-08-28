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
import Data.Default

import Text.PrettyPrint.Tabulate
import Text.PrettyPrint.Boxes
import Text.Printf



import Util.Util
-- LLVM
import qualified LLVM.AST as LLVM

-- Local 
import Compiler.CompilationUnit
import Compiler.InstructionSelection
import Compiler.IRs
import Compiler.Legalize
import Compiler.RegisterAlloc
import Compiler.Registers
import Compiler.RemoveLabels
import Compiler.Stacking

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
  , showAdvice :: Bool
  }


memSummarySize :: Integer
memSummarySize = 5

toSummaryMem :: [MWord] -> Mem -> [MWord]
toSummaryMem theseLocations m =
  map (\loc -> load loc m) theseLocations
  
defaultSummary :: CustomSummary mreg
defaultSummary = CS True True Nothing True [0..4] True True False
  
defaultCSInt :: CustomSummary Int 
defaultCSInt = defaultSummary

defaultCSName :: CustomSummary Name 
defaultCSName = defaultSummary


-- | Reserved regs
data ResRegs = RRs
  { esp :: MWord
  , ebp :: MWord
  , eax :: MWord }
  deriving (Show, G.Generic, Data)
instance Tabulate ResRegs ExpandWhenNested
instance CellValueFormatter ResRegs

data SummaryState = SState {
  pc_ :: MWord
  , flag_ :: MWord
  , answer_ :: MWord
  , regs_ :: ResRegs
  , registers_ :: [MWord] -- Custom regs
  , mem_ :: [MWord]
  , advc_ :: String
      }
  deriving (Show, G.Generic, Data)
instance Tabulate (SummaryState ) ExpandWhenNested

instance CellValueFormatter Word
instance CellValueFormatter [Word]
instance CellValueFormatter MWord
instance CellValueFormatter [MWord]


toSummaryRegs :: Regs mreg => RMap mreg MWord -> ResRegs
toSummaryRegs rmap =
  RRs
  (lookupReg sp rmap)
  (lookupReg bp rmap)
  (lookupReg ax rmap)

toSummaryRegsCustom :: Regs mreg => RMap mreg MWord -> Maybe [mreg] -> [MWord]
toSummaryRegsCustom _rmap Nothing = []
toSummaryRegsCustom rmap (Just regs) =
  map (\r -> lookupReg r rmap) regs


toSummary :: Regs mreg => Maybe [mreg] -> [MWord] -> State mreg -> SummaryState
toSummary theseRegs theseMems st  =
  SState 
  { pc_ = pc st
  , regs_ = toSummaryRegs $ regs st
  , registers_ = toSummaryRegsCustom (regs st) theseRegs
  , mem_ = toSummaryMem theseMems (mem st) 
  , flag_ = bool2word $ flag st
  , answer_ = answer st
  , advc_ = renderAdvc $ advice st
  } where bool2word True = 1
          bool2word False = 0
summary :: Regs mreg =>  Maybe [mreg] -> [MWord] -> Trace mreg -> [SummaryState]
summary theseRegs theseMems t =  map customSummary t
  where customSummary = toSummary theseRegs theseMems 


renderSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> Box
renderSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer sAdvice) t n =
  renderTableWithFlds flds $ take n $ summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
  where flds = fldDepends sPC pc_ ++
               (fldDepends (sRegs && sCustRegs) registers_) ++
               (fldDepends (sRegs && not sCustRegs) regs_) ++  -- Shows default regs or custom ones. if sCustRegs then registers_ else regs_)
               (fldDepends sMem mem_) ++ 
               (fldDepends sFlag flag_)  ++ 
               (fldDepends sAnswer answer_) ++ 
               (fldDepends sAdvice advc_)
        fldDepends cond ret = if cond then [DFld ret] else []
        sCustRegs = case custRegs of
                      Just _ -> True
                      Nothing -> False

-- | Pretty prints summary of an execution.                      
printSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> IO ()
printSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer sAdvice) t n =do
  printf fldsTxt         -- Hack to get headers (check my issue on the Tabular package!)
  printTableWithFlds flds $ take n $ theSummary
  where theSummary = summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
        flds = fldDepends sPC [DFld pc_]
               ++ (fldDepends sFlag [DFld flag_]) 
               ++ (fldDepends sAnswer [DFld answer_])
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
                  ++ (fldDepends sAnswer "Ansr    ") 
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
t::Trace Name
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

fromMRAMFile :: (Read mreg, Regs mreg) =>
                FilePath
             -> IO $ CompilationUnit (Program mreg MWord)
fromMRAMFile file = do
  contents <- readFile file
  return $ read contents
  
runFromFile  :: (Read mreg, Regs mreg) =>
  FilePath -> IO (Trace mreg)
runFromFile file = do
  prog <- fromMRAMFile file
  return $ run prog
  
summaryFromFile ::
  (Read mreg, Regs mreg) =>
  FilePath ->
  CustomSummary mreg ->
  Int -> IO ()
summaryFromFile file cs length = do
  trace <- runFromFile file
  printSummary cs trace length
  



-- * Pretty printing

pprint :: CompilationUnit (Program Name MWord) -> String
pprint compUnit =
  let prog = programCU compUnit in
  concat $ map (\(n,inst) -> show (n::Integer) ++ ". " ++ show inst ++ "\n") $ enumerate prog

pprintFromFile :: FilePath -> IO ()
pprintFromFile file = do
  prog <- fromMRAMFile file
  putStr $ pprint prog

readProg :: String -> Program Name MWord
readProg = read

firstRegs :: Word -> [Name]
firstRegs bound = map fromWord $ map (2*) [0..bound] 

myCS :: CustomSummary Name
myCS = defaultCSName
  {theseRegs = Just $ firstRegs 7
  ,theseMem = [0..15]
  ,showAdvice = True}



-- TESTING GROUNDS
fromAscii :: Int -> Char
fromAscii = toEnum



-- Example
myfile,myllvmfile :: FilePath
myfile = "test/programs/easyLinkedList.micro" -- "programs/returnInput.micro"
myllvmfile = "programs/returnInput.ll"

pprintMyFile :: IO ()
pprintMyFile = pprintFromFile myfile

mram :: IO $ CompilationUnit (Program Name MWord)
mram =  fromMRAMFile "test/return42.micro"

{- | Example
-- summaryFromFile myfile myCS 300 --emptyInitMem
-}

jpProg :: IO (Program VReg MWord)
jpProg = do
    m <- fromLLVMFile "test/programs/returnArgc.ll"
    return $ either undefined id $
      instrSelect m
      >>= legalize
      >>= registerAlloc def
      >>= stacking
      >>= removeLabels
cs :: CustomSummary mreg
cs = defaultSummary {theseMem = [0..27]}
--inp :: [MWord]
--inp = buildInitMem ["one","two", "three"]

-- m' <- jpProg
-- putStrLn $ pprint m'
-- printSummary cs (run inp m') 28

