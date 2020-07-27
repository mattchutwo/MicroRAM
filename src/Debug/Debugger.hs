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

import Text.PrettyPrint.Tabulate
import Text.PrettyPrint.Boxes
import Text.Printf

import System.Process

import Util.Util
-- LLVM
import qualified LLVM.AST as LLVM

-- Local 
import Compiler.IRs
import Compiler.Registers
import Compiler.CompilationUnit

import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM
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
  , theseMem :: [Word] -- ^ Print this memory locations. Defualt is [0,1,2,3,4]
  , showFlag :: Bool
  , showAnswer :: Bool
  }


memSummarySize = 5
toSummaryMem :: [Word] -> Mem -> [Word]
toSummaryMem theseLocations m =
  map (\loc -> load loc m) theseLocations
  
defaultSummary :: CustomSummary mreg
defaultSummary = CS True True Nothing True [0..4] True True 
  
defaultCSInt :: CustomSummary Int 
defaultCSInt = defaultSummary

defaultCSName :: CustomSummary Name 
defaultCSName = defaultSummary


-- | Reserved regs
data ResRegs = RRs
  { esp :: Word
  , ebp :: Word
  , eax :: Word }
  deriving (Show, G.Generic, Data)
instance Tabulate ResRegs ExpandWhenNested
instance CellValueFormatter ResRegs

data SummaryState = SState {
  pc_ :: Word
  , flag_ :: Word
  , answer_ :: Word
  , regs_ :: ResRegs
  , registers_ :: [Word] -- Custom regs
  , mem_ :: [Word] }
  deriving (Show, G.Generic, Data)
instance Tabulate (SummaryState ) ExpandWhenNested

instance CellValueFormatter Word
instance CellValueFormatter [Word]


toSummaryRegs :: Regs mreg => RMap mreg Word -> ResRegs
toSummaryRegs rmap =
  RRs
  (lookupReg sp rmap)
  (lookupReg bp rmap)
  (lookupReg ax rmap)

toSummaryRegsCustom :: Regs mreg => RMap mreg Word -> Maybe [mreg] -> [Word]
toSummaryRegsCustom rmap Nothing = []
toSummaryRegsCustom rmap (Just regs) =
  map (\r -> lookupReg r rmap) regs

toSummary :: Regs mreg => Maybe [mreg] -> [Word] -> State mreg -> SummaryState
toSummary theseRegs theseMems st  =
  SState 
  { pc_ = pc st
  , regs_ = toSummaryRegs $ regs st
  , registers_ = toSummaryRegsCustom (regs st) theseRegs
  , mem_ = toSummaryMem theseMems (mem st) 
  , flag_ = bool2word $ flag st
  , answer_ = answer st
  } where bool2word True = 1
          bool2word False = 0
summary :: Regs mreg =>  Maybe [mreg] -> [Word] -> Trace mreg -> [SummaryState]
summary theseRegs theseMems t =  map customSummary t
  where customSummary = toSummary theseRegs theseMems 


renderSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> Box
renderSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer) t n =
  renderTableWithFlds flds $ take n $ summary custRegs theseMem t
  -- fldDepends checks the customSummary record, and returns the fields that should be shown
  where flds = fldDepends sPC pc_ ++
               (fldDepends (sRegs && sCustRegs) registers_) ++
               (fldDepends (sRegs && not sCustRegs) regs_) ++  -- Shows default regs or custom ones. if sCustRegs then registers_ else regs_)
               (fldDepends sMem mem_) ++ 
               (fldDepends sFlag flag_)  ++ 
               (fldDepends sAnswer answer_)
        fldDepends cond ret = if cond then [DFld ret] else []
        sCustRegs = case custRegs of
                      Just _ -> True
                      Nothing -> False

-- | Pretty prints summary of an execution.                      
printSummary
  :: Regs mreg => CustomSummary mreg -> Trace mreg -> Int -> IO ()
printSummary (CS sPC sRegs custRegs sMem theseMem sFlag sAnswer) t n =do
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

fromMRAMFile :: (Read mreg, Regs mreg) => FilePath -> IO (Program mreg Word)
fromMRAMFile file = do
  contents <- readFile file
  compilationUnit <- return $ read contents
  return $ program compilationUnit

runFromFile  :: (Read mreg, Regs mreg) =>
  FilePath -> Tape -> Tape -> IO (Trace mreg)
runFromFile file input advice = do
  prog <- fromMRAMFile file
  return $ run input advice prog
  
summaryFromFile ::
  (Read mreg, Regs mreg) =>
  FilePath ->
  CustomSummary mreg ->
  Tape -> Tape -> Int -> IO ()
summaryFromFile file cs input advice length = do
  trace <- runFromFile file input advice
  printSummary cs trace length
  

-- Example
myfile = "programs/returnInput.micro"
myllvmfile = "programs/returnInput.ll"
mram :: IO (Program Name Word)
mram =  fromMRAMFile "programs/returnInput.micro"

{- | Example




-}



-- * Pretty printing

pprint :: Program Name Word -> String
pprint prog = concat $ map (\(n,inst) -> show n ++ ". " ++ show inst ++ "\n") $ enumerate prog

pprintFromFile :: FilePath -> IO ()
pprintFromFile file = do
  prog <- fromMRAMFile file
  putStr $ pprint prog

readProg :: String -> Program Name Word
readProg = read

myCS = defaultCSName
  {theseRegs = Just
               [NewName 0, NewName 1, NewName 2,
                 Name "0",
                 Name "1",Name "2",Name "3", Name "4", Name "5",
                 Name "6",Name "7",Name "8",Name "9", Name "10",
                 Name "11",Name "12",Name "13",Name "14", Name "15",
                 Name "16",Name "17",Name "18",Name "19", Name "20"]}



-- TESTING GROUNDS
-- summaryFromFile myfile myCS [1,2,3] [] 50
