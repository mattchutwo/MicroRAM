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
myfile = "programs/fibCheeky.micro"
myllvmfile = "programs/fib.ll"
mram :: IO (Program Name Word)
mram =  fromMRAMFile "programs/fib.micro"

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

myCS = defaultCSName {theseRegs = Just
                      [NewName 0, NewName 1, NewName 2,
                      Name "0",Name "1",Name "5",Name "6", Name "7", Name "9"]}

--  summaryFromFile myfile defaultCSName [1,2,3,4] [] 100

--p = readProg "[Istore (Const 0) (NewName 0),Iread (NewName 1) (Const 0),Icjmp (Const 6),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 1),Ijmp (Const 1),Imov (Name \"0\") (Reg (NewName 0)),Imov (Name \"1\") (Const 1),Imov (NewName 2) (Const 121),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 2),Iadd (NewName 0) (NewName 0) (Const 1),Istore (Reg (NewName 0)) (NewName 1),Imov (NewName 1) (Reg (NewName 0)),Isub (NewName 0) (NewName 0) (Const 0),Icmpg (Name \"0\") (Const 0),Imov (Name \"3\") (Const 0),Icmov (Name \"3\") (Const 1),Imov (Name \"52\") (Const 0),Icmpe (Name \"3\") (Const 1),Icjmp (Const 22),Ijmp (Const 116),Iadd (Name \"5\") (Name \"0\") (Const 4294967295),Iand (Name \"6\") (Name \"0\") (Const 3),Icmpae (Name \"5\") (Const 3),Imov (Name \"7\") (Const 1),Icmov (Name \"7\") (Const 0),Imov (Name \"29\") (Const 0),Imov (Name \"30\") (Const 0),Imov (Name \"31\") (Const 1),Imov (Name \"32\") (Const 0),Icmpe (Name \"7\") (Const 1),Icjmp (Const 74),Ijmp (Const 34),Isub (Name \"9\") (Name \"0\") (Reg (Name \"6\")),Imov (Name \"11\") (Const 0),Imov (Name \"12\") (Const 1),Imov (Name \"13\") (Const 0),Imov (Name \"14\") (Reg (Name \"9\")),Ijmp (Const 40),Icmpe (Name \"11\") (Const 0),Imov (Name \"15\") (Const 0),Icmov (Name \"15\") (Const 1),Icmpe (Name \"13\") (Const 1),Imov (Name \"16\") (Reg (Name \"12\")),Icmov (Name \"16\") (Reg (Name \"13\")),Imov (Name \"17\") (Reg (Name \"13\")),Iadd (Name \"18\") (Name \"12\") (Reg (Name \"17\")),Icmpe (Name \"11\") (Const 0),Imov (Name \"19\") (Const 0),Icmov (Name \"19\") (Const 1),Icmpe (Name \"16\") (Const 1),Imov (Name \"20\") (Reg (Name \"18\")),Icmov (Name \"20\") (Reg (Name \"16\")),Imov (Name \"21\") (Reg (Name \"16\")),Iadd (Name \"22\") (Name \"18\") (Reg (Name \"21\")),Iadd (Name \"23\") (Name \"22\") (Reg (Name \"20\")),Iadd (Name \"24\") (Name \"23\") (Reg (Name \"22\")),Iadd (Name \"25\") (Name \"11\") (Const 4),Iadd (Name \"26\") (Name \"14\") (Const 4294967292),Icmpe (Name \"26\") (Const 0),Imov (Name \"27\") (Const 0),Icmov (Name \"27\") (Const 1),Imov (Name \"11\") (Reg (Name \"25\")),Imov (Name \"12\") (Reg (Name \"24\")),Imov (Name \"13\") (Reg (Name \"23\")),Imov (Name \"14\") (Reg (Name \"26\")),Imov (Name \"29\") (Reg (Name \"24\")),Imov (Name \"30\") (Reg (Name \"25\")),Imov (Name \"31\") (Reg (Name \"24\")),Imov (Name \"32\") (Reg (Name \"23\")),Icmpe (Name \"27\") (Const 1),Icjmp (Const 74),Ijmp (Const 40),Icmpe (Name \"6\") (Const 0),Imov (Name \"33\") (Const 0),Icmov (Name \"33\") (Const 1),Imov (Name \"35\") (Reg (Name \"30\")),Imov (Name \"36\") (Reg (Name \"31\")),Imov (Name \"37\") (Reg (Name \"32\")),Imov (Name \"38\") (Reg (Name \"6\")),Imov (Name \"47\") (Const 0),Imov (Name \"48\") (Reg (Name \"29\")),Icmpe (Name \"33\") (Const 1),Icjmp (Const 110),Ijmp (Const 86),Icmpae (Name \"35\") (Const 2),Imov (Name \"39\") (Const 1),Icmov (Name \"39\") (Const 0),Iadd (Name \"40\") (Name \"36\") (Reg (Name \"37\")),Icmpe (Name \"37\") (Const 1),Imov (Name \"41\") (Reg (Name \"36\")),Icmov (Name \"41\") (Reg (Name \"37\")),Icmpe (Name \"36\") (Const 1),Imov (Name \"42\") (Reg (Name \"40\")),Icmov (Name \"42\") (Reg (Name \"36\")),Iadd (Name \"43\") (Name \"35\") (Const 1),Iadd (Name \"44\") (Name \"38\") (Const 4294967295),Icmpe (Name \"44\") (Const 0),Imov (Name \"45\") (Const 0),Icmov (Name \"45\") (Const 1),Imov (Name \"35\") (Reg (Name \"43\")),Imov (Name \"36\") (Reg (Name \"42\")),Imov (Name \"37\") (Reg (Name \"41\")),Imov (Name \"38\") (Reg (Name \"44\")),Imov (Name \"47\") (Reg (Name \"39\")),Imov (Name \"48\") (Reg (Name \"40\")),Icmpe (Name \"45\") (Const 1),Icjmp (Const 110),Ijmp (Const 86),Iadd (Name \"49\") (Name \"0\") (Const 4294967295),Icmpe (Name \"49\") (Const 1),Imov (Name \"50\") (Reg (Name \"48\")),Icmov (Name \"50\") (Reg (Name \"49\")),Imov (Name \"52\") (Reg (Name \"50\")),Ijmp (Const 116),Imov (NewName 2) (Reg (Name \"52\")),Imov (NewName 0) (Reg (NewName 1)),Isub (NewName 1) (NewName 1) (Const 1),Iload (NewName 1) (Reg (NewName 1)),Ijmp (Reg (NewName 1)),Ianswer (Reg (NewName 2))]"
