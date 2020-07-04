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


import Compiler.IRs
import Compiler.Registers
import MicroRAM.MRAMInterpreter


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


nats = iterate (1+) 0 

memSummarySize = 5
toSummaryMem :: [Word] -> Mem -> [Word]
toSummaryMem theseLocations m =
  map (\loc -> load loc m) theseLocations
  
defaultSummary :: CustomSummary mreg
defaultSummary = CS True True Nothing True [0..4] True True 
  

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
        fldsTxt = fldDepends sPC "PC   "                 -- Hack to get Headers
                  ++ (fldDepends sFlag "Flag  ")   
                  ++ (fldDepends sAnswer "Ansr   ") 
                  ++ (fldDepends sRegs "Regs")
                  ++ filler
                  ++ (fldDepends sMem "Mem \t")
                  ++  "\n"
        filler = take (regMemTextDistance + 1) $ cycle " "  -- super hack to align stuff (otherwise I'll have to rewrite Tabular)
        regMemTextDistance =  length $ regs0text
        regs0text = if sCustRegs then show $ registers_ (theSummary !! 0) else show $ regs_ (theSummary !! 0)
        


-- TESTING
t::Trace Name
t = [init_state [] [],init_state [] [] ]



-- * Loading and saving
{- Tools to load and store programs (at any level) and execution traces.

-}

