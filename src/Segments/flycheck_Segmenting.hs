{-
Module      : Segmenting
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.Segmenting where

import MicroRAM
import qualified Data.Map as Map 

-- | segment: Splits a program into segments and creates a map
-- The program is cut at every jump
-- The map relates the beggining of each segment, in the original program, with the segment in the cut program
data Segment reg wrd = Segment
  { segIntrs :: [Instruction reg wrd]
    , init_pc :: Word
    , segLen :: Int
    , segSuc :: [Int]
    , toNetwork :: Bool }
    
--segmenting :: Program reg wrd -> [Segment reg wrd]
--segmenting prog = 

-- | Cutting just splits a program after each jump instruction
--   and creates a map relating instructions and part sof programs
data Cut reg wrd = Cut
  { cutIntrs :: [Instruction reg wrd]
    , cutPc :: Word
    , cutLen :: Int }
  deriving Show
makeCut :: Int -> [Instruction reg wrd] -> Cut reg wrd
makeCut pc instrs = Cut instrs (toEnum pc) (length instrs) 
                                
cuttProg :: Program reg wrd -> ([Cut reg wrd], Map.Map Int Int)
cuttProg prog =
  let (cuts, lastCut, map) = foldr step init $ zip prog [1..]
      cuts' = (makeCut 0 lastCut): cuts
      len   = length cuts'  
      map'  = Map.map (\n -> len - n - 1) map
      map'' = Map.insert 0 0 map' in
    (cuts', map'')
    
  where init:: ([Cut reg wrd], Program reg wrd, Map.Map Int Int)
        init = ([], [], Map.empty)


        -- | The accumulator carries the cuts so far, the map so far and
        --   the current cut that hasn't finished.
        step :: (Instruction reg wrd, Int)
                -> ([Cut reg wrd],[Instruction reg wrd], Map.Map Int Int)
                -> ([Cut reg wrd],[Instruction reg wrd], Map.Map Int Int)
        step (instr, count) (cuts, currentCut, map) =
          if isJump instr
          then
            let map' = Map.insert count (length cuts) map in 
              ((makeCut count currentCut):cuts, [instr], map')
          else
            (cuts, instr:currentCut, map)
  
        isJump :: Instruction reg wrd -> Bool
        isJump (Ijmp _) = True
        isJump (Icjmp _ _) = True
        isJump (Icnjmp _ _) = True
        isJump _ = False

testProg :: Program () ()
testProg = [Iand () () (Reg ()),
            Isub () () (Reg ()),
            Ijmp (Reg ()),
            Icjmp () (Reg ()),
            Iadd () () (Const ()),
            Isub () () (Const ()),
            Ijmp (Reg ()),
            Iand () () (Reg ()),
            Isub () () (Const ())
           ]


cutSuccessors :: Cut reg wrd -> [Int]
cutSuccessors (Cut intrs pc len) =
  
instrSuccessor :: Instruction reg wrd -> [Int]
