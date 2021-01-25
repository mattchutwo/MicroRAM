{-# LANGUAGE TypeOperators #-}
{-
Module      : Segmenting
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.Segmenting where

import MicroRAM
import qualified Data.Map as Map 
import Compiler.Errors
import Util.Util


-- | segment: Splits a program into segments and creates a map
-- The program is cut at every jump
-- The map relates the beggining of each segment, in the original program, with the segment in the cut program
data Segment reg wrd = Segment
  { segIntrs :: [Instruction reg wrd]
    , init_pc :: Word
    , segLen :: Int
    , segSuc :: [Int]
    , fromNetwork :: Bool }
                     deriving Show
    
--segmenting :: Program reg wrd -> [Segment reg wrd]
--segmenting prog = 

-- | Cutting just splits a program after each jump instruction
--   and creates a map relating instructions and part sof programs
data Cut reg wrd = Cut
  { cutIntrs :: [Instruction reg wrd]
    , cutPc :: Word
    , cutLen :: Int }
  deriving Show
makeCut :: Word -> [Instruction reg wrd] -> Cut reg wrd
makeCut pc instrs = Cut instrs pc (length instrs) 
                                
cuttProg :: Program reg wrd -> ([Cut reg wrd], Map.Map Word Int)
cuttProg prog =
  let (cuts, lastCut, map) = foldr step init $ zip prog [1..]
      cuts' = (makeCut 0 lastCut): cuts
      len   = length cuts'  
      map'  = Map.map (\n -> len - n - 1) map
      map'' = Map.insert 0 0 map' in
    (cuts', map'')
    
  where init:: ([Cut reg wrd], Program reg wrd, Map.Map Word Int)
        init = ([], [], Map.empty)


        -- | The accumulator carries the cuts so far, the map so far and
        --   the current cut that hasn't finished.
        step :: (Instruction reg wrd, Word)
                -> ([Cut reg wrd],[Instruction reg wrd], Map.Map Word Int)
                -> ([Cut reg wrd],[Instruction reg wrd], Map.Map Word Int)
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


cutSuccessors :: Show reg => Map.Map Word Int -> Cut reg Word -> Hopefully $ [Int]
cutSuccessors map (Cut instrs pc len) =
  instrSuccessor map (pc + toEnum len - 1)  (last instrs) 
  
instrSuccessor :: Show reg => Map.Map Word Int -> Word -> Instruction reg Word -> Hopefully $ [Int]
instrSuccessor blockMap pc instr =
  case instr of
    Ijmp op       -> ifConst op
    Icjmp  _ op2  -> (:) <$> (getBlock (pc + 1)) <*> ifConst op2
    Icnjmp _ op2  -> (:) <$> (getBlock (pc + 1)) <*> ifConst op2
    _             -> return $ []

  where ifConst :: Operand regT Word -> Hopefully $ [Int]
        ifConst (Reg   _) = return $ [-1]  -- Go to network
        ifConst (Const c) = do { block <- getBlock c; return [block] }
          
        getBlock :: Word -> Hopefully Int 
        getBlock pc = 
          case Map.lookup pc blockMap of
            Just block -> return block
            Nothing  -> otherError $ "Cutting segments: found jump to an instruction not at the beggining of a block. PC: "
                        ++ show pc
  

-- | Cut to segment
cut2segment :: Show reg => Map.Map Word Int -> Cut reg Word -> Hopefully $ Segment reg Word 
cut2segment blockMap (Cut instrs pc len) = do
  succ <- cutSuccessors blockMap (Cut instrs pc len)
  return $ Segment instrs pc len succ True -- We are hardocing everithing comes from network, for now



-- TESTING
testProg :: Program () Word
testProg = [Iand () () (Reg ()),    --0
            Isub () () (Reg ()),    --1
            Ijmp (Reg ()),          --2
                                    --
            Icjmp () (Const 0),     --3
                                    --
            Iadd () () (Const 0),   --4
            Isub () () (Const 0),   --5
            Ijmp (Const 3),         --6
                                    --
            Iand () () (Reg ()),    --7
            Isub () () (Const 0)    --8
           ]


testSegments :: Hopefully $ [Segment () Word]
testSegments =
  let (cs, bMap) = cuttProg testProg in
    mapM (cut2segment bMap) cs

printSegs :: (Hopefully [Segment () Word]) -> IO ()
printSegs segs = do
  case segs of
    Left error -> putStrLn $ "ERRRO: \n " ++ show error
    Right segs -> do {_ <- mapM (putStrLn . show) segs; return ()}
  return ()
removeError :: Hopefully a -> Maybe a
removeError (Left _) = Nothing
removeError (Right x) = Just x
