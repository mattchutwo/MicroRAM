{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-
Module      : Segmenting
Description : 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.Segmenting (segmentProgram, Segment(..), Constraints(..)) where

import Compiler.Errors
import Compiler.Metadata
import Compiler.IRs
import qualified Data.Map as Map 
import MicroRAM
import Util.Util
import GHC.Generics

-- | segment: Splits a program into segments and creates a map
-- The program is cut at every jump
-- The map relates the beggining of each segment, in the original program, with the segment in the cut program
data Segment reg wrd = Segment
  { segIntrs :: [Instruction reg wrd]
    , constraints :: [Constraints]
    , segLen :: Int
    , segSuc :: [Int]
    , fromNetwork :: Bool
    , toNetwork :: Bool } deriving (Eq, Show, Generic)
-- | Constraints for the segment
data Constraints =
  PcConst MWord -- | Pc constraints indicate a public segment.
  deriving (Eq, Show, Generic)
  

-- | Cutting just splits a program after each jump instruction
--   and creates a map relating instructions and cuts starting there.
data Cut md reg wrd = Cut
  { cutIntrs :: AnnotatedProgram md reg wrd
    , cutPc :: MWord
    , cutLen :: Int }
  deriving Show
makeCut :: MWord -> AnnotatedProgram md reg wrd -> Cut md reg wrd
makeCut pc instrs = Cut instrs pc (length instrs) 

segmentProgram :: (Map.Map String Int)
        -> AnnotatedProgram Metadata reg MWord -> Hopefully $ ([Segment reg MWord], Map.Map MWord [Int])
segmentProgram funCount prog =
  let (cuts, cutMap) = cutProg funCount prog in
    do segs <- mapM (cut2segment cutMap) cuts
       return (segs, cutMap)

cutProg :: (Map.Map String Int )
        -> AnnotatedProgram Metadata reg wrd -> ([Cut Metadata reg wrd], Map.Map MWord [Int])
cutProg funCount prog =
  let (cuts, lastCut, segMap) = foldr (step funCount) init $ zip prog [1..]
      cuts' = (makeCut 0 lastCut): cuts
      len   = length cuts'  
      map'  = Map.map (\ns -> (\n -> len - n - 1) <$> ns) segMap
      map'' = Map.insert 0 [0] map' in
    (cuts', map'')
    
  where init:: ([Cut md reg wrd], AnnotatedProgram Metadata reg wrd, Map.Map MWord [Int])
        init = ([], [], Map.empty)

        -- | The accumulator carries the cuts so far, the map so far and
        --   the current cut that hasn't finished.
        step :: (Map.Map String Int)
             -> ((Instruction reg wrd, Metadata), MWord)
             -> ([Cut Metadata reg wrd],AnnotatedProgram Metadata reg wrd, Map.Map MWord [Int])
             -> ([Cut Metadata reg wrd],AnnotatedProgram Metadata reg wrd, Map.Map MWord [Int])
        step funCount (instr, count) (cuts, currentCut, map) =
          if isJump $ fst instr
          then
            -- TODO: this part will be raplaced so we can do successor mapping better.
            let many = howMany funCount currentCut
                positions = (+ length cuts) <$> [0..(many -1)] -- TODO: Each block will be connected to EVERY block that could be a successor. Can optimize
                map' = Map.insert count positions map  
                newCuts = replicate many (makeCut count currentCut) in 
              (newCuts ++ cuts, [instr], map') -- Will be folded right, so everything is built in reverse.
          else
            (cuts, instr:currentCut, map)

        -- Find how many times the current cut will be repeated.
        -- Assume all the cut is in the same function
        howMany funCount currentCut =
          case currentCut of
            hd : _ ->
              Map.findWithDefault 1 (mdFunction $ snd hd) funCount
            [] -> 0 
  
        isJump :: Instruction reg wrd -> Bool
        isJump (Ijmp _) = True
        isJump (Icjmp _ _) = True
        isJump (Icnjmp _ _) = True
        -- `answer` halts execution by jumping to itself in an infinite loop.
        isJump (Ianswer _) = True
        isJump _ = False


cutSuccessors :: Map.Map MWord [Int] -> Cut md reg MWord -> Hopefully $ [Int]
cutSuccessors blockMap (Cut instrs pc len)
  | null instrs = return []
  | otherwise = instrSuccessor blockMap (pc + toEnum len - 1)  (fst $ last instrs)
  
instrSuccessor :: Map.Map MWord [Int] -> MWord -> Instruction reg MWord -> Hopefully $ [Int]
instrSuccessor blockMap pc instr =
  case instr of
    Ijmp op       -> ifConst op
    Icjmp  _ op2  -> (++) <$> (getBlock (pc + 1)) <*> ifConst op2
    Icnjmp _ op2  -> (++) <$> (getBlock (pc + 1)) <*> ifConst op2
    _             -> return $ []

  where ifConst :: Operand regT MWord -> Hopefully $ [Int]
        ifConst (Reg   _) = return $ []  -- By default it goes to network.
        ifConst (Const c) = do { block <- getBlock c; return block }
          
        getBlock :: MWord -> Hopefully [Int] 
        getBlock pc = 
          case Map.lookup pc blockMap of
            Just blocks -> return blocks
            Nothing  -> otherError $ "Cutting segments: found jump to an instruction not at the beggining of a block. PC: "
                        ++ show pc
  

-- | Cut to segment
cut2segment :: Map.Map MWord [Int] -> Cut md reg MWord -> Hopefully $ Segment reg MWord 
cut2segment blockMap (Cut instrs pc len) = do
  succe <- cutSuccessors blockMap (Cut instrs pc len)
  return $ Segment (map fst instrs) [PcConst pc] len succe True True -- We are hardcoing everithing comes and goes to network, for now



-- TESTING
_testProg :: AnnotatedProgram Metadata  () MWord
_testProg = map (\x -> (x, defaultMetadata))
  [Iand () () (Reg ()),    --0
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


_testSegments :: Hopefully $ [Segment () MWord]
_testSegments = 
  do (segs, _bMap) <- segmentProgram (Map.fromList [("",3)]) _testProg
     return segs

_printSegs :: (Hopefully [Segment () MWord]) -> IO ()
_printSegs segs = do
  case segs of
    Left error -> putStrLn $ "ERRRO: \n " ++ show error
    Right segs -> do {_ <- mapM (putStrLn . show) segs; return ()}
  return ()
_removeError :: Hopefully a -> Maybe a
_removeError (Left _) = Nothing
_removeError (Right x) = Just x
