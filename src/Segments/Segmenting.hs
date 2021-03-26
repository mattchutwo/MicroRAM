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
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set 
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
  { cutFunction :: String -- ^ Name of the function that contains this cut
  , cutIntrs :: AnnotatedProgram md reg wrd
  , cutPc :: MWord  -- ^ Pc of the first instruction
  , cutLen :: Int }
  deriving Show
makeCut :: MWord -> AnnotatedProgram Metadata reg wrd -> Cut Metadata reg wrd
makeCut pc instrs = Cut funName instrs pc (length instrs) 
  where funName = toHead (mdFunction . snd) "" instrs

toHead :: (a -> b) -> b -> [a] -> b
toHead f def ls =
  case ls of
    [] -> def
    x:_ -> f x 

-- | Repeats each element as meny times as indicated on the factor list
-- Example:
-- `expandList "Hello" [1,2,1,4] = "Heelloooo"`
-- Both lists are assumed to be the same length
expandList :: [a] -> [Int] -> [a]
expandList ls factors = concat $ map (\(x,factor) -> replicate factor x) $ zip ls factors


segmentProgram :: (Map.Map String Int)
        -> AnnotatedProgram Metadata reg MWord -> Hopefully $ [Segment reg MWord]
segmentProgram funCount prog =
  let cuts = cutProg prog
      funNames = findAllFunctions prog      -- names all all functions in the program
      funSegments = map (segmentFunction cuts) funNames -- for each function, create the corresponding segments 
      funCountList = map (\k -> Map.findWithDefault 1 k funCount) funNames
      expandedFunSegments = expandList funSegments funCountList in -- repeat the segments in each function as necessary
    return $ foldl addFunSegments [] expandedFunSegments -- Successors need to be updated accordingly.
    where addFunSegments :: [Segment reg MWord] -> [Segment reg MWord] -> [Segment reg MWord]
          addFunSegments accumulated funSegments =
            let lenAcc = length accumulated in 
            accumulated ++ (shiftSegment lenAcc  <$> funSegments)

          -- | Shifts the successors 
          shiftSegment :: Int -> Segment reg MWord -> Segment reg MWord
          shiftSegment len seg = seg {segSuc = map (+ len) $ segSuc seg}
          
cutProg :: AnnotatedProgram Metadata reg wrd -> [Cut Metadata reg wrd]
cutProg prog =
  let (cuts, lastCut) = foldr step init $ zip prog [1..] in 
      (makeCut 0 lastCut): cuts -- Add the the remaining cut (should be the first). 
    
  where init:: ([Cut md reg wrd], AnnotatedProgram Metadata reg wrd)
        init = ([], [])

        -- | The accumulator carries the cuts so far, the map so far and
        --   the current cut that hasn't finished.
        step :: ((Instruction reg wrd, Metadata), MWord)
             -> ([Cut Metadata reg wrd],AnnotatedProgram Metadata reg wrd)
             -> ([Cut Metadata reg wrd],AnnotatedProgram Metadata reg wrd)
        step (instr, count) (cuts, currentCut) =
          if isJump $ fst instr
          then ((makeCut count currentCut): cuts, [instr]) -- Will be folded right, so everything is built in reverse.
          else
            (cuts, instr:currentCut)

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


type ToNetwork = Bool
cutSuccessors :: Map.Map MWord Int -> Cut md reg MWord -> ([Int],ToNetwork)
cutSuccessors cutMap (Cut _ instrs pc len)
  | null instrs = ([], False)
  | otherwise =
    let (pcSuccs, toNet) = pcSuccessors (pc + toEnum len - 1) $ fst $ last instrs -- Should use Seq?
        cutSuccs = mapMaybe (\pc -> Map.lookup pc cutMap) pcSuccs in
      (cutSuccs, toNet || length cutSuccs /= length pcSuccs)

pcSuccessors :: MWord -> Instruction reg MWord -> ([MWord], ToNetwork)
pcSuccessors pc instr = 
  case instr of
    Ijmp op       -> (ifConst op, isReg op)
    Icjmp  _ op2  -> ((pc + 1) : ifConst op2, isReg op2)
    Icnjmp _ op2  -> ((pc + 1) : ifConst op2, isReg op2)
    _             -> ([], True)
  where ifConst :: Operand regT MWord -> [MWord]
        ifConst (Reg   _) = []  -- By default it goes to network.
        ifConst (Const c) = [c]
        isReg :: Operand regT MWord -> Bool
        isReg (Reg   _) = True  -- By default it goes to network.
        isReg (Const _) = False

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
cut2segment :: Cut Metadata reg MWord -> [Int] -> ToNetwork -> Segment reg MWord 
cut2segment (Cut _funName instrs pc len) succs toNet =
  Segment
  { segIntrs = (map fst instrs) 
    , constraints = [PcConst pc]
    , segLen = len
    , segSuc = succs
    , fromNetwork = fromNet  -- ^ From network can change later.
    , toNetwork = toNet }
  where fromNet = case instrs of
                    [] -> False
                    (i,md):_ -> mdReturnCall md || mdFunctionStart md
        mdFunctionStart md = False -- Add to metadata



-- | Per function analysis
findAllFunctions :: AnnotatedProgram Metadata reg wrd -> [String]
findAllFunctions prog = Set.toList (foldr addFunction Set.empty prog) -- Can we make this faster with unique? 
  where addFunction (_instr, md) accumulator = Set.insert (mdFunction md) accumulator 

segmentFunction :: [Cut Metadata reg MWord] -> String -> [Segment reg MWord]
segmentFunction cuts funName = map toSegment cuts
  where functionCuts = filter (\cut -> cutFunction cut == funName) cuts 
        pcToIndexMap = foldr (\(i,cut) -> Map.insert (cutPc cut) i) Map.empty (zip [0..] functionCuts)   
        toSegment :: Cut Metadata reg MWord -> Segment reg MWord
        toSegment cut = let (cutSuccs, toNet) = cutSuccessors pcToIndexMap cut in
                          cut2segment cut cutSuccs toNet








--------------------------------
-- TESTING
--------------------------------
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
    Icjmp () (Const 8),     --6
    --
    Ijmp (Const 0)     --7
  ] ++ 
  map (\x -> (x, trivialMetadata "main" ""))
  [ Iand () () (Reg ()),    --8
    Isub () () (Const 0)    --9
  ]


_testSegments :: Hopefully $ [Segment () MWord]
_testSegments = 
  do (segs) <- segmentProgram (Map.fromList [("",3)]) _testProg
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
