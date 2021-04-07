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

import qualified Debug.Trace as T

import Compiler.Errors
import Compiler.Metadata
import Compiler.IRs
import Control.Monad (when)

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Graph as G (stronglyConnComp, SCC(..))
import qualified Data.Sequence as Seq

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


segmentProgram :: Show reg => (Map.Map String Int)
        -> AnnotatedProgram Metadata reg MWord -> Hopefully $ [Segment reg MWord]
segmentProgram funCount prog = do
  funNames <- findAllFunctions prog      -- names all all functions in the program
  let cuts = cutProg prog
  let funSegments = map (segmentFunction cuts) funNames -- for each function, create the corresponding segments 
  let funCountList = map (\k -> Map.findWithDefault 1 k funCount) funNames
  let expandedFunSegments = expandList funSegments funCountList -- repeat the segments in each function as necessary
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
          else (cuts, instr:currentCut )

        isJump :: Instruction reg wrd -> Bool
        isJump (Ijmp _) = True
        isJump (Icjmp _ _) = True
        isJump (Icnjmp _ _) = True
        -- `answer` halts execution by jumping to itself in an infinite loop.
        isJump (Ianswer _) = True
        isJump _ = False


type ToNetwork = Bool
cutSuccessors :: Map.Map MWord Int -> Cut Metadata reg MWord -> ([Int],ToNetwork)
cutSuccessors cutMap (Cut _ instrs pc len)
  | null instrs = ([], False)
  | otherwise =
    let (pcSuccs, toNet) = pcSuccessors (pc + toEnum len - 1) $ fst term -- Should use Seq?
        cutSuccs = mapMaybe (\pc -> Map.lookup pc cutMap) pcSuccs in
      --T.trace ("Function: "++ show (mdFunction $ snd term)++ "@ PC:" ++ show pc ++ "\n\tProposed: \t" ++ show pcSuccs ++ "\n\tFiltered:\t" ++ show cutSuccs)
      (cutSuccs, toNet || length cutSuccs /= length pcSuccs)
      where term = last instrs

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

-- instrSuccessor :: Map.Map MWord [Int] -> MWord -> Instruction reg MWord -> Hopefully $ [Int]
-- instrSuccessor blockMap pc instr =
--   case instr of
--     Ijmp op       -> ifConst op
--     Icjmp  _ op2  -> (++) <$> (getBlock (pc + 1)) <*> ifConst op2
--     Icnjmp _ op2  -> (++) <$> (getBlock (pc + 1)) <*> ifConst op2
--     _             -> return $ []

--   where ifConst :: Operand regT MWord -> Hopefully $ [Int]
--         ifConst (Reg   _) = return $ []  -- By default it goes to network.
--         ifConst (Const c) = do { block <- getBlock c; return block }
          
--         getBlock :: MWord -> Hopefully [Int] 
--         getBlock pc = 
--           case Map.lookup pc blockMap of
--             Just blocks -> return blocks
--             Nothing  -> otherError $ "Cutting segments: found jump to an instruction not at the beggining of a block. PC: "
--                         ++ show pc
  

-- | Cut to segment
cut2segment :: Show reg => Cut Metadata reg MWord -> [Int] -> ToNetwork -> Segment reg MWord 
cut2segment (Cut _funName instrs pc len) succs toNet =
  let ret =  
        Segment
        { segIntrs = (map fst instrs) 
        , constraints = [PcConst pc]
        , segLen = len
        , segSuc = succs
        , fromNetwork = fromNet  -- ^ From network can change later.
        , toNetwork = toNet } in
    ret --if fromNet then T.trace ("Segmetn to net: " <> (show $ constraints ret)) ret else ret
  where fromNet = case instrs of
                    [] -> False
                    (_i,md):_ -> mdReturnCall md || mdFunctionStart md



-- | Per function analysis
findAllFunctions :: AnnotatedProgram Metadata reg wrd -> Hopefully [String]
findAllFunctions prog = do
  when (not $ "Premain" `Set.member` funcSet) $ assumptError "Program doesn't have a Premain."  
  let minusPM = Set.delete  "Premain" funcSet
  return $ "Premain" : (Set.toList minusPM) 
  where funcSet = (foldl addFunction Set.empty prog) -- Can we make this faster with unique?
        addFunction accumulator (_instr, md) = Set.insert (mdFunction md) accumulator 

segmentFunction :: Show reg => [Cut Metadata reg MWord] -> String -> [Segment reg MWord]
segmentFunction cuts funName = -- T.trace ("Segments in " ++ funName ++ ": " ++ show (length $ loopConnections functionSegs) )
  loopConnections functionSegs 
  where functionSegs = map toSegment functionCuts 
        functionCuts = filter (\cut -> cutFunction cut == funName) cuts 

        pcToIndexMap = foldr (\(i,cut) -> Map.insert (cutPc cut) i) Map.empty (zip [0..] functionCuts)   
        toSegment :: Show reg => Cut Metadata reg MWord -> Segment reg MWord
        toSegment cut = let (cutSuccs, toNet) = cutSuccessors pcToIndexMap cut in
                          cut2segment cut cutSuccs toNet

-- | Loop operations
-- We define loops as Strongly Connected Component
loopConnections :: [Segment reg wrd] -> [Segment reg wrd]
loopConnections segs =
  let cfg = makeCFG segs
      sccs = G.stronglyConnComp cfg in
    toList $
    (backEdgesToNet sccs) . (connectLoopExits sccs) $
    Seq.fromList segs   
      
  
-- | 1. Make the CFG with nodes (node, key, [key])
makeCFG :: [Segment reg wrd]
        -> [(Int,Int,[Int])]
makeCFG segments = segment2node <$> zip segments [0..]
  where segment2node (seg, segIndex) = (segIndex, segIndex, segSuc seg)
    
-- | 2. Identify SCC in the CFG
-- We use the the library funciton: G.stronglyConnComp.

-- | 3. Add `fromNetwork` to every exit from a loop.
connectLoopExits :: [G.SCC Int]               -- Connected components
                 -> Seq.Seq (Segment reg wrd) -- Segments
                 -> Seq.Seq (Segment reg wrd)
connectLoopExits sccs segs = foldOverComponents connectComponent segs sccs
  where connectComponent :: [Int]
                         -> Seq.Seq (Segment reg wrd)
                         -> Seq.Seq (Segment reg wrd)
        connectComponent component segs =
          let compSet = Set.fromList component in
            foldl (connectLoop compSet) segs component 
        connectLoop compSet segsList compSeg =
          let successors = segSuc (segsList `Seq.index` compSeg) in 
            foldl (flip $ Seq.adjust' addFromNetwork) segsList $
            filter (\i -> not $ i `Set.member` compSet) $ successors
        addFromNetwork seg = seg{fromNetwork = True}

        
-- | 4. For each back edge in the loop, add a toNetwork so we can get out of the loop.
backEdgesToNet :: [G.SCC Int]               -- Connected components
                -> Seq.Seq (Segment reg wrd) -- Segments
                -> Seq.Seq (Segment reg wrd)
backEdgesToNet sccs segs = foldOverComponents go segs sccs
  where go  :: [Int]
            -> Seq.Seq (Segment reg wrd)
            -> Seq.Seq (Segment reg wrd)
        go component segs =
          let compSet = Set.fromList component in
            foldl (backEdgesToNetSeg compSet) segs component
        backEdgesToNetSeg :: Set.Set Int -> Seq.Seq (Segment reg wrd) -> Int -> Seq.Seq (Segment reg wrd)   
        backEdgesToNetSeg compSet segsList compSeg =
          let successors = segSuc (segsList `Seq.index` compSeg)
              -- See if there are any back-egdes in the component
              backEdgesComp = filter 
                              (\idx -> (idx `Set.member` compSet) && (idx <= compSeg)) $
                              successors in
            if null backEdgesComp then segsList else 
              Seq.adjust' (\seg -> seg{toNetwork = True}) compSeg segsList
                              
foldOverComponents :: Foldable t => ([Int] -> b -> b) -> b -> t (G.SCC Int) -> b
foldOverComponents f segs sccs = foldl go segs sccs
  where go segs comp = case comp of
                         G.AcyclicSCC _ -> segs
                         G.CyclicSCC component ->
                           f component segs
                         


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
