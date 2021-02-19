{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Module      : SegInterpreter
Description : Extens the linear interpreter to the segmented program. However, it only needs to CHECK the trace, not generate it. 
Maintainer  : santiago@galois.com
Stability   : experimental

-}

module Segments.SegInterpreter (doCheck, checkOutput, Result(..),compilerErrorResolve) where 

import qualified Data.Map as Map
import qualified Data.Set as Set
import Compiler.Errors 
--import Compiler.Registers
import Compiler.CompilationUnit

--import Data.Foldable 
--import MicroRAM
import MicroRAM.MRAMInterpreter
import MicroRAM
import Output.Output
--import Util.Util
-- import Debug.Trace
-- import Control.Monad
import Control.Monad.State
import Control.Lens (makeLenses, Lens', (%=), _1, _2, use)

--import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Segments.Segmenting

data Result x = Nope String | Ok x
  deriving (Functor, Show)

instance MonadFail Result where
  fail = Nope

instance Applicative Result where
  pure = Ok
  (<*>) (Ok f) (Ok x) = Ok (f x)
  (<*>) (Nope x) _ = Nope x
  (<*>) _ (Nope x) = Nope x

instance Monad Result where
  (>>=) (Nope msg) _ = Nope msg
  (>>=) (Ok x) f = f x

-- data EvalState mreg = EvalState {
--   machSt :: MachineState MWord
--   , execSt :: ExecutionState MWord
--   , segsSt :: Map.Map Int SegmentOut
--   , previousSucc :: [Int] }
-- type Evaluation x = StateT (EvalState r) Result x
-- data Extension = Extension { _segsSt :: [SegmentOut]
--                  , _previousSucc :: [Int]
--                  -- , _advSt :: AdviceMap
--                  }
-- makeLenses ''Extension

data CheckSt = CheckSt {
  _indxSt :: Int
  , _pcSt :: MWord -- Last pc of the last chunk, i.e. current pc.
  , _succSt :: [Int]
  , _toNetSt :: Bool
  , _usedSegsSt :: Set.Set Int -- tracks used segments to avoid using one twice.
  }
makeLenses ''CheckSt
  
  
--type Evaluation x = InterpM  MWord (Extension) Result x

checkOutput :: Output Int -> Result ()
checkOutput (PublicOutput _ _ _ _) = Nope "Found Public Output with no trace to check."
checkOutput (SecretOutput prog segs params initMem tr _adv) = do
  let traceA = concat $ map chunkStatesOut tr
  let len = length traceA
  -- Produce a new trace
  traceB <- compilerErrorResolve $ runPassCheck (toEnum len) (initMach prog initMem)
  -- Check states give are equal to those produced
  _ <-zipMapM_ checkStEq traceA (tail $ outputStates len regNum traceB) -- drop first state.
  let initCheck = CheckSt {_indxSt = 0, _pcSt= 0, _succSt = [0], _toNetSt = False, _usedSegsSt = Set.empty}
  -- Check each chunk separatedly.
  _ <- evalStateT (mapM_ (checkChunk (Seq.fromList segs)) tr) initCheck 
  return ()

  where checkStEq s1 s2 = check (s1 == s2) $ "States don't match: " ++ show s1 ++ " <> " ++ show s2
        zipMapM_ f ls1 ls2 = mapM_ (\(a,b) -> f a b) $ zip ls1 ls2 
        runPassCheck = runPassGeneric eTrace eAdvice postHandler (Seq.empty, Map.empty)
        regNum = numRegs params

checkChunk :: Seq.Seq SegmentOut -> TraceChunkOut Int -> StateT CheckSt Result ()
checkChunk segs chunk@(TraceChunkOut indx sts) = do
  segment@(SegmentOut constrs len succ fromNet toNet) <- lift $ maybe2error (Seq.lookup indx segs) $ "Segment not found " ++ show indx
  mapM_ checkConstraint constrs
  checkLength len segment
  checkSucc indx fromNet
  checkSegmentUnused indx
  modify $ \st -> st {_indxSt = indx,
                       _pcSt= pcOut $ last sts,
                       _succSt = succ,
                       _toNetSt = toNet}
--    CheckSt indx (pcOut $ last sts) succ toNet
  return ()
  where checkLength len segment = check (len == length sts) $
          "Chunk length and segment length don't match. \n Segment: " ++ show segment ++ "\n Chunk: " ++ show chunk
        checkConstraint :: Constraints -> StateT CheckSt Result ()
        checkConstraint (PcConst pc) = do
          pc' <- use pcSt
          check (pc == pc') $ "Pc constraints don't match in run of segment " ++ show indx ++ ". " ++ show pc ++ " <> " ++ show pc'   
        checkSucc :: Int -> Bool -> StateT CheckSt Result ()
        checkSucc indx fromNet = do
          succ' <- use succSt
          toNet' <- use toNetSt
          indx' <- use indxSt
          check (indx `elem` succ' || (toNet' && fromNet)) $ "Segment "  ++ show indx ++ "can't be reached from previous segment " ++ show indx'
        -- Ensures segments are used at most once
        checkSegmentUnused ::  Int -> StateT CheckSt Result ()
        checkSegmentUnused indx = do
          usedSegs <- use usedSegsSt
          check (not $ indx `elem` usedSegs) $ "Segment "++ show indx ++"  is used twice."
          usedSegsSt %= Set.insert indx  
          
type ExSt = (Seq.Seq (ExecutionState Int), Map.Map MWord [Advice])

-- runPassCheck :: Word -> MachineState MWord -> Hopefully (Trace MWord)
-- runPassCheck steps initMach' = do
--   -- The first entry of the trace is always the initial state.  Then `steps`
--   -- entries follow after it.
--   initExecState <- evalStateT (getStateWithAdvice eAdvice) initState
--   final <- runWith postHandler steps initState
--   return $ initExecState : toList (final ^. sExt . eTrace)
--   where
--     initState = InterpState (Seq.empty, Map.empty) initMach' (initSparsSt Map.empty)

eTrace :: Lens' (a, b) a
eTrace = _1
eAdvice :: Lens' (a, b) b
eAdvice = _2

    
postHandler :: InstrHandler Int ExSt -> InstrHandler Int ExSt 
postHandler =
  checkSparsity . checkMemAdvice
  
checkSparsity :: InstrHandler r ExSt -> InstrHandler r ExSt
checkSparsity nextIH instr = do
  cycle <- use $ sMach . mCycle
  adv <- use $ sExt . eAdvice
  if (Stutter `elemMaybe` (Map.lookup cycle adv)) then (sMach . mCycle %= (+ 1)) else (nextIH instr)
  where elemMaybe x lsm = case lsm of
                           Just ls -> x `elem` ls
                           Nothing -> False
                           
checkMemAdvice :: InstrHandler r ExSt -> InstrHandler r ExSt
checkMemAdvice = id 
      
check :: MonadFail m => Bool -> String -> m ()
check test msg = if test then return () else fail msg 
    
maybe2error :: Maybe x -> String -> Result x 
maybe2error Nothing msg = Nope msg
maybe2error (Just x) _ = Ok x

compilerErrorResolve :: Hopefully x -> Result x 
compilerErrorResolve (Left msg) = Nope $ show msg
compilerErrorResolve (Right x) = Ok x


--- Testing:
doCheck :: IO ()
doCheck = case checkOutput testOutput of
            Ok _ -> putStrLn "Ok."
            Nope msg -> putStrLn msg
-- checkOutput :: Output Int -> Result ()
testOutput :: Output Int
testOutput = SecretOutput prog segs parms mem tr adv
  where prog :: Program Int MWord
        prog = Iadd 0 0 (Const 42) :
               Iadd 0 0 (Const 42) :
               Isub 0 0 (Const 42) :
               Isub 0 0 (Const 42) : []
        segs :: [SegmentOut]
        segs = [SegmentOut [] 2 [] True True, SegmentOut [PcConst 2] 2 [] True True] 
        parms :: CircuitParameters
        parms  = CircuitParameters 3 2 Map.empty
        mem :: Compiler.CompilationUnit.InitialMem
        mem  = []
        tr :: [TraceChunkOut Int]
        tr  = TraceChunkOut 0 [StateOut False 1 [42,0,0,0], StateOut False 2 [84,0,0,0]] :
          TraceChunkOut 1 [StateOut False 3 [42,0,0,0], StateOut False 4 [0,0,0,0]] : []
        adv :: Map.Map MWord [Advice]
        adv  = Map.empty
