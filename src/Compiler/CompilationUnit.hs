{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Compiler.CompilationUnit where

{-
Module      : Compilation unit
Description : records for compilation. Carry Code and extra data.
Maintainer  : santiago@galois.com
Stability   : experimental


Wrapper for compilation programs that add compiler metadata to
compilations units.

-}

import Control.Monad.State (runStateT)
import Data.Default (def)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Vector (Vector)
import Data.ByteString.Short (ShortByteString)

import Util.Util
import MicroRAM (MWord)

import Compiler.Analysis
import Compiler.Common
import Compiler.LazyConstants
import Compiler.Registers
import Compiler.Tainted


-- | The Compilation Unit
data CompilationUnit' a prog = CompUnit
  { programCU :: prog
  , traceLen :: Word
  , regData :: RegisterData
  , aData   :: AnalysisData
  , nameBound :: Word -- ^ All names are bounded by this 
  , intermediateInfo :: a -- ^ Other stuff we carry during compilation, but starts and ends as `()`
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | Multiple variants of the compiled program.
data MultiProg prog = MultiProg
  -- | "High-level" variant, used for the first interpreter pass.  Contains
  -- extension instructions (`Iext` + `Iextval`) in their original forms,
  -- partly for instrumentation purposes.
  { highProg :: prog
  -- | "Low-level" variant, used for the second interpreter pass.  Does not
  -- contain extension instructions, with the exception of `Iextadvise`, which
  -- gets serialized as a plain `Iadvise` instead.
  , lowProg :: prog
  }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ProgAndMem prog = ProgAndMem {
  pmProg :: prog,
  pmMem :: InitialMem,
  pmLabels :: Map.Map Name MWord
}
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type CompilationUnit a prog = CompilationUnit' a (ProgAndMem prog)

type CompilationResult prog = CompilationUnit' () (MultiProg (ProgAndMem prog))

prog2unit :: Word -> prog -> CompilationUnit () prog
prog2unit len p = CompUnit (ProgAndMem p [] Map.empty) len InfinityRegs def firstUnusedName () -- ^ 2 reserves `0` and `1` for premain and main 

-- * Lifting operators

-- | Just compile, lift passes that only deal with code.
justCompile :: Monad m =>
  (progS -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
justCompile pass p = mapM (mapM pass) p

-- | Just compile with fresh names, lift passes that only deal with code and create fresh names
justCompileWithNames :: Monad m =>
  ((progS, Word) -> m (progT, Word))
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
justCompileWithNames pass p = do
  let ProgAndMem sProg _ _ = programCU p
  let nBound = nameBound p
  (tProg, nBound') <- pass (sProg, nBound)   
  return $ p {programCU = (programCU p) { pmProg = tProg }, nameBound = nBound'}
justCompileWithNamesSt :: Monad m =>
  ((progS) -> WithNextReg m (progT))
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
justCompileWithNamesSt pass p = do
  let ProgAndMem sProg _ _ = programCU p
  let nBound = nameBound p
  (tProg, nBound') <- runStateT (pass sProg) nBound   
  return $ p {programCU = (programCU p) { pmProg = tProg }, nameBound = nBound'}


-- | Informed Compilation: passes that use analysis data but only change code
informedCompile :: Monad m =>
  (progS -> AnalysisData -> m progT)
  -> CompilationUnit a progS
  -> m $ CompilationUnit a progT
informedCompile pass cUnit = do
  p' <- pass (pmProg $ programCU cUnit) (aData cUnit)
  return $ cUnit {programCU = (programCU cUnit) { pmProg = p' } }
  
-- | Just Analyse, lift passes that don't modify the code.
justAnalyse :: Monad m  =>
  (prog  -> m AnalysisPiece)
  -> CompilationUnit a prog
  -> m $ CompilationUnit a prog
justAnalyse analysis cUnit = do
  a' <-  analysis (pmProg $ programCU cUnit)
  return $ cUnit {aData = addAnalysisPiece a' $ aData cUnit}


-- TODO: Should we move this to a separate file (e.g. Compiler/InitMem.hs) ?

data InitMemSegment = InitMemSegment
  { isName :: String -- ^ human readable name that corresponds to the original global variable
  , isSecret :: Bool
  , isReadOnly :: Bool
  , isHeapInit :: Bool
  , location :: MWord
  , segmentLen :: MWord
  , content :: Maybe [MWord]
  , labels :: Maybe [Vector Label] -- ^ This is Just when content is Just and mode is leak-tainted. -- TODO: Type level stuff to enable tainted things.
  } deriving (Eq, Ord, Read, Show)

type InitialMem = [InitMemSegment]

-- | For creating an initial memory befor code labels and globals have been resolved.
type LazyInitSegment = (Maybe [LazyConst MWord], InitMemSegment)
type LazyInitialMem = [LazyInitSegment] 


flatInitMem :: InitialMem -> Map.Map MWord MWord
flatInitMem imem = Map.union public secret
  where (public, secret) = flatInitMem' imem

flatInitMem' :: InitialMem -> (Map.Map MWord MWord, Map.Map MWord MWord)
flatInitMem' = foldr initSegment (Map.empty, Map.empty)
  where initSegment ::
          InitMemSegment ->
          (Map.Map MWord MWord, Map.Map MWord MWord) ->
          (Map.Map MWord MWord, Map.Map MWord MWord)
        initSegment (InitMemSegment _ secret _ _ loc len optContent _) (pub, sec)
          | secret = (pub, sec `Map.union` words)
          | otherwise = (pub `Map.union` words, sec)
          where
            words = Map.fromList $
              zip [loc .. loc + len - 1] (maybe [] id optContent ++ repeat 0)
--  TAINTED
-- flatInitMem = foldr initSegment Map.empty
--   where initSegment :: InitMemSegment -> Map.Map MWord MWord -> Map.Map MWord MWord
--         initSegment (InitMemSegment _ _ _ _ _ Nothing _) = id
--         initSegment (InitMemSegment _ _ _ loc _ (Just content) _) =
--           Map.union $ Map.fromList $
--           -- Map with the new content
--           zip [loc..] content

flatInitTaintedMem :: InitialMem -> Map.Map MWord (Vector Label)
flatInitTaintedMem = foldr initSegment Map.empty
  where initSegment (InitMemSegment _ _ _ _ _ _ _ Nothing) = id
        initSegment (InitMemSegment _ _ _ _ loc _ _ (Just labels)) =
          Map.union $ Map.fromList $
          -- Map with the new content
          zip [loc..] labels

lengthInitMem :: InitialMem -> MWord
lengthInitMem = foldl' (\tip seg -> max tip (segTip seg)) 0
  where segTip (InitMemSegment _ _ _ heapInit loc len _ _)
          | heapInit = 0
          | otherwise = loc + len
