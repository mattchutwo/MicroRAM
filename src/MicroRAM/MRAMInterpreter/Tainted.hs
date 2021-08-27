{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.MRAMInterpreter.Tainted where

import Control.Lens (makeLenses, (^.), (&), (.~), view, Lens', over, lens)
import Control.Monad (when)
-- import Control.Monad.State
-- import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Sequence as Seq
import qualified Data.Set as Set
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vec


import Compiler.CompilationUnit
import Compiler.Errors (Hopefully, otherError)
-- import Compiler.IRs
-- import Compiler.Metadata (Metadata(..))
-- import Compiler.Registers
import MicroRAM
import MicroRAM.MRAMInterpreter.Generic
import MicroRAM.MRAMInterpreter.Concrete
--import Util.Util
import Compiler.Tainted

data TaintedValue =
  TaintedValue MWord (Vector Label)
  deriving (Show)

tval :: Lens' TaintedValue MWord
tval f (TaintedValue a b) = flip TaintedValue b <$> f a

tlbl :: Lens' TaintedValue (Vector Label)
tlbl f (TaintedValue a b) = TaintedValue a <$> f b

data TaintedMem = TaintedMem {
    _tmDefault :: MWord,
    _tmMem :: Map MWord MWord,
    _tmMemLabels :: Map MWord (Vector Label),
    _tmPoison :: Set.Set MWord
  }
  deriving (Show)
makeLenses ''TaintedMem

mkConcrete :: Functor f => (WordMemory -> f WordMemory) -> TaintedMem -> f TaintedMem
mkConcrete = lens getConcrete setConcrete
  where getConcrete (TaintedMem deflt m _ p) = WordMemory deflt m p
        setConcrete (TaintedMem _ _ ls _) (WordMemory deflt m p) = TaintedMem deflt m ls p
  
taintBinop :: (MWord -> MWord -> MWord) -> TaintedValue -> TaintedValue -> TaintedValue 
taintBinop f a b = TaintedValue (view tval a `f` view tval b) untaintedWord

instance AbsDomain TaintedValue where
  type Memory TaintedValue = TaintedMem

  absInitMem mem = TaintedMem {
    _tmDefault = 0,
    _tmMem = flatInitMem mem,
    _tmMemLabels = flatInitTaintedMem mem,
    _tmPoison = mempty
    }

  -- Creates an untainted value
  absExact v = TaintedValue v untaintedWord

  -- For operations, we use the concrete implementation,
  -- imported from MRAMInterpreter.Concrete.hs, 
  -- for the value (i.e. first component) and construct
  -- the tainting rules 
  absAdd = taintBinop absAdd
  absSub = taintBinop absSub
  absUMul a b = let (hi, lo) = view tval a `absUMul` view tval b in
    (TaintedValue hi untaintedWord, TaintedValue lo untaintedWord)
  absSMul a b = let (hi, lo) = view tval a `absSMul` view tval b in
    (TaintedValue hi untaintedWord, TaintedValue lo untaintedWord)
  absDiv = taintBinop absDiv 
  absMod = taintBinop absMod 
  absNeg = over tval absNeg 
         
  absAnd = taintBinop absAnd 
  absOr  = taintBinop absOr 
  absXor = taintBinop absXor 
  absNot = over tval absNot 
  absShl = taintBinop absShl 
  absShr = taintBinop absShr 
     
  absEq  = taintBinop absEq 
  absUGt = taintBinop absUGt 
  absUGe = taintBinop absUGe 
  absSGt = taintBinop absSGt 
  absSGe = taintBinop absSGe 

  absMux (TaintedValue c _) t e =  if c /= 0 then t else e

  absStore w (TaintedValue addr1 _) (TaintedValue val1 val2) mem = do
    concreteMem <- mem & mkConcrete (absStore w addr1 val1)
    lbls <- storeLabel w addr1 val2 $ view tmMemLabels mem
    return $ concreteMem & tmMemLabels .~ lbls
    
  absLoad w (TaintedValue addr1 _) mem = do
    value <- absLoad w addr1 $ view mkConcrete mem
    lbls <- loadLabel w addr1 $ view tmMemLabels mem 
    return $ TaintedValue value lbls
    
  absPoison w (TaintedValue addr1 _) mem =
    mem & mkConcrete (absPoison w addr1)

  absGetPoison w (TaintedValue addr1 _) mem = 
    absGetPoison w addr1 $ view mkConcrete mem
    
  absGetValue (TaintedValue x _) = return x

storeLabel :: MemWidth -> MWord -> Vector Label -> Map MWord (Vector Label) -> Hopefully (Map MWord (Vector Label))
storeLabel wd addr l memLabels = do
  (waddr, offset) <- splitAlignedAddr wd addr
  checkLabels $ Just l
  return $ memLabels & getLabels waddr . subLabels wd offset .~ l


getLabels :: Functor f => MWord
          -> (Vector Label -> f (Vector Label))
          -> Map MWord (Vector Label) -> f (Map MWord (Vector Label))
getLabels addr = lens (get addr) (set addr)
  where
    get addr m = m & maybe untaintedWord id . Map.lookup addr
    set addr m val = m & Map.insert addr val

loadLabel :: MemWidth -> MWord -> Map MWord (Vector Label) -> Hopefully (Vector Label)
loadLabel wd addr memLabels = do
  (waddr, offset) <- splitAlignedAddr wd addr
  return $ padUntaintedWord $ memLabels ^. (getLabels waddr . subLabels wd offset)

subLabels :: Functor f => MemWidth -> Int -> (Vector Label -> f (Vector Label)) -> Vector Label -> f (Vector Label)
subLabels wd offset f ls = putVec <$> f ls'
  where
    w = widthInt wd
    ls' = Vec.slice offset w ls
    -- Assumes that the provided vector has a width of at least w.
    putVec v' =
      let (ls1, lsr) = Vec.splitAt offset  ls
          (_ls2, ls3) = Vec.splitAt w lsr
          v = Vec.take w v' in
      Vec.concat [ls1, v, ls3]

-- Enforces the invariant that poisons require WWord widths.
requireWWord :: MemWidth -> Hopefully ()
requireWWord w = when (w /= WWord) $
    otherError $ "bad poison width " ++ show w
