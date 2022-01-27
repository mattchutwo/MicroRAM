{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MicroRAM.MRAMInterpreter.Tainted where

import Control.Lens (makeLenses, (^.), (&), (.~), view, Lens', lens)
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
import Compiler.Errors (Hopefully, otherError, progError)
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
  
approx :: Label -> Label
approx l1 | l1 == bottom = bottom
          | otherwise    = maybeTainted

approx2 :: Label -> Label -> Label
approx2 l1 l2 | l1 == bottom && l2 == bottom = bottom
              | otherwise                    = maybeTainted

approxVec :: Vector Label -> Vector Label
approxVec l1 = replicateWord $ Vec.foldl1' approx2 l1

approxVec2 :: Vector Label -> Vector Label -> Vector Label
approxVec2 l1 l2 = replicateWord $ Vec.foldl' approx2 (Vec.foldl1' approx2 l1) l2

checkVec :: Vector Label -> Vector Label -> Vector Label
checkVec guards l =
  let guard = Vec.foldr1' join guards in
  if guard == bottom then l else maybeTaintedWord

taintBinop :: (MWord -> MWord -> MWord) -> TaintedValue -> TaintedValue -> TaintedValue 
taintBinop f (TaintedValue a l1) (TaintedValue b l2) = TaintedValue (f a b) (approxVec2 l1 l2)

instance AbsDomain TaintedValue where
  type Memory TaintedValue = TaintedMem

  absInitMem mem = TaintedMem {
    _tmDefault = 0,
    _tmMem = flatInitMem mem,
    _tmMemLabels = flatInitTaintedMem mem,
    _tmPoison = mempty
    }

  -- Creates an untainted value
  absExact v = TaintedValue v bottomWord

  -- For operations, we use the concrete implementation,
  -- imported from MRAMInterpreter.Concrete.hs, 
  -- for the value (i.e. first component) and construct
  -- the tainting rules 
  absAdd = taintBinop absAdd
  absSub = taintBinop absSub
  absUMul (TaintedValue a l1) (TaintedValue b l2) = 
    let (hi, lo) = absUMul a b in
    let l = approxVec2 l1 l2 in
    (TaintedValue hi l, TaintedValue lo l)
  absSMul (TaintedValue a l1) (TaintedValue b l2) = 
    let (hi, lo) = absSMul a b in
    let l = approxVec2 l1 l2 in
    (TaintedValue hi l, TaintedValue lo l)
  absDiv = taintBinop absDiv 
  absMod = taintBinop absMod 
         
  absAnd = taintBinop absAnd 
  absOr  = taintBinop absOr 
  absXor = taintBinop absXor 
  absNot (TaintedValue a l1) = TaintedValue (absNot a) (approxVec l1)
  absShl = taintBinop absShl 
  absShr = taintBinop absShr 
     
  absEq  = taintBinop absEq 
  absUGt = taintBinop absUGt 
  absUGe = taintBinop absUGe 
  absSGt = taintBinop absSGt 
  absSGe = taintBinop absSGe 

  absMux (TaintedValue c cls) t e =
    let (TaintedValue branch branchL) = if c /= 0 then t else e in
    let cl = Vec.foldr1' join cls  in
    TaintedValue branch $ Vec.map (join cl) branchL

  absStore w (TaintedValue addr addrL) (TaintedValue val1 valL) mem = do
    concreteMem <- mem & mkConcrete (absStore w addr val1)
    lbls <- storeLabel w addr addrL valL $ view tmMemLabels mem
    return $ concreteMem & tmMemLabels .~ lbls
    
  -- TODO: Does poison not write a value? No impact on taint?
  absPoison w (TaintedValue addr _) mem =
    mem & mkConcrete (absPoison w addr)

  absLoad w (TaintedValue addr addrL) mem = do
    value <- absLoad w addr $ view mkConcrete mem
    lbls <- loadLabel w addr addrL $ view tmMemLabels mem 
    return $ TaintedValue value lbls
    
  absTaint w offset l old = do
    ls <- toLabel $ l ^. tval
    return $ old & tlbl . subLabels w offset .~ replicateWord ls
      
  absSink w ls (TaintedValue l2 _) = do
    -- Write of value in `rj` to label `op2`.
    -- Bug here if label of rj cannot flow into op2.
    let ljs = (Vec.take (widthInt w)) $ (view tlbl ls)
    checkLabelsBound (Just ljs)
    lbl2 <- toLabel l2
    let isBug = any (\lj -> not $ lj `canFlowTo` lbl2) ljs
    return isBug

  absValidJump (TaintedValue _ condL) (TaintedValue _ destL)
    | Vec.foldr1' join condL /= bottom = progError $ "Invalid jump. Cannot branch on tainted data with label: " <> show condL
    | Vec.foldr1' join destL /= bottom = progError $ "Invalid jump. Cannot jump to tainted destination with label: " <> show condL
    | otherwise                        = return ()

  absGetPoison w (TaintedValue addr1 _) mem = 
    absGetPoison w addr1 $ view mkConcrete mem
    
  absGetValue (TaintedValue x _) = return x

storeLabel :: MemWidth -> MWord -> Vector Label -> Vector Label -> Map MWord (Vector Label) -> Hopefully (Map MWord (Vector Label))
storeLabel wd addr addrL l' memLabels = do
  (waddr, offset) <- splitAlignedAddr wd addr
  let l = checkVec addrL l'
  checkLabelsBound $ Just l
  return $ memLabels & getLabels waddr . subLabels wd offset .~ l


getLabels :: Functor f => MWord
          -> (Vector Label -> f (Vector Label))
          -> Map MWord (Vector Label) -> f (Map MWord (Vector Label))
getLabels addr = lens (get addr) (set addr)
  where
    get addr m = m & maybe bottomWord id . Map.lookup addr
    set addr m val = m & Map.insert addr val

loadLabel :: MemWidth -> MWord -> Vector Label -> Map MWord (Vector Label) -> Hopefully (Vector Label)
loadLabel wd addr addrL memLabels = do
  (waddr, offset) <- splitAlignedAddr wd addr
  return $ checkVec addrL $ padBottomWord $ memLabels ^. (getLabels waddr . subLabels wd offset)

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
