{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RiscV.Intrinsics where -- (transpiler)

import           Control.Monad.Trans.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Bits (complement)
import           Data.Text (Text)
import qualified Data.Text as Text

import           MicroRAM
import           RiscV.RiscVAsm
import           Compiler.IRs
import           Compiler.Metadata
import           Compiler.Name
import           Compiler.Errors
import           Compiler.LazyConstants
import           Compiler.Extension (lowerInstr')

import           Control.Lens (makeLenses, at, to, (^.), (.=), (%=), (?=), use, over)

import Debug.Trace (trace)

-- | #Names
-- We create a map of names that relates every intrinsic to it's unique ID.
-- The map is used by the transpiler to correctly identify the calls to intrinsics.
data StateIntrNames = StateIntrNames
  { _nameIntrID :: Word
  , _nameMap :: Map.Map String Name
  }

initState :: StateIntrNames
initState = StateIntrNames
  { _nameIntrID = firstUnusedName
  , _nameMap = mempty
  }
makeLenses ''StateIntrNames

getName :: String -> State StateIntrNames Name
getName st = do
  nmap <- use nameMap
  nameRet <-
    -- We probably don't need to look it up, since we are creating
    -- each intrinsic once, but this mimics the behavior of the
    -- transpiler
    case Map.lookup st nmap of 
               Just name -> trace ("Just found the name "<> show name) $
                 return name
               Nothing -> do uniqueID <- use nameIntrID
                             nameIntrID %= (1 +)
                             let nameRet = Name uniqueID $ string2short st
                             nameMap .= Map.insert st nameRet nmap
                             trace ("Created a new name "<> show nameRet) $
                               return $ nameRet
  return nameRet


{-

__cc_access_invalid
__cc_access_valid
__cc_advise_poison_offset
__cc_flag_invalid
__cc_read_unchecked
__cc_write_and_poison
__cc_write_unchecked

The strategy for replacing intrinsics is as follows. 

1. Intrinsics will remain function calls.
2. The functions will only use caller saved registers to avoid saving registers `x1,x5–7, x10–17, x28–31`
3. There shall be two versions with and without externals and thsi will produce the two programs.

-}

-- shall be wrapped in `justCompile`
addIntrinsics :: MAProgram Metadata Int MWord -> Hopefully (MAProgram Metadata Int MWord)
addIntrinsics prog = return $ prog ++ intrinsics

intrinsics :: [NamedBlock Metadata Int MWord] -- Also [Intrinsic]
intrinsics = fst intrinsicsAndMap

loweredIntrinsics  :: [NamedBlock Metadata Int MWord]
loweredIntrinsics = lowerBlock <$> intrinsics
  where lowerBlock (NBlock name instrs) =
          NBlock name (concat $ lowerInstrMd <$> instrs)
        lowerInstrMd :: (MAInstruction Int MWord, Metadata)
                     -> [(MAInstruction Int MWord, Metadata)]
        lowerInstrMd (instr,md) = (\a -> (a,md)) <$> lowerInstr' newReg instr
        
  
intrinsicsMap :: Map.Map String Name
intrinsicsMap = _nameMap $ snd  intrinsicsAndMap

intrinsicsFstUnusedName :: Word
intrinsicsFstUnusedName = _nameIntrID $ snd  intrinsicsAndMap

intrinsicsAndMap
  :: ([NamedBlock Metadata Int MWord], StateIntrNames)
intrinsicsAndMap = flip runState initState $
  sequence [ cc_noop
           , cc_malloc
           , cc_access_valid
           , cc_access_invalid
           , cc_advise_poison_offset
           , cc_write_and_poison
           , cc_read_unchecked
           , cc_write_unchecked
           , cc_flag_invalid
           , cc_flag_bug
           , cc_trace]



{- | # Intrinsiscs: Implementation
   All intrinsics foollow the following strict rules:

   1. All functions end in a function return (`Ijmp 1`). In
   particular, all intrinsics are non-empty.

   2. The only branches/jumps is the single return (`Ijmp 1`) at the
   end. In particular, it consists of a single block. 

   3. Intrinsics only use caller saved registers (and don't overwrite
   the return address X1 (1). The registers that can be used, with the
   Int equivalent:
   `x5–7, x10–17, x28–31`
   (5-7, 10-17, 28-31)

   4. The return value, if any, is a single word and passed in X10 (10)

   5. Function arguments are taking from registers `x10-x17` (10-17) in order.

-}

type Intrinsic = State StateIntrNames (NamedBlock Metadata Int MWord)

intrinsicMetadata :: Name -> Metadata
intrinsicMetadata name = defaultMetadata
                         { mdFunction = name
                         , mdBlock = name }

-- | This function is not efficient, but intrinsics are very short so
-- this shouldn't be noticeable.
-- 
-- This implementation Assumes (1) and (2) described above. In particular
-- the only return point is the last instruction and there are no
-- other branches/jumps.
putTheMetadata :: Metadata
               -> [MAInstruction Int MWord]
               -> [(MAInstruction Int MWord, Metadata)]
putTheMetadata md instrs =
  updateHead addFunctionStart  $
  updateLast addFunctionReturn $
  (\instr -> (instr, md)) <$> instrs

  where
    addFunctionStart, addFunctionReturn :: (MAInstruction Int MWord, Metadata)
                                        -> (MAInstruction Int MWord, Metadata)
    addFunctionStart (instr, md) =
      (instr, md {mdFunctionStart = True} )
    addFunctionReturn (instr, md) =
      (instr, md {mdIsReturn = True} )
    -- | Update the first element of a list, if it exists.
    --   O(1).
    updateHead :: (a -> a) -> [a] -> [a]
    updateHead _ []       = []
    updateHead f (a : as) = f a : as
    -- | Update the last element of a list, if it exists.
    --   O(n).
    updateLast :: (a -> a) -> [a] -> [a]
    updateLast _ [] = []
    updateLast f (a : as) = loop a as
      -- Using a helper function to minimize the pattern matching.
      where
        loop a []       = [f a]
        loop a (b : bs) = a : loop b bs

buildIntrinsic :: String -> [MAInstruction Int MWord] -> Intrinsic
buildIntrinsic name instrs = do
  nameID <- getName name
  let md = intrinsicMetadata nameID
  let instrs_md = putTheMetadata md instrs
  return $ NBlock (Just nameID) instrs_md

ret :: MAInstruction Int MWord
ret = Ijmp . AReg $ fromEnum X1

retReg :: Int
retReg = fromEnum X10


arg0', arg1' :: Int
arg0' = fromEnum X10
arg1' = fromEnum X11

arg0, arg1 :: MAOperand Int MWord
arg0 = AReg $ arg0'
arg1 = AReg $ arg1'

-- This on is not used in riscv.
cc_noop :: Intrinsic
cc_noop = buildIntrinsic "__cc_noop" [ret]

cc_trap :: Text -> [MAInstruction Int MWord]
cc_trap desc  = [ Iext (XTrace ("Trap: " <> desc) []),
                  Ianswer (LImm $ SConst 0)]

cc_malloc :: Intrinsic
cc_malloc  =
  buildIntrinsic "__cc_malloc"  
  [Iextadvise retReg (LImm $ SConst $ complement 0) (XMalloc arg0)]

cc_access_valid :: Intrinsic
cc_access_valid = 
  buildIntrinsic "__cc_access_valid"
  [Iext (XAccessValid arg0 arg1)]
  
cc_access_invalid :: Intrinsic
cc_access_invalid = 
  buildIntrinsic "__cc_access_invalid"
  [Iext (XAccessInvalid arg0 arg1)]
  
cc_advise_poison_offset :: Intrinsic
cc_advise_poison_offset =
    buildIntrinsic "__cc_advise_poison_offset"
  [Iextadvise retReg arg1 (XAdvisePoison arg0 arg1)]

cc_write_and_poison :: Intrinsic
cc_write_and_poison =
    buildIntrinsic "__cc_write_and_poison"
  [IpoisonW arg0 arg1']

cc_read_unchecked :: Intrinsic
cc_read_unchecked =
  buildIntrinsic "__cc_read_unchecked"
  [Iextval retReg (XLoadUnchecked arg0)]

cc_write_unchecked :: Intrinsic
cc_write_unchecked =
  buildIntrinsic "__cc_write_unchecked"
  [Iext (XStoreUnchecked arg0 arg1)]

cc_flag_invalid :: Intrinsic
cc_flag_invalid =
  buildIntrinsic "__cc_flag_invalid" [
     Iext (XTrace "@__cc_flag_invalid" []),
     Imov (fromEnum X5) zero,
     IpoisonW zero (fromEnum X5) ]
  where zero = LImm $ SConst 0

cc_flag_bug :: Intrinsic
cc_flag_bug =
  buildIntrinsic "__cc_flag_bug"
  [Imov (fromEnum X5) zero,
   IstoreW zero (fromEnum X5)]
  where zero = LImm $ SConst 0

cc_trace :: Intrinsic
cc_trace =
  buildIntrinsic "__cc_trace"
  [Iext (XTraceStr arg0)]

-- TODO: we could implement trace trace_exec with vargs. Or more simply
-- fix the number of arguments, say 8.
-- cc_trace_exec :: Intrinsic
-- cc_trace_exec (name : args) Nothing md _ =
--   buildIntrinsic "__" [Iext (XTraceExec name args)]
