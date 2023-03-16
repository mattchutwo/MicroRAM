{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RiscV.Intrinsics
  ( addIntrinsicsHigh
  , addIntrinsicsLow
  , addIntrinsicsWithNames
  , riscvIntrinsicsCU
  , intrinsicsFstUnusedName
  , intrinsicsMap
  , lowerExtensionInstrsAsm
  ) where -- (transpiler)

import           Control.Monad.Trans.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Bits (complement)
import           Data.Text (Text)
import qualified Data.Text as Text

import           MicroRAM
import           RiscV.RiscVAsm
import           RiscV.Transpiler (makeRiscvCompUnit)
import           Compiler.IRs
import           Compiler.Metadata
import           Compiler.Name
import           Compiler.Errors
import           Compiler.LazyConstants
import           Compiler.Extension (lowerInstr')
import           Compiler.CompilationUnit
import           Compiler.Common (GEnv)

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
               Just name -> return name
               Nothing -> do uniqueID <- use nameIntrID
                             nameIntrID %= (1 +)
                             let nameRet = Name uniqueID $ string2short st
                             nameMap .= Map.insert st nameRet nmap
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

2. As long as possible, the functions will only use caller saved
   registers to avoid saving registers x1,x5–7, x10–17, x28–31. All
   intrinsics we have so far use fewer than 16 registers.

3. There shall be two versions of each function with and without
   externals and this will produce the two programs. The second
   version produced by feeding the intrinsic through a filter that
   removes the externals.  The register names will be passed to the
   transpiler, so that calls to intrinsics will get the right name and
   ID.

4. Then all the intrinsics will be added in a new executable section
   for intrinsics. In MicroRAM terms this is equivalent to sticking
   them at the end of the program.

-}

-- shall be wrapped in `justCompile`
-- | Adds intrinsics with externals 
addIntrinsicsHigh :: MAProgram Metadata Int MWord -> Hopefully (MAProgram Metadata Int MWord)
addIntrinsicsHigh prog = return $ prog ++ intrinsics

-- | Adds intrinsics with lowered externals 
addIntrinsicsLow :: MAProgram Metadata Int MWord -> Hopefully (MAProgram Metadata Int MWord)
addIntrinsicsLow prog = map lowerBlock <$> addIntrinsicsHigh prog   
  -- return $ prog ++ loweredIntrinsics

intrinsics :: [NamedBlock Metadata Int MWord] -- Also [Intrinsic]
intrinsics = fst intrinsicsAndMap

loweredIntrinsics  :: [NamedBlock Metadata Int MWord]
loweredIntrinsics = lowerBlock <$> intrinsics

lowerBlock
  :: NamedBlock Metadata Int MWord
  -> NamedBlock Metadata Int MWord
lowerBlock blk@NamedBlock { blockInstrs = instrs } =
  blk { blockInstrs = concat $ lowerInstrMd <$> instrs }
  where lowerInstrMd :: (MAInstruction Int MWord, Metadata)
                     -> [(MAInstruction Int MWord, Metadata)]
        lowerInstrMd (instr,md) = (\a -> (a,md)) <$> lowerInstr' newReg instr
        
  
intrinsicsMap :: Map.Map String Name
intrinsicsMap = _nameMap $ snd  intrinsicsAndMap

intrinsicsFstUnusedName :: Word
intrinsicsFstUnusedName = _nameIntrID $ snd  intrinsicsAndMap

intrinsicsAndMap
  :: ([NamedBlock Metadata Int MWord], StateIntrNames)
intrinsicsAndMap = runState buildIntrinsics initState

buildIntrinsics :: State StateIntrNames [NamedBlock Metadata Int MWord]
buildIntrinsics =
  sequence $ [ cc_noop
           , cc_malloc
           , cc_access_valid
           , cc_access_invalid
           , cc_advise_poison_offset
           , cc_write_and_poison
           , cc_read_unchecked
           , cc_write_unchecked
           , cc_flag_invalid
           , cc_flag_bug
           , cc_trace
           , cc_trace_exec
           , cc_answer
           , cc_exit
           ]
  ++ exceptions ++ otherTraps

addIntrinsicsWithNames
  :: (MAProgram Metadata Int MWord, Word)
  -> Hopefully (MAProgram Metadata Int MWord, Word)
addIntrinsicsWithNames (blks, nextName) = return (blks ++ intrinBlks, nextName')
  where st = StateIntrNames nextName mempty
        (intrinBlks, st') = runState buildIntrinsics st
        nextName' = st' ^. nameIntrID

riscvIntrinsicsCU
  :: Word
  -> Hopefully (CompilationUnit (GEnv MWord) (MAProgram Metadata Int MWord))
riscvIntrinsicsCU nextName = do
  (prog, nextName') <- addIntrinsicsWithNames ([], nextName)
  return $ makeRiscvCompUnit prog [] nextName'

lowerExtensionInstrsAsm
  :: MAProgram Metadata Int MWord
  -> Hopefully (MAProgram Metadata Int MWord)
lowerExtensionInstrsAsm blks = return $ map goBlock blks
  where goBlock blk = blk { blockInstrs = concatMap goInstr $ blockInstrs blk }
        goInstr (instr, md) = [(instr', md) | instr' <- lowerInstr' newReg instr]


{- | # Intrinsiscs: Implementation
   All intrinsics follow the following strict rules:

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
  let instrs_md = putTheMetadata md (instrs ++ [Ijmp $ AReg 1])
  return $ (makeNamedBlock (Just nameID) instrs_md) { blockExtern = True }

ret :: MAInstruction Int MWord
ret = Ijmp . AReg $ fromEnum X1

retReg :: Int
retReg = fromEnum X10


arg0', arg1' :: Int
arg0' = fromEnum X10
arg1' = fromEnum X11
arg2' = fromEnum X12
arg3' = fromEnum X13
arg4' = fromEnum X14

arg0, arg1 :: MAOperand Int MWord
arg0 = AReg $ arg0'
arg1 = AReg $ arg1'
arg2 = AReg $ arg2'
arg3 = AReg $ arg3'
arg4 = AReg $ arg4'

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

-- | Trace exec is fixed to 4 arguments. 
cc_trace_exec :: Intrinsic
cc_trace_exec =
  buildIntrinsic "__cc_trace_exec"
  [Iext (XTraceExec arg0 otherArgs)]
  where otherArgs = [arg1, arg2, arg3, arg4]
-- # Exceptions
exceptions = [cxa_allocate_exception, cxa_begin_catch, cxa_end_catch, cxa_throw, gxx_personality_v0]

cc_answer :: Intrinsic
cc_answer =
  buildIntrinsic "__cc_answer"
  [Ianswer arg0]

cc_exit :: Intrinsic
cc_exit = do
  blk <- buildIntrinsic "__cc_exit"
    [Ianswer arg0]
  -- TODO: adjust blk to ensure it's placed at exactly 0xffff_ffff
  return blk

cxa_allocate_exception, cxa_begin_catch, cxa_end_catch, cxa_throw, gxx_personality_v0 :: Intrinsic

cxa_allocate_exception = buildIntrinsic "__cxa_allocate_exception" $ cc_trap "__cxa_allocate_exception"
cxa_begin_catch        = buildIntrinsic "__cxa_begin_catch" $ cc_trap "__cxa_begin_catch"
cxa_end_catch          = buildIntrinsic "__cxa_end_catch" $ cc_trap "__cxa_end_catch"
cxa_throw              = buildIntrinsic "__cxa_throw" $ cc_trap "__cxa_throw"
gxx_personality_v0     = buildIntrinsic "__gxx_personality_v0" $ cc_trap "__gxx_personality_v0"



--  #Other traps
-- These functions show up on Grit and shouldn't be called.
-- However we should have a better pipeline to remove them automatically.
--otherTraps = [iob, unwind_Resume, zTIPKc]
otherTraps = [unwind_Resume, zTIPKc]

iob, unwind_Resume, zTIPKc, memcpy, pos :: Intrinsic
iob           = buildIntrinsic "__iob" $ cc_trap "iob"
unwind_Resume = buildIntrinsic "_Unwind_Resume" $ cc_trap "Unwind_Resume"
zTIPKc        = buildIntrinsic "_ZTIPKc" $ cc_trap "ZTIPKc"
memcpy        = buildIntrinsic "memcpy" $ cc_trap "memcpy"
pos           = buildIntrinsic "pos" $ cc_trap "pos"
