{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Intrinsic lowering
Description : Converts calls to intrinsics to MicroASM instructions
Maintainer  : santiago@galois.com
Stability   : prototype

Inlines calls to intrinsics to MicroASM instructions. Renames the
intrinsics from using underscors ("__") to using dot (".") and removes
the empty bodies of those functions.

-}

module Compiler.Intrinsics
    ( lowerIntrinsics,
      renameLLVMIntrinsicImpls,
    ) where


import           Control.Monad
import           Control.Monad.State (evalStateT, StateT, get, modify')
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Compiler.Common (Name(Name), Ty(..), tySize)
import Compiler.Errors
import Compiler.IRs
import Compiler.LazyConstants

import MicroRAM

data CallingContextMetadata = CCM {
    _argTys :: [Ty]
  }

expandInstrs :: forall f m w.
  Monad f => (CallingContextMetadata -> MIRInstr m w -> StateT IState f [MIRInstr m w]) -> MIRprog m w -> f (MIRprog m w)
expandInstrs f = goProg
  -- TODO: this can probably be done more cleanly with traverse or lenses
  where goProg :: MIRprog m w -> f (MIRprog m w)
        goProg (IRprog te gs code) = IRprog te gs <$> traverse goFunc code

        goFunc :: MIRFunction m w -> f (MIRFunction m w)
        goFunc (Function nm rty atys bbs nr) = flip evalStateT (IState nr) $
          Function nm rty atys <$> traverse (goBB $ CCM atys) bbs <*> fmap iNextRegister get

        goBB :: CallingContextMetadata -> BB n (MIRInstr m w) -> StateT IState f (BB n (MIRInstr m w))
        goBB ccm (BB nm body term dag) = BB nm <$> goInstrs ccm body <*> goInstrs ccm term <*> pure dag

        goInstrs :: CallingContextMetadata -> [MIRInstr m w] -> StateT IState f [MIRInstr m w]
        goInstrs ccm is = concat <$> traverse (f ccm) is

data IState = IState {
    iNextRegister :: Word
  }

getNextRegister :: Monad m => StateT IState m VReg
getNextRegister = do
  reg <- (\i -> Name $ "_intrinsic" <> Short.toShort (BSC.pack $ show $ iNextRegister i)) <$> get
  modify' $ \(IState c) -> IState (c+1)
  return reg

type IntrinsicImpl m w = [MAOperand VReg w] -> Maybe VReg -> m -> CallingContextMetadata -> StateT IState Hopefully [MIRInstr m w]

expandIntrinsicCall :: forall m w. Show w => Map String (IntrinsicImpl m w) -> CallingContextMetadata -> MIRInstr m w -> StateT IState Hopefully [MIRInstr m w]
expandIntrinsicCall intrinMap ccm (MirI (RCall _ dest (Label name) _ args) meta)
  | Just impl <- Map.lookup name intrinMap =
    tagState ("bad call to intrinsic " ++ name) $ impl args dest meta ccm
expandIntrinsicCall _ _ instr = return [instr]


cc_test_add :: IntrinsicImpl m w
cc_test_add [x, y] (Just dest) md _ = return [MirM (Iadd dest x y) md]
cc_test_add _ _ _ _ = progError "bad arguments"

cc_noop :: IntrinsicImpl m w
cc_noop _ _ _ _ = return []

cc_trap :: IntrinsicImpl m MWord
cc_trap _ _ md _ = return [
  MirM (Iext "trace_trap" []) md,
  MirM (Ianswer (LImm $ SConst 0)) md] -- TODO

cc_malloc :: IntrinsicImpl m w
cc_malloc [size] (Just dest) md _ = return [MirM (Iextval "malloc" dest [size]) md]
cc_malloc _ _ _ _ = progError "bad arguments"

cc_free :: IntrinsicImpl m w
cc_free [ptr] Nothing md _ = return [MirM (Iext "free" [ptr]) md]
cc_free _ _ _ _ = progError "bad arguments"

cc_advise_poison :: IntrinsicImpl m w
cc_advise_poison [lo, hi] (Just dest) md _ = return [MirM (Iextval "advise_poison" dest [lo, hi]) md]
cc_advise_poison _ _ _ _ = progError "bad arguments"

cc_write_and_poison :: IntrinsicImpl m w
cc_write_and_poison [ptr, val] Nothing md _ =
  return [MirM (IpoisonW ptr val) md]
cc_write_and_poison _ _ _ _ = progError "bad arguments"

cc_flag_invalid :: IntrinsicImpl m MWord
cc_flag_invalid [] Nothing md _ =
  return [MirM (IpoisonW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_invalid _ _ _ _ = progError "bad arguments"

cc_flag_bug :: IntrinsicImpl m MWord
cc_flag_bug [] Nothing md _ =
  return [MirM (IstoreW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_bug _ _ _ _ = progError "bad arguments"

cc_trace :: IntrinsicImpl m w
cc_trace [msg] Nothing md _ = return [MirM (Iext "tracestr" [msg]) md]
cc_trace _ _ _ _ = progError "bad arguments"

va_start :: IntrinsicImpl m MWord
va_start [ptr] Nothing md (CCM argTys) = do
  reg <- getNextRegister
  return [
      MirI (RGetBP reg) md
    , MirI (RCall TVoid Nothing (Label "Name \"__cc_va_start\"") [Tptr, Tptr, Tint] [ptr, AReg reg, offset]) md
    ]
  where
    -- Skip return address and nonvariadic arguments.
    offset = LImm $ SConst $ fromIntegral wordBytes * (2 + (sum $ map tySize argTys))
va_start _ _ _ _ = progError "bad arguments"

intrinsics :: Map String (IntrinsicImpl m MWord)
intrinsics = Map.fromList $ map (\(x :: String, y) -> ("Name " ++ show x, y)) $
  [ ("__cc_test_add", cc_test_add)
  , ("__cc_flag_invalid", cc_flag_invalid)
  , ("__cc_flag_bug", cc_flag_bug)

  , ("__cc_malloc", cc_malloc)
  , ("__cc_free", cc_free)
  , ("__cc_advise_poison", cc_advise_poison)
  , ("__cc_write_and_poison", cc_write_and_poison)

  , ("__cc_trace", cc_trace)

  , ("llvm.lifetime.start.p0i8", cc_noop)
  , ("llvm.lifetime.end.p0i8", cc_noop)

  , ("llvm.va_start", va_start)
  , ("llvm.va_end", cc_noop)

  -- Exception handling
  , ("__gxx_personality_v0", cc_trap)
  , ("__cxa_allocate_exception", cc_trap)
  , ("__cxa_throw", cc_trap)
  , ("__cxa_begin_catch", cc_trap)
  , ("__cxa_end_catch", cc_trap)
  , ("llvm.eh.typeid.for", cc_trap)

  -- Explicit trap
  , ("__cxa_pure_virtual", cc_trap)
  , ("llvm.trap", cc_trap)
  ]

lowerIntrinsics :: forall m. MIRprog m MWord -> Hopefully (MIRprog m MWord)
lowerIntrinsics = expandInstrs (expandIntrinsicCall intrinsics)
                  >=> removeIntrinsics

-- | removes the intrinsics declarations which have been inlined and are not needed anymore.
-- This overlaps with dead code elimination (a bit), but enables checking for undefined functions
removeIntrinsics :: MIRprog m MWord -> Hopefully (MIRprog m MWord)
removeIntrinsics prog = 
  return $ prog {code = filter (not . isIntrinsic) $ code prog}
  where isIntrinsic f = show (funcName f) `Map.member` intrinsics
  
-- | Rename C/LLVM implementations of LLVM intrinsics to line up with their
-- intrinsic name.
--
-- The problem this solves is that we can't directly define a function with a
-- name like `llvm.memset.p0i8.i64` in C.  Instead, we define a function name
-- `__llvm__memset__p0i8__i64`, then this pass renames it to the dotted form.
-- (It also renames the empty definition of the dotted form to `orig.llvm.foo`,
-- to avoid conflicts later on.)
renameLLVMIntrinsicImpls :: forall m. MIRprog m MWord -> Hopefully (MIRprog m MWord)
renameLLVMIntrinsicImpls (IRprog te gs code) = return $ IRprog te gs code'
  where
    renameList :: [(Name, Name)]
    renameList = do
      Function nm _ _ _ _ <- code
      Name ss <- return nm
      Just name <- return $ Text.stripPrefix "__llvm__" $ toText ss
      return (nm, Name $ fromText $ "llvm." <> Text.replace "__" "." name)

    renameMap = Map.fromList renameList
    removeSet = Set.fromList $ map snd renameList

    code' :: [MIRFunction m MWord]
    code' = do
      Function nm rty atys bbs nr <- code
      guard $ not $ Set.member nm removeSet
      let nm' = maybe nm id $ Map.lookup nm renameMap
      return $ Function nm' rty atys bbs nr

    fromText t = Short.toShort $ BSU.fromString $ Text.unpack t
    toText s = Text.pack $ BSU.toString $ Short.fromShort s
