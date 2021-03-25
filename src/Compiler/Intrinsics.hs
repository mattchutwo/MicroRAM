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


import Control.Monad
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Compiler.Common (Name(Name))
import Compiler.Errors
import Compiler.IRs
import Compiler.LazyConstants

import MicroRAM


expandInstrs :: forall f m w.
  Applicative f => (MIRInstr m w -> f [MIRInstr m w]) -> MIRprog m w -> f (MIRprog m w)
expandInstrs f = goProg
  -- TODO: this can probably be done more cleanly with traverse or lenses
  where goProg :: MIRprog m w -> f (MIRprog m w)
        goProg (IRprog te gs code) = IRprog te gs <$> traverse goFunc code

        goFunc :: MIRFunction m w -> f (MIRFunction m w)
        goFunc (Function nm rty atys bbs nr) =
          Function nm rty atys <$> traverse goBB bbs <*> pure nr

        goBB :: BB n (MIRInstr m w) -> f (BB n (MIRInstr m w))
        goBB (BB nm body term dag) = BB nm <$> goInstrs body <*> goInstrs term <*> pure dag

        goInstrs :: [MIRInstr m w] -> f [MIRInstr m w]
        goInstrs is = concat <$> traverse f is

type IntrinsicImpl m w = [MAOperand VReg w] -> Maybe VReg -> m -> Hopefully [MIRInstr m w]

expandIntrinsicCall :: forall m w. Show w => Map String (IntrinsicImpl m w) -> MIRInstr m w -> Hopefully [MIRInstr m w]
expandIntrinsicCall intrinMap (MirI (RCall _ dest (Label name) _ args) meta)
  | Just impl <- Map.lookup name intrinMap =
    tag ("bad call to intrinsic " ++ name) $ impl args dest meta
expandIntrinsicCall _ instr = return [instr]


cc_test_add :: IntrinsicImpl m w
cc_test_add [x, y] (Just dest) md = return [MirM (Iadd dest x y) md]
cc_test_add _ _ _ = progError "bad arguments"

cc_noop :: IntrinsicImpl m w
cc_noop _ _ _ = return []

cc_trap :: IntrinsicImpl m MWord
cc_trap _ _ md = return [
  MirM (Iext "trace_trap" []) md,
  MirM (Ianswer (LImm $ SConst 0)) md] -- TODO

cc_malloc :: IntrinsicImpl m w
cc_malloc [size] (Just dest) md = return [MirM (Iextval "malloc" dest [size]) md]
cc_malloc _ _ _ = progError "bad arguments"

cc_free :: IntrinsicImpl m w
cc_free [ptr] Nothing md = return [MirM (Iext "free" [ptr]) md]
cc_free _ _ _ = progError "bad arguments"

cc_advise_poison :: IntrinsicImpl m w
cc_advise_poison [lo, hi] (Just dest) md = return [MirM (Iextval "advise_poison" dest [lo, hi]) md]
cc_advise_poison _ _ _ = progError "bad arguments"

cc_write_and_poison :: IntrinsicImpl m w
cc_write_and_poison [ptr, val] Nothing md =
  return [MirM (IpoisonW ptr val) md]
cc_write_and_poison _ _ _ = progError "bad arguments"

cc_flag_invalid :: IntrinsicImpl m MWord
cc_flag_invalid [] Nothing md =
  return [MirM (IpoisonW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_invalid _ _ _ = progError "bad arguments"

cc_flag_bug :: IntrinsicImpl m MWord
cc_flag_bug [] Nothing md =
  return [MirM (IstoreW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_bug _ _ _ = progError "bad arguments"

cc_trace :: IntrinsicImpl m w
cc_trace [msg] Nothing md = return [MirM (Iext "tracestr" [msg]) md]
cc_trace _ _ _ = progError "bad arguments"


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
