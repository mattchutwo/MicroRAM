{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set (member, fromList) 
import           Data.Text (Text)
import qualified Data.Text as Text


import Compiler.Common (Name(..), Ty(..), tySize, short2string, WithNextReg, newLocalName, va_startName)
import Compiler.Metadata
import Compiler.Errors
import Compiler.IRs
import Compiler.LazyConstants

import MicroRAM

-- Metadata needed to expand intrinsics for function calls.
data CallingContextMetadata = CCM {
    _argTys :: [Ty] -- The list of nonvariadic argument types for a function. This is needed to compute the offset for variadic arguments on the stack.
  }

expandInstrs :: forall f m w.
  Monad f => (CallingContextMetadata -> MIRInstr m w -> WithNextReg f [MIRInstr m w]) -> MIRprog m w -> WithNextReg f (MIRprog m w)
expandInstrs f = goProg
  -- TODO: this can probably be done more cleanly with traverse or lenses
  where goProg :: MIRprog m w -> WithNextReg f (MIRprog m w)
        goProg (IRprog te gs code) = IRprog te gs <$> traverse goFunc code

        goFunc :: MIRFunction m w -> WithNextReg f (MIRFunction m w)
        goFunc (Function nm rty atys anms bbs) =
          Function nm rty atys anms <$> traverse (goBB $ CCM atys) bbs

        goBB :: CallingContextMetadata -> BB n (MIRInstr m w) -> WithNextReg f (BB n (MIRInstr m w))
        goBB ccm (BB nm body term dag) = BB nm <$> goInstrs ccm body <*> goInstrs ccm term <*> pure dag

        goInstrs :: CallingContextMetadata -> [MIRInstr m w] -> WithNextReg f [MIRInstr m w]
        goInstrs ccm is = concat <$> traverse (f ccm) is


getNextRegister :: Monad m => WithNextReg m VReg
getNextRegister = newLocalName "_intrinsic"

type IntrinsicImpl m w = [MAOperand VReg w] -> Maybe VReg -> m -> CallingContextMetadata -> WithNextReg Hopefully [MIRInstr m w]

expandIntrinsicCall :: forall m w. Show w => Map ShortByteString (IntrinsicImpl m w) -> CallingContextMetadata -> MIRInstr m w -> WithNextReg Hopefully [MIRInstr m w]
expandIntrinsicCall intrinMap ccm (MirI (RCall _ dest (Label (Name _ debugName)) _ args) meta)
  | Just impl <- Map.lookup debugName intrinMap = -- ^ uses the debugName 
    tagState ("bad call to intrinsic " ++ show debugName) $ impl args dest meta ccm
expandIntrinsicCall _ _ instr = return [instr]

cc_test_add :: IntrinsicImpl m w
cc_test_add [x, y] (Just dest) md _ = return [MirM (Iadd dest x y) md]
cc_test_add _ _ _ _ = progError "bad arguments"

cc_noop :: IntrinsicImpl m w
cc_noop _ _ _ _ = return []

cc_trap :: Text -> IntrinsicImpl m MWord
cc_trap desc _ _ md _ = return [
  MirM (Iext (XTrace ("Trap: " <> desc) [])) md,
  MirM (Ianswer (LImm $ SConst 0)) md] -- TODO

cc_malloc :: IntrinsicImpl m w
cc_malloc [size] (Just dest) md _ = return [MirM (Iextadvise dest (XMalloc size)) md]
cc_malloc _ _ _ _ = progError "bad arguments"

cc_access_valid :: IntrinsicImpl m w
cc_access_valid [lo, hi] Nothing md _ = return [MirM (Iext (XAccessValid lo hi)) md]
cc_access_valid _ _ _ _ = progError "bad arguments"

cc_access_invalid :: IntrinsicImpl m w
cc_access_invalid [lo, hi] Nothing md _ = return [MirM (Iext (XAccessInvalid lo hi)) md]
cc_access_invalid _ _ _ _ = progError "bad arguments"

cc_advise_poison :: IntrinsicImpl m w
cc_advise_poison [lo, hi] (Just dest) md _ =
  return [MirM (Iextadvise dest (XAdvisePoison lo hi)) md]
cc_advise_poison _ _ _ _ = progError "bad arguments"

cc_write_and_poison :: IntrinsicImpl m w
cc_write_and_poison [ptr, val] Nothing md _ =
  return [MirM (IpoisonW ptr val) md]
cc_write_and_poison _ _ _ _ = progError "bad arguments"

cc_read_unchecked :: IntrinsicImpl m w
cc_read_unchecked [ptr] (Just dest) md _ =
  return [MirM (Iextval dest (XLoadUnchecked ptr)) md]
cc_read_unchecked _ _ _ _ = progError "bad arguments"

cc_write_unchecked :: IntrinsicImpl m w
cc_write_unchecked [ptr, val] Nothing md _ =
  return [MirM (Iext (XStoreUnchecked ptr val)) md]
cc_write_unchecked _ _ _ _ = progError "bad arguments"

cc_flag_invalid :: IntrinsicImpl m MWord
cc_flag_invalid [] Nothing md _ =
  return [
    MirM (Iext (XTrace "@__cc_flag_invalid" [])) md,
    MirM (IpoisonW zero zero) md ]
  where zero = LImm $ SConst 0
cc_flag_invalid _ _ _ _ = progError "bad arguments"

cc_flag_bug :: IntrinsicImpl m MWord
cc_flag_bug [] Nothing md _ =
  return [MirM (IstoreW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_bug _ _ _ _ = progError "bad arguments"

noniSetLabel :: MemWidth -> IntrinsicImpl m MWord
noniSetLabel wd [ptr, label] Nothing md _ = do
  r <- getNextRegister
  return $ map (\i -> MirM i md) [
      Iload wd r ptr
    , Itaint wd r label
    , Istore wd ptr (AReg r)
    ]
noniSetLabel _ _ _ _ _ = progError "bad arguments"

noniSink :: MemWidth -> IntrinsicImpl m MWord
noniSink wd [ptr, label] Nothing md _ = do
  r <- getNextRegister
  return $ map (\i -> MirM i md) [
      Iload wd r ptr
    , Isink wd (AReg r) label
    ]
  -- return $ [MirM (Isink ptr label) ()]
  -- error $ show ptr <> " " <> show label
noniSink _ _ _ _ _ = progError "bad arguments"

cc_trace :: IntrinsicImpl m w
cc_trace [msg] Nothing md _ = return [MirM (Iext (XTraceStr msg)) md]
cc_trace _ _ _ _ = progError "bad arguments"

cc_trace_exec :: IntrinsicImpl m w
cc_trace_exec (name : args) Nothing md _ =
  return [MirM (Iext (XTraceExec name args)) md]
cc_trace_exec _ _ _ _ = progError "bad arguments"

va_start :: IntrinsicImpl m MWord
va_start [ptr] Nothing md (CCM argTys) = do
  reg <- getNextRegister
  return [
      MirI (RGetBP reg) md
    , MirI (RCall TVoid Nothing (Label va_startName) [Tptr, Tptr, Tint] [ptr, AReg reg, offset]) md
    ]
  where
    -- Skip return address and nonvariadic arguments.
    offset = LImm $ SConst $ fromIntegral wordBytes * (2 + (sum $ map tySize argTys))
va_start _ _ _ _ = progError "bad arguments"

intrinsics :: Map ShortByteString (IntrinsicImpl m MWord)
intrinsics = Map.fromList $ map (\(x :: ShortByteString, y) -> (x, y)) $ intrinsicsList

intrinsicsList :: [(ShortByteString, IntrinsicImpl m MWord)]
intrinsicsList =
  [ ("@__cc_test_add", cc_test_add)
  , ("@__cc_flag_invalid", cc_flag_invalid)
  , ("@__cc_flag_bug", cc_flag_bug)

  , ("@__cc_malloc", cc_malloc)
  , ("@__cc_access_valid", cc_access_valid)
  , ("@__cc_access_invalid", cc_access_invalid)
  , ("@__cc_advise_poison", cc_advise_poison)
  , ("@__cc_write_and_poison", cc_write_and_poison)
  , ("@__cc_read_unchecked", cc_read_unchecked)
  , ("@__cc_write_unchecked", cc_write_unchecked)

  , ("@__cc_trace", cc_trace)
  , ("@__cc_trace_exec", cc_trace_exec)

  , ("@llvm.lifetime.start.p0i8", cc_noop)
  , ("@llvm.lifetime.end.p0i8", cc_noop)

  -- Varargs
  , ("@llvm.va_start", va_start)
  , ("@llvm.va_end", cc_noop)

  -- Exception handling
  , mkTrap "@__gxx_personality_v0"
  , mkTrap "@__cxa_allocate_exception"
  , mkTrap "@__cxa_throw"
  , mkTrap "@__cxa_begin_catch"
  , mkTrap "@__cxa_end_catch"
  , mkTrap "@llvm.eh.typeid.for"

  -- Dynamic taint tracking
  , ("@noniSetLabelU8", noniSetLabel W1) -- TODO: Is there a better way to get the width?
  , ("@noniSetLabelI8", noniSetLabel W1)
  , ("@noniSinkU8", noniSink W1)
  , ("@noniSinkI8", noniSink W1)
  , ("@noniSetLabelU16", noniSetLabel W2)
  , ("@noniSetLabelI16", noniSetLabel W2)
  , ("@noniSinkU16", noniSink W2)
  , ("@noniSinkI16", noniSink W2)
  , ("@noniSetLabelU32", noniSetLabel W4)
  , ("@noniSetLabelI32", noniSetLabel W4)
  , ("@noniSinkU32", noniSink W4)
  , ("@noniSinkI32", noniSink W4)
  , ("@noniSetLabelU64", noniSetLabel W8)
  , ("@noniSetLabelI64", noniSetLabel W8)
  , ("@noniSinkU64", noniSink W8)
  , ("@noniSinkI64", noniSink W8)

  -- Explicit trap
  , mkTrap "@__cxa_pure_virtual"
  , mkTrap "@llvm.trap"
  , mkTrap "@_ZSt9terminatev"

  -- Floating-point ops
  , mkTrap "@llvm.ceil.f64"
  , mkTrap "@llvm.copysign.f64"
  , mkTrap "@llvm.exp2.f64"
  , mkTrap "@llvm.exp.f64"
  , mkTrap "@llvm.fabs.f64"
  , mkTrap "@llvm.floor.f64"
  , mkTrap "@llvm.log.f64"
  , mkTrap "@llvm.pow.f64"
  , mkTrap "@llvm.sqrt.f64"
  , mkTrap "@llvm.trunc.f64"
  , mkTrap "@llvm.llrint.i64.f64"
  ]
  where
    mkTrap :: ShortByteString -> (ShortByteString, IntrinsicImpl m MWord)
    mkTrap name = (name, cc_trap $ Text.pack $ short2string name)


lowerIntrinsics :: forall m. MIRprog m MWord -> WithNextReg Hopefully (MIRprog m MWord)
lowerIntrinsics = expandInstrs (expandIntrinsicCall intrinsics)
                  >=> removeIntrinsics

-- | removes the intrinsics declarations which have been inlined and are not needed anymore.
-- This overlaps with dead code elimination (a bit), but enables checking for undefined functions
removeIntrinsics :: MIRprog m MWord -> WithNextReg Hopefully (MIRprog m MWord)
removeIntrinsics prog = 
  return $ prog {code = filter (not . isIntrinsic) $ code prog}
  where isIntrinsic f = dbName (funcName f) `Map.member` intrinsics
        
-- | Rename C/LLVM implementations of LLVM intrinsics to line up with their
-- intrinsic name.
--
-- The problem this solves is that we can't directly define a function with a
-- name like `@llvm.memset.p0i8.i64` in C.  Instead, we define a function name
-- `@__llvm__memset__p0i8__i64`, then this pass renames it to the dotted form.
-- It also removes the empty definition of the dotted form, to avoid conflicts later on.
renameLLVMIntrinsicImpls :: MIRprog Metadata MWord -> Hopefully (MIRprog Metadata MWord)
renameLLVMIntrinsicImpls (IRprog te gs code) = return $ IRprog te gs code'
  where
    renameList :: [(ShortByteString, ShortByteString)]
    renameList = do
      Function nm _ _ _ _ <- code
      Name _ ss <- return nm
      Just name <- return $ Text.stripPrefix "@__llvm__" $ toText ss
      return (ss, fromText $ "@llvm." <> Text.replace "__" "." name) -- ^ Doesn't change the Word

    renameMap = Map.fromList renameList
    removeSet = Set.fromList $ map snd renameList

    -- | If the code calls `@llvm.memset.p0i8.i64` it will actually call
    -- a dummy, empty function called `Name n "@llvm.memset.p0i8.i64"`.
    -- When we rename `@__llvm__memset__p0i8__i64` we need to know the
    -- right Word `n`.
    -- So, for every empty function named `Name n "emptyFoo"`
    -- we create the Map `"emptyFoo" -> Name n "emptyFoo"` 
    emptyFuncMap :: Map.Map ShortByteString Name
    emptyFuncMap = foldr go Map.empty code
      where go (Function nm _ _ _ bbs) mapNE =
              if null bbs then Map.insert (dbName nm) nm mapNE else mapNE

    -- | Renames all the function of the form `@__LLVM__foo`
    -- into the dotted form and removes the original, empty,
    -- function with the same dotted name.
    code' :: [MIRFunction Metadata MWord]
    code' = do
      Function nm rty atys anms bbs <- code
      -- Remove functions in the remove set (i.e. dummy version of the function)
      guard $ not $ Set.member (dbName nm) removeSet
      let replaceName = Function (changeName nm) rty atys anms bbs
      return $ mapMetadataMIRFunction changeMetadata replaceName
    
    -- | Changes the underscored ShortByteString for the dotted form
    changeString name = maybe name id $ Map.lookup name renameMap

    -- | Replaces names with underscore string for
    -- the version with the dotted string. Notice that,
    -- it also has to find the same Word `n` from the
    -- dummy (empty) version fo the funciton (see `emptyFuncMap`).
    changeName :: Name -> Name
    changeName name@(Name _ dbnm) =
      let dbName' =  changeString dbnm in
        if dbnm == dbName' then name else 
          case Map.lookup dbName' emptyFuncMap of
            Just name' -> name'
            Nothing -> name -- ^ The dotted form had no dummy definition, so it's probaly never called in the code. 

    changeMetadata :: Metadata -> Metadata
    changeMetadata md = md {mdFunction = changeName $ mdFunction md}

    fromText t = Short.toShort $ BSU.fromString $ Text.unpack t
    toText s = Text.pack $ BSU.toString $ Short.fromShort s

mapMetadataMIRFunction ::  (md1 -> md2) ->  MIRFunction md1 wrdT -> MIRFunction md2 wrdT
mapMetadataMIRFunction mdF fn =
  fn {funcBlocks = map mapMetadataBlocks (funcBlocks fn)}
  where mapMetadataBlocks (BB nm instrs1 instrs2 dg) =
          BB nm (map mapMetadataInstr instrs1) (map mapMetadataInstr instrs2) dg

        mapMetadataInstr (MirM inst md) = MirM inst $ mdF md
        mapMetadataInstr (MirI inst md) = MirI inst $ mdF md

