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

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text


import Compiler.Common (Name(..), short2string)
import Compiler.Metadata
import Compiler.Errors
import Compiler.IRs
import Compiler.LazyConstants

import MicroRAM

type IntrinsicImpl m w = [MAOperand VReg w] -> Maybe VReg -> m -> Hopefully [MIRInstr m w]

cc_test_add :: IntrinsicImpl m w
cc_test_add [x, y] (Just dest) md = return [MirM (Iadd dest x y) md]
cc_test_add _ _ _ = progError "bad arguments"

cc_noop :: IntrinsicImpl m w
cc_noop _ _ _ = return []

cc_trap :: Text -> IntrinsicImpl m MWord
cc_trap desc _ _ md = return [
  MirM (Iext (XTrace ("Trap: " <> desc) [])) md,
  MirM (Ianswer (LImm $ SConst 0)) md] -- TODO

cc_malloc :: IntrinsicImpl m w
cc_malloc [size] (Just dest) md = return [MirM (Iextadvise dest (XMalloc size)) md]
cc_malloc _ _ _ = progError "bad arguments"

cc_access_valid :: IntrinsicImpl m w
cc_access_valid [lo, hi] Nothing md = return [MirM (Iext (XAccessValid lo hi)) md]
cc_access_valid _ _ _ = progError "bad arguments"

cc_access_invalid :: IntrinsicImpl m w
cc_access_invalid [lo, hi] Nothing md = return [MirM (Iext (XAccessInvalid lo hi)) md]
cc_access_invalid _ _ _ = progError "bad arguments"

cc_advise_poison :: IntrinsicImpl m w
cc_advise_poison [lo, hi] (Just dest) md =
  return [MirM (Iextadvise dest (XAdvisePoison lo hi)) md]
cc_advise_poison _ _ _ = progError "bad arguments"

cc_write_and_poison :: IntrinsicImpl m w
cc_write_and_poison [ptr, val] Nothing md =
  return [MirM (IpoisonW ptr val) md]
cc_write_and_poison _ _ _ = progError "bad arguments"

cc_read_unchecked :: IntrinsicImpl m w
cc_read_unchecked [ptr] (Just dest) md =
  return [MirM (Iextval dest (XLoadUnchecked ptr)) md]
cc_read_unchecked _ _ _ = progError "bad arguments"

cc_write_unchecked :: IntrinsicImpl m w
cc_write_unchecked [ptr, val] Nothing md =
  return [MirM (Iext (XStoreUnchecked ptr val)) md]
cc_write_unchecked _ _ _ = progError "bad arguments"

cc_flag_invalid :: IntrinsicImpl m MWord
cc_flag_invalid [] Nothing md =
  return [
    MirM (Iext (XTrace "__cc_flag_invalid" [])) md,
    MirM (IpoisonW zero zero) md ]
  where zero = LImm $ SConst 0
cc_flag_invalid _ _ _ = progError "bad arguments"

cc_flag_bug :: IntrinsicImpl m MWord
cc_flag_bug [] Nothing md =
  return [MirM (IstoreW zero zero) md]
  where zero = LImm $ SConst 0
cc_flag_bug _ _ _ = progError "bad arguments"

cc_trace :: IntrinsicImpl m w
cc_trace [msg] Nothing md = return [MirM (Iext (XTraceStr msg)) md]
cc_trace _ _ _ = progError "bad arguments"

cc_trace_exec :: IntrinsicImpl m w
cc_trace_exec (name : args) Nothing md =
  return [MirM (Iext (XTraceExec name args)) md]
cc_trace_exec _ _ _ = progError "bad arguments"


intrinsics :: Map ShortByteString (IntrinsicImpl m MWord)
intrinsics = Map.fromList $ map (\(x :: ShortByteString, y) -> (x, y)) $ intrinsicsList

intrinsicsList :: [(ShortByteString, IntrinsicImpl m MWord)]
intrinsicsList =
  [ ("__cc_test_add", cc_test_add)
  , ("__cc_flag_invalid", cc_flag_invalid)
  , ("__cc_flag_bug", cc_flag_bug)

  , ("__cc_malloc", cc_malloc)
  , ("__cc_access_valid", cc_access_valid)
  , ("__cc_access_invalid", cc_access_invalid)
  , ("__cc_advise_poison", cc_advise_poison)
  , ("__cc_write_and_poison", cc_write_and_poison)
  , ("__cc_read_unchecked", cc_read_unchecked)
  , ("__cc_write_unchecked", cc_write_unchecked)

  , ("__cc_trace", cc_trace)
  , ("__cc_trace_exec", cc_trace_exec)

  , ("llvm.lifetime.start.p0i8", cc_noop)
  , ("llvm.lifetime.end.p0i8", cc_noop)

  -- Exception handling
  , mkTrap "__gxx_personality_v0"
  , mkTrap "__cxa_allocate_exception"
  , mkTrap "__cxa_throw"
  , mkTrap "__cxa_begin_catch"
  , mkTrap "__cxa_end_catch"
  , mkTrap "llvm.eh.typeid.for"

  -- Explicit trap
  , mkTrap "__cxa_pure_virtual"
  , mkTrap "llvm.trap"
  , mkTrap "_ZSt9terminatev"

  -- Floating-point ops
  , mkTrap "llvm.ceil.f64"
  , mkTrap "llvm.copysign.f64"
  , mkTrap "llvm.exp2.f64"
  , mkTrap "llvm.exp.f64"
  , mkTrap "llvm.fabs.f64"
  , mkTrap "llvm.floor.f64"
  , mkTrap "llvm.log.f64"
  , mkTrap "llvm.pow.f64"
  , mkTrap "llvm.sqrt.f64"
  , mkTrap "llvm.trunc.f64"
  , mkTrap "llvm.llrint.i64.f64"

  -- Varargs
  , mkTrap "llvm.va_start"
  , mkTrap "llvm.va_end"
  ]
  where
    mkTrap :: ShortByteString -> (ShortByteString, IntrinsicImpl m MWord)
    mkTrap name = (name, cc_trap $ Text.pack $ short2string name)

-- | Inlines the code for intrinsic funcitons.
-- It uses the debugName to search for the function,
-- so it expectds the names to be reserved (not enforced right now). 
expandIntrinsicCall :: forall m w. Show w => Map ShortByteString (IntrinsicImpl m w) -> MIRInstr m w -> Hopefully [MIRInstr m w]
expandIntrinsicCall intrinMap (MirI (RCall _ dest (Label (Name _ debugName)) _ args) meta)
  | Just impl <- Map.lookup debugName intrinMap = -- ^ uses the debugName 
    tag ("bad call to intrinsic " ++ show debugName) $ impl args dest meta
expandIntrinsicCall _ instr = return [instr]

expandInstrs :: forall f m w.
  Applicative f => (MIRInstr m w -> f [MIRInstr m w]) -> MIRprog m w -> f (MIRprog m w)
expandInstrs f = goProg
  -- TODO: this can probably be done more cleanly with traverse or lenses
  where goProg :: MIRprog m w -> f (MIRprog m w)
        goProg (IRprog te gs code) = IRprog te gs <$> traverse goFunc code

        goFunc :: MIRFunction m w -> f (MIRFunction m w)
        goFunc (Function nm rty atys anms bbs nr) =
          Function nm rty atys anms <$> traverse goBB bbs <*> pure nr

        goBB :: BB n (MIRInstr m w) -> f (BB n (MIRInstr m w))
        goBB (BB nm body term dag) = BB nm <$> goInstrs body <*> goInstrs term <*> pure dag

        goInstrs :: [MIRInstr m w] -> f [MIRInstr m w]
        goInstrs is = concat <$> traverse f is

lowerIntrinsics :: forall m. MIRprog m MWord -> Hopefully (MIRprog m MWord)
lowerIntrinsics = expandInstrs (expandIntrinsicCall intrinsics)
                  >=> removeIntrinsics

-- | removes the intrinsics declarations which have been inlined and are not needed anymore.
-- This overlaps with dead code elimination (a bit), but enables checking for undefined functions
removeIntrinsics :: MIRprog m MWord -> Hopefully (MIRprog m MWord)
removeIntrinsics prog = 
  return $ prog {code = filter (not . isIntrinsic) $ code prog}
  where isIntrinsic f = debugName (funcName f) `Map.member` intrinsics
        debugName (Name _ dName) = dName
-- | Rename C/LLVM implementations of LLVM intrinsics to line up with their
-- intrinsic name.
--
-- The problem this solves is that we can't directly define a function with a
-- name like `llvm.memset.p0i8.i64` in C.  Instead, we define a function name
-- `__llvm__memset__p0i8__i64`, then this pass renames it to the dotted form.
-- (It also renames the empty definition of the dotted form to `orig.llvm.foo`,
-- to avoid conflicts later on.)
renameLLVMIntrinsicImpls :: MIRprog Metadata MWord -> Hopefully (MIRprog Metadata MWord)
renameLLVMIntrinsicImpls (IRprog te gs code) = return $ IRprog te gs code'
  where
    renameList :: [(ShortByteString, ShortByteString)]
    renameList = do
      Function nm _ _ _ _ _ <- code
      Name _ ss <- return nm
      Just name <- return $ Text.stripPrefix "__llvm__" $ toText ss
      return (ss, fromText $ "llvm." <> Text.replace "__" "." name) -- ^ Doesn't change the Word

    renameMap = Map.fromList renameList

    -- | Remove all functions with no body and
    -- build a map from remembering all those Names.
    codeNoEmptyFuncs = fst noEmptyFuncs
    mapNoEmptyFuncs = snd noEmptyFuncs
    noEmptyFuncs :: ([MIRFunction Metadata MWord], Map.Map ShortByteString Name)
    noEmptyFuncs = foldr go ([], Map.empty) code
      where go f@(Function nm _ _ _ bbs _) (funcs, mapNE) =
              if null bbs then (funcs, Map.insert (dbName nm) nm mapNE) else (f:funcs, mapNE)

    code' :: [MIRFunction Metadata MWord]
    code' = do
      Function nm rty atys anms bbs nr <- codeNoEmptyFuncs
      let replaceName = Function (changeName nm) rty atys anms bbs nr
      return $ mapMetadataMIRFunction changeMetadata replaceName
    
      
    changeString name = maybe name id $ Map.lookup name renameMap
    changeName :: Name -> Name
    changeName name@(Name n dbnm) =
      let dbName' =  (changeString dbnm) in
        if dbnm == dbName' then name else 
          case Map.lookup dbName' mapNoEmptyFuncs of
            Just name' -> name'
            Nothing -> Name n dbName' -- ^ This function had no implementation so it's probably not called. 

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

