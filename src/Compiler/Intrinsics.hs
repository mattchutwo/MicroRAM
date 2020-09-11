{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Intrinsic lowering
Description : Converts calls to intrinsics to MicroASM instructions
Maintainer  : santiago@galois.com
Stability   : prototype
-}

module Compiler.Intrinsics
    ( lowerIntrinsics,
    ) where


import qualified Data.Map as Map
import Data.Map (Map)

import Compiler.Errors
import Compiler.IRs

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

type IntrinsicImpl m w = [MAOperand VReg w] -> Maybe VReg -> Hopefully [MIRInstr m w]

expandIntrinsicCall :: Show w => Map String (IntrinsicImpl m w) -> MIRInstr m w -> Hopefully [MIRInstr m w]
expandIntrinsicCall intrinMap (MirI (RCall _ dest (Label name) _ args) _meta)
  | Just impl <- Map.lookup name intrinMap =
    tag ("bad call to intrinsic " ++ name) $ impl args dest
expandIntrinsicCall _ instr = return [instr]


cc_test_add :: IntrinsicImpl () w
cc_test_add [x, y] (Just dest) = return [MirM (Iadd dest x y) ()]
cc_test_add _ _ = progError "bad arguments"

cc_noop :: IntrinsicImpl () w
cc_noop _ _ = return []

intrinsics :: Map String (IntrinsicImpl () MWord)
intrinsics = Map.fromList $ map (\(x, y) -> ("Name " ++ show x, y)) $
  [ ("__cc_test_add", cc_test_add)
  , ("__cc_valid_if", cc_noop)  -- TODO
  , ("__cc_bug_if", cc_noop)  -- TODO
  ]

lowerIntrinsics :: MIRprog () MWord -> Hopefully (MIRprog () MWord)
lowerIntrinsics prog = expandInstrs (expandIntrinsicCall intrinsics) prog
  
