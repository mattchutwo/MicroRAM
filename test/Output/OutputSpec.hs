{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Output.OutputSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series


import Compiler.Registers
import Output.CBORFormat

import Codec.Serialise
import Codec.CBOR
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)


import MicroRAM.MicroRAM

instance (Serial m a,Serial m b) => Serial m (Operand a b)
instance (Serial m a,Serial m b) => Serial m (Instruction a b)

main :: IO ()
main = defaultMain tests

tests = testGroup "Testing serialising and deserialising roundtrip"
        [testProgs]

testProgs = testProperty "Serialising programs" $
        \p -> (p::Instruction Word (Operand Word Word)) == p ==>
        (fromFlatTerm decode $ toFlatTerm $ encode p) == Right p
