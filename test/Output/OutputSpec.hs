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


main :: IO ()
main = defaultMain tests

tests = testGroup "Testing serialising and deserialising roundtrip"
        [testProgs, testTrace]

-- * Testing Programs

-- instance automatically derived from generics
instance (Serial m a,Serial m b) => Serial m (Operand a b)
instance (Serial m a,Serial m b) => Serial m (Instruction a b)

testProgs = testProperty "Serialising programs" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p::Instruction Word (Operand Word Word))

-- * Testing Programs
instance Monad m => Serial m (StateOut)

testTrace = testProperty "Serialising traces" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: StateOut)

