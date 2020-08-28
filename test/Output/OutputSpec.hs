{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Output.OutputSpec where


import Test.Tasty
import Test.Tasty.Options
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
--import Test.SmallCheck.Series

import qualified Data.Map as Map

import Compiler.Registers
import Compiler.Sparsity
import Compiler.CompilationUnit
import Output.CBORFormat
import Output.Output

import Codec.Serialise
import Codec.CBOR
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)

import MicroRAM
import MicroRAM.MRAMInterpreter


main :: IO ()
main = defaultMain tests

tests = testGroup "Testing serialising and deserialising roundtrip"
        [testProgs, testParams, testTrace, testAdvice, testOutput]

-- * Testing Programs

-- instance automatically derived from generics
instance (Arbitrary a, Arbitrary b) => Arbitrary (Operand a b) where
  arbitrary = oneof [Reg <$> arbitrary, Const <$> arbitrary]
instance (Arbitrary a, Arbitrary b) => Arbitrary (Instruction a b) where
  arbitrary = oneof 
    [ Iand    <$> arbitrary <*> arbitrary <*> arbitrary
    , Ior     <$> arbitrary <*> arbitrary <*> arbitrary
    , Ixor    <$> arbitrary <*> arbitrary <*> arbitrary
    , Inot    <$> arbitrary               <*> arbitrary
    , Iadd    <$> arbitrary <*> arbitrary <*> arbitrary
    , Isub    <$> arbitrary <*> arbitrary <*> arbitrary
    , Imull   <$> arbitrary <*> arbitrary <*> arbitrary
    , Iumulh  <$> arbitrary <*> arbitrary <*> arbitrary
    , Ismulh  <$> arbitrary <*> arbitrary <*> arbitrary
    , Iudiv   <$> arbitrary <*> arbitrary <*> arbitrary
    , Iumod   <$> arbitrary <*> arbitrary <*> arbitrary
    , Ishl    <$> arbitrary <*> arbitrary <*> arbitrary
    , Ishr    <$> arbitrary <*> arbitrary <*> arbitrary
    , Icmpe   <$>               arbitrary <*> arbitrary
    , Icmpa   <$>               arbitrary <*> arbitrary
    , Icmpae  <$>               arbitrary <*> arbitrary
    , Icmpg   <$>               arbitrary <*> arbitrary
    , Icmpge  <$>               arbitrary <*> arbitrary
    , Imov    <$> arbitrary <*>               arbitrary
    , Icmov   <$> arbitrary <*>               arbitrary
    , Ijmp    <$>                             arbitrary
    , Icjmp   <$>                             arbitrary
    , Icnjmp  <$>                             arbitrary
    , Istore  <$>               arbitrary <*> arbitrary
    , Iload   <$>               arbitrary <*> arbitrary
    , Iread   <$>               arbitrary <*> arbitrary
    , Ianswer <$>                             arbitrary
    ]
    

testProgs =
  testProperty "Serialising programs" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p::Instruction Word (Operand Word Word))



-- * Testing Parameters     
instance Arbitrary InstrKind where
  arbitrary = oneof $ map return
    [ Kand     
    , Kor    
    , Kxor   
    , Knot   
    , Kadd   
    , Ksub   
    , Kmull  
    , Kumulh 
    , Ksmulh 
    , Kudiv  
    , Kumod  
    , Kshl   
    , Kshr   
    , Kcmpe  
    , Kcmpa  
    , Kcmpae 
    , Kcmpg  
    , Kcmpge 
    , Kmov   
    , Kcmov  
    , Kjmp   
    , Kcjmp  
    , Kcnjmp 
    , Kstore 
    , Kload  
    , Kread  
    , Kanswer
    , KmemOp 
    , Kalu   
    , Kjumps 
    ]
  
instance Arbitrary CircuitParameters where
  arbitrary = CircuitParameters <$> arbitrary <*> arbitrary <*> arbitrary

testParams = testProperty "Serialising Parameters" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: CircuitParameters)


-- * Testing Traces
instance Arbitrary (StateOut) where
  arbitrary = StateOut <$> arbitrary <*> arbitrary <*> arbitrary

testTrace = testProperty "Serialising traces" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: [StateOut])



-- * Testing advice 
instance Arbitrary MemOpType where
  arbitrary = oneof $ map return [MOStore, MOLoad]

instance Arbitrary Advice where
  arbitrary = oneof $
    [ MemOp <$> arbitrary <*> arbitrary <*> arbitrary
    , return Stutter 
    ]

testAdvice = testProperty "Serialising advice" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: Map.Map Word Advice)
        

-- * Testing Output
instance Arbitrary InitMemSegment where
  arbitrary = InitMemSegment  <$> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary
    

instance Arbitrary reg => Arbitrary (Output reg) where
  arbitrary = oneof
    [ SecretOutput  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , PublicOutput  <$> arbitrary <*> arbitrary <*> arbitrary
    ]

testOutput = testProperty "Serialising full outputs" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: Output Word)
