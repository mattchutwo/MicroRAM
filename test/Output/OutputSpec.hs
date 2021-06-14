{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Output.OutputSpec where


import Test.Tasty
-- import Test.Tasty.Options
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
-- import Test.SmallCheck.Series

import qualified Data.Map as Map

-- import Compiler.Registers
import Compiler.CompilationUnit
import Output.CBORFormat()
import Output.Output

import Codec.Serialise
-- import Codec.CBOR
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)

import MicroRAM
import MicroRAM.MRAMInterpreter

import Segments.Segmenting
import Sparsity.Sparsity

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing serialising and deserialising roundtrip"
        [testProgs, testParams, testTrace, testAdvice, testOutput]

-- * Testing Programs

instance Arbitrary MemWidth where
  arbitrary = oneof [pure W1, pure W2, pure W4, pure W8]
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
    , Icmpe   <$> arbitrary <*> arbitrary <*> arbitrary
    , Icmpa   <$> arbitrary <*> arbitrary <*> arbitrary
    , Icmpae  <$> arbitrary <*> arbitrary <*> arbitrary
    , Icmpg   <$> arbitrary <*> arbitrary <*> arbitrary
    , Icmpge  <$> arbitrary <*> arbitrary <*> arbitrary
    , Imov    <$> arbitrary <*>               arbitrary
    , Icmov   <$> arbitrary <*> arbitrary <*> arbitrary
    , Ijmp    <$>                             arbitrary
    , Icjmp   <$>               arbitrary <*> arbitrary
    , Icnjmp  <$>               arbitrary <*> arbitrary
    , Istore  <$> arbitrary <*> arbitrary <*> arbitrary
    , Iload   <$> arbitrary <*> arbitrary <*> arbitrary
    , Iread   <$>               arbitrary <*> arbitrary
    , Ianswer <$>                             arbitrary
    ]
    

testProgs :: TestTree
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
  arbitrary = CircuitParameters <$> arbitrary <*> arbitrary

testParams :: TestTree
testParams = testProperty "Serialising Parameters" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: CircuitParameters)


-- * Testing Traces
instance Arbitrary (StateOut) where
  arbitrary = StateOut <$> arbitrary <*> arbitrary

testTrace :: TestTree
testTrace = testProperty "Serialising traces" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: [TraceChunkOut Int])



-- * Testing advice 
instance Arbitrary MemOpType where
  arbitrary = oneof $ map return [MOStore, MOLoad]

instance Arbitrary Advice where
  arbitrary = oneof $
    [ MemOp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , return Stutter 
    ]

testAdvice :: TestTree
testAdvice = testProperty "Serialising advice" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: Map.Map Word Advice)
        

-- * Testing Output
instance Arbitrary InitMemSegment where
  arbitrary = InitMemSegment  <$> arbitrary <*> arbitrary <*> arbitrary <*>
              arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary reg => Arbitrary (Segment reg MWord) where
  arbitrary = Segment  <$> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentOut where
  arbitrary = SegmentOut <$> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary reg => Arbitrary (TraceChunkOut reg) where
  arbitrary = TraceChunkOut  <$> arbitrary <*> arbitrary

instance Arbitrary Constraints where
  arbitrary = PcConst <$> arbitrary

instance Arbitrary reg => Arbitrary (Output reg) where
  arbitrary = oneof
    [ SecretOutput  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , PublicOutput  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

testOutput :: TestTree
testOutput = testProperty "Serialising full outputs" $
        \p -> (fromFlatTerm decode $ toFlatTerm $ encode p) == Right (p:: Output Word)
