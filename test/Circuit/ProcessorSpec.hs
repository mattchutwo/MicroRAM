module ProcessorSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


import Circuit.CircuitBuilder
import Circuit.CircuitIR


processorTests = testGroup "Testing the construction of the processor" []

{-
-- ** Test 1
-- Calculate (a1 + b1) * (a2, b2)
a = TC ["a1","b1"] [TG "c1" $ Gadd ["a1","b1"]]
b = TC ["a2","b2"] [TG "c2" $ Gadd ["a2","b2"]]
c = TC ["a","b"] [TG "c3" $ Gmul ["a","b"]]

myC1 :: Err (TagCircuit Int (Either String String))
myC1 = do
  ab <- Ok $ a +++ b
  abc <- plugCircuits ab c [Just "c1", Just "c2"]
  return abc

evalC1 a1 b1 a2 b2 = do
  myC' <- myC1
  evalCircuit myC' [a1, b1, a2, b2] [Right "c3"]


test1 :: TestTree
test1 = testProperty "Testing (a1 + b1) * (a2, b2)" $
  \a1 -> (a1 :: Int) >= 0 ==>
  \b1 -> (b1 :: Int) >= 0 ==>
  \a2 -> (a2 :: Int) >= 0 ==>
  \b2 -> (b2 :: Int) >= 0 ==>
  evalC1 a1 b1 a2 b2 == Ok [(a1 + b1) * (a2 + b2)]
-}
