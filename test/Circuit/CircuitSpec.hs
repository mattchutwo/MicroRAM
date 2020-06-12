module Circuit.CircuitSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


import CircuitIRSpec
import CircuitBuilderSpec


main :: IO ()
main = defaultMain tests
tests = testGroup "Circuit Tests" [circuitEvaluatorTests, buildingBlockTests]

