{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec where

import MicroRAM.MRAMInterpreter
import MicroRAM (MWord)

import Data.Either (isLeft)

import Compiler
import Compiler.Errors

import LLVMutil.LLVMIO
import Test.Tasty

import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

import Programs.Programs

main :: IO ()
main = defaultMain testsWithOptions 

-- We can't generate inputs right now, so set test number to 1
testsWithOptions :: TestTree
testsWithOptions = localOption (QuickCheckTests 1) compilerTests
  where compilerTests = mkCompilerTests allTests -- oneTest

mkCompilerTests :: TestGroupAbs -> TestTree
mkCompilerTests tg = case tg of
  OneTest t -> mkCompilerTest t
  ManyTests nm ts -> testGroup nm $ mkCompilerTests <$> ts 

mkCompilerTest :: TestProgram -> TestTree
mkCompilerTest (TestProgram name file len cmpErr res hasBug leakTainted) =
  if cmpErr then compileErrorTest name file len else 
  if hasBug then compileBugTest leakTainted name file len else compileCorrectTest leakTainted name file len res



-- # Full compilation tests

compileTest
  :: Executor AReg t -- ^ executes the program and returns some result
  -> (t -> Bool)     -- ^ tests if result is satisfactory
  -> TestName
  -> FilePath
  -> Word
  -> TestTree
compileTest executionFunction tester name file len = 
  testGroup name [monadicTest False, monadicTest True]
  where monadicTest skipRegAlloc =
          testProperty (if skipRegAlloc then "Skip RegAlloc" else "Regular") $ 
          QCM.monadicIO $ do
          answer <- QCM.run $ compileTest' file len skipRegAlloc
          QCM.assert $ tester answer
          
        compileTest' file len skipRegAlloc = do
          llvmProg <- llvmParse file
          mramProg <- handleErrorWith $ compile False False False skipRegAlloc len llvmProg Nothing
          return $ executionFunction (fmap (tripleFmap fst) mramProg)

tripleFmap :: (Functor f1, Functor f2, Functor f3) =>
     (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
tripleFmap f compRes =  fmap (fmap (fmap f)) compRes 

                            
compileErrorTest
  :: TestName
  -> FilePath
  -> Word
  -> TestTree
compileErrorTest name file len = 
  testProperty name $ 
  QCM.monadicIO $ do
  compResult <- QCM.run $ compileFromFile file len
  QCM.assert $ isLeft compResult
  where compileFromFile file len = do
          llvmProg <- llvmParse file
          return $ compile False False False False len llvmProg Nothing
          
-- ## Full compilation tests of correctness

-- | compileCorrectTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileCorrectTest ::
     Bool
  -> String
  -> FilePath
  -> Word -- ^ Length
  -> MWord  -- ^ return value
  -> TestTree
compileCorrectTest leakTainted name file len ret =
  compileTest (execAnswer False leakTainted) (== ret) name file len

-- ## Full compilation tests looking for bugs

-- | compileCorrectTest : compile step by step llvm code from file:

compileBugTest ::
     Bool
  -> String
  -> FilePath
  -> Word -- ^ Length
  -> TestTree
compileBugTest leakTainted name file len = 
  compileTest (execBug False leakTainted) id name file len



