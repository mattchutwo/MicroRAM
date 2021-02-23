{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec where

import MicroRAM.MRAMInterpreter
import MicroRAM (MWord)

-- Compiler imports
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
  where compilerTests = mkCompilerTests allTests

mkCompilerTests :: TestGroup -> TestTree
mkCompilerTests tg = case tg of
  OneTest t -> mkCompilerTest t
  ManyTests nm ts -> testGroup nm $ mkCompilerTests <$> ts 

mkCompilerTest :: TestProgram -> TestTree
mkCompilerTest (TestProgram name file len res hasBug) =
  if hasBug then compileBugTest name file len else compileCorrectTest name file len res



-- # Full compilation tests

compileTest
  :: Executor AReg t -- ^ executes the program and returns some result
  -> (t -> Bool)     -- ^ tests if result is satisfactory
  -> TestName
  -> FilePath
  -> Word
  -> TestTree
compileTest executionFunction tester name file len = 
  testProperty name $ 
  QCM.monadicIO $ do
  --QCM.run $ putStrLn "Here"
  answer <- QCM.run $ compileTest' file len  False
  --QCM.run $ putStrLn "Here 2" 
  QCM.assert $ tester answer
  where {-compileTest' ::
          FilePath
          -> Word -- ^ Length
          -> Bool -- ^ verbose
          -> IO MWord -- TestTree-}
        compileTest' file len _verb = do
          llvmProg <- llvmParse file
          mramProg <- handleErrorWith $ compile len llvmProg Nothing 
          return $ executionFunction mramProg
  

-- ## Full compilation tests of correctness

-- | compileCorrectTest : compile step by step llvm code from file:

type AssertionInfo = IO String

compileCorrectTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> MWord  -- ^ return value
  -> TestTree
compileCorrectTest name file len ret =
  compileTest (execAnswer False) (== ret) name file len

-- ## Full compilation tests looking for bugs

-- | compileCorrectTest : compile step by step llvm code from file:

compileBugTest ::
  String
  -> FilePath
  -> Word -- ^ Length
  -> TestTree
compileBugTest name file len = 
  compileTest (execBug False) id name file len



