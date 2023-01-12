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
main = undefined -- defaultMain testsWithOptions 

-- -- We can't generate inputs right now, so set test number to 1
-- testsWithOptions :: TestTree
-- testsWithOptions = localOption (QuickCheckTests 1) compilerTests
--   where compilerTests = mkCompilerTests allTests -- oneTest

-- mkCompilerTests :: TestGroupAbs -> TestTree
-- mkCompilerTests tg = case tg of
--   OneTest t -> mkCompilerTest t
--   ManyTests nm ts -> testGroup nm $ mkCompilerTests <$> ts 

-- mkCompilerTest :: TestProgram -> TestTree
-- mkCompilerTest (TestProgram name inputs len cmpErr res hasBug leakTainted) =
--   if cmpErr then compileErrorTest name inputs len else
--   if hasBug then compileBugTest leakTainted name inputs len else
--   compileCorrectTest leakTainted name inputs len res



-- -- # Full compilation tests

-- compileTest
--   :: Executor AReg t -- ^ executes the program and returns some result
--   -> (t -> Bool)     -- ^ tests if result is satisfactory
--   -> TestName
--   -> [Domain]
--   -> Word
--   -> TestTree
-- compileTest executionFunction tester name domains len =
--   testGroup name [monadicTest False, monadicTest True]
--   where monadicTest skipRegAlloc =
--           testProperty (if skipRegAlloc then "Skip RegAlloc" else "Regular") $ 
--           QCM.monadicIO $ do
--           let options = defOptions { skipRegisterAllocation = skipRegAlloc }
--           answer <- QCM.run $ case domains of
--             OneLLVM file -> compileLLVM options file
--             domains -> compileMulti options domains
--           QCM.assert $ tester answer

--         compileLLVM options file = do
--           llvmProg <- llvmParse file
--           mramProg <- handleErrorWith $ compile options len llvmProg
--           return $ executionFunction (fmap (tripleFmap fst) mramProg)

--         compileMulti options domains = do
--           domains' <- mapM (loadCode llvmParse readFile) domains
--           mramProg <- handleErrorWith $ compileDomains options len domains'
--           return $ executionFunction (fmap (tripleFmap fst) mramProg)

-- tripleFmap :: (Functor f1, Functor f2, Functor f3) =>
--      (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
-- tripleFmap f compRes =  fmap (fmap (fmap f)) compRes 

                            
-- compileErrorTest
--   :: TestName
--   -> [Domain]
--   -> Word
--   -> TestTree
-- compileErrorTest name (OneLLVM file) len =
--   testProperty name $ 
--   QCM.monadicIO $ do
--   compResult <- QCM.run $ compileFromFile file len
--   QCM.assert $ isLeft compResult
--   where compileFromFile file len = do
--           llvmProg <- llvmParse file
--           return $ compile defOptions len llvmProg
-- compileErrorTest _ _ _ = testGroup "multi-file and non-LLVM tests are ignored" []

-- -- ## Full compilation tests of correctness

-- -- | compileCorrectTest : compile step by step llvm code from file:

-- type AssertionInfo = IO String

-- compileCorrectTest ::
--      Bool
--   -> String
--   -> [Domain]
--   -> Word -- ^ Length
--   -> MWord  -- ^ return value
--   -> TestTree
-- compileCorrectTest leakTainted name inputs len ret =
--   compileTest (execAnswer False leakTainted) (== ret) name inputs len

-- -- ## Full compilation tests looking for bugs

-- -- | compileCorrectTest : compile step by step llvm code from file:

-- compileBugTest ::
--      Bool
--   -> String
--   -> [Domain]
--   -> Word -- ^ Length
--   -> TestTree
-- compileBugTest leakTainted name inputs len =
--   compileTest (execBug False leakTainted) id name inputs len




