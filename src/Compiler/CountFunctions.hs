{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : AnalysisCount Functions
Description : Counts the number of times a function is statically called.
Maintainer  : santiago@galois.com
Stability   : experimental


-}
module Compiler.CountFunctions (countFunctions, countFunctionsEmpty) where
import qualified Debug.Trace as T (trace, )

import Compiler.Analysis
import Compiler.Errors
import Compiler.IRs

import Control.Monad.State.Lazy (State, get, modify, execState)

import qualified Data.Map as Map

type FunctionCount = Map.Map String Int

countFunctionsEmpty :: Lprog mdata mreg wrdT -> Hopefully (AnalysisPiece)
countFunctionsEmpty _ = return $ FunctionUsage Map.empty

countFunctions :: Lprog mdata mreg wrdT -> Hopefully (AnalysisPiece)
countFunctions prog = T.trace ("Function name count: " ++ show doFCount) $ return $ FunctionUsage doFCount
    
  where doFCount :: FunctionCount
        doFCount = execState (mapM_ goFunc $ code prog) Map.empty

        goFunc :: LFunction m r w -> State FunctionCount () 
        goFunc fun = mapM_ goBB $ funBody fun

        goBB :: BB name (LTLInstr mreg r w) -> State FunctionCount ()
        goBB (BB _ code1 code2 _) = mapM_ goInstr (code1 ++ code2)

        goInstr :: LTLInstr mreg r w -> State FunctionCount ()
        goInstr instr = case instr of
                  IRI (LCall _ _ (Label lbl) _ _) _md ->
                    addCount lbl
                  _ -> return ()

        addCount :: String -> State FunctionCount ()
        addCount lbl = do
          fCount <- get
          let n = fromMaybe $ Map.lookup lbl fCount
          modify (Map.insert lbl (n + 1))

        fromMaybe :: Maybe Int -> Int
        fromMaybe (Just n) = n
        fromMaybe Nothing  = 0

