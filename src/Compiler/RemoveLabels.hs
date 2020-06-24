{-# LANGUAGE TypeOperators #-}
{-|
Module      : Removing Labels
Description : Replaces labels with concrete instruction numbers : MARAM -> MRAM
Maintainer  : 
Stability   : 


Description

-}
module Compiler.RemoveLabels
    ( removeLabels, 
    ) where

import Compiler.CompileErrors
import Compiler.IRs
import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 

removeLabels :: MRAM.MAProgram Int Word -> Hopefully $ MRAM.Program Int Word
removeLabels = undefined
