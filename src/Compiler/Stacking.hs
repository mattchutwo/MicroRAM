{-# LANGUAGE TypeOperators #-}
{-|
Module      : Stacking
Description : LTL -> MARAM
Maintainer  : 
Stability   : 


Description

-}
module Compiler.Stacking
    ( stacking, 
    ) where

import Compiler.CompileErrors
import Compiler.IRs
import qualified MicroRAM.MicroRAM as MRAM  (MAProgram,Program,NamedBlock(..)) 

stacking :: Lprog () Int Word -> Hopefully $ MRAM.MAProgram Int Word
stacking = undefined
