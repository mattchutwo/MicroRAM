{-# LANGUAGE TypeOperators #-}
{-|
Module      : Register allocation
Description : RTL -> LTL
Maintainer  : 
Stability   : 


Description

-}
module Compiler.RegisterAlloc
    ( registerAlloc, --compileStraight
      trivialRegisterAlloc, -- FIXME : remove when reg alloc completed
    ) where

import qualified MicroRAM.MicroRAM as MRAM


import Compiler.CompileErrors
import Compiler.IRs


registerAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
registerAlloc = undefined







-- * Triviall allocation: we provide a pass that erases the code. Usefull for early testing.
-- FIXME: remove this once registerAlloc is implemented and can be tested!

trivialRegisterAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
trivialRegisterAlloc = return . rtlToLtl

