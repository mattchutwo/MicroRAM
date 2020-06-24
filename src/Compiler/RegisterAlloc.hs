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
    ) where

import Compiler.CompileErrors
import Compiler.IRs

registerAlloc :: Rprog () Word -> Hopefully $ Lprog () Int Word
registerAlloc = undefined
