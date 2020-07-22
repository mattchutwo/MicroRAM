{-# LANGUAGE FlexibleContexts #-}
import LLVM.AST
import LLVM.Internal.Operand

import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI



import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.InlineAssembly ()
import LLVM.Internal.Metadata (getByteStringFromFFI)

import qualified LLVM.AST as A hiding (GlobalVariable, Module, PointerType, type')
import qualified LLVM.AST.Operand as A

import LLVM.Internal.FFI.LLVMCTypes (mdSubclassIdP)

a diN = FFI.getMetadataClassId (FFI.upCast diN)
