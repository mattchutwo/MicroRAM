{-# LANGUAGE CPP #-}
{-|
Module      : LLVMIO
Description : read llvm file
Maintainer  : santiago@galois.com
Stability   : prototype


-}
module LLVMutil.LLVMIO where

#if ! NO_LLVM
import LLVM.Module
import LLVM.Context
import qualified LLVM.AST as LLVM 

-- | Read LLVM file
llvmParse :: FilePath -> IO LLVM.Module
llvmParse file = do
  contents <- readFile file 
  withContext $ \context -> do
    ast <- withModuleFromLLVMAssembly context contents moduleAST
    return ast
#endif



{- useful test case written by hand:

  handString = "; ModuleID = '<string>'\n\
    \source_filename = \"<string>\"\n\
    \\n\
    \%0 = type { i32, %1*, %0* }\n\
    \%1 = type opaque\n\
    \\n\
    \$bob = comdat largest\n\
    \\n\
    \@0 = global i32 1\n\
    \@1 = external protected addrspace(3) global i32, section \"foo\", comdat($bob)\n\
    \@2 = unnamed_addr global i8 2\n\
    \@3 = external dllimport global %0\n\
    \@4 = external global [4294967296 x i32]\n\
    \@.argyle = thread_local global i32 0\n\
    \@5 = thread_local(localdynamic) global i32 1\n\
    \\n\
    \@three = private alias i32, i32 addrspace(3)* @1\n\
    \@two = unnamed_addr alias i32, i32 addrspace(3)* @three\n\
    \@one = thread_local(initialexec) alias i32, i32* @5\n\
    \\n\
    \define i32 @bar() prefix i32 1 {\n\
    \  %1 = musttail call zeroext i32 @foo(i32 inreg align 16 1, i8 signext 4) #0\n\
    \  ret i32 %1\n\
    \}\n\
    \\n\
    \define i32 @baz() prefix i32 1 {\n\
    \  %1 = notail call zeroext i32 @foo(i32 inreg align 16 1, i8 signext 4) #0\n\
    \  ret i32 %1\n\
    \}\n\
    \\n\
    \; Function Attrs: nounwind readnone uwtable\n\
    \define zeroext i32 @foo(i32 inreg align 16 %x, i8 signext %y) #0 {\n\
    \  %1 = mul nsw i32 %x, %x\n\
    \  br label %here\n\
    \\n\
    \here:                                             ; preds = %0\n\
    \  %go = icmp eq i32 %1, %x\n\
    \  br i1 %go, label %there, label %elsewhere\n\
    \\n\
    \there:                                            ; preds = %here\n\
    \  %2 = add nsw i32 %1, 3\n\
    \  br label %elsewhere\n\
    \\n\
    \elsewhere:                                        ; preds = %there, %here\n\
    \  %r = phi i32 [ 2, %there ], [ 57, %here ]\n\
    \  ret i32 %r\n\
    \}\n\
    \\n\
    \attributes #0 = { nounwind readnone uwtable \"eep\" }\n"

-}
