module Frontend.BashCalls where

import System.Process

a = readProcess "clang" ["programs/fib.c", "-S", "-emit-llvm", "-o", "programs/fibSlowish2.ll", "-O1"] ""

dir = readProcess "pwd" [] ""
