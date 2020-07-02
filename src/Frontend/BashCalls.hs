module Frontend.BashCalls where

import System.Process

a = readProcess "clang" ["Cprograms/fib.c", "-S", "-emit-llvm", "-o", "Cprograms/fibSlowish2.ll", "-O1"] ""

dir = readProcess "pwd" [] ""
