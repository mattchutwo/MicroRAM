# MicroRAM
 
 This is a toy experiment containing:
 
 * An implementation of TinyRAM 
 * A interpreter of TinyRAM in Haskell 
 * A compiler from LLVM to TinyRAM


There is a collection of examples in the `examples` and `llvm-examples` folders. You can run them like so

First build the project:

```
% stack build
```

Then compile some basic block or a program

```
% stack exec MicroRAM-compile basicblock llvm-examples/example0
\\ Prints the MicroRAM program (list of instructions)
% stack exec MicroRAM-compile llvm-examples/fibonacci-loop
\\ Prints the MicroRAM program (list of instructions)
```

Then run a Program:

```
% stack exec MicroRAM-exe examples/fibonacci 4
\\ Prints trace with 40 steps of running the program
\\ Register 0 should contain the 10th fibonacci
```

OS X
----

```
brew install llvm-hs/llvm/llvm-9
```
