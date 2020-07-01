# MicroRAM
 
 This is a toy experiment containing:
 
 * An implementation of TinyRAM 
 * A interpreter of TinyRAM in Haskell 
 * A compiler from LLVM to TinyRAM

To try the this out first build the project

```
% stack build
```

Then run the tests. In theory this compiles several C programs to MicroRAM and runs them. In reality I provide the llvm intermediate repressentation already. Feel free to recompile them in the `Cexamples` folder with your clang.

```
% stack test
```

Some of the tests will fail for now. For example we don't currently support `phi`, so if the front end introduces a `phi` we can't compile. You will see this error in the optimized fibonacci example.


