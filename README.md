
# MicroRAM
 
MicroRAM is a random-access machine designed to efficiently do zero knowledge proofs of program properties. The design is mased on [TinyRAM](https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf). The current implementation includes the following tools:
 
 * An ADT implementation of MicroRAM 
 * A interpreter of MicroRAM in Haskell 
 * A compiler from C to MicroRAM

## Installing

First get llvm compatible with the haskell bindings:

```
brew install llvm-hs/llvm/llvm-9
```

Make sure clang is installed

```
clang --version
```

Clone this repository and build it

```
% stack build
```


## Quick use example:

To compile and run the program `programs/fib.c` do:

```
% stack exec compile programs/fib.c  
```

This will create the compiled file `programs/fib.micro` which you can run for 400 steps like so ([notice fib takes it's argument in unary, explained bellow](#why-inputs-inunary))

```
% stack exec run programs/fib.micro 400 1 1 1 1 1 1 1 1 1 1
Running program programs/fib.mic for 400 steps.
Result: 34
```

Returns the 10th fibonacci number. Yay!

## Usage

The compiler recognizes the following usage

```
Usage: compile [-benstuv] [file ...]
           --llvm-out=FILE   Save the llvm IR to a file
  -O[arg]  --optimize[=arg]  Optimization level of the front end
  -o FILE  --output=FILE     Output to file
           --from-llvm       Compile only with the backend. Compiles from an LLVM file.
           --just-llvm       Compile only with the frontend. 
  -v       --verbose         Chatty compiler
  -h       --help            Print this help message
```

Remember that for stack, all arguments go after a `--`. For example, we can compile `fib.c` with full optimizations, saving the intermediate llvm file and customizing the output file:

```
stack exec compile -- programs/fib.c --llvm-out=programs/fib.ll -oprograms/mysuperfancyfib.micro
```

**Beware:** if you get too fancy with the optimizations, clang might use instructions we do not support yet. We are working on it. For example

```
% stack exec compile -- programs/fib.c -O3

  Backend compilation error: Feature not supported by the compiler yet: Phi. Not implemented in the trivial Register allocation.
```



## Running the tests

You can also run our test suite loke so:

```
% stack test
```

Some of the tests will fail for now. For example we don't currently support `phi`, so if the front end introduces a `phi` we can't compile. You will see this error in the optimized fibonacci example.


## More details

### Why inputs inunary

Well, c programs take only two arguments: ` int argc, char *argv[]`. Our current implementation only supports ints. To avoid dealing with conversions (i.e. `char -> int`) the current hack is to use `argc` as the input. But that's only the number of arguments...

Yes this is a big hack, but it get's us where we need... so if it works...