
# MicroRAM
 
MicroRAM is a random-access machine and accompanying C-compiler designed to efficiently do zero knowledge proofs of program properties. The design is mased on [TinyRAM](https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf). The current implementation includes the following tools:
 
 * [An ADT implementation of MicroRAM](src/MicroRAM.hs) 
 * [A interpreter of MicroRAM in Haskell ](src/MicroRAM/MRAMInterpreter.hs)
 * [A compiler from C to MicroRAM with custom optimisations](src/Compiler.hs)
 * [A CBOR serialiser for the output](src/Output/Output.hs)

## Installing

### LLVM support

The llvm pipeline is known to have build issues in OSX. If you don't need to support the llvm pipeline, skip to the next section.

First get llvm compatible with the haskell bindings:

```
brew install llvm-hs/llvm/llvm-9
```

Make sure clang is installed

```
clang --version
```

### MicroRAM

Clone this repository and build it

```
% stack build
```

If you want to build without llvm, you can use the `no-llvm` flag

```
% stack build --flag MicroRam:no-llvm
```

If you use REPL, you will also need to pass that flag. For Emacs you can achieve that by adding the flag in your `haskell-process-args-cabal-repl` flags, in your `emacs.d/init.el` or wherever you initialize your emacs custom variables:

```
(custom-set-variables
 ...
 '(haskell-process-args-cabal-repl '(YOUR-OTHER-FLAGS "--flags MicroRam:no-llvm"))
 ...
 )
```



## Documentation:

Just run

```
% stack haddock
```

To produce the documentation. By default it will be located at:
`.stack-work/dist/[Your architecture]/Cabal-[version]/doc/html/index.html`

## Quick use examples:

### Simplest example:

To fully process the trivial program `test/programs/return42.c` do:

```
% stack exec compile -- test/programs/return42.c 25
```

Here `25` is the desired length of the trace. This will output the CBOR-hex encoding of:

* The compiled MicroRAM program  
* The parameters passed to circuit generation:
  * Number of registers
  * trace length (i.e. 25)
  * Sparsity information
* Trace of running the program for 25 cycles
* Nondeterministic advice for the circuit builder
* The input (as initial memory). This program has no input so initial memory is empty.


### Passing arguemnts and running the interpreter:

Lets see another example

```
% stack exec compile -- test/programs/fib.c 300 -O3 --mram-out --verifier
```
Here:
* 300 is the desired length of the trace
* `-O3` runs clang with full optimisations
* `--mram-out` writes the compiled MicroRAM program to `test/programs/fib.micro`
* `--verifier` runs the backend in "public mode" so rthe resulting CBOR code only has the compiled program and the parameters (number of registers, trace length and sparsity information).
* Notice that the "input" (in this example is 10) is passed in the program as extern global. This can be linked separatedly if needed to remain secret.


We can further run the interpreter on the compiled code (explained below): 

```
% stack exec run test/programs/fib.micro
Running program programs/fib.mic for 300 steps.
Result: 34
```
Returns the 10th fibonacci number. Yay!

Finally, if we are happy with the execution. We can go ahead and generate the secret output (with the oh-so-secret-input "10").

```
% stack exec compile -- programs/fib.micro 300 --from-mram
```

Here `--from-mram` skips the compiler and only runs the interpreter and the serialisation of the result.

## Usage

The compiler recognizes the following usage

```
Usage: compile file length [arguments] [options]. 
 Options: 
  -h       --help                   Print this help message
           --llvm-out[=FILE]        Save the llvm IR to file
           --mram-out[=FILE]        Save the compiled MicroRAM program to file
  -O[arg]  --optimize[=arg]         Optimization level of the front end
  -o FILE  --output=FILE            Write ouput to file
           --from-llvm              Compile only with the backend. Compiles from an LLVM file.
           --just-llvm              Compile only with the frontend. 
           --just-mram, --verifier  Only run the compiler (no interpreter). 
           --from-mram              Only run the interpreter from a compiled MicroRAM file.
  -v       --verbose                Chatty compiler
           --pretty-hex             Pretty print the CBOR output. Won't work if writting to file. 
           --flat-hex               Output in flat CBOR format. Won't work if writting to file. 
  -c       --double-check           check the result
```

## Running the tests

You can also run our test suite like so:

```
% stack test
```
