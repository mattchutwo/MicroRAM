
# MicroRAM
 
MicroRAM is a random-access machine designed to efficiently do zero knowledge proofs of program properties. The design is mased on [TinyRAM](https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf). The current implementation includes the following tools:
 
 * An ADT implementation of MicroRAM 
 * A interpreter of MicroRAM in Haskell 
 * A compiler from C to MicroRAM with custom optimisations
 * A CBOR serialiser for the output

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


## Quick use examples:

### Simplest example:

To fully process the trivial program `programs/return42.c` do:

```
% stack exec compile -- programs/return42.c 25
```

Here `25` is the desired length of the trace. This will output the CBOR binary encoding of:

* The compiled MicroRAM program  
* The parameters passed to circuit generation:
  * Number of registers
  * trace length (i.e. 25)
  * Sparsity information
* Trace of running the program for 25 cycles
* Circuit advice
* The input (as initial memory). Here the memory is a trivial string representing the name of the program and no other argument.


### Passing arguemnts and running the interpreter:

Lets see another example

```
% stack exec compile -- programs/fib.c 300 1 1 1 1 1 1 1 1 1 1 -O3 --mram-out -verifier
```
Here:
* 300 is the desired length of the trace
* `-O3` runs clang with full optimisations
* `--mram-out` writes the compiled MicroRAM program to `programs/fib.micro`
* `--verifier` runs the backend in "public mode" so rthe resulting CBOR code only has the compiled program and the parameters (number of registers, trace length and sprsity information).

We can further run the interpreter on the compiled code (explained below): 

```
% stack exec run programs/fib.micro 400 1 1 1 1 1 1 1 1 1 1
With arguments ["1","1","1","1","1","1","1","1","1","1"]
Running program programs/fib.mic for 400 steps.
Result: 55
```
Returns the 10th fibonacci number. Yay!

Finally, if we are happy with the execution. We can go ahead and generate the secret output (with the oh-so-secret-input "10").

```
% stack exec compile -- programs/fib.micro 300 1 1 1 1 1 1 1 1 1 1 --from-mram
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

## More details

### Why inputs in unary

Well, c programs take only two arguments: ` int argc, char *argv[]`. Our current implementation only supports ints. To avoid dealing with conversions (i.e. `char -> int`) the current hack is to use `argc` as the input. But that's only the number of arguments...

Yes this is a big hack, but it get's us where we need... so if it works...