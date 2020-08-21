#!/bin/bash

for prog in test/programs/*.c
do
    clang $prog -S -emit-llvm -o ${prog%.c}.ll
done 

# 
