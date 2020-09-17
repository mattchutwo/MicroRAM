#!/bin/sh
clang -emit-llvm -S -o test/programs/fieldMemset.{ll,c} -O1
clang -emit-llvm -S -o test/programs/libfromager.{ll,c} -O1 -fno-builtin
llvm-link-9 test/programs/fieldMemset.ll test/programs/libfromager.ll \
    -S -o test/programs/fieldMemset-link.ll
sed -i -e 's/nofree//g' test/programs/fieldMemset-link.ll
