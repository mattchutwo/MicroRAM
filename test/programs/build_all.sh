#!/bin/bash
set -e

progs=(
    binaryTree/binaryTree.c
    DoubleFree/DoubleFree.c
    LinkedList/linkedList.c
    MallocOOB/mallocOOB.c
    UseAfterFree/useAfterFree.c
    WrongFree/wrongFree.c
)

for prog in "${progs[@]}"; do
    echo "building $prog"
    "$(dirname "$0")"/build/build.sh "$(dirname "$0")/$prog"
done
