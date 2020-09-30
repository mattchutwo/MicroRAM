#!/bin/bash
set -e

# Directory containing this script
script_dir="$(dirname "$0")"
# Directory containing the `libfromager` library files
lib_dir=$script_dir

# $secret_c contains the path of the secret input file if it exists.
# Otherwise, it's empty, so `[ -n "$secret_c" ]` can be used to check if a
# secret file should be used.
secret_c="${1%.c}_secret.c"
if ! [ -f "$secret_c" ]; then
    secret_c=
fi

CC="clang${LLVM_SUFFIX} -flto -O1 -mprefer-vector-width=1"
CXX="clang++${LLVM_SUFFIX} -flto -O1 -mprefer-vector-width=1 -fno-rtti"

$CC -O3 -c "$lib_dir/libfromager.c" -o "$lib_dir/libfromager.c.o" -fno-builtin
$CXX -O3 -c "$lib_dir/libfromager++.cpp" -o "$lib_dir/libfromager++.cpp.o"

$CC -c "$1" -o "$1.o"

llvm-link${LLVM_SUFFIX} \
    "$1.o" "$lib_dir/libfromager.c.o" "$lib_dir/libfromager++.cpp.o" \
    -o "$1.link.bc"

opt${LLVM_SUFFIX} \
    --internalize --internalize-public-api-list=main,__llvm__memset__p0i8__i64,__llvm__memcpy__p0i8__p0i8__i64 \
    --strip-debug --force-vector-width=1 \
    -O3 --scalarizer -O1 \
    "$1.link.bc" -o "$1.opt.bc"

if [ -n "$secret_c" ]; then
    $CC -c "$secret_c" -o "$secret_c.o"
    llvm-link${LLVM_SUFFIX} \
        "$1.opt.bc" "$secret_c.o" -o "$1.bc"
else
    cp "$1.opt.bc" "$1.bc"
fi

llvm-dis${LLVM_SUFFIX} \
    "$1.bc" -o "$1.ll"
sed -i -e 's/nofree//g' $1.ll
