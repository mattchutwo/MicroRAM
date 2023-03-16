        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "example.c"
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
        li      a0, 3
        ret
.Lfunc_end0:
        .size   main, .Lfunc_end0-main
        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 0477cac332d5abf7b2b51b470370afcbb1e8d513)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
