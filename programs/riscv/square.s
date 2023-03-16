# Computes the square of 5 by calling a function
        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "example.c"
        .globl  square                          # -- Begin function square
        .p2align        1
        .type   square,@function
square:                                 # @square
        mulw    a0, a0, a0
        ret
.Lfunc_end0:
        .size   square, .Lfunc_end0-square
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
        lui     a0, %hi(B)
        lw      a0, %lo(B)(a0)
        tail    square
.Lfunc_end1:
        .size   main, .Lfunc_end1-main
        .type   B,@object                       # @B
        .section        .sdata,"aw",@progbits
        .globl  B
        .p2align        2
B:
        .word   1                               # 0x1
        .size   B, 4

        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git d97f997eb79d91b2872ac13619f49cb3a7120781)"
        .section        ".note.GNU-stack","",@progbits
