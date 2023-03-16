        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "example.c"
        .globl  square                          # -- Begin function square
        .p2align        1
        .type   square,@function
square:                                 # @square
# %bb.0:
        mulw    a0, a0, a0
        ret
.Lfunc_end0:
        .size   square, .Lfunc_end0-square
                                        # -- End function
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
# %bb.0:
.LBB1_1:                                # Label of block must be emitted
        lui   a0, %hi(INPUT)
        addi    a0, a0, %lo(INPUT)  # (.LBB1_1)
        lw      a0, 0(a0)
        mulw    a0, a0, a0
        ret
.Lfunc_end1:
        .size   main, .Lfunc_end1-main
                                        # -- End function
        .type   INPUT,@object                   # @INPUT
        .section        .sdata,"aw",@progbits
        .globl  INPUT
        .p2align        2
INPUT:
        .word   8                               # 0x8
        .size   INPUT, 4

        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 70f13bd752f00cdb41b7a8f2bdd690fa90375e02)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
