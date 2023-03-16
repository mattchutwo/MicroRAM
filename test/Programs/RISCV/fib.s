        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "fib.c"
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
        lui     a0, %hi(SECRET_NUMBER)
        lw      a0, %lo(SECRET_NUMBER)(a0)
        bltz    a0, .LBB0_8
        li      a3, 0
        li      a4, 0
        li      a5, 1
        li      a6, 2
        j       .LBB0_3
.LBB0_2:                                #   in Loop: Header=BB0_3 Depth=1
        addiw   a3, a2, 1
        bge     a2, a0, .LBB0_5
.LBB0_3:                                # =>This Inner Loop Header: Depth=1
        mv      a2, a3
        addw    a1, a5, a4
        bltu    a3, a6, .LBB0_2
        mv      a4, a5
        mv      a5, a1
        j       .LBB0_2
.LBB0_5:
        li      a3, 2
        bltu    a2, a3, .LBB0_7
        mv      a0, a1
.LBB0_7:
        sext.w  a0, a0
        ret
.LBB0_8:
        sext.w  a0, a0
        ret
.Lfunc_end0:
        .size   main, .Lfunc_end0-main
        .type   SECRET_NUMBER,@object           # @SECRET_NUMBER
        .section        "__DATA,__secret","aw",@progbits
        .globl  SECRET_NUMBER
        .p2align        2
SECRET_NUMBER:
        .word   10                              # 0xa
        .size   SECRET_NUMBER, 4

        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 0477cac332d5abf7b2b51b470370afcbb1e8d513)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
