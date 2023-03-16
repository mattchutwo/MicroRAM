        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "fibRec.c"
        .globl  fibbonacci                      # -- Begin function fibbonacci
        .p2align        1
        .type   fibbonacci,@function
fibbonacci:                             # @fibbonacci
        addi    sp, sp, -32
        sd      ra, 24(sp)                      # 8-byte Folded Spill
        sd      s0, 16(sp)                      # 8-byte Folded Spill
        sd      s1, 8(sp)                       # 8-byte Folded Spill
        sd      s2, 0(sp)                       # 8-byte Folded Spill
        mv      s0, a0
        li      s1, 0
        li      s2, 2
        sext.w  a0, s0
        bltu    a0, s2, .LBB0_2
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
        addiw   a0, s0, -1
        call    fibbonacci
        addiw   s0, s0, -2
        addw    s1, s1, a0
        sext.w  a0, s0
        bgeu    a0, s2, .LBB0_1
.LBB0_2:
        addw    a0, s0, s1
        ld      ra, 24(sp)                      # 8-byte Folded Reload
        ld      s0, 16(sp)                      # 8-byte Folded Reload
        ld      s1, 8(sp)                       # 8-byte Folded Reload
        ld      s2, 0(sp)                       # 8-byte Folded Reload
        addi    sp, sp, 32
        ret
.Lfunc_end0:
        .size   fibbonacci, .Lfunc_end0-fibbonacci
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
        li      a0, 2
        tail    fibbonacci
.Lfunc_end1:
        .size   main, .Lfunc_end1-main
        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 0477cac332d5abf7b2b51b470370afcbb1e8d513)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
