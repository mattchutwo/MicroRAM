        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "example.c"
        .globl  main                            # -- Begin function main
        .p2align        1
        .type   main,@function
main:                                   # @main
# %bb.0:
        addi    sp, sp, -32
        sd      ra, 24(sp)                      # 8-byte Folded Spill
        sd      s0, 16(sp)                      # 8-byte Folded Spill
        addi    s0, sp, 32
        li      a0, 0
        sw      a0, -20(s0)
        li      a0, 21
        sw      a0, -24(s0)
        lw      a0, -24(s0)
        addw    a0, a0, a0
        ld      ra, 24(sp)                      # 8-byte Folded Reload
        ld      s0, 16(sp)                      # 8-byte Folded Reload
        addi    sp, sp, 32
        ret
.Lfunc_end0:
        .size   main, .Lfunc_end0-main
                                        # -- End function
        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 70f13bd752f00cdb41b7a8f2bdd690fa90375e02)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
