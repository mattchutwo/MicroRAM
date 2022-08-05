        .text
        .attribute      4, 16
        .attribute      5, "rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0"
        .file   "example.c"
        .globl  foo                             # -- Begin function foo
        .p2align        1
        .type   foo,@function
foo:                                    # @foo
# %bb.0:
        addi    sp, sp, -32
        sd      ra, 24(sp)                      # 8-byte Folded Spill
        sd      s0, 16(sp)                      # 8-byte Folded Spill
        addi    s0, sp, 32
                                        # kill: def $x11 killed $x10
        sw      a0, -24(s0)
        lw      a0, -24(s0)
        li      a1, 1
        bne     a0, a1, .LBB0_2
        j       .LBB0_1
.LBB0_1:
        li      a0, 0
        sw      a0, -20(s0)
        j       .LBB0_3
.LBB0_2:
        lw      a0, -24(s0)
        sw      a0, -20(s0)
        j       .LBB0_3
.LBB0_3:
        lw      a0, -20(s0)
        ld      ra, 24(sp)                      # 8-byte Folded Reload
        ld      s0, 16(sp)                      # 8-byte Folded Reload
        addi    sp, sp, 32
        ret
.Lfunc_end0:
        .size   foo, .Lfunc_end0-foo
                                        # -- End function
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
.LBB1_1:                                # Label of block must be emitted
        lui   a0, %hi(foo)
        addi    a0, a0, %lo(foo)  #(.LBB1_1)
        sd      a0, -32(s0)
        ld      a1, -32(s0)
        li      a0, 5
        jalr    a1
        ld      ra, 24(sp)                      # 8-byte Folded Reload
        ld      s0, 16(sp)                      # 8-byte Folded Reload
        addi    sp, sp, 32
        ret
.Lfunc_end1:
        .size   main, .Lfunc_end1-main
                                        # -- End function
        .ident  "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 70f13bd752f00cdb41b7a8f2bdd690fa90375e02)"
        .section        ".note.GNU-stack","",@progbits
        .addrsig
        .addrsig_sym foo
