	.text
	.attribute	4, 16
	.attribute	5, "rv64i2p0_m2p0"
	.file	"sraw.c"
	.globl	sraw1                           # -- Begin function sraw1
	.p2align	2
	.type	sraw1,@function
sraw1:                                  # @sraw1
# %bb.0:
	sraw	a0, a0, a1
	ret
.Lfunc_end0:
	.size	sraw1, .Lfunc_end0-sraw1
                                        # -- End function
	.globl	sraw2                           # -- Begin function sraw2
	.p2align	2
	.type	sraw2,@function
sraw2:                                  # @sraw2
# %bb.0:
	sraw	a0, a1, a0
	ret
.Lfunc_end1:
	.size	sraw2, .Lfunc_end1-sraw2
                                        # -- End function
	.globl	test_both_sraw                  # -- Begin function test_both_sraw
	.p2align	2
	.type	test_both_sraw,@function
test_both_sraw:                         # @test_both_sraw
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	sd	s1, 8(sp)
	sd	s2, 0(sp)
	mv	s2, a2
	mv	s0, a1
	mv	s1, a0
	call	sraw1
	bne	a0, s2, .LBB2_2
# %bb.1:
	mv	a0, s0
	mv	a1, s1
	call	sraw2
	xor	a0, a0, s2
	seqz	a0, a0
	j	.LBB2_3
.LBB2_2:
	mv	a0, zero
.LBB2_3:
	ld	s2, 0(sp)
	ld	s1, 8(sp)
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end2:
	.size	test_both_sraw, .Lfunc_end2-test_both_sraw
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	sd	s1, 8(sp)
	sd	s2, 0(sp)
	addi	a0, zero, 12
	addi	a1, zero, 1
	call	sraw1
	addi	a1, zero, 6
	bne	a0, a1, .LBB3_2
# %bb.1:
	addi	a0, zero, 1
	addi	a1, zero, 12
	call	sraw2
	addi	a0, a0, -6
	seqz	s2, a0
	j	.LBB3_3
.LBB3_2:
	mv	s2, zero
.LBB3_3:
	addi	a0, zero, -1
	addi	a1, zero, 1
	addi	s0, zero, -1
	call	sraw1
	beq	a0, s0, .LBB3_5
# %bb.4:
	mv	s1, zero
	j	.LBB3_6
.LBB3_5:
	addi	a0, zero, 1
	addi	a1, zero, -1
	call	sraw2
	addi	a0, a0, 1
	seqz	s1, a0
.LBB3_6:
	addi	a0, zero, -1
	addi	a1, zero, 3
	call	sraw1
	beq	a0, s0, .LBB3_8
# %bb.7:
	mv	a0, zero
	j	.LBB3_9
.LBB3_8:
	addi	a0, zero, 3
	addi	a1, zero, -1
	call	sraw2
	addi	a0, a0, 1
	seqz	a0, a0
.LBB3_9:
	and	a1, s2, s1
	and	a0, a1, a0
	ld	s2, 0(sp)
	ld	s1, 8(sp)
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end3:
	.size	main, .Lfunc_end3-main
                                        # -- End function
	.ident	"Debian clang version 11.0.1-2"
	.section	".note.GNU-stack","",@progbits
	.addrsig
