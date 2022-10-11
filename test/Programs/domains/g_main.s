	.text
	.attribute	4, 16
	.attribute	5, "rv64i2p0_m2p0_a2p0_c2p0"
	.file	"g_main.c"
	.globl	g                               # -- Begin function g
	.p2align	1
	.type	g,@function
g:                                      # @g
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	addi	s0, sp, 32
	add	a1, zero, a0
	sw	a0, -20(s0)
	lw	a0, -20(s0)
	slliw	a0, a0, 1
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end0:
	.size	g, .Lfunc_end0-g
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	1
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	addi	s0, sp, 32
	mv	a0, zero
	sw	a0, -20(s0)
	addi	a0, zero, 100
	call	g
	call	f
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
                                        # -- End function
	.ident	"Debian clang version 11.0.1-2"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym g
	.addrsig_sym f
