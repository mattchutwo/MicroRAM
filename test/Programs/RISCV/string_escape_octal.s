	.text
	.attribute	4, 16
	.attribute	5, "rv64i2p0_m2p0"
	.file	"string_escape2.c"
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi	sp, sp, -64
	sd	ra, 56(sp)
	sd	s0, 48(sp)
	addi	s0, sp, 64
	mv	a0, zero
	sw	a0, -20(s0)
	sw	a0, -24(s0)
	sw	a0, -28(s0)
	j	.LBB0_1
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
	lw	a0, -28(s0)
	addi	a1, zero, 19
	blt	a1, a0, .LBB0_4
	j	.LBB0_2
.LBB0_2:                                #   in Loop: Header=BB0_1 Depth=1
	lw	a0, -28(s0)
	lui	a1, %hi(ARR)
	addi	a1, a1, %lo(ARR)
	add	a0, a0, a1
	lbu	a0, 0(a0)
	lui	a2, %hi(.L.str)
	addi	a2, a2, %lo(.L.str)
	mv	a3, zero
	sd	a0, -40(s0)
	mv	a0, a2
	ld	a2, -40(s0)
	sd	a1, -48(s0)
	mv	a1, a2
	mv	a2, a3
	sd	a3, -56(s0)
	ld	a4, -56(s0)
	call	__cc_trace_exec
	lw	a0, -28(s0)
	ld	a1, -48(s0)
	add	a0, a0, a1
	lbu	a0, 0(a0)
	lw	a2, -24(s0)
	add	a0, a2, a0
	sw	a0, -24(s0)
	j	.LBB0_3
.LBB0_3:                                #   in Loop: Header=BB0_1 Depth=1
	lw	a0, -28(s0)
	addi	a0, a0, 1
	sw	a0, -28(s0)
	j	.LBB0_1
.LBB0_4:
	lw	a1, -24(s0)
	lui	a0, %hi(.L.str.1)
	addi	a0, a0, %lo(.L.str.1)
	mv	a2, zero
	sd	a2, -64(s0)
	ld	a3, -64(s0)
	ld	a4, -64(s0)
	call	__cc_trace_exec
	lw	a0, -24(s0)
	addi	a0, a0, -1366
	seqz	a0, a0
	ld	s0, 48(sp)
	ld	ra, 56(sp)
	addi	sp, sp, 64
	ret
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.type	ARR,@object                     # @ARR
	.section	.rodata,"a",@progbits
	.globl	ARR
ARR:
	.ascii	"GIF89a\b\000\b\000\200\000\000\000\000\000\377\377\377!"
	.size	ARR, 20

	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"got"
	.size	.L.str, 4

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"answer"
	.size	.L.str.1, 7

	.ident	"Debian clang version 11.0.1-2"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym __cc_trace_exec
	.addrsig_sym ARR
