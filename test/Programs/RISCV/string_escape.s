	.text
	.attribute	4, 16
	.attribute	5, "rv64i2p0_m2p0_a2p0_c2p0"
	.file	"string_escape.c"
	.globl	main                            # -- Begin function main
	.p2align	1
	.type	main,@function
main:                                   # @main
# %bb.0:
	lui	a0, %hi(S1)
	ld	a0, %lo(S1)(a0)
	mv	a1, zero
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
	add	a2, a0, a1
	lbu	a2, 0(a2)
	addi	a1, a1, 1
	bnez	a2, .LBB0_1
# %bb.2:
	slli	a0, a1, 32
	srli	a0, a0, 32
	addi	a0, a0, -22
	seqz	a0, a0
	ret
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"hello, world!\r\n\007\b\003\004\"\\"
	.size	.L.str, 22

	.type	S1,@object                      # @S1
	.section	.sdata,"aw",@progbits
	.globl	S1
	.p2align	3
S1:
	.quad	.L.str
	.size	S1, 8

	.ident	"Debian clang version 11.0.1-2"
	.section	".note.GNU-stack","",@progbits
	.addrsig
