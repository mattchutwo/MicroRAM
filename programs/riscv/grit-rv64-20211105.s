	.text
	.attribute	4, 16
	.attribute	5, "rv64i2p0"
	.file	"llvm-link"
	.p2align	2                               # -- Begin function posix_exit
	.type	posix_exit,@function
posix_exit:                             # @posix_exit
# %bb.0:
	lui	a0, %hi(__iob+8)
	ld	a0, %lo(__iob+8)(a0)
	lw	a1, 56(a0)
	beqz	a1, .LBB0_2
# %bb.1:                                # %.preheader.i
	sw	zero, 56(a0)
.LBB0_2:                                # %__posix_flush.exit
	ret
.Lfunc_end0:
	.size	posix_exit, .Lfunc_end0-posix_exit
                                        # -- End function
	.globl	__llvm__memcpy__p0i8__p0i8__i64 # -- Begin function __llvm__memcpy__p0i8__p0i8__i64
	.p2align	2
	.type	__llvm__memcpy__p0i8__p0i8__i64,@function
__llvm__memcpy__p0i8__p0i8__i64:        # @__llvm__memcpy__p0i8__p0i8__i64
# %bb.0:
	add	a3, a0, a2
	beqz	a2, .LBB1_4
	j	.LBB1_1
.LBB1_1:                                # %.preheader
	j	.LBB1_2
.LBB1_2:                                # =>This Inner Loop Header: Depth=1
	lb	a2, 0(a1)
	sb	a2, 0(a0)
	addi	a1, a1, 1
	addi	a0, a0, 1
	bne	a0, a3, .LBB1_2
	j	.LBB1_3
.LBB1_3:                                # %.loopexit
	j	.LBB1_4
.LBB1_4:
	ret
.Lfunc_end1:
	.size	__llvm__memcpy__p0i8__p0i8__i64, .Lfunc_end1-__llvm__memcpy__p0i8__p0i8__i64
                                        # -- End function
	.globl	__llvm__memmove__p0i8__p0i8__i64 # -- Begin function __llvm__memmove__p0i8__p0i8__i64
	.p2align	2
	.type	__llvm__memmove__p0i8__p0i8__i64,@function
__llvm__memmove__p0i8__p0i8__i64:       # @__llvm__memmove__p0i8__p0i8__i64
# %bb.0:
	add	a3, a0, a2
	seqz	a4, a2
	bltu	a1, a0, .LBB2_4
	j	.LBB2_1
.LBB2_1:
	andi	a2, a4, 1
	bnez	a2, .LBB2_9
	j	.LBB2_2
.LBB2_2:                                # %.preheader
	j	.LBB2_3
.LBB2_3:                                # =>This Inner Loop Header: Depth=1
	lb	a2, 0(a1)
	sb	a2, 0(a0)
	addi	a1, a1, 1
	addi	a0, a0, 1
	beq	a0, a3, .LBB2_8
	j	.LBB2_3
.LBB2_4:
	andi	a4, a4, 1
	bnez	a4, .LBB2_9
	j	.LBB2_5
.LBB2_5:
	add	a1, a1, a2
	j	.LBB2_6
.LBB2_6:                                # =>This Inner Loop Header: Depth=1
	addi	a2, a1, -1
	addi	a4, a3, -1
	lb	a1, -1(a1)
	sb	a1, -1(a3)
	mv	a3, a4
	mv	a1, a2
	bne	a4, a0, .LBB2_6
	j	.LBB2_7
.LBB2_7:                                # %.loopexit
	j	.LBB2_9
.LBB2_8:                                # %.loopexit4
	j	.LBB2_9
.LBB2_9:
	ret
.Lfunc_end2:
	.size	__llvm__memmove__p0i8__p0i8__i64, .Lfunc_end2-__llvm__memmove__p0i8__p0i8__i64
                                        # -- End function
	.globl	__llvm__memset__p0i8__i64       # -- Begin function __llvm__memset__p0i8__i64
	.p2align	2
	.type	__llvm__memset__p0i8__i64,@function
__llvm__memset__p0i8__i64:              # @__llvm__memset__p0i8__i64
# %bb.0:
	add	a6, a0, a2
	addi	a3, zero, 31
	bltu	a3, a2, .LBB3_3
	j	.LBB3_1
.LBB3_1:
	beqz	a2, .LBB3_20
	j	.LBB3_2
.LBB3_2:                                # %.preheader9
	j	.LBB3_17
.LBB3_3:
	addi	a2, a0, 7
	andi	a4, a2, -8
	andi	t1, a6, -8
	andi	a7, a1, 255
	mv	a3, zero
	mv	a5, zero
	j	.LBB3_7
.LBB3_4:
	bne	a4, a0, .LBB3_6
# %bb.5:
	mv	a4, a0
	j	.LBB3_9
.LBB3_6:                                # %.preheader7
	j	.LBB3_11
.LBB3_7:                                # =>This Inner Loop Header: Depth=1
	sext.w	t0, a3
	slli	a2, a3, 3
	sllw	a2, a7, a2
	or	a5, a5, a2
	addi	a3, a3, 1
	addi	a2, zero, 7
	bltu	t0, a2, .LBB3_7
	j	.LBB3_4
.LBB3_8:                                # %.loopexit8
	j	.LBB3_9
.LBB3_9:
	beq	a4, t1, .LBB3_13
	j	.LBB3_10
.LBB3_10:                               # %.preheader5
	j	.LBB3_15
.LBB3_11:                               # =>This Inner Loop Header: Depth=1
	sb	a1, 0(a0)
	addi	a0, a0, 1
	beq	a0, a4, .LBB3_8
	j	.LBB3_11
.LBB3_12:                               # %.loopexit6
	j	.LBB3_13
.LBB3_13:
	beq	a6, t1, .LBB3_20
	j	.LBB3_14
.LBB3_14:                               # %.preheader
	j	.LBB3_16
.LBB3_15:                               # =>This Inner Loop Header: Depth=1
	sd	a5, 0(a4)
	addi	a4, a4, 8
	beq	a4, t1, .LBB3_12
	j	.LBB3_15
.LBB3_16:                               # =>This Inner Loop Header: Depth=1
	sb	a1, 0(t1)
	addi	t1, t1, 1
	beq	t1, a6, .LBB3_18
	j	.LBB3_16
.LBB3_17:                               # =>This Inner Loop Header: Depth=1
	sb	a1, 0(a0)
	addi	a0, a0, 1
	beq	a0, a6, .LBB3_19
	j	.LBB3_17
.LBB3_18:                               # %.loopexit
	j	.LBB3_20
.LBB3_19:                               # %.loopexit10
	j	.LBB3_20
.LBB3_20:
	ret
.Lfunc_end3:
	.size	__llvm__memset__p0i8__i64, .Lfunc_end3-__llvm__memset__p0i8__i64
                                        # -- End function
	.globl	__llvm__bswap__i32              # -- Begin function __llvm__bswap__i32
	.p2align	2
	.type	__llvm__bswap__i32,@function
__llvm__bswap__i32:                     # @__llvm__bswap__i32
# %bb.0:
	tail	__bswapsi2
.Lfunc_end4:
	.size	__llvm__bswap__i32, .Lfunc_end4-__llvm__bswap__i32
                                        # -- End function
	.p2align	2                               # -- Begin function __bswapsi2
	.type	__bswapsi2,@function
__bswapsi2:                             # @__bswapsi2
# %bb.0:
	slli	a1, a0, 8
	addi	a2, zero, 255
	slli	a3, a2, 32
	and	a1, a1, a3
	slli	a3, a0, 24
	slli	a4, a2, 40
	and	a3, a3, a4
	or	a1, a3, a1
	slli	a3, a0, 40
	slli	a2, a2, 48
	and	a2, a3, a2
	slli	a0, a0, 56
	or	a0, a0, a2
	or	a0, a0, a1
	srai	a0, a0, 32
	ret
.Lfunc_end5:
	.size	__bswapsi2, .Lfunc_end5-__bswapsi2
                                        # -- End function
	.globl	__llvm__ctpop__i32              # -- Begin function __llvm__ctpop__i32
	.p2align	2
	.type	__llvm__ctpop__i32,@function
__llvm__ctpop__i32:                     # @__llvm__ctpop__i32
# %bb.0:
	tail	__popcountsi2
.Lfunc_end6:
	.size	__llvm__ctpop__i32, .Lfunc_end6-__llvm__ctpop__i32
                                        # -- End function
	.p2align	2                               # -- Begin function __popcountsi2
	.type	__popcountsi2,@function
__popcountsi2:                          # @__popcountsi2
# %bb.0:
	srli	a1, a0, 1
	lui	a2, 349525
	addiw	a2, a2, 1365
	and	a1, a1, a2
	subw	a0, a0, a1
	srli	a1, a0, 2
	lui	a2, 209715
	addiw	a2, a2, 819
	and	a1, a1, a2
	and	a0, a0, a2
	add	a0, a1, a0
	srli	a1, a0, 4
	add	a0, a1, a0
	lui	a1, 61681
	addiw	a1, a1, -241
	and	a1, a0, a1
	lui	a2, 61680
	and	a0, a0, a2
	srli	a0, a0, 16
	add	a0, a0, a1
	srli	a1, a0, 8
	add	a0, a1, a0
	andi	a0, a0, 63
	ret
.Lfunc_end7:
	.size	__popcountsi2, .Lfunc_end7-__popcountsi2
                                        # -- End function
	.globl	__llvm__ctlz__i32               # -- Begin function __llvm__ctlz__i32
	.p2align	2
	.type	__llvm__ctlz__i32,@function
__llvm__ctlz__i32:                      # @__llvm__ctlz__i32
# %bb.0:
	tail	__clzsi2
.Lfunc_end8:
	.size	__llvm__ctlz__i32, .Lfunc_end8-__llvm__ctlz__i32
                                        # -- End function
	.p2align	2                               # -- Begin function __clzsi2
	.type	__clzsi2,@function
__clzsi2:                               # @__clzsi2
# %bb.0:
	srliw	a1, a0, 16
	seqz	a1, a1
	slli	a1, a1, 4
	addi	a2, zero, 16
	sub	a2, a2, a1
	srlw	a0, a0, a2
	lui	a2, 16
	addiw	a2, a2, -256
	and	a2, a0, a2
	seqz	a2, a2
	slli	a2, a2, 3
	addi	a3, zero, 8
	sub	a3, a3, a2
	srlw	a0, a0, a3
	or	a1, a2, a1
	andi	a2, a0, 240
	seqz	a2, a2
	slli	a2, a2, 2
	addi	a3, zero, 4
	sub	a3, a3, a2
	srlw	a0, a0, a3
	or	a1, a1, a2
	andi	a2, a0, 12
	seqz	a2, a2
	slli	a2, a2, 1
	addi	a3, zero, 2
	sub	a4, a3, a2
	srlw	a0, a0, a4
	andi	a4, a0, 2
	or	a1, a1, a2
	sub	a0, a3, a0
	srli	a2, a4, 1
	addi	a2, a2, -1
	and	a0, a2, a0
	addw	a0, a0, a1
	ret
.Lfunc_end9:
	.size	__clzsi2, .Lfunc_end9-__clzsi2
                                        # -- End function
	.globl	__llvm__ctlz__i64               # -- Begin function __llvm__ctlz__i64
	.p2align	2
	.type	__llvm__ctlz__i64,@function
__llvm__ctlz__i64:                      # @__llvm__ctlz__i64
# %bb.0:
	addi	sp, sp, -16
	sd	ra, 8(sp)
	call	__clzdi2
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end10:
	.size	__llvm__ctlz__i64, .Lfunc_end10-__llvm__ctlz__i64
                                        # -- End function
	.p2align	2                               # -- Begin function __clzdi2
	.type	__clzdi2,@function
__clzdi2:                               # @__clzdi2
# %bb.0:
	srai	a2, a0, 32
	seqz	a1, a2
	beqz	a2, .LBB11_2
# %bb.1:
	srli	a0, a0, 32
.LBB11_2:
	slli	a2, a0, 32
	srli	a2, a2, 32
	srliw	a0, a0, 1
	or	a0, a2, a0
	srli	a2, a0, 2
	or	a0, a0, a2
	srli	a2, a0, 4
	or	a0, a0, a2
	srli	a2, a0, 8
	or	a0, a0, a2
	srli	a2, a0, 16
	or	a0, a0, a2
	srli	a2, a0, 32
	or	a0, a0, a2
	not	a0, a0
	srli	a2, a0, 1
	lui	a3, 21845
	addiw	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	and	a2, a2, a3
	sub	a0, a0, a2
	lui	a2, 13107
	addiw	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	and	a3, a0, a2
	srli	a0, a0, 2
	and	a0, a0, a2
	add	a0, a3, a0
	srli	a2, a0, 4
	add	a0, a0, a2
	lui	a2, 3855
	addiw	a2, a2, 241
	slli	a2, a2, 12
	addi	a2, a2, -241
	slli	a2, a2, 12
	addi	a2, a2, 241
	slli	a2, a2, 12
	addi	a2, a2, -241
	and	a0, a0, a2
	lui	a2, 4112
	addiw	a2, a2, 257
	slli	a2, a2, 16
	addi	a2, a2, 257
	slli	a2, a2, 16
	addi	a2, a2, 257
	mul	a0, a0, a2
	srli	a0, a0, 56
	addi	a0, a0, -32
	slli	a1, a1, 5
	or	a0, a0, a1
	slli	a0, a0, 32
	srli	a0, a0, 32
	ret
.Lfunc_end11:
	.size	__clzdi2, .Lfunc_end11-__clzdi2
                                        # -- End function
	.globl	__llvm__cttz__i32               # -- Begin function __llvm__cttz__i32
	.p2align	2
	.type	__llvm__cttz__i32,@function
__llvm__cttz__i32:                      # @__llvm__cttz__i32
# %bb.0:
	tail	__ctzsi2
.Lfunc_end12:
	.size	__llvm__cttz__i32, .Lfunc_end12-__llvm__cttz__i32
                                        # -- End function
	.p2align	2                               # -- Begin function __ctzsi2
	.type	__ctzsi2,@function
__ctzsi2:                               # @__ctzsi2
# %bb.0:
	lui	a1, 16
	addiw	a1, a1, -1
	and	a1, a0, a1
	seqz	a1, a1
	slli	a1, a1, 4
	srlw	a0, a0, a1
	andi	a2, a0, 255
	seqz	a2, a2
	slli	a2, a2, 3
	srlw	a0, a0, a2
	or	a1, a2, a1
	andi	a2, a0, 15
	seqz	a2, a2
	slli	a2, a2, 2
	srlw	a0, a0, a2
	or	a1, a1, a2
	andi	a2, a0, 3
	seqz	a2, a2
	slli	a2, a2, 1
	srlw	a0, a0, a2
	or	a1, a1, a2
	srli	a2, a0, 1
	andi	a2, a2, 1
	addi	a3, zero, 2
	sub	a2, a3, a2
	ori	a0, a0, -2
	addi	a0, a0, 1
	and	a0, a2, a0
	add	a0, a0, a1
	ret
.Lfunc_end13:
	.size	__ctzsi2, .Lfunc_end13-__ctzsi2
                                        # -- End function
	.globl	__llvm__cttz__i64               # -- Begin function __llvm__cttz__i64
	.p2align	2
	.type	__llvm__cttz__i64,@function
__llvm__cttz__i64:                      # @__llvm__cttz__i64
# %bb.0:
	addi	sp, sp, -16
	sd	ra, 8(sp)
	call	__ctzdi2
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end14:
	.size	__llvm__cttz__i64, .Lfunc_end14-__llvm__cttz__i64
                                        # -- End function
	.p2align	2                               # -- Begin function __ctzdi2
	.type	__ctzdi2,@function
__ctzdi2:                               # @__ctzdi2
# %bb.0:
	slli	a1, a0, 32
	srli	a2, a1, 32
	seqz	a1, a2
	bnez	a2, .LBB15_2
# %bb.1:
	srli	a0, a0, 32
.LBB15_2:
	addi	a2, a0, -1
	not	a0, a0
	and	a0, a0, a2
	srli	a2, a0, 1
	lui	a3, 21845
	addiw	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	slli	a3, a3, 12
	addi	a3, a3, 1365
	and	a2, a2, a3
	sub	a0, a0, a2
	lui	a2, 13107
	addiw	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	slli	a2, a2, 12
	addi	a2, a2, 819
	and	a3, a0, a2
	srli	a0, a0, 2
	and	a0, a0, a2
	add	a0, a3, a0
	srli	a2, a0, 4
	add	a0, a0, a2
	lui	a2, 3855
	addiw	a2, a2, 241
	slli	a2, a2, 12
	addi	a2, a2, -241
	slli	a2, a2, 12
	addi	a2, a2, 241
	slli	a2, a2, 12
	addi	a2, a2, -241
	and	a0, a0, a2
	lui	a2, 4112
	addiw	a2, a2, 257
	slli	a2, a2, 16
	addi	a2, a2, 257
	slli	a2, a2, 16
	addi	a2, a2, 257
	mul	a0, a0, a2
	srli	a0, a0, 56
	slli	a1, a1, 5
	or	a0, a0, a1
	ret
.Lfunc_end15:
	.size	__ctzdi2, .Lfunc_end15-__ctzdi2
                                        # -- End function
	.globl	__cc_sdiv_i32_i32               # -- Begin function __cc_sdiv_i32_i32
	.p2align	2
	.type	__cc_sdiv_i32_i32,@function
__cc_sdiv_i32_i32:                      # @__cc_sdiv_i32_i32
# %bb.0:
	srai	a3, a0, 63
	add	a4, a0, a3
	addi	a2, zero, -1
	bltz	a0, .LBB16_2
# %bb.1:
	addi	a2, zero, 1
.LBB16_2:
	xor	a0, a4, a3
	srai	a3, a1, 63
	add	a4, a1, a3
	xor	a3, a4, a3
	bgez	a1, .LBB16_4
# %bb.3:
	neg	a2, a2
.LBB16_4:
	divuw	a0, a0, a3
	mulw	a0, a0, a2
	ret
.Lfunc_end16:
	.size	__cc_sdiv_i32_i32, .Lfunc_end16-__cc_sdiv_i32_i32
                                        # -- End function
	.globl	__cc_srem_i32_i32               # -- Begin function __cc_srem_i32_i32
	.p2align	2
	.type	__cc_srem_i32_i32,@function
__cc_srem_i32_i32:                      # @__cc_srem_i32_i32
# %bb.0:
	srai	a3, a0, 63
	add	a4, a0, a3
	addi	a2, zero, -1
	bltz	a0, .LBB17_2
# %bb.1:
	addi	a2, zero, 1
.LBB17_2:
	xor	a0, a4, a3
	srai	a3, a1, 63
	add	a4, a1, a3
	xor	a3, a4, a3
	bgez	a1, .LBB17_4
# %bb.3:
	neg	a2, a2
.LBB17_4:
	remuw	a0, a0, a3
	mulw	a0, a0, a2
	ret
.Lfunc_end17:
	.size	__cc_srem_i32_i32, .Lfunc_end17-__cc_srem_i32_i32
                                        # -- End function
	.globl	__cc_sdiv_i64_i64               # -- Begin function __cc_sdiv_i64_i64
	.p2align	2
	.type	__cc_sdiv_i64_i64,@function
__cc_sdiv_i64_i64:                      # @__cc_sdiv_i64_i64
# %bb.0:
	srai	a3, a0, 63
	add	a4, a0, a3
	addi	a2, zero, -1
	bltz	a0, .LBB18_2
# %bb.1:
	addi	a2, zero, 1
.LBB18_2:
	xor	a0, a4, a3
	srai	a3, a1, 63
	add	a4, a1, a3
	xor	a3, a4, a3
	bgez	a1, .LBB18_4
# %bb.3:
	neg	a2, a2
.LBB18_4:
	divu	a0, a0, a3
	mul	a0, a0, a2
	ret
.Lfunc_end18:
	.size	__cc_sdiv_i64_i64, .Lfunc_end18-__cc_sdiv_i64_i64
                                        # -- End function
	.globl	__cc_srem_i64_i64               # -- Begin function __cc_srem_i64_i64
	.p2align	2
	.type	__cc_srem_i64_i64,@function
__cc_srem_i64_i64:                      # @__cc_srem_i64_i64
# %bb.0:
	srai	a3, a0, 63
	add	a4, a0, a3
	addi	a2, zero, -1
	bltz	a0, .LBB19_2
# %bb.1:
	addi	a2, zero, 1
.LBB19_2:
	xor	a0, a4, a3
	srai	a3, a1, 63
	add	a4, a1, a3
	xor	a3, a4, a3
	bgez	a1, .LBB19_4
# %bb.3:
	neg	a2, a2
.LBB19_4:
	remu	a0, a0, a3
	mul	a0, a0, a2
	ret
.Lfunc_end19:
	.size	__cc_srem_i64_i64, .Lfunc_end19-__cc_srem_i64_i64
                                        # -- End function
	.globl	__cc_va_start                   # -- Begin function __cc_va_start
	.p2align	2
	.type	__cc_va_start,@function
__cc_va_start:                          # @__cc_va_start
# %bb.0:
	addi	a3, zero, 999
	slli	a3, a3, 32
	addi	a3, a3, 999
	sd	a3, 0(a0)
	add	a1, a1, a2
	sd	a1, 8(a0)
	lui	a1, 16
	addiw	a1, a1, -1
	slli	a1, a1, 16
	sd	a1, 16(a0)
	ret
.Lfunc_end20:
	.size	__cc_va_start, .Lfunc_end20-__cc_va_start
                                        # -- End function
	.globl	__llvm__va_copy                 # -- Begin function __llvm__va_copy
	.p2align	2
	.type	__llvm__va_copy,@function
__llvm__va_copy:                        # @__llvm__va_copy
# %bb.0:
	ld	a2, 16(a1)
	sd	a2, 16(a0)
	ld	a2, 8(a1)
	sd	a2, 8(a0)
	ld	a1, 0(a1)
	sd	a1, 0(a0)
	ret
.Lfunc_end21:
	.size	__llvm__va_copy, .Lfunc_end21-__llvm__va_copy
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   # @main
.Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, DW.ref.__gxx_personality_v0
	.cfi_lsda 27, .Lexception0
# %bb.0:
	addi	sp, sp, -80
	.cfi_def_cfa_offset 80
	sd	ra, 72(sp)
	sd	s0, 64(sp)
	sd	s1, 56(sp)
	sd	s2, 48(sp)
	.cfi_offset ra, -8
	.cfi_offset s0, -16
	.cfi_offset s1, -24
	.cfi_offset s2, -32
	sb	zero, 8(sp)
	sd	zero, 16(sp)
	sd	zero, 24(sp)
	addi	s1, zero, 8
	sw	s1, 32(sp)
	sd	zero, 40(sp)
	lui	a0, %hi(_ZTV8CBmpFile+16)
	addi	a0, a0, %lo(_ZTV8CBmpFile+16)
	sd	a0, 0(sp)
.Ltmp0:
	lui	a0, %hi(.L.str.1641)
	addi	a1, a0, %lo(.L.str.1641)
	mv	a0, sp
	call	_ZN8CBmpFile4LoadEPKc
.Ltmp1:
# %bb.1:                                # %_Z8dib_freeP5CLDIB.exit.i2.i
	ld	s0, 24(sp)
	sb	zero, 8(sp)
	lui	a0, %hi(_ZTV8CImgFile+16)
	addi	a0, a0, %lo(_ZTV8CImgFile+16)
	sd	a0, 0(sp)
	sd	zero, 24(sp)
	sw	s1, 32(sp)
	ld	s1, 40(sp)
	beqz	s1, .LBB22_3
# %bb.2:
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB22_3:                               # %_Z7BmpLoadPKc.exit
	beqz	s0, .LBB22_7
# %bb.4:
	ld	s1, 0(s0)
	beqz	s1, .LBB22_6
# %bb.5:
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB22_6:                               # %free.exit1.i
	sd	zero, 0(s0)
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	call	__cc_access_invalid
.LBB22_7:                               # %_Z8dib_freeP5CLDIB.exit
	mv	a0, zero
	ld	s2, 48(sp)
	ld	s1, 56(sp)
	ld	s0, 64(sp)
	ld	ra, 72(sp)
	addi	sp, sp, 80
	ret
.LBB22_8:
.Ltmp2:
	lui	a1, %hi(_ZTV8CImgFile+16)
	addi	a1, a1, %lo(_ZTV8CImgFile+16)
	sd	a1, 0(sp)
	ld	s1, 24(sp)
	mv	s2, a0
	beqz	s1, .LBB22_12
# %bb.9:
	ld	s0, 0(s1)
	beqz	s0, .LBB22_11
# %bb.10:
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	call	__cc_access_invalid
.LBB22_11:                              # %free.exit1.i.i.i
	sd	zero, 0(s1)
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB22_12:                              # %_Z8dib_freeP5CLDIB.exit.i.i
	sd	zero, 24(sp)
	addi	a0, zero, 8
	sw	a0, 32(sp)
	ld	s0, 40(sp)
	beqz	s0, .LBB22_14
# %bb.13:
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	call	__cc_access_invalid
.LBB22_14:                              # %_ZN8CImgFile5ClearEv.exit.i
	mv	a0, s2
	call	_Unwind_Resume
.Lfunc_end22:
	.size	main, .Lfunc_end22-main
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table22:
.Lexception0:
	.byte	255                             # @LPStart Encoding = omit
	.byte	255                             # @TType Encoding = omit
	.byte	3                               # Call site Encoding = udata4
	.uleb128 .Lcst_end0-.Lcst_begin0
.Lcst_begin0:
	.word	.Ltmp0-.Lfunc_begin0            # >> Call Site 1 <<
	.word	.Ltmp1-.Ltmp0                   #   Call between .Ltmp0 and .Ltmp1
	.word	.Ltmp2-.Lfunc_begin0            #     jumps to .Ltmp2
	.byte	0                               #   On action: cleanup
	.word	.Ltmp1-.Lfunc_begin0            # >> Call Site 2 <<
	.word	.Lfunc_end22-.Ltmp1             #   Call between .Ltmp1 and .Lfunc_end22
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
.Lcst_end0:
	.p2align	2
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function _ZN8CBmpFile4LoadEPKc
	.type	_ZN8CBmpFile4LoadEPKc,@function
_ZN8CBmpFile4LoadEPKc:                  # @_ZN8CBmpFile4LoadEPKc
.Lfunc_begin1:
	.cfi_startproc
	.cfi_personality 155, DW.ref.__gxx_personality_v0
	.cfi_lsda 27, .Lexception1
# %bb.0:
	addi	sp, sp, -128
	.cfi_def_cfa_offset 128
	sd	ra, 120(sp)
	sd	s0, 112(sp)
	sd	s1, 104(sp)
	sd	s2, 96(sp)
	sd	s3, 88(sp)
	sd	s4, 80(sp)
	sd	s5, 72(sp)
	sd	s6, 64(sp)
	sd	s7, 56(sp)
	sd	s8, 48(sp)
	sd	s9, 40(sp)
	sd	s10, 32(sp)
	sd	s11, 24(sp)
	.cfi_offset ra, -8
	.cfi_offset s0, -16
	.cfi_offset s1, -24
	.cfi_offset s2, -32
	.cfi_offset s3, -40
	.cfi_offset s4, -48
	.cfi_offset s5, -56
	.cfi_offset s6, -64
	.cfi_offset s7, -72
	.cfi_offset s8, -80
	.cfi_offset s9, -88
	.cfi_offset s10, -96
	.cfi_offset s11, -104
	mv	s2, a1
	lui	a3, %hi(.L.str.1641)
	addi	a1, a3, %lo(.L.str.1641)
	or	a2, s2, a1
	andi	a2, a2, 7
	mv	s3, a0
	mv	s1, s2
	sd	s2, 0(sp)
	sd	a0, 8(sp)
	beqz	a2, .LBB23_5
.LBB23_1:
	lbu	a0, 0(s1)
	beqz	a0, .LBB23_10
.LBB23_2:                               # %.preheader.i.i.preheader
	addi	a2, s1, 1
.LBB23_3:                               # %.preheader.i.i
                                        # =>This Inner Loop Header: Depth=1
	lbu	a3, 0(a1)
	andi	a4, a0, 255
	bne	a4, a3, .LBB23_11
# %bb.4:                                #   in Loop: Header=BB23_3 Depth=1
	lbu	a0, 0(a2)
	addi	a1, a1, 1
	addi	a2, a2, 1
	bnez	a0, .LBB23_3
	j	.LBB23_10
.LBB23_5:
	ld	a0, 0(s2)
	lui	a1, 14643
	addiw	a1, a1, -1221
	slli	a1, a1, 13
	addi	a1, a1, 365
	slli	a1, a1, 12
	addi	a1, a1, 1783
	slli	a1, a1, 12
	addi	a2, a1, 614
	bne	a0, a2, .LBB23_9
# %bb.6:                                # %.preheader6.i.i.preheader
	lui	a0, %hi(.L.str.1641)
	addi	a0, a0, %lo(.L.str.1641)
	lui	a1, 1048560
	addiw	a1, a1, 257
	slli	a1, a1, 16
	addi	a1, a1, 257
	slli	a1, a1, 16
	addi	a1, a1, 257
	slli	a1, a1, 15
	addi	a3, a1, 128
	lui	a1, 1044464
	addiw	a1, a1, -257
	slli	a1, a1, 16
	addi	a1, a1, -257
	slli	a1, a1, 16
	addi	a5, a1, -257
	mv	a4, s2
.LBB23_7:                               # %.preheader6.i.i
                                        # =>This Inner Loop Header: Depth=1
	add	a1, a2, a5
	not	a2, a2
	and	a2, a2, a3
	and	a1, a2, a1
	bnez	a1, .LBB23_12
# %bb.8:                                #   in Loop: Header=BB23_7 Depth=1
	ld	a2, 8(a4)
	ld	s0, 8(a0)
	addi	s1, a4, 8
	addi	a1, a0, 8
	mv	a0, a1
	mv	a4, s1
	beq	a2, s0, .LBB23_7
	j	.LBB23_1
.LBB23_9:
	addi	a1, a3, %lo(.L.str.1641)
	mv	s1, s2
	lbu	a0, 0(s1)
	bnez	a0, .LBB23_2
.LBB23_10:
	mv	a0, zero
.LBB23_11:                              # %strcmp.exit.i
	lbu	a1, 0(a1)
	andi	a0, a0, 255
	bne	a0, a1, .LBB23_62
.LBB23_12:                              # %fopen.exit
	addi	a0, sp, 16
	addi	a1, zero, 8
	call	posix_memalign
	ld	s5, 16(sp)
	sd	zero, 0(s5)
	beqz	s5, .LBB23_62
# %bb.13:                               # %fread.exit
	lui	a0, %hi(bmp_file_header)
	addi	a1, a0, %lo(bmp_file_header)
	lbu	a1, 1(a1)
	lbu	a0, %lo(bmp_file_header)(a0)
	slli	a1, a1, 8
	or	a0, a1, a0
	addi	a1, zero, 14
	lui	a2, 5
	addiw	a2, a2, -702
	sd	a1, 0(s5)
	bne	a0, a2, .LBB23_63
# %bb.14:
	lui	a0, %hi(bmp_info_header)
	addi	a1, a0, %lo(bmp_info_header)
	lwu	s8, 4(a1)
	lw	s6, 8(a1)
	lhu	a2, 12(a1)
	lhu	s1, 14(a1)
	lwu	a0, 16(a1)
	lwu	s9, 32(a1)
	addi	a1, zero, 54
	addi	a3, zero, 2
	sd	a1, 0(s5)
	bgeu	a2, a3, .LBB23_64
# %bb.15:
	bnez	a0, .LBB23_65
# %bb.16:                               # %.preheader
	srai	a0, s6, 63
	add	a1, s6, a0
	xor	s11, a1, a0
	mul	a0, s8, s1
	addiw	a0, a0, 31
	srli	a0, a0, 5
	sltiu	a1, s1, 9
	seqz	a2, s9
	and	a1, a1, a2
	mulw	a0, a0, s11
	beqz	a1, .LBB23_18
# %bb.17:
	addi	a1, zero, 1
	sllw	s9, a1, s1
.LBB23_18:                              # %.preheader
	mv	a1, zero
	slli	s0, a0, 2
	lui	a0, %hi(.L__const._Z9dib_allociiiPKhb.bpp_allowed)
	addi	a0, a0, %lo(.L__const._Z9dib_allociiiPKhb.bpp_allowed)
	addi	a2, zero, 6
.LBB23_19:                              # =>This Inner Loop Header: Depth=1
	lw	a3, 0(a0)
	beq	a3, s1, .LBB23_23
# %bb.20:                               #   in Loop: Header=BB23_19 Depth=1
	addi	a1, a1, 1
	addi	a0, a0, 4
	bne	a1, a2, .LBB23_19
.LBB23_21:                              # %_Z9dib_allociiiPKhb.exit.thread
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.2.1226)
	addi	a1, a1, %lo(.L.str.2.1226)
	sd	a1, 0(a0)
.Ltmp8:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp9:
.LBB23_22:
.LBB23_23:
	sext.w	a0, a1
	addi	a1, zero, 5
	bltu	a1, a0, .LBB23_21
# %bb.24:
	addi	a0, zero, 8
	mv	s2, zero
	bltu	a0, s1, .LBB23_26
# %bb.25:
	addi	a0, zero, 1
	sllw	s2, a0, s1
.LBB23_26:
	addi	a0, sp, 16
	addi	a1, zero, 8
	call	posix_memalign
	ld	s4, 16(sp)
	beqz	s4, .LBB23_21
# %bb.27:
	sext.w	s3, s2
	slli	a0, s3, 2
	addi	s10, a0, 40
	sext.w	s7, s0
	add	a1, s10, s7
	addi	a0, sp, 16
	call	posix_memalign
	ld	a1, 16(sp)
	sd	a1, 0(s4)
	beqz	a1, .LBB23_66
# %bb.28:
	addi	a0, zero, 40
	sw	a0, 0(a1)
	sw	s8, 4(a1)
	neg	a0, s11
	sw	a0, 8(a1)
	addi	a2, zero, 1
	sh	a2, 12(a1)
	sh	s1, 14(a1)
	sw	zero, 16(a1)
	sw	s0, 20(a1)
	sw	zero, 24(a1)
	sw	zero, 28(a1)
	sw	s2, 32(a1)
	sw	zero, 36(a1)
	addi	a0, a1, 40
	blt	s3, a2, .LBB23_31
# %bb.29:
	mv	a2, zero
	slli	a3, s2, 32
	srli	a3, a3, 32
	mv	a4, a0
.LBB23_30:                              # =>This Inner Loop Header: Depth=1
	slli	a5, a2, 16
	sw	a5, 0(a4)
	addi	a2, a2, 1
	addi	a4, a4, 4
	bne	a3, a2, .LBB23_30
.LBB23_31:                              # %_Z9dib_allociiiPKhb.exit.thread49
	ld	a2, 0(s5)
	sext.w	a3, s9
	xori	a3, a3, 1
	xori	a4, a2, 14
	or	a3, a3, a4
	bnez	a3, .LBB23_33
# %bb.32:                               # %fread.exit8.thread
	addi	a0, zero, 40
	sw	a0, 40(a1)
	lui	a0, %hi(bmp_info_header)
	addi	a0, a0, %lo(bmp_info_header)
	lw	a2, 4(a0)
	sw	a2, 44(a1)
	lw	a2, 8(a0)
	sw	a2, 48(a1)
	lh	a2, 12(a0)
	sh	a2, 52(a1)
	lh	a2, 14(a0)
	sh	a2, 54(a1)
	lw	a2, 16(a0)
	sw	a2, 56(a1)
	lw	a2, 20(a0)
	sw	a2, 60(a1)
	lw	a2, 24(a0)
	sw	a2, 64(a1)
	lw	a2, 28(a0)
	sw	a2, 68(a1)
	lw	a2, 32(a0)
	sw	a2, 72(a1)
	lw	a0, 36(a0)
	sw	a0, 76(a1)
	addi	a2, zero, 18
	sd	a2, 0(s5)
	add	a0, a1, s10
	ld	s3, 8(sp)
	j	.LBB23_37
.LBB23_33:                              # %fread.exit8
	slli	a1, s9, 32
	srli	a1, a1, 32
	addi	a3, zero, 256
	sub	a3, a3, a2
	srli	a3, a3, 2
	bltu	a3, a1, .LBB23_35
# %bb.34:                               # %fread.exit8
	mv	a3, a1
.LBB23_35:                              # %fread.exit8
	lui	a1, %hi(bmp_image_data)
	addi	a1, a1, %lo(bmp_image_data)
	add	a1, a2, a1
	slli	s0, a3, 2
	mv	a2, s0
	call	memcpy
	ld	a0, 0(s4)
	ld	a1, 0(s5)
	lw	a3, 32(a0)
	add	a2, a1, s0
	slli	a1, a3, 2
	add	a0, a1, a0
	sd	a2, 0(s5)
	xori	a1, a2, 14
	xori	a3, s7, 4
	or	a1, a3, a1
	addi	a0, a0, 40
	ld	s3, 8(sp)
	bnez	a1, .LBB23_37
# %bb.36:
	addi	a1, zero, 40
	sw	a1, 0(a0)
	lui	a1, %hi(bmp_info_header)
	addi	a1, a1, %lo(bmp_info_header)
	lw	a2, 4(a1)
	sw	a2, 4(a0)
	lw	a2, 8(a1)
	sw	a2, 8(a0)
	lh	a2, 12(a1)
	sh	a2, 12(a0)
	lh	a2, 14(a1)
	sh	a2, 14(a0)
	lw	a2, 16(a1)
	sw	a2, 16(a0)
	lw	a2, 20(a1)
	sw	a2, 20(a0)
	lw	a2, 24(a1)
	sw	a2, 24(a0)
	lw	a2, 28(a1)
	sw	a2, 28(a0)
	lw	a2, 32(a1)
	sw	a2, 32(a0)
	lw	a1, 36(a1)
	sw	a1, 36(a0)
	addi	a1, zero, 18
	j	.LBB23_42
.LBB23_37:
	xori	a1, a2, 18
	xori	a3, s7, 36
	or	a3, a3, a1
	addi	a1, zero, 54
	beqz	a3, .LBB23_42
# %bb.38:
	addi	a1, zero, 256
	sub	a1, a1, a2
	bltu	a1, s7, .LBB23_40
# %bb.39:
	addi	a3, zero, 1
	j	.LBB23_41
.LBB23_40:
	divu	a3, a1, s7
.LBB23_41:
	lui	a1, %hi(bmp_image_data)
	addi	a1, a1, %lo(bmp_image_data)
	add	a1, a2, a1
	mul	s0, a3, s7
	mv	a2, s0
	call	memcpy
	ld	a0, 0(s5)
	add	a1, s0, a0
.LBB23_42:                              # %fread.exit9
	sd	a1, 0(s5)
	bltz	s6, .LBB23_48
# %bb.43:
	ld	a0, 0(s4)
	lw	a1, 4(a0)
	lhu	a2, 14(a0)
	mul	a1, a1, a2
	addi	a1, a1, 31
	sraiw	a1, a1, 5
	lw	s1, 8(a0)
	slli	s0, a1, 2
	slli	a0, s0, 32
	srli	s7, a0, 32
	addi	a0, sp, 16
	mv	a1, s7
	call	posix_memalign
	ld	s6, 16(sp)
	beqz	s6, .LBB23_48
# %bb.44:
	srai	a0, s1, 63
	add	a1, s1, a0
	xor	a0, a1, a0
	srliw	s2, a0, 1
	beqz	s2, .LBB23_47
# %bb.45:
	ld	a1, 0(s4)
	lw	a2, 32(a1)
	slli	a2, a2, 2
	add	a1, a2, a1
	addi	s1, a1, 40
	addi	a0, a0, -1
	mul	a0, s0, a0
	slli	a0, a0, 32
	srli	a0, a0, 32
	add	s0, s1, a0
	neg	s8, s7
.LBB23_46:                              # =>This Inner Loop Header: Depth=1
	addi	s2, s2, -1
	mv	a0, s6
	mv	a1, s1
	mv	a2, s7
	call	memcpy
	mv	a0, s1
	mv	a1, s0
	mv	a2, s7
	call	memcpy
	mv	a0, s0
	mv	a1, s6
	mv	a2, s7
	call	memcpy
	add	s1, s1, s7
	slli	a0, s2, 32
	srli	a0, a0, 32
	add	s0, s0, s8
	bnez	a0, .LBB23_46
.LBB23_47:                              # %free.exit.i10
	addi	a0, s6, -8
	call	__cc_read_unchecked
	add	a1, s6, a0
	mv	a0, s6
	call	__cc_access_invalid
.LBB23_48:
	addi	a0, s5, -8
	call	__cc_read_unchecked
	add	a1, s5, a0
	mv	a0, s5
	call	__cc_access_invalid
	beqz	s4, .LBB23_60
# %bb.49:
	lui	a0, %hi(.L.str.1224)
	addi	a0, a0, %lo(.L.str.1224)
	ld	s0, 24(s3)
	sd	a0, 16(s3)
	sd	s4, 24(s3)
	addi	a0, zero, 1
	sb	a0, 8(s3)
	beqz	s0, .LBB23_53
# %bb.50:
	ld	s1, 0(s0)
	beqz	s1, .LBB23_52
# %bb.51:
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB23_52:                              # %free.exit1.i
	sd	zero, 0(s0)
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	call	__cc_access_invalid
.LBB23_53:                              # %_Z8dib_freeP5CLDIB.exit
	ld	a0, 0(s4)
	lhu	a0, 14(a0)
	ld	s0, 40(s3)
	sw	a0, 32(s3)
	beqz	s0, .LBB23_55
# %bb.54:
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	call	__cc_access_invalid
.LBB23_55:                              # %free.exit.i
	sd	zero, 40(s3)
	ld	s0, 0(sp)
	addi	a0, s0, -1
.LBB23_56:                              # =>This Inner Loop Header: Depth=1
	lbu	a1, 1(a0)
	addi	a0, a0, 1
	bnez	a1, .LBB23_56
# %bb.57:                               # %strlen.exit.i.i
	sub	a0, a0, s0
	addi	s2, a0, 1
	addi	a0, sp, 16
	mv	a1, s2
	call	posix_memalign
	ld	s1, 16(sp)
	beqz	s1, .LBB23_59
# %bb.58:
	mv	a0, s1
	mv	a1, s0
	mv	a2, s2
	call	memcpy
.LBB23_59:                              # %_ZN8CImgFile7SetPathEPKc.exit
	sd	s1, 40(s3)
	addi	a0, zero, 1
	j	.LBB23_61
.LBB23_60:
	mv	a0, zero
.LBB23_61:                              # %.thread50
	ld	s11, 24(sp)
	ld	s10, 32(sp)
	ld	s9, 40(sp)
	ld	s8, 48(sp)
	ld	s7, 56(sp)
	ld	s6, 64(sp)
	ld	s5, 72(sp)
	ld	s4, 80(sp)
	ld	s3, 88(sp)
	ld	s2, 96(sp)
	ld	s1, 104(sp)
	ld	s0, 112(sp)
	ld	ra, 120(sp)
	addi	sp, sp, 128
	ret
.LBB23_62:                              # %fopen.exit.thread
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.3.1227)
	addi	a1, a1, %lo(.L.str.3.1227)
	sd	a1, 0(a0)
.Ltmp14:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp15:
	j	.LBB23_22
.LBB23_63:
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.5.1229)
	addi	a1, a1, %lo(.L.str.5.1229)
	sd	a1, 0(a0)
.Ltmp3:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp4:
	j	.LBB23_22
.LBB23_64:
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.1173)
	addi	a1, a1, %lo(.L.str.1173)
	sd	a1, 0(a0)
.Ltmp11:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp12:
	j	.LBB23_22
.LBB23_65:
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.1.1174)
	addi	a1, a1, %lo(.L.str.1.1174)
	sd	a1, 0(a0)
.Ltmp6:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp7:
	j	.LBB23_22
.LBB23_66:                              # %free.exit.i7
	addi	a0, s4, -8
	call	__cc_read_unchecked
	add	a1, s4, a0
	mv	a0, s4
	call	__cc_access_invalid
	j	.LBB23_21
.LBB23_67:
.Ltmp5:
	j	.LBB23_71
.LBB23_68:
.Ltmp16:
	mv	s5, zero
	addi	s0, zero, 1
	j	.LBB23_72
.LBB23_69:
.Ltmp13:
	j	.LBB23_71
.LBB23_70:
.Ltmp10:
.LBB23_71:
	mv	s0, zero
.LBB23_72:
	slli	a1, a1, 32
	srli	a1, a1, 32
	addi	a2, zero, 1
	bne	a1, a2, .LBB23_74
# %bb.73:
	call	__cxa_begin_catch
	ld	s3, 8(sp)
	sd	a0, 16(s3)
	call	__cxa_end_catch
	mv	s4, zero
	mv	a0, zero
	beqz	s0, .LBB23_48
	j	.LBB23_61
.LBB23_74:
	call	_Unwind_Resume
.Lfunc_end23:
	.size	_ZN8CBmpFile4LoadEPKc, .Lfunc_end23-_ZN8CBmpFile4LoadEPKc
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table23:
.Lexception1:
	.byte	255                             # @LPStart Encoding = omit
	.byte	155                             # @TType Encoding = indirect pcrel sdata4
	.uleb128 .Lttbase0-.Lttbaseref0
.Lttbaseref0:
	.byte	3                               # Call site Encoding = udata4
	.uleb128 .Lcst_end1-.Lcst_begin1
.Lcst_begin1:
	.word	.Lfunc_begin1-.Lfunc_begin1     # >> Call Site 1 <<
	.word	.Ltmp8-.Lfunc_begin1            #   Call between .Lfunc_begin1 and .Ltmp8
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp8-.Lfunc_begin1            # >> Call Site 2 <<
	.word	.Ltmp9-.Ltmp8                   #   Call between .Ltmp8 and .Ltmp9
	.word	.Ltmp10-.Lfunc_begin1           #     jumps to .Ltmp10
	.byte	3                               #   On action: 2
	.word	.Ltmp9-.Lfunc_begin1            # >> Call Site 3 <<
	.word	.Ltmp14-.Ltmp9                  #   Call between .Ltmp9 and .Ltmp14
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp14-.Lfunc_begin1           # >> Call Site 4 <<
	.word	.Ltmp15-.Ltmp14                 #   Call between .Ltmp14 and .Ltmp15
	.word	.Ltmp16-.Lfunc_begin1           #     jumps to .Ltmp16
	.byte	3                               #   On action: 2
	.word	.Ltmp15-.Lfunc_begin1           # >> Call Site 5 <<
	.word	.Ltmp3-.Ltmp15                  #   Call between .Ltmp15 and .Ltmp3
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp3-.Lfunc_begin1            # >> Call Site 6 <<
	.word	.Ltmp4-.Ltmp3                   #   Call between .Ltmp3 and .Ltmp4
	.word	.Ltmp5-.Lfunc_begin1            #     jumps to .Ltmp5
	.byte	3                               #   On action: 2
	.word	.Ltmp4-.Lfunc_begin1            # >> Call Site 7 <<
	.word	.Ltmp11-.Ltmp4                  #   Call between .Ltmp4 and .Ltmp11
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp11-.Lfunc_begin1           # >> Call Site 8 <<
	.word	.Ltmp12-.Ltmp11                 #   Call between .Ltmp11 and .Ltmp12
	.word	.Ltmp13-.Lfunc_begin1           #     jumps to .Ltmp13
	.byte	3                               #   On action: 2
	.word	.Ltmp12-.Lfunc_begin1           # >> Call Site 9 <<
	.word	.Ltmp6-.Ltmp12                  #   Call between .Ltmp12 and .Ltmp6
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp6-.Lfunc_begin1            # >> Call Site 10 <<
	.word	.Ltmp7-.Ltmp6                   #   Call between .Ltmp6 and .Ltmp7
	.word	.Ltmp13-.Lfunc_begin1           #     jumps to .Ltmp13
	.byte	3                               #   On action: 2
	.word	.Ltmp7-.Lfunc_begin1            # >> Call Site 11 <<
	.word	.Lfunc_end23-.Ltmp7             #   Call between .Ltmp7 and .Lfunc_end23
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
.Lcst_end1:
	.byte	0                               # >> Action Record 1 <<
                                        #   Cleanup
	.byte	0                               #   No further actions
	.byte	1                               # >> Action Record 2 <<
                                        #   Catch TypeInfo 1
	.byte	125                             #   Continue to action 1
	.p2align	2
                                        # >> Catch TypeInfos <<
.Ltmp17:                                # TypeInfo 1
	.word	.L_ZTIPKc.DW.stub-.Ltmp17
.Lttbase0:
	.p2align	2
                                        # -- End function
	.text
	.p2align	2                               # -- Begin function _ZN8CImgFileD2Ev
	.type	_ZN8CImgFileD2Ev,@function
_ZN8CImgFileD2Ev:                       # @_ZN8CImgFileD2Ev
	.cfi_startproc
# %bb.0:
	lui	a1, %hi(_ZTV8CImgFile+16)
	addi	a1, a1, %lo(_ZTV8CImgFile+16)
	sd	a1, 0(a0)
	tail	_ZN8CImgFile5ClearEv
.Lfunc_end24:
	.size	_ZN8CImgFileD2Ev, .Lfunc_end24-_ZN8CImgFileD2Ev
	.cfi_endproc
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CImgFileD0Ev
	.type	_ZN8CImgFileD0Ev,@function
_ZN8CImgFileD0Ev:                       # @_ZN8CImgFileD0Ev
# %bb.0:
	unimp	
.Lfunc_end25:
	.size	_ZN8CImgFileD0Ev, .Lfunc_end25-_ZN8CImgFileD0Ev
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CImgFile5ClearEv
	.type	_ZN8CImgFile5ClearEv,@function
_ZN8CImgFile5ClearEv:                   # @_ZN8CImgFile5ClearEv
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	sd	s1, 8(sp)
	sd	s2, 0(sp)
	mv	s0, a0
	ld	s1, 24(a0)
	beqz	s1, .LBB26_4
# %bb.1:
	ld	s2, 0(s1)
	beqz	s2, .LBB26_3
# %bb.2:
	addi	a0, s2, -8
	call	__cc_read_unchecked
	add	a1, s2, a0
	mv	a0, s2
	call	__cc_access_invalid
.LBB26_3:                               # %free.exit1.i
	sd	zero, 0(s1)
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB26_4:                               # %_Z8dib_freeP5CLDIB.exit
	ld	s1, 40(s0)
	addi	a0, zero, 8
	sw	a0, 32(s0)
	sd	zero, 24(s0)
	beqz	s1, .LBB26_6
# %bb.5:
	addi	a0, s1, -8
	call	__cc_read_unchecked
	add	a1, s1, a0
	mv	a0, s1
	call	__cc_access_invalid
.LBB26_6:                               # %free.exit
	sd	zero, 40(s0)
	sb	zero, 8(s0)
	ld	s2, 0(sp)
	ld	s1, 8(sp)
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end26:
	.size	_ZN8CImgFile5ClearEv, .Lfunc_end26-_ZN8CImgFile5ClearEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CImgFile5VMakeEv
	.type	_ZN8CImgFile5VMakeEv,@function
_ZN8CImgFile5VMakeEv:                   # @_ZN8CImgFile5VMakeEv
# %bb.0:
	mv	a0, zero
	ret
.Lfunc_end27:
	.size	_ZN8CImgFile5VMakeEv, .Lfunc_end27-_ZN8CImgFile5VMakeEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CImgFile7GetTypeEv
	.type	_ZNK8CImgFile7GetTypeEv,@function
_ZNK8CImgFile7GetTypeEv:                # @_ZNK8CImgFile7GetTypeEv
# %bb.0:
	addi	a0, zero, -1
	ret
.Lfunc_end28:
	.size	_ZNK8CImgFile7GetTypeEv, .Lfunc_end28-_ZNK8CImgFile7GetTypeEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CImgFile6GetExtEv
	.type	_ZNK8CImgFile6GetExtEv,@function
_ZNK8CImgFile6GetExtEv:                 # @_ZNK8CImgFile6GetExtEv
# %bb.0:
	lui	a0, %hi(.L.str.14.1236)
	addi	a0, a0, %lo(.L.str.14.1236)
	ret
.Lfunc_end29:
	.size	_ZNK8CImgFile6GetExtEv, .Lfunc_end29-_ZNK8CImgFile6GetExtEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CImgFile7GetDescEv
	.type	_ZNK8CImgFile7GetDescEv,@function
_ZNK8CImgFile7GetDescEv:                # @_ZNK8CImgFile7GetDescEv
# %bb.0:
	lui	a0, %hi(.L.str.14.1236)
	addi	a0, a0, %lo(.L.str.14.1236)
	ret
.Lfunc_end30:
	.size	_ZNK8CImgFile7GetDescEv, .Lfunc_end30-_ZNK8CImgFile7GetDescEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CImgFile9GetFormatEv
	.type	_ZNK8CImgFile9GetFormatEv,@function
_ZNK8CImgFile9GetFormatEv:              # @_ZNK8CImgFile9GetFormatEv
# %bb.0:
	lui	a0, %hi(.L.str.14.1236)
	addi	a0, a0, %lo(.L.str.14.1236)
	ret
.Lfunc_end31:
	.size	_ZNK8CImgFile9GetFormatEv, .Lfunc_end31-_ZNK8CImgFile9GetFormatEv
                                        # -- End function
	.p2align	2                               # -- Begin function posix_memalign
	.type	posix_memalign,@function
posix_memalign:                         # @posix_memalign
# %bb.0:
	addi	sp, sp, -32
	sd	ra, 24(sp)
	sd	s0, 16(sp)
	sd	s1, 8(sp)
	lui	s1, %hi(pos)
	ld	a2, %lo(pos)(s1)
	mv	s0, a1
	bnez	a2, .LBB32_2
# %bb.1:
	addi	a1, zero, 1
	slli	a2, a1, 32
	sd	a2, %lo(pos)(s1)
.LBB32_2:
	addi	a1, a2, 23
	andi	a2, a1, -16
	sd	a2, %lo(pos)(s1)
	sd	a2, 0(a0)
	add	a1, a2, s0
	mv	a0, a2
	call	__cc_access_valid
	ld	a0, %lo(pos)(s1)
	addi	a0, a0, -8
	mv	a1, s0
	call	__cc_write_unchecked
	ld	a0, %lo(pos)(s1)
	add	s0, a0, s0
	addi	a0, s0, 64
	sd	a0, %lo(pos)(s1)
	addi	a1, zero, 64
	mv	a0, s0
	call	__cc_advise_poison_offset
	addi	a1, zero, 63
	bltu	a1, a0, .LBB32_6
# %bb.3:
	add	s0, s0, a0
	andi	a0, s0, 7
	beqz	a0, .LBB32_5
# %bb.4:
	call	__cc_flag_invalid
.LBB32_5:
	mv	a0, s0
	mv	a1, zero
	ld	s1, 8(sp)
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	tail	__cc_write_and_poison
.LBB32_6:
	ld	s1, 8(sp)
	ld	s0, 16(sp)
	ld	ra, 24(sp)
	addi	sp, sp, 32
	ret
.Lfunc_end32:
	.size	posix_memalign, .Lfunc_end32-posix_memalign
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CBmpFileD0Ev
	.type	_ZN8CBmpFileD0Ev,@function
_ZN8CBmpFileD0Ev:                       # @_ZN8CBmpFileD0Ev
	.cfi_startproc
# %bb.0:
	addi	sp, sp, -16
	.cfi_def_cfa_offset 16
	sd	ra, 8(sp)
	sd	s0, 0(sp)
	.cfi_offset ra, -8
	.cfi_offset s0, -16
	mv	s0, a0
	lui	a0, %hi(_ZTV8CImgFile+16)
	addi	a0, a0, %lo(_ZTV8CImgFile+16)
	sd	a0, 0(s0)
	mv	a0, s0
	call	_ZN8CImgFile5ClearEv
	beqz	s0, .LBB33_2
# %bb.1:
	addi	a0, s0, -8
	call	__cc_read_unchecked
	add	a1, s0, a0
	mv	a0, s0
	ld	s0, 0(sp)
	ld	ra, 8(sp)
	addi	sp, sp, 16
	tail	__cc_access_invalid
.LBB33_2:                               # %free.exit
	ld	s0, 0(sp)
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end33:
	.size	_ZN8CBmpFileD0Ev, .Lfunc_end33-_ZN8CBmpFileD0Ev
	.cfi_endproc
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CBmpFile5VMakeEv
	.type	_ZN8CBmpFile5VMakeEv,@function
_ZN8CBmpFile5VMakeEv:                   # @_ZN8CBmpFile5VMakeEv
	.cfi_startproc
# %bb.0:
	addi	sp, sp, -16
	.cfi_def_cfa_offset 16
	sd	ra, 8(sp)
	.cfi_offset ra, -8
	mv	a0, sp
	addi	a1, zero, 48
	call	posix_memalign
	ld	a0, 0(sp)
	sb	zero, 8(a0)
	sd	zero, 16(a0)
	sd	zero, 24(a0)
	addi	a1, zero, 8
	sw	a1, 32(a0)
	sd	zero, 40(a0)
	lui	a1, %hi(_ZTV8CBmpFile+16)
	addi	a1, a1, %lo(_ZTV8CBmpFile+16)
	sd	a1, 0(a0)
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end34:
	.size	_ZN8CBmpFile5VMakeEv, .Lfunc_end34-_ZN8CBmpFile5VMakeEv
	.cfi_endproc
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CBmpFile7GetTypeEv
	.type	_ZNK8CBmpFile7GetTypeEv,@function
_ZNK8CBmpFile7GetTypeEv:                # @_ZNK8CBmpFile7GetTypeEv
# %bb.0:
	mv	a0, zero
	ret
.Lfunc_end35:
	.size	_ZNK8CBmpFile7GetTypeEv, .Lfunc_end35-_ZNK8CBmpFile7GetTypeEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CBmpFile6GetExtEv
	.type	_ZNK8CBmpFile6GetExtEv,@function
_ZNK8CBmpFile6GetExtEv:                 # @_ZNK8CBmpFile6GetExtEv
# %bb.0:
	lui	a0, %hi(.L.str.4.1188)
	addi	a0, a0, %lo(.L.str.4.1188)
	ret
.Lfunc_end36:
	.size	_ZNK8CBmpFile6GetExtEv, .Lfunc_end36-_ZNK8CBmpFile6GetExtEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CBmpFile7GetDescEv
	.type	_ZNK8CBmpFile7GetDescEv,@function
_ZNK8CBmpFile7GetDescEv:                # @_ZNK8CBmpFile7GetDescEv
# %bb.0:
	lui	a0, %hi(.L.str.5.1187)
	addi	a0, a0, %lo(.L.str.5.1187)
	ret
.Lfunc_end37:
	.size	_ZNK8CBmpFile7GetDescEv, .Lfunc_end37-_ZNK8CBmpFile7GetDescEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZNK8CBmpFile9GetFormatEv
	.type	_ZNK8CBmpFile9GetFormatEv,@function
_ZNK8CBmpFile9GetFormatEv:              # @_ZNK8CBmpFile9GetFormatEv
# %bb.0:
	lui	a0, %hi(.L.str.6.1186)
	addi	a0, a0, %lo(.L.str.6.1186)
	ret
.Lfunc_end38:
	.size	_ZNK8CBmpFile9GetFormatEv, .Lfunc_end38-_ZNK8CBmpFile9GetFormatEv
                                        # -- End function
	.p2align	2                               # -- Begin function _ZN8CBmpFile4SaveEPKc
	.type	_ZN8CBmpFile4SaveEPKc,@function
_ZN8CBmpFile4SaveEPKc:                  # @_ZN8CBmpFile4SaveEPKc
.Lfunc_begin2:
	.cfi_startproc
	.cfi_personality 155, DW.ref.__gxx_personality_v0
	.cfi_lsda 27, .Lexception2
# %bb.0:
	addi	sp, sp, -16
	.cfi_def_cfa_offset 16
	sd	ra, 8(sp)
	sd	s0, 0(sp)
	.cfi_offset ra, -8
	.cfi_offset s0, -16
	mv	s0, a0
	ld	a0, 24(a0)
	bnez	a0, .LBB39_2
# %bb.1:
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.1.1225)
	addi	a1, a1, %lo(.L.str.1.1225)
	sd	a1, 0(a0)
.Ltmp20:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp21:
	j	.LBB39_13
.LBB39_2:
	lui	a2, %hi(.L.str.1641)
	addi	a0, a2, %lo(.L.str.1641)
	or	a3, a1, a0
	andi	a3, a3, 7
	bnez	a3, .LBB39_8
# %bb.3:
	ld	a0, 0(a1)
	lui	a3, 14643
	addiw	a3, a3, -1221
	slli	a3, a3, 13
	addi	a3, a3, 365
	slli	a3, a3, 12
	addi	a3, a3, 1783
	slli	a3, a3, 12
	addi	a3, a3, 614
	bne	a0, a3, .LBB39_7
# %bb.4:                                # %.preheader6.i.i.preheader
	lui	a0, %hi(.L.str.1641)
	addi	a2, a0, %lo(.L.str.1641)
	lui	a0, 1048560
	addiw	a0, a0, 257
	slli	a0, a0, 16
	addi	a0, a0, 257
	slli	a0, a0, 16
	addi	a0, a0, 257
	slli	a0, a0, 15
	addi	a6, a0, 128
	lui	a0, 1044464
	addiw	a0, a0, -257
	slli	a0, a0, 16
	addi	a0, a0, -257
	slli	a0, a0, 16
	addi	a5, a0, -257
	mv	a4, a1
.LBB39_5:                               # %.preheader6.i.i
                                        # =>This Inner Loop Header: Depth=1
	add	a0, a3, a5
	not	a1, a3
	and	a1, a1, a6
	and	a0, a1, a0
	bnez	a0, .LBB39_12
# %bb.6:                                #   in Loop: Header=BB39_5 Depth=1
	ld	a3, 8(a4)
	ld	a7, 8(a2)
	addi	a1, a4, 8
	addi	a0, a2, 8
	mv	a2, a0
	mv	a4, a1
	beq	a3, a7, .LBB39_5
	j	.LBB39_8
.LBB39_7:
	addi	a0, a2, %lo(.L.str.1641)
.LBB39_8:
	lbu	a2, 0(a1)
	beqz	a2, .LBB39_12
# %bb.9:                                # %.preheader.i.i.preheader
	addi	a1, a1, 1
.LBB39_10:                              # %.preheader.i.i
                                        # =>This Inner Loop Header: Depth=1
	lbu	a3, 0(a0)
	andi	a2, a2, 255
	bne	a2, a3, .LBB39_12
# %bb.11:                               #   in Loop: Header=BB39_10 Depth=1
	lbu	a2, 0(a1)
	addi	a0, a0, 1
	addi	a1, a1, 1
	bnez	a2, .LBB39_10
.LBB39_12:                              # %fopen.exit.thread
	addi	a0, zero, 8
	call	__cxa_allocate_exception
	lui	a1, %hi(.L.str.3.1227)
	addi	a1, a1, %lo(.L.str.3.1227)
	sd	a1, 0(a0)
.Ltmp18:
	lui	a1, %hi(_ZTIPKc)
	addi	a1, a1, %lo(_ZTIPKc)
	mv	a2, zero
	call	__cxa_throw
.Ltmp19:
.LBB39_13:
.LBB39_14:
.Ltmp22:
	slli	a1, a1, 32
	srli	a1, a1, 32
	addi	a2, zero, 1
	bne	a1, a2, .LBB39_16
# %bb.15:                               # %fclose.exit
	call	__cxa_begin_catch
	sd	a0, 16(s0)
	call	__cxa_end_catch
	mv	a0, zero
	ld	s0, 0(sp)
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.LBB39_16:
	call	_Unwind_Resume
.Lfunc_end39:
	.size	_ZN8CBmpFile4SaveEPKc, .Lfunc_end39-_ZN8CBmpFile4SaveEPKc
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table39:
.Lexception2:
	.byte	255                             # @LPStart Encoding = omit
	.byte	155                             # @TType Encoding = indirect pcrel sdata4
	.uleb128 .Lttbase1-.Lttbaseref1
.Lttbaseref1:
	.byte	3                               # Call site Encoding = udata4
	.uleb128 .Lcst_end2-.Lcst_begin2
.Lcst_begin2:
	.word	.Lfunc_begin2-.Lfunc_begin2     # >> Call Site 1 <<
	.word	.Ltmp20-.Lfunc_begin2           #   Call between .Lfunc_begin2 and .Ltmp20
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp20-.Lfunc_begin2           # >> Call Site 2 <<
	.word	.Ltmp21-.Ltmp20                 #   Call between .Ltmp20 and .Ltmp21
	.word	.Ltmp22-.Lfunc_begin2           #     jumps to .Ltmp22
	.byte	3                               #   On action: 2
	.word	.Ltmp21-.Lfunc_begin2           # >> Call Site 3 <<
	.word	.Ltmp18-.Ltmp21                 #   Call between .Ltmp21 and .Ltmp18
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
	.word	.Ltmp18-.Lfunc_begin2           # >> Call Site 4 <<
	.word	.Ltmp19-.Ltmp18                 #   Call between .Ltmp18 and .Ltmp19
	.word	.Ltmp22-.Lfunc_begin2           #     jumps to .Ltmp22
	.byte	3                               #   On action: 2
	.word	.Ltmp19-.Lfunc_begin2           # >> Call Site 5 <<
	.word	.Lfunc_end39-.Ltmp19            #   Call between .Ltmp19 and .Lfunc_end39
	.word	0                               #     has no landing pad
	.byte	0                               #   On action: cleanup
.Lcst_end2:
	.byte	0                               # >> Action Record 1 <<
                                        #   Cleanup
	.byte	0                               #   No further actions
	.byte	1                               # >> Action Record 2 <<
                                        #   Catch TypeInfo 1
	.byte	125                             #   Continue to action 1
	.p2align	2
                                        # >> Catch TypeInfos <<
.Ltmp23:                                # TypeInfo 1
	.word	.L_ZTIPKc.DW.stub-.Ltmp23
.Lttbase1:
	.p2align	2
                                        # -- End function
	.section	.fini_array.101,"aw",@fini_array
	.p2align	3
	.quad	posix_exit
	.type	_ZTV8CBmpFile,@object           # @_ZTV8CBmpFile
	.section	.rodata,"a",@progbits
	.p2align	3
_ZTV8CBmpFile:
	.quad	0
	.quad	0
	.quad	_ZN8CImgFileD2Ev
	.quad	_ZN8CBmpFileD0Ev
	.quad	_ZN8CImgFile5ClearEv
	.quad	_ZN8CBmpFile5VMakeEv
	.quad	_ZNK8CBmpFile7GetTypeEv
	.quad	_ZNK8CBmpFile6GetExtEv
	.quad	_ZNK8CBmpFile7GetDescEv
	.quad	_ZNK8CBmpFile9GetFormatEv
	.quad	_ZN8CBmpFile4LoadEPKc
	.quad	_ZN8CBmpFile4SaveEPKc
	.size	_ZTV8CBmpFile, 96

	.type	.L.str.1641,@object             # @.str.1641
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1641:
	.asciz	"fromager.bmp"
	.size	.L.str.1641, 13

	.type	_ZTV8CImgFile,@object           # @_ZTV8CImgFile
	.section	.rodata,"a",@progbits
	.p2align	3
_ZTV8CImgFile:
	.quad	0
	.quad	0
	.quad	_ZN8CImgFileD2Ev
	.quad	_ZN8CImgFileD0Ev
	.quad	_ZN8CImgFile5ClearEv
	.quad	_ZN8CImgFile5VMakeEv
	.quad	_ZNK8CImgFile7GetTypeEv
	.quad	_ZNK8CImgFile6GetExtEv
	.quad	_ZNK8CImgFile7GetDescEv
	.quad	_ZNK8CImgFile9GetFormatEv
	.quad	__cxa_pure_virtual
	.quad	__cxa_pure_virtual
	.size	_ZTV8CImgFile, 96

	.type	.L.str.14.1236,@object          # @.str.14.1236
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.14.1236:
	.zero	1
	.size	.L.str.14.1236, 1

	.type	.L.str.3.1227,@object           # @.str.3.1227
.L.str.3.1227:
	.asciz	"File doesn't exist."
	.size	.L.str.3.1227, 20

	.type	.L.str.5.1229,@object           # @.str.5.1229
.L.str.5.1229:
	.asciz	"Ack! File format not recognized."
	.size	.L.str.5.1229, 33

	.type	.L.str.1173,@object             # @.str.1173
.L.str.1173:
	.asciz	"Multiple bitplanes are unsupported"
	.size	.L.str.1173, 35

	.type	.L.str.1.1174,@object           # @.str.1.1174
.L.str.1.1174:
	.asciz	"BMP compression unsupported"
	.size	.L.str.1.1174, 28

	.type	.L__const._Z9dib_allociiiPKhb.bpp_allowed,@object # @__const._Z9dib_allociiiPKhb.bpp_allowed
	.section	.rodata,"a",@progbits
	.p2align	2
.L__const._Z9dib_allociiiPKhb.bpp_allowed:
	.word	1                               # 0x1
	.word	4                               # 0x4
	.word	8                               # 0x8
	.word	16                              # 0x10
	.word	24                              # 0x18
	.word	32                              # 0x20
	.size	.L__const._Z9dib_allociiiPKhb.bpp_allowed, 24

	.type	.L.str.2.1226,@object           # @.str.2.1226
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.2.1226:
	.asciz	"Memory allocation error."
	.size	.L.str.2.1226, 25

	.type	.L.str.1224,@object             # @.str.1224
.L.str.1224:
	.asciz	"All clear!"
	.size	.L.str.1224, 11

	.type	pos,@object                     # @pos
	.local	pos
	.comm	pos,8,8
	.type	.L.str.1.1225,@object           # @.str.1.1225
.L.str.1.1225:
	.asciz	"Non-descript error :(."
	.size	.L.str.1.1225, 23

	.type	.L.str.6.1186,@object           # @.str.6.1186
.L.str.6.1186:
	.asciz	"BMP"
	.size	.L.str.6.1186, 4

	.type	.L.str.5.1187,@object           # @.str.5.1187
.L.str.5.1187:
	.asciz	"Windows Bitmap"
	.size	.L.str.5.1187, 15

	.type	.L.str.4.1188,@object           # @.str.4.1188
.L.str.4.1188:
	.asciz	"bmp"
	.size	.L.str.4.1188, 4

	.type	bmp_file_header,@object         # @bmp_file_header
	.section	.data.secret,"aw",@progbits
	.globl	bmp_file_header
bmp_file_header:
	.half	19778                           # 0x4d42
	.word	102                             # 0x66
	.half	0                               # 0x0
	.half	0                               # 0x0
	.word	54                              # 0x36
	.size	bmp_file_header, 14

	.type	bmp_info_header,@object         # @bmp_info_header
	.globl	bmp_info_header
	.p2align	2
bmp_info_header:
	.word	40                              # 0x28
	.word	4                               # 0x4
	.word	4                               # 0x4
	.half	1                               # 0x1
	.half	24                              # 0x18
	.word	0                               # 0x0
	.word	48                              # 0x30
	.word	2835                            # 0xb13
	.word	2835                            # 0xb13
	.word	16                              # 0x10
	.word	0                               # 0x0
	.size	bmp_info_header, 40

	.type	bmp_image_data,@object          # @bmp_image_data
	.globl	bmp_image_data
bmp_image_data:
	.zero	256
	.size	bmp_image_data, 256

	.data
	.p2align	3
.L_ZTIPKc.DW.stub:
	.quad	_ZTIPKc
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.DW.ref.__gxx_personality_v0,"aGw",@progbits,DW.ref.__gxx_personality_v0,comdat
	.p2align	3
	.type	DW.ref.__gxx_personality_v0,@object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.ident	"Debian clang version 11.0.1-2"
	.section	".note.GNU-stack","",@progbits
