; ModuleID = 'wrongFree.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@SECRET_BOUND = local_unnamed_addr global i32 144, section "__DATA,__secret", align 4
@SECRET_NUMBER = local_unnamed_addr global i32 42, section "__DATA,__secret", align 4

; Function Attrs: nounwind ssp uwtable
define i32 @main() local_unnamed_addr #0 {
  %1 = tail call i8* @__cc_malloc(i64 12) #5
  %2 = ptrtoint i8* %1 to i64
  %3 = lshr i64 %2, 58
  %4 = shl nuw i64 1, %3
  %5 = icmp ult i8* %1, inttoptr (i64 1152921504606846976 to i8*)
  %6 = add i64 %4, -1
  %7 = and i64 %6, %2
  %8 = icmp ne i64 %7, 0
  %9 = or i1 %5, %8
  br i1 %9, label %10, label %11

10:                                               ; preds = %0
  tail call void @__cc_flag_invalid() #5
  br label %11

11:                                               ; preds = %10, %0
  %12 = getelementptr inbounds i8, i8* %1, i64 %4
  %13 = getelementptr inbounds i8, i8* %12, i64 -1
  tail call void @__cc_write_and_poison(i8* nonnull %13, i64 1) #5
  %14 = getelementptr inbounds i8, i8* %1, i64 12
  %15 = tail call i8* @__cc_advise_poison(i8* nonnull %14, i8* nonnull %13) #5
  %16 = icmp eq i8* %15, null
  br i1 %16, label %malloc.exit, label %17

17:                                               ; preds = %11
  %18 = icmp ugt i8* %14, %15
  %19 = icmp uge i8* %15, %13
  %20 = or i1 %18, %19
  br i1 %20, label %21, label %22

21:                                               ; preds = %17
  tail call void @__cc_flag_invalid() #5
  br label %22

22:                                               ; preds = %21, %17
  tail call void @__cc_write_and_poison(i8* nonnull %15, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %11, %22
  %23 = bitcast i8* %1 to i32*
  store i32 21, i32* %23, align 4, !tbaa !4
  %24 = getelementptr inbounds i8, i8* %1, i64 4
  %25 = bitcast i8* %24 to i32*
  store i32 22, i32* %25, align 4, !tbaa !4
  %26 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  %27 = getelementptr inbounds i8, i8* %1, i64 8
  %28 = bitcast i8* %27 to i32*
  store i32 %26, i32* %28, align 4, !tbaa !4
  %29 = load i32, i32* @SECRET_BOUND, align 4, !tbaa !4
  %30 = icmp slt i32 %29, 145
  br i1 %30, label %31, label %free.exit.thread

31:                                               ; preds = %malloc.exit
  %32 = getelementptr inbounds i8, i8* %1, i64 20
  %33 = ptrtoint i8* %32 to i64
  %34 = lshr i64 %33, 58
  %35 = shl nuw i64 1, %34
  %36 = add i64 %35, -1
  %37 = and i64 %36, %33
  %38 = icmp eq i64 %37, 0
  br i1 %38, label %40, label %39

39:                                               ; preds = %31
  tail call void @__cc_flag_bug() #5
  br label %40

40:                                               ; preds = %39, %31
  store i8 0, i8* %32, align 1, !tbaa !8
  tail call void @__cc_free(i8* nonnull %32) #5
  %41 = getelementptr inbounds i8, i8* %32, i64 %35
  %42 = getelementptr inbounds i8, i8* %41, i64 -1
  %43 = tail call i8* @__cc_advise_poison(i8* nonnull %32, i8* nonnull %42) #5
  %44 = icmp eq i8* %43, null
  br i1 %44, label %free.exit, label %45

45:                                               ; preds = %40
  %46 = icmp ult i8* %43, %32
  %47 = icmp uge i8* %43, %42
  %48 = or i1 %46, %47
  br i1 %48, label %49, label %50

49:                                               ; preds = %45
  tail call void @__cc_flag_invalid() #5
  br label %50

50:                                               ; preds = %49, %45
  tail call void @__cc_write_and_poison(i8* nonnull %43, i64 0) #5
  br label %free.exit

free.exit:                                        ; preds = %50, %40
  %.pr = load i32, i32* @SECRET_BOUND, align 4, !tbaa !4
  %51 = icmp sgt i32 %.pr, 143
  br i1 %51, label %free.exit.free.exit.thread_crit_edge, label %free.exit.thread

free.exit.free.exit.thread_crit_edge:             ; preds = %free.exit
  %.pre = load i32, i32* %23, align 4, !tbaa !4
  br label %free.exit.thread

free.exit.thread:                                 ; preds = %malloc.exit, %free.exit.free.exit.thread_crit_edge, %free.exit
  %52 = phi i32 [ %26, %free.exit ], [ %.pre, %free.exit.free.exit.thread_crit_edge ], [ 21, %malloc.exit ]
  ret i32 %52
}

declare i8* @__cc_malloc(i64) local_unnamed_addr #1

declare void @__cc_flag_invalid() local_unnamed_addr #1

declare void @__cc_write_and_poison(i8*, i64) local_unnamed_addr #1

declare i8* @__cc_advise_poison(i8*, i8*) local_unnamed_addr #1

declare void @__cc_flag_bug() local_unnamed_addr #1

declare void @__cc_free(i8*) local_unnamed_addr #1

; Function Attrs:  norecurse nounwind ssp uwtable
define void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #2 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %.loopexit, label %5

5:                                                ; preds = %3
  %6 = add i64 %2, -1
  %7 = and i64 %2, 3
  %8 = icmp ult i64 %6, 3
  br i1 %8, label %.loopexit2, label %9

9:                                                ; preds = %5
  %10 = sub nuw i64 %2, %7
  br label %21

.loopexit2:                                       ; preds = %21, %5
  %11 = phi i64 [ 0, %5 ], [ %39, %21 ]
  %12 = icmp eq i64 %7, 0
  br i1 %12, label %.loopexit, label %.preheader

.preheader:                                       ; preds = %.loopexit2, %.preheader
  %13 = phi i64 [ %18, %.preheader ], [ %11, %.loopexit2 ]
  %14 = phi i64 [ %19, %.preheader ], [ %7, %.loopexit2 ]
  %15 = getelementptr inbounds i8, i8* %1, i64 %13
  %16 = load i8, i8* %15, align 1, !tbaa !8
  %17 = getelementptr inbounds i8, i8* %0, i64 %13
  store i8 %16, i8* %17, align 1, !tbaa !8
  %18 = add nuw i64 %13, 1
  %19 = add nsw i64 %14, -1
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %.loopexit, label %.preheader, !llvm.loop !9

.loopexit:                                        ; preds = %.preheader, %.loopexit2, %3
  ret void

21:                                               ; preds = %21, %9
  %22 = phi i64 [ 0, %9 ], [ %39, %21 ]
  %23 = phi i64 [ %10, %9 ], [ %40, %21 ]
  %24 = getelementptr inbounds i8, i8* %1, i64 %22
  %25 = load i8, i8* %24, align 1, !tbaa !8
  %26 = getelementptr inbounds i8, i8* %0, i64 %22
  store i8 %25, i8* %26, align 1, !tbaa !8
  %27 = or i64 %22, 1
  %28 = getelementptr inbounds i8, i8* %1, i64 %27
  %29 = load i8, i8* %28, align 1, !tbaa !8
  %30 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %29, i8* %30, align 1, !tbaa !8
  %31 = or i64 %22, 2
  %32 = getelementptr inbounds i8, i8* %1, i64 %31
  %33 = load i8, i8* %32, align 1, !tbaa !8
  %34 = getelementptr inbounds i8, i8* %0, i64 %31
  store i8 %33, i8* %34, align 1, !tbaa !8
  %35 = or i64 %22, 3
  %36 = getelementptr inbounds i8, i8* %1, i64 %35
  %37 = load i8, i8* %36, align 1, !tbaa !8
  %38 = getelementptr inbounds i8, i8* %0, i64 %35
  store i8 %37, i8* %38, align 1, !tbaa !8
  %39 = add nuw i64 %22, 4
  %40 = add i64 %23, -4
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %.loopexit2, label %21
}

; Function Attrs:  norecurse nounwind ssp uwtable writeonly
define void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #3 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %.loopexit, label %5

5:                                                ; preds = %3
  %6 = add i64 %2, -1
  %7 = and i64 %2, 7
  %8 = icmp ult i64 %6, 7
  br i1 %8, label %.loopexit2, label %9

9:                                                ; preds = %5
  %10 = sub nuw i64 %2, %7
  br label %13

.loopexit2:                                       ; preds = %13, %5
  %11 = phi i64 [ 0, %5 ], [ %31, %13 ]
  %12 = icmp eq i64 %7, 0
  br i1 %12, label %.loopexit, label %.preheader.preheader

.preheader.preheader:                             ; preds = %.loopexit2
  %scevgep = getelementptr i8, i8* %0, i64 %11
  tail call void @llvm.memset.p0i8.i64(i8* align 1 %scevgep, i8 %1, i64 %7, i1 false)
  br label %.loopexit

.loopexit:                                        ; preds = %.preheader.preheader, %.loopexit2, %3
  ret void

13:                                               ; preds = %13, %9
  %14 = phi i64 [ 0, %9 ], [ %31, %13 ]
  %15 = phi i64 [ %10, %9 ], [ %32, %13 ]
  %16 = getelementptr inbounds i8, i8* %0, i64 %14
  store i8 %1, i8* %16, align 1, !tbaa !8
  %17 = or i64 %14, 1
  %18 = getelementptr inbounds i8, i8* %0, i64 %17
  store i8 %1, i8* %18, align 1, !tbaa !8
  %19 = or i64 %14, 2
  %20 = getelementptr inbounds i8, i8* %0, i64 %19
  store i8 %1, i8* %20, align 1, !tbaa !8
  %21 = or i64 %14, 3
  %22 = getelementptr inbounds i8, i8* %0, i64 %21
  store i8 %1, i8* %22, align 1, !tbaa !8
  %23 = or i64 %14, 4
  %24 = getelementptr inbounds i8, i8* %0, i64 %23
  store i8 %1, i8* %24, align 1, !tbaa !8
  %25 = or i64 %14, 5
  %26 = getelementptr inbounds i8, i8* %0, i64 %25
  store i8 %1, i8* %26, align 1, !tbaa !8
  %27 = or i64 %14, 6
  %28 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %1, i8* %28, align 1, !tbaa !8
  %29 = or i64 %14, 7
  %30 = getelementptr inbounds i8, i8* %0, i64 %29
  store i8 %1, i8* %30, align 1, !tbaa !8
  %31 = add nuw i64 %14, 8
  %32 = add i64 %15, -8
  %33 = icmp eq i64 %32, 0
  br i1 %33, label %.loopexit2, label %13
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #4

attributes #0 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-builtins" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = {  norecurse nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = {  norecurse nounwind ssp uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { nobuiltin nounwind "no-builtins" }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"Apple clang version 12.0.0 (clang-1200.0.32.2)"}
!1 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 6]}
!2 = !{i32 1, !"wchar_size", i32 4}
!3 = !{i32 7, !"PIC Level", i32 2}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!6, !6, i64 0}
!9 = distinct !{!9, !10}
!10 = !{!"llvm.loop.unroll.disable"}
