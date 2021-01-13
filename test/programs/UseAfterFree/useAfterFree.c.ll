; ModuleID = './UseAfterFree/useAfterFree.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@SECRET_BOUND = dso_local local_unnamed_addr global i32 144, section "__DATA,__secret", align 4
@SECRET_NUMBER = dso_local local_unnamed_addr global i32 42, section "__DATA,__secret", align 4

; Function Attrs: nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  %1 = tail call i8* @__cc_malloc(i64 12) #5
  %2 = ptrtoint i8* %1 to i64
  %3 = lshr i64 %2, 58
  %4 = shl i64 1, %3
  %5 = icmp ult i8* %1, inttoptr (i64 1441151880758558720 to i8*)
  br i1 %5, label %10, label %6

6:                                                ; preds = %0
  %7 = add i64 %4, -1
  %8 = and i64 %7, %2
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %11, label %10

10:                                               ; preds = %6, %0
  tail call void @__cc_flag_invalid() #5
  br label %11

11:                                               ; preds = %10, %6
  %12 = getelementptr inbounds i8, i8* %1, i64 %4
  %13 = getelementptr inbounds i8, i8* %12, i64 -8
  %14 = bitcast i8* %13 to i64*
  tail call void @__cc_write_and_poison(i64* nonnull %14, i64 1) #5
  %15 = getelementptr inbounds i8, i8* %1, i64 12
  %16 = tail call i64* @__cc_advise_poison(i8* nonnull %15, i8* nonnull %13) #5
  %17 = icmp eq i64* %16, null
  br i1 %17, label %malloc.exit, label %18

18:                                               ; preds = %11
  %19 = ptrtoint i64* %16 to i64
  %20 = and i64 %19, 7
  %21 = icmp eq i64 %20, 0
  br i1 %21, label %23, label %22

22:                                               ; preds = %18
  tail call void @__cc_flag_invalid() #5
  br label %23

23:                                               ; preds = %22, %18
  %24 = bitcast i64* %16 to i8*
  %25 = icmp ugt i8* %15, %24
  %26 = icmp uge i64* %16, %14
  %27 = or i1 %26, %25
  br i1 %27, label %28, label %29

28:                                               ; preds = %23
  tail call void @__cc_flag_invalid() #5
  br label %29

29:                                               ; preds = %28, %23
  tail call void @__cc_write_and_poison(i64* nonnull %16, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %11, %29
  %30 = bitcast i8* %1 to i32*
  store i32 21, i32* %30, align 4, !tbaa !4
  %31 = getelementptr inbounds i8, i8* %1, i64 4
  %32 = bitcast i8* %31 to i32*
  store i32 22, i32* %32, align 4, !tbaa !4
  %33 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  %34 = getelementptr inbounds i8, i8* %1, i64 8
  %35 = bitcast i8* %34 to i32*
  store i32 %33, i32* %35, align 4, !tbaa !4
  %36 = load i32, i32* @SECRET_BOUND, align 4, !tbaa !4
  %37 = icmp slt i32 %36, 145
  br i1 %37, label %38, label %60

38:                                               ; preds = %malloc.exit
  %39 = add i64 %4, -1
  %40 = and i64 %39, %2
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %43, label %42

42:                                               ; preds = %38
  tail call void @__cc_flag_bug() #5
  br label %43

43:                                               ; preds = %42, %38
  store i8 0, i8* %1, align 1, !tbaa !8
  tail call void @__cc_free(i8* nonnull %1) #5
  %44 = tail call i64* @__cc_advise_poison(i8* nonnull %1, i8* nonnull %13) #5
  %45 = icmp eq i64* %44, null
  br i1 %45, label %free.exit, label %46

46:                                               ; preds = %43
  %47 = ptrtoint i64* %44 to i64
  %48 = and i64 %47, 7
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %51, label %50

50:                                               ; preds = %46
  tail call void @__cc_flag_invalid() #5
  br label %51

51:                                               ; preds = %50, %46
  %52 = bitcast i64* %44 to i8*
  %53 = icmp ugt i8* %1, %52
  %54 = icmp uge i64* %44, %14
  %55 = or i1 %54, %53
  br i1 %55, label %56, label %57

56:                                               ; preds = %51
  tail call void @__cc_flag_invalid() #5
  br label %57

57:                                               ; preds = %56, %51
  tail call void @__cc_write_and_poison(i64* nonnull %44, i64 0) #5
  br label %free.exit

free.exit:                                        ; preds = %43, %57
  %58 = load i32, i32* @SECRET_BOUND, align 4, !tbaa !4
  %59 = icmp sgt i32 %58, 143
  br i1 %59, label %free.exit._crit_edge, label %60

free.exit._crit_edge:                             ; preds = %free.exit
  %.pre = load i32, i32* %35, align 4, !tbaa !4
  br label %60

60:                                               ; preds = %malloc.exit, %free.exit._crit_edge, %free.exit
  %61 = phi i32 [ 22, %free.exit ], [ %.pre, %free.exit._crit_edge ], [ %33, %malloc.exit ]
  ret i32 %61
}

declare dso_local i8* @__cc_malloc(i64) local_unnamed_addr #1

declare dso_local void @__cc_flag_invalid() local_unnamed_addr #1

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare dso_local i64* @__cc_advise_poison(i8*, i8*) local_unnamed_addr #1

declare dso_local void @__cc_flag_bug() local_unnamed_addr #1

declare dso_local void @__cc_free(i8*) local_unnamed_addr #1

; Function Attrs:  norecurse nounwind uwtable
define dso_local void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #2 {
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

; Function Attrs:  norecurse nounwind uwtable writeonly
define dso_local void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #3 {
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

attributes #0 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = {  norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { nobuiltin nounwind }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"clang version 9.0.1-12 "}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"ThinLTO", i32 0}
!3 = !{i32 1, !"EnableSplitLTOUnit", i32 0}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!6, !6, i64 0}
!9 = distinct !{!9, !10}
!10 = !{!"llvm.loop.unroll.disable"}
