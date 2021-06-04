; ModuleID = './MallocOOB/mallocOOB.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@SECRET_SIZE = dso_local local_unnamed_addr global i32 3, section "__DATA,__secret", align 4
@SECRET_NUMBER = dso_local local_unnamed_addr global i32 42, section "__DATA,__secret", align 4

; Function Attrs:  nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  %1 = load i32, i32* @SECRET_SIZE, align 4, !tbaa !4
  %2 = sext i32 %1 to i64
  %3 = shl nsw i64 %2, 2
  %4 = tail call i8* @__cc_malloc(i64 %3) #5
  %5 = ptrtoint i8* %4 to i64
  %6 = lshr i64 %5, 58
  %7 = shl i64 1, %6
  %8 = add nsw i64 %3, 8
  %9 = icmp ult i64 %7, %8
  br i1 %9, label %10, label %11

10:                                               ; preds = %0
  tail call void @__cc_flag_invalid() #5
  br label %11

11:                                               ; preds = %10, %0
  %12 = add i64 %7, -1
  %13 = and i64 %12, %5
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %16, label %15

15:                                               ; preds = %11
  tail call void @__cc_flag_invalid() #5
  br label %16

16:                                               ; preds = %15, %11
  %17 = getelementptr inbounds i8, i8* %4, i64 %7
  %18 = getelementptr inbounds i8, i8* %17, i64 -8
  %19 = bitcast i8* %18 to i64*
  tail call void @__cc_write_and_poison(i64* nonnull %19, i64 1) #5
  %20 = getelementptr inbounds i8, i8* %4, i64 %3
  tail call void @__cc_access_valid(i8* %4, i8* %20) #5
  %21 = tail call i64* @__cc_advise_poison(i8* %20, i8* nonnull %18) #5
  %22 = icmp eq i64* %21, null
  br i1 %22, label %malloc.exit, label %23

23:                                               ; preds = %16
  %24 = ptrtoint i64* %21 to i64
  %25 = and i64 %24, 7
  %26 = icmp eq i64 %25, 0
  br i1 %26, label %28, label %27

27:                                               ; preds = %23
  tail call void @__cc_flag_invalid() #5
  br label %28

28:                                               ; preds = %27, %23
  %29 = bitcast i64* %21 to i8*
  %30 = icmp ugt i8* %20, %29
  br i1 %30, label %31, label %32

31:                                               ; preds = %28
  tail call void @__cc_flag_invalid() #5
  br label %32

32:                                               ; preds = %31, %28
  %33 = icmp ult i64* %21, %19
  br i1 %33, label %35, label %34

34:                                               ; preds = %32
  tail call void @__cc_flag_invalid() #5
  br label %35

35:                                               ; preds = %34, %32
  tail call void @__cc_write_and_poison(i64* nonnull %21, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %16, %35
  %36 = bitcast i8* %4 to i32*
  store i32 21, i32* %36, align 4, !tbaa !4
  %37 = getelementptr inbounds i8, i8* %4, i64 4
  %38 = bitcast i8* %37 to i32*
  store i32 22, i32* %38, align 4, !tbaa !4
  %39 = getelementptr inbounds i8, i8* %4, i64 8
  %40 = bitcast i8* %39 to i32*
  store i32 23, i32* %40, align 4, !tbaa !4
  %41 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  %42 = getelementptr inbounds i8, i8* %4, i64 16
  %43 = bitcast i8* %42 to i32*
  store i32 %41, i32* %43, align 4, !tbaa !4
  %44 = getelementptr inbounds i32, i32* %36, i64 %2
  %45 = load i32, i32* %44, align 4, !tbaa !4
  ret i32 %45
}

declare dso_local i8* @__cc_malloc(i64) local_unnamed_addr #1

declare dso_local void @__cc_flag_invalid() local_unnamed_addr #1

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare dso_local void @__cc_access_valid(i8*, i8*) local_unnamed_addr #1

declare dso_local i64* @__cc_advise_poison(i8*, i8*) local_unnamed_addr #1

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

attributes #0 = {  nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = {  norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { nobuiltin nounwind }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"clang version 9.0.1-16 "}
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
