; ModuleID = 'programs/fibExtern.c'
source_filename = "programs/fibExtern.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@my_fib = external local_unnamed_addr global i32, align 4

; Function Attrs: norecurse nounwind readonly ssp uwtable
define i32 @main() local_unnamed_addr #0 {
  %1 = load i32, i32* @my_fib, align 4, !tbaa !4
  %2 = icmp sgt i32 %1, 0
  br i1 %2, label %3, label %50

3:                                                ; preds = %0
  %4 = add i32 %1, -1
  %5 = and i32 %1, 3
  %6 = icmp ult i32 %4, 3
  br i1 %6, label %27, label %7

7:                                                ; preds = %3
  %8 = sub i32 %1, %5
  br label %9

9:                                                ; preds = %9, %7
  %10 = phi i32 [ 0, %7 ], [ %24, %9 ]
  %11 = phi i32 [ 1, %7 ], [ %23, %9 ]
  %12 = phi i32 [ 0, %7 ], [ %22, %9 ]
  %13 = phi i32 [ %8, %7 ], [ %25, %9 ]
  %14 = icmp eq i32 %10, 0
  %15 = select i1 %14, i32 %12, i32 %11
  %16 = select i1 %14, i32 0, i32 %12
  %17 = add nsw i32 %11, %16
  %18 = icmp eq i32 %10, 0
  %19 = select i1 %18, i32 %15, i32 %17
  %20 = select i1 %18, i32 0, i32 %15
  %21 = add nsw i32 %17, %20
  %22 = add nsw i32 %21, %19
  %23 = add nsw i32 %22, %21
  %24 = add nuw nsw i32 %10, 4
  %25 = add i32 %13, -4
  %26 = icmp eq i32 %25, 0
  br i1 %26, label %27, label %9

27:                                               ; preds = %9, %3
  %28 = phi i32 [ undef, %3 ], [ %23, %9 ]
  %29 = phi i32 [ 0, %3 ], [ %24, %9 ]
  %30 = phi i32 [ 1, %3 ], [ %23, %9 ]
  %31 = phi i32 [ 0, %3 ], [ %22, %9 ]
  %32 = icmp eq i32 %5, 0
  br i1 %32, label %45, label %33

33:                                               ; preds = %27, %33
  %34 = phi i32 [ %42, %33 ], [ %29, %27 ]
  %35 = phi i32 [ %41, %33 ], [ %30, %27 ]
  %36 = phi i32 [ %40, %33 ], [ %31, %27 ]
  %37 = phi i32 [ %43, %33 ], [ %5, %27 ]
  %38 = icmp ult i32 %34, 2
  %39 = add nsw i32 %35, %36
  %40 = select i1 %38, i32 %36, i32 %35
  %41 = select i1 %38, i32 %35, i32 %39
  %42 = add nuw nsw i32 %34, 1
  %43 = add i32 %37, -1
  %44 = icmp eq i32 %43, 0
  br i1 %44, label %45, label %33, !llvm.loop !8

45:                                               ; preds = %33, %27
  %46 = phi i1 [ false, %27 ], [ %38, %33 ]
  %47 = phi i32 [ %28, %27 ], [ %39, %33 ]
  %48 = add i32 %1, -1
  %49 = select i1 %46, i32 %48, i32 %47
  br label %50

50:                                               ; preds = %45, %0
  %51 = phi i32 [ %49, %45 ], [ undef, %0 ]
  ret i32 %51
}

attributes #0 = { norecurse nounwind readonly ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = distinct !{!8, !9}
!9 = !{!"llvm.loop.unroll.disable"}
