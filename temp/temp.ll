; ModuleID = 'programs/fib.c'
source_filename = "programs/fib.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readnone ssp uwtable
define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
  %3 = icmp sgt i32 %0, 0
  br i1 %3, label %4, label %51

4:                                                ; preds = %2
  %5 = add i32 %0, -1
  %6 = and i32 %0, 3
  %7 = icmp ult i32 %5, 3
  br i1 %7, label %28, label %8

8:                                                ; preds = %4
  %9 = sub i32 %0, %6
  br label %10

10:                                               ; preds = %10, %8
  %11 = phi i32 [ 0, %8 ], [ %25, %10 ]
  %12 = phi i32 [ 1, %8 ], [ %24, %10 ]
  %13 = phi i32 [ 0, %8 ], [ %23, %10 ]
  %14 = phi i32 [ %9, %8 ], [ %26, %10 ]
  %15 = icmp eq i32 %11, 0
  %16 = select i1 %15, i32 %13, i32 %12
  %17 = select i1 %15, i32 0, i32 %13
  %18 = add nsw i32 %12, %17
  %19 = icmp eq i32 %11, 0
  %20 = select i1 %19, i32 %16, i32 %18
  %21 = select i1 %19, i32 0, i32 %16
  %22 = add nsw i32 %18, %21
  %23 = add nsw i32 %22, %20
  %24 = add nsw i32 %23, %22
  %25 = add nuw nsw i32 %11, 4
  %26 = add i32 %14, -4
  %27 = icmp eq i32 %26, 0
  br i1 %27, label %28, label %10

28:                                               ; preds = %10, %4
  %29 = phi i32 [ undef, %4 ], [ %24, %10 ]
  %30 = phi i32 [ 0, %4 ], [ %25, %10 ]
  %31 = phi i32 [ 1, %4 ], [ %24, %10 ]
  %32 = phi i32 [ 0, %4 ], [ %23, %10 ]
  %33 = icmp eq i32 %6, 0
  br i1 %33, label %46, label %34

34:                                               ; preds = %28, %34
  %35 = phi i32 [ %43, %34 ], [ %30, %28 ]
  %36 = phi i32 [ %42, %34 ], [ %31, %28 ]
  %37 = phi i32 [ %41, %34 ], [ %32, %28 ]
  %38 = phi i32 [ %44, %34 ], [ %6, %28 ]
  %39 = icmp ult i32 %35, 2
  %40 = add nsw i32 %36, %37
  %41 = select i1 %39, i32 %37, i32 %36
  %42 = select i1 %39, i32 %36, i32 %40
  %43 = add nuw nsw i32 %35, 1
  %44 = add i32 %38, -1
  %45 = icmp eq i32 %44, 0
  br i1 %45, label %46, label %34, !llvm.loop !4

46:                                               ; preds = %34, %28
  %47 = phi i1 [ false, %28 ], [ %39, %34 ]
  %48 = phi i32 [ %29, %28 ], [ %40, %34 ]
  %49 = add i32 %0, -1
  %50 = select i1 %47, i32 %49, i32 %48
  br label %51

51:                                               ; preds = %46, %2
  %52 = phi i32 [ %50, %46 ], [ undef, %2 ]
  ret i32 %52
}

attributes #0 = { norecurse nounwind readnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
!4 = distinct !{!4, !5}
!5 = !{!"llvm.loop.unroll.disable"}
