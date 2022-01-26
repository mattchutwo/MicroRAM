; ModuleID = 'select.c'
source_filename = "select.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx11.1.0"

@A0 = common local_unnamed_addr global i32 0, align 4
@A1 = common local_unnamed_addr global i32 0, align 4
@A2 = common local_unnamed_addr global i32 0, align 4
@A3 = common local_unnamed_addr global i32 0, align 4
@A4 = common local_unnamed_addr global i32 0, align 4
@A5 = common local_unnamed_addr global i32 0, align 4
@A6 = common local_unnamed_addr global i32 0, align 4
@A7 = common local_unnamed_addr global i32 0, align 4
@A8 = common local_unnamed_addr global i32 0, align 4
@A9 = common local_unnamed_addr global i32 0, align 4
; Function Attrs: norecurse nounwind readonly ssp uwtable
define i32 @main() local_unnamed_addr #0 {
  %1 = load i32, i32* @A0, align 4, !tbaa !4
  %2 = icmp eq i32 %1, 1
  %3 = load i32, i32* @A1, align 4, !tbaa !4
  %4 = load i32, i32* @A2, align 4, !tbaa !4
  br i1 %2, label %13, label %5

5:                                                ; preds = %0
  %6 = load i32, i32* @A3, align 4, !tbaa !4
  %7 = load i32, i32* @A4, align 4, !tbaa !4
  %8 = load i32, i32* @A5, align 4, !tbaa !4
  %9 = load i32, i32* @A6, align 4, !tbaa !4
  %10 = load i32, i32* @A7, align 4, !tbaa !4
  %11 = load i32, i32* @A8, align 4, !tbaa !4
  %12 = load i32, i32* @A9, align 4, !tbaa !4
  br label %29

13:                                               ; preds = %0
  %14 = add nsw i32 %4, %3
  %15 = load i32, i32* @A3, align 4, !tbaa !4
  %16 = add nsw i32 %14, %15
  %17 = load i32, i32* @A4, align 4, !tbaa !4
  %18 = add nsw i32 %16, %17
  %19 = load i32, i32* @A5, align 4, !tbaa !4
  %20 = add nsw i32 %18, %19
  %21 = load i32, i32* @A6, align 4, !tbaa !4
  %22 = add nsw i32 %20, %21
  %23 = load i32, i32* @A7, align 4, !tbaa !4
  %24 = add nsw i32 %22, %23
  %25 = load i32, i32* @A8, align 4, !tbaa !4
  %26 = add nsw i32 %24, %25
  %27 = load i32, i32* @A9, align 4, !tbaa !4
  %28 = add nsw i32 %26, %27
  br label %29

29:                                               ; preds = %5, %13
  %30 = phi i32 [ %27, %13 ], [ %12, %5 ]
  %31 = phi i32 [ %25, %13 ], [ %11, %5 ]
  %32 = phi i32 [ %23, %13 ], [ %10, %5 ]
  %33 = phi i32 [ %21, %13 ], [ %9, %5 ]
  %34 = phi i32 [ %19, %13 ], [ %8, %5 ]
  %35 = phi i32 [ %17, %13 ], [ %7, %5 ]
  %36 = phi i32 [ %15, %13 ], [ %6, %5 ]
  %37 = phi i32 [ %28, %13 ], [ 0, %5 ]
  %38 = icmp eq i32 %3, 2
  %39 = select i1 %38, i32 %37, i32 1
  %40 = icmp eq i32 %4, 3
  %41 = select i1 %40, i32 %39, i32 2
  %42 = icmp eq i32 %36, 4
  %43 = select i1 %42, i32 %41, i32 3
  %44 = icmp eq i32 %35, 5
  %45 = select i1 %44, i32 %43, i32 4
  %46 = icmp eq i32 %34, 6
  %47 = select i1 %46, i32 %45, i32 5
  %48 = icmp eq i32 %33, 7
  %49 = select i1 %48, i32 %47, i32 6
  %50 = icmp eq i32 %32, 8
  %51 = select i1 %50, i32 %49, i32 7
  %52 = icmp eq i32 %31, 9
  %53 = select i1 %52, i32 %51, i32 8
  %54 = icmp eq i32 %30, 10
  %55 = select i1 %54, i32 %53, i32 9
  %56 = add nsw i32 %41, %37
  %57 = add nsw i32 %56, %43
  %58 = add nsw i32 %57, %45
  %59 = add nsw i32 %58, %47
  %60 = add nsw i32 %59, %49
  %61 = add nsw i32 %60, %51
  %62 = add nsw i32 %61, %53
  %63 = add nsw i32 %62, %55
  ret i32 %63
}

attributes #0 = { norecurse nounwind readonly ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 11, i32 1]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 12.0.0 (clang-1200.0.32.29)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
