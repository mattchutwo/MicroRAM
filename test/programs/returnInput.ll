; ModuleID = 'programs/returnInput.c'
source_filename = "programs/returnInput.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: norecurse nounwind readonly ssp uwtable
define i32 @main(i32, i8** nocapture readonly) local_unnamed_addr #0 {
  %3 = icmp slt i32 %0, 2
  br i1 %3, label %21, label %4

4:                                                ; preds = %2
  %5 = getelementptr inbounds i8*, i8** %1, i64 1
  %6 = load i8*, i8** %5, align 8, !tbaa !4
  %7 = load i8, i8* %6, align 1, !tbaa !8
  %8 = icmp eq i8 %7, 0
  br i1 %8, label %21, label %9

9:                                                ; preds = %4, %9
  %10 = phi i64 [ %17, %9 ], [ 0, %4 ]
  %11 = phi i8 [ %19, %9 ], [ %7, %4 ]
  %12 = phi i32 [ %16, %9 ], [ 0, %4 ]
  %13 = sext i8 %11 to i32
  %14 = mul nsw i32 %12, 10
  %15 = add i32 %14, -48
  %16 = add i32 %15, %13
  %17 = add nuw i64 %10, 1
  %18 = getelementptr inbounds i8, i8* %6, i64 %17
  %19 = load i8, i8* %18, align 1, !tbaa !8
  %20 = icmp eq i8 %19, 0
  br i1 %20, label %21, label %9

21:                                               ; preds = %9, %4, %2
  %22 = phi i32 [ 0, %2 ], [ 0, %4 ], [ %16, %9 ]
  ret i32 %22
}

attributes #0 = { norecurse nounwind readonly ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"any pointer", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!6, !6, i64 0}
