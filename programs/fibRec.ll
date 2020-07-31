; ModuleID = 'programs/fibRec.c'
source_filename = "programs/fibRec.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: nounwind readnone ssp uwtable
define i32 @fibbonacci(i32) local_unnamed_addr #0 {
  switch i32 %0, label %3 [
    i32 0, label %9
    i32 1, label %2
  ]

2:                                                ; preds = %1
  br label %9

3:                                                ; preds = %1
  %4 = add nsw i32 %0, -1
  %5 = tail call i32 @fibbonacci(i32 %4)
  %6 = add nsw i32 %0, -2
  %7 = tail call i32 @fibbonacci(i32 %6)
  %8 = add nsw i32 %7, %5
  ret i32 %8

9:                                                ; preds = %1, %2
  %10 = phi i32 [ 1, %2 ], [ %0, %1 ]
  ret i32 %10
}

; Function Attrs: nounwind readnone ssp uwtable
define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
  %3 = tail call i32 @fibbonacci(i32 %0)
  ret i32 %3
}

attributes #0 = { nounwind readnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
