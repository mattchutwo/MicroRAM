; ModuleID = 'test/programs/arithShr.c'
source_filename = "test/programs/arithShr.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone uwtable
define dso_local i32 @test_ashr(i32, i32, i32) local_unnamed_addr #0 {
  %4 = ashr i32 %0, %1
  %5 = icmp eq i32 %4, %2
  %6 = zext i1 %5 to i32
  ret i32 %6
}

; Function Attrs: norecurse nounwind readnone uwtable
define dso_local i32 @main() local_unnamed_addr #1 {
  %1 = tail call i32 @test_ashr(i32 -4, i32 0, i32 -4)
  %2 = tail call i32 @test_ashr(i32 -4, i32 1, i32 -2)
  %3 = add nsw i32 %2, %1
  %4 = tail call i32 @test_ashr(i32 -4, i32 2, i32 -1)
  %5 = add nsw i32 %3, %4
  %6 = tail call i32 @test_ashr(i32 -4, i32 3, i32 -1)
  %7 = add nsw i32 %5, %6
  %8 = tail call i32 @test_ashr(i32 4, i32 0, i32 4)
  %9 = add nsw i32 %7, %8
  %10 = tail call i32 @test_ashr(i32 4, i32 1, i32 2)
  %11 = add nsw i32 %9, %10
  %12 = tail call i32 @test_ashr(i32 4, i32 2, i32 1)
  %13 = add nsw i32 %11, %12
  %14 = tail call i32 @test_ashr(i32 4, i32 3, i32 0)
  %15 = add nsw i32 %13, %14
  ret i32 %15
}

attributes #0 = { noinline norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-12 "}
