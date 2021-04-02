; This LLVM code was written by hand (based on a trivial clang-9 output) to
; ensure we get exactly the desired instructions for the test.  There is no
; corresponding C source.

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: norecurse nounwind readnone uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  ; Test behavior of signed icmp instructions
  %a = icmp sgt i32 0, -1       ; 0 > -1 (should produce 1)
  %b = icmp sge i32 0, 0        ; 0 >= 0 (should produce 1)
  %c = icmp sle i32 3, 5        ; 3 <= 5 (should produce 1)
  ; Overall result should be 3
  %aext = zext i1 %a to i32
  %bext = zext i1 %b to i32
  %cext = zext i1 %c to i32
  %resultb = add i32 %aext, %bext
  %result = add i32 %resultb, %cext
  ret i32 %result
}

attributes #0 = { norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-16 "}
