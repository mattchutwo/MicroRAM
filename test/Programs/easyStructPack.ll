; ModuleID = 'test/programs/easyStructPack.c'
source_filename = "test/programs/easyStructPack.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Point = type <{ i32, i8, i64 }>

@SECRET_NUMBER2 = internal global i32 5, section "__DATA,__secret", align 4
@SECRET_NUMBER1 = internal global i32 3, section "__DATA,__secret", align 4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.Point, align 1
  store i32 0, i32* %1, align 4
  %3 = getelementptr inbounds %struct.Point, %struct.Point* %2, i32 0, i32 0
  %4 = load i32, i32* @SECRET_NUMBER2, align 4
  store i32 %4, i32* %3, align 1
  %5 = getelementptr inbounds %struct.Point, %struct.Point* %2, i32 0, i32 1
  store i8 98, i8* %5, align 1
  %6 = getelementptr inbounds %struct.Point, %struct.Point* %2, i32 0, i32 2
  %7 = load i32, i32* @SECRET_NUMBER1, align 4
  %8 = sext i32 %7 to i64
  store i64 %8, i64* %6, align 1
  %9 = getelementptr inbounds %struct.Point, %struct.Point* %2, i32 0, i32 2
  %10 = load i64, i64* %9, align 1
  %11 = trunc i64 %10 to i32
  ret i32 %11
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-12 "}
