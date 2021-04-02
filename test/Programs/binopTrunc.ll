; ModuleID = 'test/Programs/binopTrunc.c'
source_filename = "test/Programs/binopTrunc.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i64, align 8
  %3 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  store i64 4294967298, i64* %2, align 8
  store i32 0, i32* %3, align 4
  %4 = load i64, i64* %2, align 8
  %5 = trunc i64 %4 to i32
  %6 = icmp eq i32 %5, 2
  %7 = zext i1 %6 to i32
  %8 = load i32, i32* %3, align 4
  %9 = add nsw i32 %8, %7
  store i32 %9, i32* %3, align 4
  %10 = load i64, i64* %2, align 8
  %11 = trunc i64 %10 to i32
  %12 = udiv i32 3, %11
  %13 = icmp eq i32 %12, 1
  %14 = zext i1 %13 to i32
  %15 = load i32, i32* %3, align 4
  %16 = add nsw i32 %15, %14
  store i32 %16, i32* %3, align 4
  %17 = load i64, i64* %2, align 8
  %18 = trunc i64 %17 to i32
  %19 = udiv i32 3, %18
  %20 = icmp ne i32 %19, 0
  %21 = zext i1 %20 to i32
  %22 = load i32, i32* %3, align 4
  %23 = add nsw i32 %22, %21
  store i32 %23, i32* %3, align 4
  %24 = load i64, i64* %2, align 8
  %25 = trunc i64 %24 to i32
  %26 = urem i32 3, %25
  %27 = icmp eq i32 %26, 1
  %28 = zext i1 %27 to i32
  %29 = load i32, i32* %3, align 4
  %30 = add nsw i32 %29, %28
  store i32 %30, i32* %3, align 4
  %31 = load i64, i64* %2, align 8
  %32 = trunc i64 %31 to i32
  %33 = urem i32 3, %32
  %34 = icmp ne i32 %33, 3
  %35 = zext i1 %34 to i32
  %36 = load i32, i32* %3, align 4
  %37 = add nsw i32 %36, %35
  store i32 %37, i32* %3, align 4
  %38 = load i64, i64* %2, align 8
  %39 = trunc i64 %38 to i32
  %40 = lshr i32 %39, 2
  %41 = icmp eq i32 %40, 0
  %42 = zext i1 %41 to i32
  %43 = load i32, i32* %3, align 4
  %44 = add nsw i32 %43, %42
  store i32 %44, i32* %3, align 4
  %45 = load i64, i64* %2, align 8
  %46 = trunc i64 %45 to i32
  %47 = lshr i32 %46, 2
  %48 = icmp ne i32 %47, 1073741824
  %49 = zext i1 %48 to i32
  %50 = load i32, i32* %3, align 4
  %51 = add nsw i32 %50, %49
  store i32 %51, i32* %3, align 4
  %52 = load i64, i64* %2, align 8
  %53 = trunc i64 %52 to i32
  %54 = ashr i32 %53, 2
  %55 = icmp eq i32 %54, 0
  %56 = zext i1 %55 to i32
  %57 = load i32, i32* %3, align 4
  %58 = add nsw i32 %57, %56
  store i32 %58, i32* %3, align 4
  %59 = load i64, i64* %2, align 8
  %60 = trunc i64 %59 to i32
  %61 = ashr i32 %60, 2
  %62 = icmp ne i32 %61, 1073741824
  %63 = zext i1 %62 to i32
  %64 = load i32, i32* %3, align 4
  %65 = add nsw i32 %64, %63
  store i32 %65, i32* %3, align 4
  %66 = load i32, i32* %3, align 4
  ret i32 %66
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-16 "}
