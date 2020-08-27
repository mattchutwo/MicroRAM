; ModuleID = 'test/programs/easyLinkedList.c'
source_filename = "test/programs/easyLinkedList.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.Node = type { i32, %struct.Node* }

@SECRET_NUMBER3 = internal global i32 8, section "__DATA,__secret", align 4
@SECRET_NUMBER2 = internal global i32 5, section "__DATA,__secret", align 4
@SECRET_NUMBER1 = internal global i32 3, section "__DATA,__secret", align 4

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.Node, align 8
  %3 = alloca %struct.Node, align 8
  %4 = alloca %struct.Node, align 8
  %5 = alloca %struct.Node*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %8 = getelementptr inbounds %struct.Node, %struct.Node* %2, i32 0, i32 0
  %9 = load i32, i32* @SECRET_NUMBER3, align 4
  store i32 %9, i32* %8, align 8
  %10 = getelementptr inbounds %struct.Node, %struct.Node* %2, i32 0, i32 1
  store %struct.Node* null, %struct.Node** %10, align 8
  %11 = getelementptr inbounds %struct.Node, %struct.Node* %3, i32 0, i32 0
  %12 = load i32, i32* @SECRET_NUMBER2, align 4
  store i32 %12, i32* %11, align 8
  %13 = getelementptr inbounds %struct.Node, %struct.Node* %3, i32 0, i32 1
  store %struct.Node* %2, %struct.Node** %13, align 8
  %14 = getelementptr inbounds %struct.Node, %struct.Node* %4, i32 0, i32 0
  %15 = load i32, i32* @SECRET_NUMBER1, align 4
  store i32 %15, i32* %14, align 8
  %16 = getelementptr inbounds %struct.Node, %struct.Node* %4, i32 0, i32 1
  store %struct.Node* %3, %struct.Node** %16, align 8
  %17 = getelementptr inbounds %struct.Node, %struct.Node* %4, i32 0, i32 0
  %18 = load i32, i32* %17, align 8
  store i32 %18, i32* %6, align 4
  %19 = getelementptr inbounds %struct.Node, %struct.Node* %4, i32 0, i32 1
  %20 = load %struct.Node*, %struct.Node** %19, align 8
  store %struct.Node* %20, %struct.Node** %5, align 8
  %21 = load %struct.Node*, %struct.Node** %5, align 8
  %22 = getelementptr inbounds %struct.Node, %struct.Node* %21, i32 0, i32 0
  %23 = load i32, i32* %22, align 8
  store i32 %23, i32* %7, align 4
  %24 = load i32, i32* %6, align 4
  %25 = load i32, i32* %7, align 4
  %26 = add nsw i32 %24, %25
  store i32 %26, i32* %6, align 4
  %27 = load %struct.Node*, %struct.Node** %5, align 8
  %28 = getelementptr inbounds %struct.Node, %struct.Node* %27, i32 0, i32 1
  %29 = load %struct.Node*, %struct.Node** %28, align 8
  store %struct.Node* %29, %struct.Node** %5, align 8
  %30 = load %struct.Node*, %struct.Node** %5, align 8
  %31 = getelementptr inbounds %struct.Node, %struct.Node* %30, i32 0, i32 0
  %32 = load i32, i32* %31, align 8
  store i32 %32, i32* %7, align 4
  %33 = load i32, i32* %6, align 4
  %34 = load i32, i32* %7, align 4
  %35 = add nsw i32 %33, %34
  store i32 %35, i32* %6, align 4
  %36 = load i32, i32* %6, align 4
  ret i32 %36
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
