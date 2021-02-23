; ModuleID = 'test/programs/easyBinaryTree.later.c'
source_filename = "test/programs/easyBinaryTree.later.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.node = type { i32, i32, %struct.node*, %struct.node* }

@SECRET_NUMBER = internal global i32 42, section "__DATA,__secret", align 4
@__const.main.leaf2 = private unnamed_addr constant %struct.node { i32 1, i32 41, %struct.node* null, %struct.node* null }, align 8

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @search(%struct.node*, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.node*, align 8
  %5 = alloca i32, align 4
  store %struct.node* %0, %struct.node** %4, align 8
  store i32 %1, i32* %5, align 4
  %6 = load %struct.node*, %struct.node** %4, align 8
  %7 = icmp eq %struct.node* %6, null
  br i1 %7, label %8, label %9

8:                                                ; preds = %2
  store i32 0, i32* %3, align 4
  br label %37

9:                                                ; preds = %2
  %10 = load %struct.node*, %struct.node** %4, align 8
  %11 = getelementptr inbounds %struct.node, %struct.node* %10, i32 0, i32 0
  %12 = load i32, i32* %11, align 8
  %13 = load i32, i32* %5, align 4
  %14 = icmp eq i32 %12, %13
  br i1 %14, label %15, label %19

15:                                               ; preds = %9
  %16 = load %struct.node*, %struct.node** %4, align 8
  %17 = getelementptr inbounds %struct.node, %struct.node* %16, i32 0, i32 1
  %18 = load i32, i32* %17, align 4
  store i32 %18, i32* %3, align 4
  br label %37

19:                                               ; preds = %9
  %20 = load i32, i32* %5, align 4
  %21 = load %struct.node*, %struct.node** %4, align 8
  %22 = getelementptr inbounds %struct.node, %struct.node* %21, i32 0, i32 0
  %23 = load i32, i32* %22, align 8
  %24 = icmp sgt i32 %20, %23
  br i1 %24, label %25, label %31

25:                                               ; preds = %19
  %26 = load %struct.node*, %struct.node** %4, align 8
  %27 = getelementptr inbounds %struct.node, %struct.node* %26, i32 0, i32 3
  %28 = load %struct.node*, %struct.node** %27, align 8
  %29 = load i32, i32* %5, align 4
  %30 = call i32 @search(%struct.node* %28, i32 %29)
  store i32 %30, i32* %3, align 4
  br label %37

31:                                               ; preds = %19
  %32 = load %struct.node*, %struct.node** %4, align 8
  %33 = getelementptr inbounds %struct.node, %struct.node* %32, i32 0, i32 2
  %34 = load %struct.node*, %struct.node** %33, align 8
  %35 = load i32, i32* %5, align 4
  %36 = call i32 @search(%struct.node* %34, i32 %35)
  store i32 %36, i32* %3, align 4
  br label %37

37:                                               ; preds = %31, %25, %15, %8
  %38 = load i32, i32* %3, align 4
  ret i32 %38
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.node, align 8
  %3 = alloca %struct.node, align 8
  %4 = alloca %struct.node, align 8
  store i32 0, i32* %1, align 4
  %5 = getelementptr inbounds %struct.node, %struct.node* %2, i32 0, i32 0
  store i32 10, i32* %5, align 8
  %6 = getelementptr inbounds %struct.node, %struct.node* %2, i32 0, i32 1
  %7 = load i32, i32* @SECRET_NUMBER, align 4
  store i32 %7, i32* %6, align 4
  %8 = getelementptr inbounds %struct.node, %struct.node* %2, i32 0, i32 2
  store %struct.node* null, %struct.node** %8, align 8
  %9 = getelementptr inbounds %struct.node, %struct.node* %2, i32 0, i32 3
  store %struct.node* null, %struct.node** %9, align 8
  %10 = bitcast %struct.node* %3 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %10, i8* align 8 bitcast (%struct.node* @__const.main.leaf2 to i8*), i64 24, i1 false)
  %11 = getelementptr inbounds %struct.node, %struct.node* %4, i32 0, i32 0
  store i32 5, i32* %11, align 8
  %12 = getelementptr inbounds %struct.node, %struct.node* %4, i32 0, i32 1
  store i32 45, i32* %12, align 4
  %13 = getelementptr inbounds %struct.node, %struct.node* %4, i32 0, i32 2
  store %struct.node* %3, %struct.node** %13, align 8
  %14 = getelementptr inbounds %struct.node, %struct.node* %4, i32 0, i32 3
  store %struct.node* %2, %struct.node** %14, align 8
  %15 = call i32 @search(%struct.node* %4, i32 10)
  ret i32 %15
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}
