; ModuleID = 'varArgs2.c'
source_filename = "varArgs2.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.14.0"

%struct.__cc_va_list = type { i32, i32, i8*, i8* }
%struct.__va_list_tag = type { i32, i32, i8*, i8* }
%struct.test_struct = type { i32, i8, i32* }

@__const.main.arg3 = private unnamed_addr constant [3 x i32] [i32 0, i32 5, i32 0], align 4

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @__cc_va_start(i8*, i8*, i32) #0 {
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca %struct.__cc_va_list*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i32 %2, i32* %6, align 4
  %8 = load i8*, i8** %4, align 8
  %9 = bitcast i8* %8 to %struct.__cc_va_list*
  store %struct.__cc_va_list* %9, %struct.__cc_va_list** %7, align 8
  %10 = load %struct.__cc_va_list*, %struct.__cc_va_list** %7, align 8
  %11 = getelementptr inbounds %struct.__cc_va_list, %struct.__cc_va_list* %10, i32 0, i32 0
  store i32 999, i32* %11, align 8
  %12 = load %struct.__cc_va_list*, %struct.__cc_va_list** %7, align 8
  %13 = getelementptr inbounds %struct.__cc_va_list, %struct.__cc_va_list* %12, i32 0, i32 1
  store i32 999, i32* %13, align 4
  %14 = load i8*, i8** %5, align 8
  %15 = load i32, i32* %6, align 4
  %16 = sext i32 %15 to i64
  %17 = getelementptr inbounds i8, i8* %14, i64 %16
  %18 = load %struct.__cc_va_list*, %struct.__cc_va_list** %7, align 8
  %19 = getelementptr inbounds %struct.__cc_va_list, %struct.__cc_va_list* %18, i32 0, i32 2
  store i8* %17, i8** %19, align 8
  %20 = load %struct.__cc_va_list*, %struct.__cc_va_list** %7, align 8
  %21 = getelementptr inbounds %struct.__cc_va_list, %struct.__cc_va_list* %20, i32 0, i32 3
  store i8* inttoptr (i64 4294901760 to i8*), i8** %21, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @__llvm__memcpy__p0i8__p0i8__i64(i8*, i8*, i64) #0 {
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i64 %2, i64* %6, align 8
  store i64 0, i64* %7, align 8
  br label %8

; <label>:8:                                      ; preds = %20, %3
  %9 = load i64, i64* %7, align 8
  %10 = load i64, i64* %6, align 8
  %11 = icmp ult i64 %9, %10
  br i1 %11, label %12, label %23

; <label>:12:                                     ; preds = %8
  %13 = load i8*, i8** %5, align 8
  %14 = load i64, i64* %7, align 8
  %15 = getelementptr inbounds i8, i8* %13, i64 %14
  %16 = load i8, i8* %15, align 1
  %17 = load i8*, i8** %4, align 8
  %18 = load i64, i64* %7, align 8
  %19 = getelementptr inbounds i8, i8* %17, i64 %18
  store i8 %16, i8* %19, align 1
  br label %20

; <label>:20:                                     ; preds = %12
  %21 = load i64, i64* %7, align 8
  %22 = add i64 %21, 1
  store i64 %22, i64* %7, align 8
  br label %8

; <label>:23:                                     ; preds = %8
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @test(i32, ...) #0 {
  %2 = alloca i32, align 4
  %3 = alloca [1 x %struct.__va_list_tag], align 16
  %4 = alloca i32, align 4
  %5 = alloca %struct.test_struct, align 8
  %6 = alloca i32*, align 8
  %7 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %8 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %9 = bitcast %struct.__va_list_tag* %8 to i8*
  call void @llvm.va_start(i8* %9)
  %10 = load i32, i32* %2, align 4
  store i32 %10, i32* %4, align 4
  %11 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %12 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %11, i32 0, i32 0
  %13 = load i32, i32* %12, align 16
  %14 = icmp ule i32 %13, 32
  br i1 %14, label %15, label %21

; <label>:15:                                     ; preds = %1
  %16 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %11, i32 0, i32 3
  %17 = load i8*, i8** %16, align 16
  %18 = getelementptr i8, i8* %17, i32 %13
  %19 = bitcast i8* %18 to %struct.test_struct*
  %20 = add i32 %13, 16
  store i32 %20, i32* %12, align 16
  br label %26

; <label>:21:                                     ; preds = %1
  %22 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %11, i32 0, i32 2
  %23 = load i8*, i8** %22, align 8
  %24 = bitcast i8* %23 to %struct.test_struct*
  %25 = getelementptr i8, i8* %23, i32 16
  store i8* %25, i8** %22, align 8
  br label %26

; <label>:26:                                     ; preds = %21, %15
  %27 = phi %struct.test_struct* [ %19, %15 ], [ %24, %21 ]
  %28 = bitcast %struct.test_struct* %5 to i8*
  %29 = bitcast %struct.test_struct* %27 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %28, i8* align 8 %29, i64 16, i1 false)
  %30 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %5, i32 0, i32 0
  %31 = load i32, i32* %30, align 8
  %32 = load i32, i32* %4, align 4
  %33 = add nsw i32 %32, %31
  store i32 %33, i32* %4, align 4
  %34 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %5, i32 0, i32 1
  %35 = load i8, i8* %34, align 4
  %36 = trunc i8 %35 to i1
  br i1 %36, label %37, label %40

; <label>:37:                                     ; preds = %26
  %38 = load i32, i32* %4, align 4
  %39 = add nsw i32 %38, 3
  store i32 %39, i32* %4, align 4
  br label %40

; <label>:40:                                     ; preds = %37, %26
  %41 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %5, i32 0, i32 2
  %42 = load i32*, i32** %41, align 8
  %43 = load i32, i32* %42, align 4
  %44 = load i32, i32* %4, align 4
  %45 = add nsw i32 %44, %43
  store i32 %45, i32* %4, align 4
  %46 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %47 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %46, i32 0, i32 0
  %48 = load i32, i32* %47, align 16
  %49 = icmp ule i32 %48, 40
  br i1 %49, label %50, label %56

; <label>:50:                                     ; preds = %40
  %51 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %46, i32 0, i32 3
  %52 = load i8*, i8** %51, align 16
  %53 = getelementptr i8, i8* %52, i32 %48
  %54 = bitcast i8* %53 to i32**
  %55 = add i32 %48, 8
  store i32 %55, i32* %47, align 16
  br label %61

; <label>:56:                                     ; preds = %40
  %57 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %46, i32 0, i32 2
  %58 = load i8*, i8** %57, align 8
  %59 = bitcast i8* %58 to i32**
  %60 = getelementptr i8, i8* %58, i32 8
  store i8* %60, i8** %57, align 8
  br label %61

; <label>:61:                                     ; preds = %56, %50
  %62 = phi i32** [ %54, %50 ], [ %59, %56 ]
  %63 = load i32*, i32** %62, align 8
  store i32* %63, i32** %6, align 8
  %64 = load i32*, i32** %6, align 8
  %65 = getelementptr inbounds i32, i32* %64, i64 0
  %66 = load i32, i32* %65, align 4
  %67 = load i32, i32* %4, align 4
  %68 = add nsw i32 %67, %66
  store i32 %68, i32* %4, align 4
  %69 = load i32*, i32** %6, align 8
  %70 = getelementptr inbounds i32, i32* %69, i64 1
  %71 = load i32, i32* %70, align 4
  %72 = load i32, i32* %4, align 4
  %73 = add nsw i32 %72, %71
  store i32 %73, i32* %4, align 4
  %74 = load i32*, i32** %6, align 8
  %75 = getelementptr inbounds i32, i32* %74, i64 2
  %76 = load i32, i32* %75, align 4
  %77 = load i32, i32* %4, align 4
  %78 = add nsw i32 %77, %76
  store i32 %78, i32* %4, align 4
  %79 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %80 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %79, i32 0, i32 0
  %81 = load i32, i32* %80, align 16
  %82 = icmp ule i32 %81, 40
  br i1 %82, label %83, label %89

; <label>:83:                                     ; preds = %61
  %84 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %79, i32 0, i32 3
  %85 = load i8*, i8** %84, align 16
  %86 = getelementptr i8, i8* %85, i32 %81
  %87 = bitcast i8* %86 to i32*
  %88 = add i32 %81, 8
  store i32 %88, i32* %80, align 16
  br label %94

; <label>:89:                                     ; preds = %61
  %90 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %79, i32 0, i32 2
  %91 = load i8*, i8** %90, align 8
  %92 = bitcast i8* %91 to i32*
  %93 = getelementptr i8, i8* %91, i32 8
  store i8* %93, i8** %90, align 8
  br label %94

; <label>:94:                                     ; preds = %89, %83
  %95 = phi i32* [ %87, %83 ], [ %92, %89 ]
  %96 = load i32, i32* %95, align 4
  store i32 %96, i32* %7, align 4
  %97 = load i32, i32* %7, align 4
  %98 = load i32, i32* %4, align 4
  %99 = add nsw i32 %98, %97
  store i32 %99, i32* %4, align 4
  %100 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %101 = bitcast %struct.__va_list_tag* %100 to i8*
  call void @llvm.va_end(i8* %101)
  %102 = load i32, i32* %4, align 4
  ret i32 %102
}

; Function Attrs: nounwind
declare void @llvm.va_start(i8*) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1) #2

; Function Attrs: nounwind
declare void @llvm.va_end(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca %struct.test_struct, align 8
  %4 = alloca [3 x i32], align 4
  store i32 0, i32* %1, align 4
  store i32 1, i32* %2, align 4
  %5 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %3, i32 0, i32 0
  store i32 2, i32* %5, align 8
  %6 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %3, i32 0, i32 1
  store i8 1, i8* %6, align 4
  %7 = getelementptr inbounds %struct.test_struct, %struct.test_struct* %3, i32 0, i32 2
  store i32* %2, i32** %7, align 8
  %8 = bitcast [3 x i32]* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %8, i8* align 4 bitcast ([3 x i32]* @__const.main.arg3 to i8*), i64 12, i1 false)
  %9 = getelementptr inbounds [3 x i32], [3 x i32]* %4, i64 0, i64 0
  store i32 4, i32* %9, align 4
  %10 = getelementptr inbounds [3 x i32], [3 x i32]* %4, i64 0, i64 2
  store i32 6, i32* %10, align 4
  %11 = load i32, i32* %2, align 4
  %12 = getelementptr inbounds [3 x i32], [3 x i32]* %4, i32 0, i32 0
  %13 = load i32, i32* %2, align 4
  %14 = bitcast %struct.test_struct* %3 to { i64, i32* }*
  %15 = getelementptr inbounds { i64, i32* }, { i64, i32* }* %14, i32 0, i32 0
  %16 = load i64, i64* %15, align 8
  %17 = getelementptr inbounds { i64, i32* }, { i64, i32* }* %14, i32 0, i32 1
  %18 = load i32*, i32** %17, align 8
  %19 = call i32 (i32, ...) @test(i32 %11, i64 %16, i32* %18, i32* %12, i32 %13)
  ret i32 %19
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }
attributes #2 = { argmemonly nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
