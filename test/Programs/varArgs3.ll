; ModuleID = 'varArgs3.c'
source_filename = "varArgs3.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.14.0"

%struct.__cc_va_list = type { i32, i32, i8*, i8* }
%struct.__va_list_tag = type { i32, i32, i8*, i8* }

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
define void @__cc_va_copy(i8*, i8*) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca %struct.__cc_va_list*, align 8
  %6 = alloca %struct.__cc_va_list*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %7 = load i8*, i8** %3, align 8
  %8 = bitcast i8* %7 to %struct.__cc_va_list*
  store %struct.__cc_va_list* %8, %struct.__cc_va_list** %5, align 8
  %9 = load i8*, i8** %4, align 8
  %10 = bitcast i8* %9 to %struct.__cc_va_list*
  store %struct.__cc_va_list* %10, %struct.__cc_va_list** %6, align 8
  %11 = load %struct.__cc_va_list*, %struct.__cc_va_list** %5, align 8
  %12 = load %struct.__cc_va_list*, %struct.__cc_va_list** %6, align 8
  %13 = bitcast %struct.__cc_va_list* %11 to i8*
  %14 = bitcast %struct.__cc_va_list* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %13, i8* align 8 %14, i64 24, i1 false)
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1) #1

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
define i32 @sum(i32, ...) #0 {
  %2 = alloca i32, align 4
  %3 = alloca [1 x %struct.__va_list_tag], align 16
  %4 = alloca [1 x %struct.__va_list_tag], align 16
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %8 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %9 = bitcast %struct.__va_list_tag* %8 to i8*
  call void @llvm.va_start(i8* %9)
  %10 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %4, i32 0, i32 0
  %11 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %12 = bitcast %struct.__va_list_tag* %10 to i8*
  %13 = bitcast %struct.__va_list_tag* %11 to i8*
  call void @llvm.va_copy(i8* %12, i8* %13)
  store i32 0, i32* %5, align 4
  store i32 0, i32* %6, align 4
  br label %14

; <label>:14:                                     ; preds = %39, %1
  %15 = load i32, i32* %6, align 4
  %16 = load i32, i32* %2, align 4
  %17 = icmp slt i32 %15, %16
  br i1 %17, label %18, label %42

; <label>:18:                                     ; preds = %14
  %19 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %20 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %19, i32 0, i32 0
  %21 = load i32, i32* %20, align 16
  %22 = icmp ule i32 %21, 40
  br i1 %22, label %23, label %29

; <label>:23:                                     ; preds = %18
  %24 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %19, i32 0, i32 3
  %25 = load i8*, i8** %24, align 16
  %26 = getelementptr i8, i8* %25, i32 %21
  %27 = bitcast i8* %26 to i32*
  %28 = add i32 %21, 8
  store i32 %28, i32* %20, align 16
  br label %34

; <label>:29:                                     ; preds = %18
  %30 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %19, i32 0, i32 2
  %31 = load i8*, i8** %30, align 8
  %32 = bitcast i8* %31 to i32*
  %33 = getelementptr i8, i8* %31, i32 8
  store i8* %33, i8** %30, align 8
  br label %34

; <label>:34:                                     ; preds = %29, %23
  %35 = phi i32* [ %27, %23 ], [ %32, %29 ]
  %36 = load i32, i32* %35, align 4
  %37 = load i32, i32* %5, align 4
  %38 = add nsw i32 %37, %36
  store i32 %38, i32* %5, align 4
  br label %39

; <label>:39:                                     ; preds = %34
  %40 = load i32, i32* %6, align 4
  %41 = add nsw i32 %40, 1
  store i32 %41, i32* %6, align 4
  br label %14

; <label>:42:                                     ; preds = %14
  %43 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %44 = bitcast %struct.__va_list_tag* %43 to i8*
  call void @llvm.va_end(i8* %44)
  store i32 0, i32* %7, align 4
  br label %45

; <label>:45:                                     ; preds = %70, %42
  %46 = load i32, i32* %7, align 4
  %47 = load i32, i32* %2, align 4
  %48 = icmp slt i32 %46, %47
  br i1 %48, label %49, label %73

; <label>:49:                                     ; preds = %45
  %50 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %4, i32 0, i32 0
  %51 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %50, i32 0, i32 0
  %52 = load i32, i32* %51, align 16
  %53 = icmp ule i32 %52, 40
  br i1 %53, label %54, label %60

; <label>:54:                                     ; preds = %49
  %55 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %50, i32 0, i32 3
  %56 = load i8*, i8** %55, align 16
  %57 = getelementptr i8, i8* %56, i32 %52
  %58 = bitcast i8* %57 to i32*
  %59 = add i32 %52, 8
  store i32 %59, i32* %51, align 16
  br label %65

; <label>:60:                                     ; preds = %49
  %61 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %50, i32 0, i32 2
  %62 = load i8*, i8** %61, align 8
  %63 = bitcast i8* %62 to i32*
  %64 = getelementptr i8, i8* %62, i32 8
  store i8* %64, i8** %61, align 8
  br label %65

; <label>:65:                                     ; preds = %60, %54
  %66 = phi i32* [ %58, %54 ], [ %63, %60 ]
  %67 = load i32, i32* %66, align 4
  %68 = load i32, i32* %5, align 4
  %69 = add nsw i32 %68, %67
  store i32 %69, i32* %5, align 4
  br label %70

; <label>:70:                                     ; preds = %65
  %71 = load i32, i32* %7, align 4
  %72 = add nsw i32 %71, 1
  store i32 %72, i32* %7, align 4
  br label %45

; <label>:73:                                     ; preds = %45
  %74 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %4, i32 0, i32 0
  %75 = bitcast %struct.__va_list_tag* %74 to i8*
  call void @llvm.va_end(i8* %75)
  %76 = load i32, i32* %5, align 4
  ret i32 %76
}

; Function Attrs: nounwind
declare void @llvm.va_start(i8*) #2

; Function Attrs: nounwind
declare void @llvm.va_copy(i8*, i8*) #2

; Function Attrs: nounwind
declare void @llvm.va_end(i8*) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %2 = call i32 (i32, ...) @sum(i32 5, i32 1, i32 2, i32 3, i32 4, i32 5)
  ret i32 %2
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
