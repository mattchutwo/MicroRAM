; ModuleID = 'varArgs.c'
source_filename = "varArgs.c"
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
define i32 @sum(i32, ...) #0 {
  %2 = alloca i32, align 4
  %3 = alloca [1 x %struct.__va_list_tag], align 16
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %6 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %7 = bitcast %struct.__va_list_tag* %6 to i8*
  call void @llvm.va_start(i8* %7)
  store i32 0, i32* %4, align 4
  store i32 0, i32* %5, align 4
  br label %8

; <label>:8:                                      ; preds = %33, %1
  %9 = load i32, i32* %5, align 4
  %10 = load i32, i32* %2, align 4
  %11 = icmp slt i32 %9, %10
  br i1 %11, label %12, label %36

; <label>:12:                                     ; preds = %8
  %13 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %14 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %13, i32 0, i32 0
  %15 = load i32, i32* %14, align 16
  %16 = icmp ule i32 %15, 40
  br i1 %16, label %17, label %23

; <label>:17:                                     ; preds = %12
  %18 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %13, i32 0, i32 3
  %19 = load i8*, i8** %18, align 16
  %20 = getelementptr i8, i8* %19, i32 %15
  %21 = bitcast i8* %20 to i32*
  %22 = add i32 %15, 8
  store i32 %22, i32* %14, align 16
  br label %28

; <label>:23:                                     ; preds = %12
  %24 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %13, i32 0, i32 2
  %25 = load i8*, i8** %24, align 8
  %26 = bitcast i8* %25 to i32*
  %27 = getelementptr i8, i8* %25, i32 8
  store i8* %27, i8** %24, align 8
  br label %28

; <label>:28:                                     ; preds = %23, %17
  %29 = phi i32* [ %21, %17 ], [ %26, %23 ]
  %30 = load i32, i32* %29, align 4
  %31 = load i32, i32* %4, align 4
  %32 = add nsw i32 %31, %30
  store i32 %32, i32* %4, align 4
  br label %33

; <label>:33:                                     ; preds = %28
  %34 = load i32, i32* %5, align 4
  %35 = add nsw i32 %34, 1
  store i32 %35, i32* %5, align 4
  br label %8

; <label>:36:                                     ; preds = %8
  %37 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %3, i32 0, i32 0
  %38 = bitcast %struct.__va_list_tag* %37 to i8*
  call void @llvm.va_end(i8* %38)
  %39 = load i32, i32* %4, align 4
  ret i32 %39
}

; Function Attrs: nounwind
declare void @llvm.va_start(i8*) #1

; Function Attrs: nounwind
declare void @llvm.va_end(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %2 = call i32 (i32, ...) @sum(i32 5, i32 1, i32 2, i32 3, i32 4, i32 5)
  ret i32 %2
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
