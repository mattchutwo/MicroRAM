; ModuleID = 'pos/simple_heartbleed_nobranch/simple_heartbleed_nobranch.c'
source_filename = "pos/simple_heartbleed_nobranch/simple_heartbleed_nobranch.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.14.0"

@requests = global <{ <{ [10 x i8], [10 x i8] }>, <{ i8, i8, i8, i8, [16 x i8] }> }> <{ <{ [10 x i8], [10 x i8] }> <{ [10 x i8] c"\0A\00passwurd", [10 x i8] zeroinitializer }>, <{ i8, i8, i8, i8, [16 x i8] }> <{ i8 3, i8 1, i8 7, i8 104, [16 x i8] zeroinitializer }> }>, align 16
@p = global [20 x i8] zeroinitializer, align 16

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @my_malloc(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  ret i8* getelementptr inbounds ([20 x i8], [20 x i8]* @p, i32 0, i32 0)
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @read_request_from_network(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %6 = load i32, i32* %2, align 4
  %7 = sext i32 %6 to i64
  %8 = getelementptr inbounds [2 x [20 x i8]], [2 x [20 x i8]]* bitcast (<{ <{ [10 x i8], [10 x i8] }>, <{ i8, i8, i8, i8, [16 x i8] }> }>* @requests to [2 x [20 x i8]]*), i64 0, i64 %7
  %9 = getelementptr inbounds [20 x i8], [20 x i8]* %8, i64 0, i64 0
  %10 = load i8, i8* %9, align 4
  %11 = sext i8 %10 to i32
  store i32 %11, i32* %3, align 4
  %12 = load i32, i32* %3, align 4
  %13 = call i8* @my_malloc(i32 %12)
  store i8* %13, i8** %4, align 8
  store i32 0, i32* %5, align 4
  br label %14

; <label>:14:                                     ; preds = %31, %1
  %15 = load i32, i32* %5, align 4
  %16 = load i32, i32* %3, align 4
  %17 = icmp slt i32 %15, %16
  br i1 %17, label %18, label %34

; <label>:18:                                     ; preds = %14
  %19 = load i32, i32* %2, align 4
  %20 = sext i32 %19 to i64
  %21 = getelementptr inbounds [2 x [20 x i8]], [2 x [20 x i8]]* bitcast (<{ <{ [10 x i8], [10 x i8] }>, <{ i8, i8, i8, i8, [16 x i8] }> }>* @requests to [2 x [20 x i8]]*), i64 0, i64 %20
  %22 = load i32, i32* %5, align 4
  %23 = add nsw i32 %22, 1
  %24 = sext i32 %23 to i64
  %25 = getelementptr inbounds [20 x i8], [20 x i8]* %21, i64 0, i64 %24
  %26 = load i8, i8* %25, align 1
  %27 = load i8*, i8** %4, align 8
  %28 = load i32, i32* %5, align 4
  %29 = sext i32 %28 to i64
  %30 = getelementptr inbounds i8, i8* %27, i64 %29
  store i8 %26, i8* %30, align 1
  br label %31

; <label>:31:                                     ; preds = %18
  %32 = load i32, i32* %5, align 4
  %33 = add nsw i32 %32, 1
  store i32 %33, i32* %5, align 4
  br label %14

; <label>:34:                                     ; preds = %14
  %35 = load i8*, i8** %4, align 8
  ret i8* %35
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @write_response_to_network(i32, i8*, i32) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i8* %1, i8** %5, align 8
  store i32 %2, i32* %6, align 4
  store i32 0, i32* %7, align 4
  br label %8

; <label>:8:                                      ; preds = %25, %3
  %9 = load i32, i32* %7, align 4
  %10 = load i32, i32* %6, align 4
  %11 = icmp slt i32 %9, %10
  br i1 %11, label %12, label %28

; <label>:12:                                     ; preds = %8
  %13 = load i8*, i8** %5, align 8
  %14 = load i32, i32* %7, align 4
  %15 = sext i32 %14 to i64
  %16 = getelementptr inbounds i8, i8* %13, i64 %15
  %17 = load i32, i32* %4, align 4
  call void @noniSinkU8(i8* %16, i32 %17)
  %18 = load i8*, i8** %5, align 8
  %19 = load i32, i32* %7, align 4
  %20 = sext i32 %19 to i64
  %21 = getelementptr inbounds i8, i8* %18, i64 %20
  %22 = load i8, i8* %21, align 1
  %23 = sext i8 %22 to i32
  %24 = call i32 @putchar(i32 %23)
  br label %25

; <label>:25:                                     ; preds = %12
  %26 = load i32, i32* %7, align 4
  %27 = add nsw i32 %26, 1
  store i32 %27, i32* %7, align 4
  br label %8

; <label>:28:                                     ; preds = %8
  %29 = call i32 @putchar(i32 10)
  ret void
}

declare void @noniSinkU8(i8*, i32) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @putchar(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @run_login(i32, i8*) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i8* %1, i8** %4, align 8
  %7 = load i8*, i8** %4, align 8
  %8 = getelementptr inbounds i8, i8* %7, i64 1
  store i8* %8, i8** %5, align 8
  store i32 0, i32* %6, align 4
  br label %9

; <label>:9:                                      ; preds = %18, %2
  %10 = load i32, i32* %6, align 4
  %11 = icmp slt i32 %10, 19
  br i1 %11, label %12, label %21

; <label>:12:                                     ; preds = %9
  %13 = load i8*, i8** %5, align 8
  %14 = load i32, i32* %6, align 4
  %15 = sext i32 %14 to i64
  %16 = getelementptr inbounds i8, i8* %13, i64 %15
  %17 = load i32, i32* %3, align 4
  call void @noniSetLabelU8(i8* %16, i32 %17)
  br label %18

; <label>:18:                                     ; preds = %12
  %19 = load i32, i32* %6, align 4
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %6, align 4
  br label %9

; <label>:21:                                     ; preds = %9
  ret void
}

declare void @noniSetLabelU8(i8*, i32) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @run_heartbeat(i32, i8*) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i8*, align 8
  store i32 %0, i32* %3, align 4
  store i8* %1, i8** %4, align 8
  %7 = load i8*, i8** %4, align 8
  %8 = getelementptr inbounds i8, i8* %7, i64 1
  %9 = load i8, i8* %8, align 1
  %10 = sext i8 %9 to i32
  store i32 %10, i32* %5, align 4
  %11 = load i8*, i8** %4, align 8
  %12 = getelementptr inbounds i8, i8* %11, i64 2
  store i8* %12, i8** %6, align 8
  %13 = load i32, i32* %3, align 4
  %14 = load i8*, i8** %6, align 8
  %15 = load i32, i32* %5, align 4
  call void @write_response_to_network(i32 %13, i8* %14, i32 %15)
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @request_to_label(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp sge i32 %4, 3
  br i1 %5, label %6, label %7

; <label>:6:                                      ; preds = %1
  store i32 3, i32* %2, align 4
  br label %9

; <label>:7:                                      ; preds = %1
  %8 = load i32, i32* %3, align 4
  store i32 %8, i32* %2, align 4
  br label %9

; <label>:9:                                      ; preds = %7, %6
  %10 = load i32, i32* %2, align 4
  ret i32 %10
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @run_handler(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i8*, align 8
  %4 = alloca i32, align 4
  %5 = alloca i8, align 1
  store i32 %0, i32* %2, align 4
  %6 = load i32, i32* %2, align 4
  %7 = call i8* @read_request_from_network(i32 %6)
  store i8* %7, i8** %3, align 8
  %8 = load i32, i32* %2, align 4
  %9 = call i32 @request_to_label(i32 %8)
  store i32 %9, i32* %4, align 4
  %10 = load i8*, i8** %3, align 8
  %11 = getelementptr inbounds i8, i8* %10, i64 0
  %12 = load i8, i8* %11, align 1
  store i8 %12, i8* %5, align 1
  %13 = load i8, i8* %5, align 1
  %14 = sext i8 %13 to i32
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %19

; <label>:16:                                     ; preds = %1
  %17 = load i32, i32* %4, align 4
  %18 = load i8*, i8** %3, align 8
  call void @run_login(i32 %17, i8* %18)
  br label %19

; <label>:19:                                     ; preds = %16, %1
  %20 = load i8, i8* %5, align 1
  %21 = sext i8 %20 to i32
  %22 = icmp eq i32 %21, 1
  br i1 %22, label %23, label %26

; <label>:23:                                     ; preds = %19
  %24 = load i32, i32* %4, align 4
  %25 = load i8*, i8** %3, align 8
  call void @run_heartbeat(i32 %24, i8* %25)
  br label %26

; <label>:26:                                     ; preds = %23, %19
  ret i32 0
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  store i32 0, i32* %2, align 4
  br label %3

; <label>:3:                                      ; preds = %9, %0
  %4 = load i32, i32* %2, align 4
  %5 = icmp slt i32 %4, 2
  br i1 %5, label %6, label %12

; <label>:6:                                      ; preds = %3
  %7 = load i32, i32* %2, align 4
  %8 = call i32 @run_handler(i32 %7)
  br label %9

; <label>:9:                                      ; preds = %6
  %10 = load i32, i32* %2, align 4
  %11 = add nsw i32 %10, 1
  store i32 %11, i32* %2, align 4
  br label %3

; <label>:12:                                     ; preds = %3
  ret i32 0
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @strcmp(i8*, i8*) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  br label %9

; <label>:9:                                      ; preds = %33, %2
  %10 = load i8*, i8** %4, align 8
  %11 = load i8, i8* %10, align 1
  %12 = sext i8 %11 to i32
  store i32 %12, i32* %6, align 4
  %13 = load i8*, i8** %5, align 8
  %14 = load i8, i8* %13, align 1
  %15 = sext i8 %14 to i32
  store i32 %15, i32* %7, align 4
  %16 = load i32, i32* %6, align 4
  %17 = load i32, i32* %7, align 4
  %18 = sub nsw i32 %16, %17
  store i32 %18, i32* %8, align 4
  %19 = load i32, i32* %8, align 4
  %20 = icmp eq i32 %19, 0
  br i1 %20, label %21, label %31

; <label>:21:                                     ; preds = %9
  %22 = load i32, i32* %6, align 4
  %23 = icmp eq i32 %22, 0
  br i1 %23, label %24, label %25

; <label>:24:                                     ; preds = %21
  store i32 0, i32* %3, align 4
  br label %34

; <label>:25:                                     ; preds = %21
  %26 = load i8*, i8** %4, align 8
  %27 = getelementptr inbounds i8, i8* %26, i32 1
  store i8* %27, i8** %4, align 8
  %28 = load i8*, i8** %5, align 8
  %29 = getelementptr inbounds i8, i8* %28, i32 1
  store i8* %29, i8** %5, align 8
  br label %30

; <label>:30:                                     ; preds = %25
  br label %33

; <label>:31:                                     ; preds = %9
  %32 = load i32, i32* %8, align 4
  store i32 %32, i32* %3, align 4
  br label %34

; <label>:33:                                     ; preds = %30
  br label %9

; <label>:34:                                     ; preds = %31, %24
  %35 = load i32, i32* %3, align 4
  ret i32 %35
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
