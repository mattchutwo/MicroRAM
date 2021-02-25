; ModuleID = 'pos/simple_heartbleed/heartbleed.c'
source_filename = "pos/simple_heartbleed/heartbleed.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.14.0"

@requests = global <{ <{ [10 x i8], [10 x i8] }>, <{ i8, i8, i8, i8, [16 x i8] }> }> <{ <{ [10 x i8], [10 x i8] }> <{ [10 x i8] c"\0A\00password", [10 x i8] zeroinitializer }>, <{ i8, i8, i8, i8, [16 x i8] }> <{ i8 3, i8 1, i8 7, i8 104, [16 x i8] zeroinitializer }> }>, align 16
@p = global i8* null, align 8
@.str = private unnamed_addr constant [9 x i8] c"password\00", align 1
@.str.1 = private unnamed_addr constant [11 x i8] c"Logged in!\00", align 1
@.str.2 = private unnamed_addr constant [20 x i8] c"Incorrect password.\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @my_malloc(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i8*, i8** @p, align 8
  %4 = icmp eq i8* %3, null
  br i1 %4, label %5, label %7

; <label>:5:                                      ; preds = %1
  %6 = call i8* @malloc(i64 20) #3
  store i8* %6, i8** @p, align 8
  br label %7

; <label>:7:                                      ; preds = %5, %1
  %8 = load i8*, i8** @p, align 8
  ret i8* %8
}

; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #1

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

; <label>:8:                                      ; preds = %26, %3
  %9 = load i32, i32* %7, align 4
  %10 = load i32, i32* %6, align 4
  %11 = icmp slt i32 %9, %10
  br i1 %11, label %12, label %29

; <label>:12:                                     ; preds = %8
  %13 = load i8*, i8** %5, align 8
  %14 = load i32, i32* %7, align 4
  %15 = sext i32 %14 to i64
  %16 = getelementptr inbounds i8, i8* %13, i64 %15
  %17 = load i32, i32* %4, align 4
  %18 = trunc i32 %17 to i16
  call void @noniSink(i8* %16, i16 zeroext %18)
  %19 = load i8*, i8** %5, align 8
  %20 = load i32, i32* %7, align 4
  %21 = sext i32 %20 to i64
  %22 = getelementptr inbounds i8, i8* %19, i64 %21
  %23 = load i8, i8* %22, align 1
  %24 = sext i8 %23 to i32
  %25 = call i32 @putchar(i32 %24)
  br label %26

; <label>:26:                                     ; preds = %12
  %27 = load i32, i32* %7, align 4
  %28 = add nsw i32 %27, 1
  store i32 %28, i32* %7, align 4
  br label %8

; <label>:29:                                     ; preds = %8
  %30 = call i32 @putchar(i32 10)
  ret void
}

declare void @noniSink(i8*, i16 zeroext) #2

declare i32 @putchar(i32) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @run_login(i32, i8*) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i8* %1, i8** %4, align 8
  %8 = load i8*, i8** %4, align 8
  %9 = getelementptr inbounds i8, i8* %8, i64 1
  store i8* %9, i8** %5, align 8
  store i32 0, i32* %6, align 4
  br label %10

; <label>:10:                                     ; preds = %20, %2
  %11 = load i32, i32* %6, align 4
  %12 = icmp slt i32 %11, 19
  br i1 %12, label %13, label %23

; <label>:13:                                     ; preds = %10
  %14 = load i8*, i8** %5, align 8
  %15 = load i32, i32* %6, align 4
  %16 = sext i32 %15 to i64
  %17 = getelementptr inbounds i8, i8* %14, i64 %16
  %18 = load i32, i32* %3, align 4
  %19 = trunc i32 %18 to i16
  call void @noniSetLabel(i8* %17, i16 zeroext %19)
  br label %20

; <label>:20:                                     ; preds = %13
  %21 = load i32, i32* %6, align 4
  %22 = add nsw i32 %21, 1
  store i32 %22, i32* %6, align 4
  br label %10

; <label>:23:                                     ; preds = %10
  %24 = load i8*, i8** %5, align 8
  %25 = call i32 @strcmp(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @.str, i32 0, i32 0), i8* %24)
  %26 = icmp eq i32 %25, 0
  %27 = zext i1 %26 to i32
  store i32 %27, i32* %7, align 4
  %28 = load i32, i32* %7, align 4
  %29 = icmp ne i32 %28, 0
  br i1 %29, label %30, label %32

; <label>:30:                                     ; preds = %23
  %31 = load i32, i32* %3, align 4
  call void @write_response_to_network(i32 %31, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.1, i32 0, i32 0), i32 10)
  br label %34

; <label>:32:                                     ; preds = %23
  %33 = load i32, i32* %3, align 4
  call void @write_response_to_network(i32 %33, i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.2, i32 0, i32 0), i32 19)
  br label %34

; <label>:34:                                     ; preds = %32, %30
  ret void
}

declare void @noniSetLabel(i8*, i16 zeroext) #2

declare i32 @strcmp(i8*, i8*) #2

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
define i32 @run_handler(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i8*, align 8
  %4 = alloca i8, align 1
  store i32 %0, i32* %2, align 4
  %5 = load i32, i32* %2, align 4
  %6 = call i8* @read_request_from_network(i32 %5)
  store i8* %6, i8** %3, align 8
  %7 = load i8*, i8** %3, align 8
  %8 = getelementptr inbounds i8, i8* %7, i64 0
  %9 = load i8, i8* %8, align 1
  store i8 %9, i8* %4, align 1
  %10 = load i8, i8* %4, align 1
  %11 = sext i8 %10 to i32
  %12 = icmp eq i32 %11, 0
  br i1 %12, label %13, label %16

; <label>:13:                                     ; preds = %1
  %14 = load i32, i32* %2, align 4
  %15 = load i8*, i8** %3, align 8
  call void @run_login(i32 %14, i8* %15)
  br label %16

; <label>:16:                                     ; preds = %13, %1
  %17 = load i8, i8* %4, align 1
  %18 = sext i8 %17 to i32
  %19 = icmp eq i32 %18, 1
  br i1 %19, label %20, label %23

; <label>:20:                                     ; preds = %16
  %21 = load i32, i32* %2, align 4
  %22 = load i8*, i8** %3, align 8
  call void @run_heartbeat(i32 %21, i8* %22)
  br label %23

; <label>:23:                                     ; preds = %20, %16
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

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { allocsize(0) }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
