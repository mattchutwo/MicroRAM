; ModuleID = 'test/programs/binaryTree.c'
source_filename = "test/programs/binaryTree.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.node = type { i32, i32, %struct.node*, %struct.node* }

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
  %27 = getelementptr inbounds %struct.node, %struct.node* %26, i32 0, i32 2
  %28 = load %struct.node*, %struct.node** %27, align 8
  %29 = load i32, i32* %5, align 4
  %30 = call i32 @search(%struct.node* %28, i32 %29)
  store i32 %30, i32* %3, align 4
  br label %37

31:                                               ; preds = %19
  %32 = load %struct.node*, %struct.node** %4, align 8
  %33 = getelementptr inbounds %struct.node, %struct.node* %32, i32 0, i32 3
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
define %struct.node* @new_node(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca %struct.node*, align 8
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %6 = call i8* @malloc(i64 24)
  %7 = bitcast i8* %6 to %struct.node*
  store %struct.node* %7, %struct.node** %5, align 8
  %8 = load i32, i32* %3, align 4
  %9 = load %struct.node*, %struct.node** %5, align 8
  %10 = getelementptr inbounds %struct.node, %struct.node* %9, i32 0, i32 0
  store i32 %8, i32* %10, align 8
  %11 = load i32, i32* %4, align 4
  %12 = load %struct.node*, %struct.node** %5, align 8
  %13 = getelementptr inbounds %struct.node, %struct.node* %12, i32 0, i32 1
  store i32 %11, i32* %13, align 4
  %14 = load %struct.node*, %struct.node** %5, align 8
  %15 = getelementptr inbounds %struct.node, %struct.node* %14, i32 0, i32 3
  store %struct.node* null, %struct.node** %15, align 8
  %16 = load %struct.node*, %struct.node** %5, align 8
  %17 = getelementptr inbounds %struct.node, %struct.node* %16, i32 0, i32 2
  store %struct.node* null, %struct.node** %17, align 8
  %18 = load %struct.node*, %struct.node** %5, align 8
  ret %struct.node* %18
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.node* @insert(%struct.node*, i32, i32) #0 {
  %4 = alloca %struct.node*, align 8
  %5 = alloca %struct.node*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store %struct.node* %0, %struct.node** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  %8 = load %struct.node*, %struct.node** %5, align 8
  %9 = icmp eq %struct.node* %8, null
  br i1 %9, label %10, label %14

10:                                               ; preds = %3
  %11 = load i32, i32* %6, align 4
  %12 = load i32, i32* %7, align 4
  %13 = call %struct.node* @new_node(i32 %11, i32 %12)
  store %struct.node* %13, %struct.node** %4, align 8
  br label %41

14:                                               ; preds = %3
  %15 = load i32, i32* %6, align 4
  %16 = load %struct.node*, %struct.node** %5, align 8
  %17 = getelementptr inbounds %struct.node, %struct.node* %16, i32 0, i32 1
  %18 = load i32, i32* %17, align 4
  %19 = icmp sgt i32 %15, %18
  br i1 %19, label %20, label %29

20:                                               ; preds = %14
  %21 = load %struct.node*, %struct.node** %5, align 8
  %22 = getelementptr inbounds %struct.node, %struct.node* %21, i32 0, i32 2
  %23 = load %struct.node*, %struct.node** %22, align 8
  %24 = load i32, i32* %6, align 4
  %25 = load i32, i32* %7, align 4
  %26 = call %struct.node* @insert(%struct.node* %23, i32 %24, i32 %25)
  %27 = load %struct.node*, %struct.node** %5, align 8
  %28 = getelementptr inbounds %struct.node, %struct.node* %27, i32 0, i32 2
  store %struct.node* %26, %struct.node** %28, align 8
  br label %38

29:                                               ; preds = %14
  %30 = load %struct.node*, %struct.node** %5, align 8
  %31 = getelementptr inbounds %struct.node, %struct.node* %30, i32 0, i32 3
  %32 = load %struct.node*, %struct.node** %31, align 8
  %33 = load i32, i32* %6, align 4
  %34 = load i32, i32* %7, align 4
  %35 = call %struct.node* @insert(%struct.node* %32, i32 %33, i32 %34)
  %36 = load %struct.node*, %struct.node** %5, align 8
  %37 = getelementptr inbounds %struct.node, %struct.node* %36, i32 0, i32 3
  store %struct.node* %35, %struct.node** %37, align 8
  br label %38

38:                                               ; preds = %29, %20
  br label %39

39:                                               ; preds = %38
  %40 = load %struct.node*, %struct.node** %5, align 8
  store %struct.node* %40, %struct.node** %4, align 8
  br label %41

41:                                               ; preds = %39, %10
  %42 = load %struct.node*, %struct.node** %4, align 8
  ret %struct.node* %42
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.node*, align 8
  store i32 0, i32* %1, align 4
  %3 = call %struct.node* @new_node(i32 20, i32 40)
  store %struct.node* %3, %struct.node** %2, align 8
  %4 = load %struct.node*, %struct.node** %2, align 8
  %5 = call %struct.node* @insert(%struct.node* %4, i32 5, i32 10)
  %6 = load %struct.node*, %struct.node** %2, align 8
  %7 = call %struct.node* @insert(%struct.node* %6, i32 1, i32 2)
  %8 = load %struct.node*, %struct.node** %2, align 8
  %9 = call %struct.node* @insert(%struct.node* %8, i32 15, i32 30)
  %10 = load %struct.node*, %struct.node** %2, align 8
  %11 = call %struct.node* @insert(%struct.node* %10, i32 9, i32 18)
  %12 = load %struct.node*, %struct.node** %2, align 8
  %13 = call %struct.node* @insert(%struct.node* %12, i32 7, i32 14)
  %14 = load %struct.node*, %struct.node** %2, align 8
  %15 = call %struct.node* @insert(%struct.node* %14, i32 12, i32 24)
  %16 = load %struct.node*, %struct.node** %2, align 8
  %17 = call %struct.node* @insert(%struct.node* %16, i32 30, i32 60)
  %18 = load %struct.node*, %struct.node** %2, align 8
  %19 = call %struct.node* @insert(%struct.node* %18, i32 25, i32 50)
  %20 = load %struct.node*, %struct.node** %2, align 8
  %21 = call %struct.node* @insert(%struct.node* %20, i32 40, i32 80)
  %22 = load %struct.node*, %struct.node** %2, align 8
  %23 = call %struct.node* @insert(%struct.node* %22, i32 45, i32 90)
  %24 = load %struct.node*, %struct.node** %2, align 8
  %25 = call %struct.node* @insert(%struct.node* %24, i32 42, i32 84)
  %26 = load %struct.node*, %struct.node** %2, align 8
  %27 = call i32 @search(%struct.node* %26, i32 42)
  ret i32 %27
}

; Function Attrs: nounwind uwtable
define dso_local noalias i8* @malloc(i64) local_unnamed_addr #3 {
  %2 = tail call i64* @malloc_words(i64 %0) #22
  %3 = bitcast i64* %2 to i8*
  ret i8* %3
}

; Function Attrs: nounwind uwtable
define dso_local void @free(i8*) local_unnamed_addr #3 {
  %2 = bitcast i8* %0 to i64*
  tail call void @free_words(i64* %2) #22
  ret void
}


; Function Attrs: nounwind uwtable
define dso_local i64* @malloc_words(i64) local_unnamed_addr #3 {
  %2 = tail call i64* @__cc_malloc(i64 %0) #21
  %3 = ptrtoint i64* %2 to i64
  %4 = lshr i64 %3, 58
  %5 = shl i64 1, %4
  %6 = add i64 %0, 1
  %7 = icmp ult i64 %5, %6
  br i1 %7, label %12, label %8

8:                                                ; preds = %1
  %9 = add i64 %5, -1
  %10 = and i64 %9, %3
  %11 = icmp eq i64 %10, 0
  br i1 %11, label %13, label %12

12:                                               ; preds = %8, %1
  tail call void @__cc_flag_invalid() #21
  br label %13

13:                                               ; preds = %12, %8
  %14 = getelementptr inbounds i64, i64* %2, i64 %5
  %15 = getelementptr inbounds i64, i64* %14, i64 -1
  tail call void @__cc_write_and_poison(i64* nonnull %15, i64 1) #21
  %16 = getelementptr inbounds i64, i64* %2, i64 %0
  %17 = tail call i64* @__cc_advise_poison(i64* %16, i64* nonnull %15) #21
  %18 = icmp eq i64* %17, null
  br i1 %18, label %25, label %19

19:                                               ; preds = %13
  %20 = icmp ugt i64* %16, %17
  %21 = icmp uge i64* %17, %15
  %22 = or i1 %20, %21
  br i1 %22, label %23, label %24

23:                                               ; preds = %19
  tail call void @__cc_flag_invalid() #21
  br label %24

24:                                               ; preds = %23, %19
  tail call void @__cc_write_and_poison(i64* nonnull %17, i64 0) #21
  br label %25

25:                                               ; preds = %24, %13
  ret i64* %2
}



declare dso_local i64* @__cc_malloc(i64) local_unnamed_addr #9

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #9

declare dso_local i64* @__cc_advise_poison(i64*, i64*) local_unnamed_addr #9

declare dso_local void @__cc_flag_bug() local_unnamed_addr #9

declare dso_local void @__cc_flag_invalid() local_unnamed_addr #9

declare dso_local void @__cc_free(i64*) local_unnamed_addr #9

; Function Attrs: nounwind uwtable
define dso_local void @free_words(i64*) local_unnamed_addr #3 {
  %2 = icmp eq i64* %0, null
  br i1 %2, label %27, label %3

3:                                                ; preds = %1
  %4 = ptrtoint i64* %0 to i64
  %5 = lshr i64 %4, 58
  %6 = shl i64 1, %5
  %7 = add i64 %6, -1
  %8 = and i64 %7, %4
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %11, label %10

10:                                               ; preds = %3
  tail call void @__cc_flag_bug() #21
  br label %11

11:                                               ; preds = %10, %3
  store i64 0, i64* %0, align 8, !tbaa !59
  tail call void @__cc_free(i64* nonnull %0) #21
  %12 = getelementptr inbounds i64, i64* %0, i64 %6
  %13 = getelementptr inbounds i64, i64* %12, i64 -1
  %14 = tail call i64* @__cc_advise_poison(i64* nonnull %0, i64* nonnull %13) #21
  %15 = icmp eq i64* %14, null
  br i1 %15, label %27, label %16

16:                                               ; preds = %11
  %17 = ptrtoint i64* %14 to i64
  %18 = and i64 %17, 7
  %19 = icmp eq i64 %18, 0
  br i1 %19, label %21, label %20

20:                                               ; preds = %16
  tail call void @__cc_flag_invalid() #21
  br label %21

21:                                               ; preds = %20, %16
  %22 = icmp ult i64* %14, %0
  %23 = icmp uge i64* %14, %13
  %24 = or i1 %22, %23
  br i1 %24, label %25, label %26

25:                                               ; preds = %21
  tail call void @__cc_flag_invalid() #21
  br label %26

26:                                               ; preds = %25, %21
  tail call void @__cc_write_and_poison(i64* nonnull %14, i64 0) #21
  br label %27

27:                                               ; preds = %26, %11, %1
  ret void
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 4]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.3 (clang-1103.0.32.29)"}


