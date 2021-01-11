; ModuleID = './LinkedList/linkedList.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, i32, %struct.node* }

@head = internal unnamed_addr global %struct.node* null, align 8
@SECRET_NUMBER = dso_local local_unnamed_addr global i32 42, section "__DATA,__secret", align 4

; Function Attrs:  nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  tail call fastcc void @insertFirst(i32 1, i32 10)
  tail call fastcc void @insertFirst(i32 2, i32 20)
  %1 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  tail call fastcc void @insertFirst(i32 3, i32 %1)
  tail call fastcc void @insertFirst(i32 4, i32 1)
  tail call fastcc void @insertFirst(i32 5, i32 55)
  tail call fastcc void @insertFirst(i32 6, i32 56)
  %2 = load %struct.node*, %struct.node** @head, align 8, !tbaa !8
  %3 = getelementptr inbounds %struct.node, %struct.node* %2, i64 0, i32 1
  %4 = load i32, i32* %3, align 4, !tbaa !10
  %5 = icmp eq i32 %4, 6
  br i1 %5, label %16, label %.preheader.i

6:                                                ; preds = %.preheader.i
  %7 = getelementptr inbounds %struct.node, %struct.node* %12, i64 0, i32 1
  %8 = load i32, i32* %7, align 4, !tbaa !10
  %9 = icmp eq i32 %8, 6
  br i1 %9, label %14, label %.preheader.i

.preheader.i:                                     ; preds = %0, %6
  %10 = phi %struct.node* [ %12, %6 ], [ %2, %0 ]
  %11 = getelementptr inbounds %struct.node, %struct.node* %10, i64 0, i32 2
  %12 = load %struct.node*, %struct.node** %11, align 8, !tbaa !12
  %13 = icmp eq %struct.node* %12, null
  br i1 %13, label %delete.exit, label %6

14:                                               ; preds = %6
  %15 = icmp eq %struct.node* %12, %2
  br i1 %15, label %16, label %21

16:                                               ; preds = %14, %0
  %17 = getelementptr inbounds %struct.node, %struct.node* %2, i64 0, i32 2
  %18 = bitcast %struct.node** %17 to i64*
  %19 = load i64, i64* %18, align 8, !tbaa !12
  store i64 %19, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %20 = inttoptr i64 %19 to %struct.node*
  br label %delete.exit

21:                                               ; preds = %14
  %22 = getelementptr inbounds %struct.node, %struct.node* %10, i64 0, i32 2
  %23 = getelementptr inbounds %struct.node, %struct.node* %12, i64 0, i32 2
  %24 = bitcast %struct.node** %23 to i64*
  %25 = load i64, i64* %24, align 8, !tbaa !12
  %26 = bitcast %struct.node** %22 to i64*
  store i64 %25, i64* %26, align 8, !tbaa !12
  br label %delete.exit

delete.exit:                                      ; preds = %.preheader.i, %21, %16
  %.pr1044 = phi %struct.node* [ %20, %16 ], [ %2, %21 ], [ %2, %.preheader.i ]
  %27 = icmp eq %struct.node* %.pr1044, null
  br i1 %27, label %delete.exit5, label %28

28:                                               ; preds = %delete.exit
  %29 = getelementptr inbounds %struct.node, %struct.node* %.pr1044, i64 0, i32 1
  %30 = load i32, i32* %29, align 4, !tbaa !10
  %31 = icmp eq i32 %30, 4
  br i1 %31, label %42, label %.preheader.i6

32:                                               ; preds = %.preheader.i6
  %33 = getelementptr inbounds %struct.node, %struct.node* %38, i64 0, i32 1
  %34 = load i32, i32* %33, align 4, !tbaa !10
  %35 = icmp eq i32 %34, 4
  br i1 %35, label %40, label %.preheader.i6

.preheader.i6:                                    ; preds = %28, %32
  %36 = phi %struct.node* [ %38, %32 ], [ %.pr1044, %28 ]
  %37 = getelementptr inbounds %struct.node, %struct.node* %36, i64 0, i32 2
  %38 = load %struct.node*, %struct.node** %37, align 8, !tbaa !12
  %39 = icmp eq %struct.node* %38, null
  br i1 %39, label %delete.exit8, label %32

40:                                               ; preds = %32
  %41 = icmp eq %struct.node* %38, %.pr1044
  br i1 %41, label %42, label %47

42:                                               ; preds = %40, %28
  %43 = getelementptr inbounds %struct.node, %struct.node* %.pr1044, i64 0, i32 2
  %44 = bitcast %struct.node** %43 to i64*
  %45 = load i64, i64* %44, align 8, !tbaa !12
  store i64 %45, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %46 = inttoptr i64 %45 to %struct.node*
  br label %delete.exit8

47:                                               ; preds = %40
  %48 = getelementptr inbounds %struct.node, %struct.node* %36, i64 0, i32 2
  %49 = getelementptr inbounds %struct.node, %struct.node* %38, i64 0, i32 2
  %50 = bitcast %struct.node** %49 to i64*
  %51 = load i64, i64* %50, align 8, !tbaa !12
  %52 = bitcast %struct.node** %48 to i64*
  store i64 %51, i64* %52, align 8, !tbaa !12
  br label %delete.exit8

delete.exit8:                                     ; preds = %.preheader.i6, %47, %42
  %.pr10 = phi %struct.node* [ %46, %42 ], [ %.pr1044, %47 ], [ %.pr1044, %.preheader.i6 ]
  %53 = icmp eq %struct.node* %.pr10, null
  br i1 %53, label %delete.exit5, label %54

54:                                               ; preds = %delete.exit8
  %55 = getelementptr inbounds %struct.node, %struct.node* %.pr10, i64 0, i32 1
  %56 = load i32, i32* %55, align 4, !tbaa !10
  %57 = icmp eq i32 %56, 2
  br i1 %57, label %68, label %.preheader.i3

58:                                               ; preds = %.preheader.i3
  %59 = getelementptr inbounds %struct.node, %struct.node* %64, i64 0, i32 1
  %60 = load i32, i32* %59, align 4, !tbaa !10
  %61 = icmp eq i32 %60, 2
  br i1 %61, label %66, label %.preheader.i3

.preheader.i3:                                    ; preds = %54, %58
  %62 = phi %struct.node* [ %64, %58 ], [ %.pr10, %54 ]
  %63 = getelementptr inbounds %struct.node, %struct.node* %62, i64 0, i32 2
  %64 = load %struct.node*, %struct.node** %63, align 8, !tbaa !12
  %65 = icmp eq %struct.node* %64, null
  br i1 %65, label %delete.exit5, label %58

66:                                               ; preds = %58
  %67 = icmp eq %struct.node* %64, %.pr10
  br i1 %67, label %68, label %73

68:                                               ; preds = %66, %54
  %69 = getelementptr inbounds %struct.node, %struct.node* %.pr10, i64 0, i32 2
  %70 = bitcast %struct.node** %69 to i64*
  %71 = load i64, i64* %70, align 8, !tbaa !12
  store i64 %71, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %72 = inttoptr i64 %71 to %struct.node*
  br label %delete.exit5

73:                                               ; preds = %66
  %74 = getelementptr inbounds %struct.node, %struct.node* %62, i64 0, i32 2
  %75 = getelementptr inbounds %struct.node, %struct.node* %64, i64 0, i32 2
  %76 = bitcast %struct.node** %75 to i64*
  %77 = load i64, i64* %76, align 8, !tbaa !12
  %78 = bitcast %struct.node** %74 to i64*
  store i64 %77, i64* %78, align 8, !tbaa !12
  br label %delete.exit5

delete.exit5:                                     ; preds = %.preheader.i3, %delete.exit, %delete.exit8, %73, %68
  %79 = phi %struct.node* [ %72, %68 ], [ %.pr10, %73 ], [ null, %delete.exit8 ], [ null, %delete.exit ], [ %.pr10, %.preheader.i3 ]
  br label %.preheader.i.i

.preheader.i.i:                                   ; preds = %delete.exit5, %.preheader.i.i
  %80 = phi %struct.node* [ %84, %.preheader.i.i ], [ %79, %delete.exit5 ]
  %81 = phi i32 [ %82, %.preheader.i.i ], [ 0, %delete.exit5 ]
  %82 = add nuw nsw i32 %81, 1
  %83 = getelementptr inbounds %struct.node, %struct.node* %80, i64 0, i32 2
  %84 = load %struct.node*, %struct.node** %83, align 8, !tbaa !8
  %85 = icmp eq %struct.node* %84, null
  br i1 %85, label %length.exit.i, label %.preheader.i.i

length.exit.i:                                    ; preds = %.preheader.i.i
  %86 = icmp eq i32 %81, 0
  br i1 %86, label %.preheader.i1.preheader, label %87

87:                                               ; preds = %length.exit.i
  %88 = getelementptr inbounds %struct.node, %struct.node* %79, i64 0, i32 2
  %89 = load %struct.node*, %struct.node** %88, align 8, !tbaa !12
  br label %90

90:                                               ; preds = %.loopexit.i, %87
  %91 = phi i32 [ 0, %87 ], [ %114, %.loopexit.i ]
  %92 = phi i32 [ %82, %87 ], [ %115, %.loopexit.i ]
  %93 = icmp sgt i32 %92, 1
  br i1 %93, label %.preheader.i2, label %.loopexit.i

.preheader.i2:                                    ; preds = %90, %107
  %94 = phi %struct.node* [ %112, %107 ], [ %89, %90 ]
  %95 = phi %struct.node* [ %109, %107 ], [ %79, %90 ]
  %96 = phi i32 [ %110, %107 ], [ 1, %90 ]
  %97 = getelementptr inbounds %struct.node, %struct.node* %95, i64 0, i32 0
  %98 = load i32, i32* %97, align 8, !tbaa !13
  %99 = getelementptr inbounds %struct.node, %struct.node* %94, i64 0, i32 0
  %100 = load i32, i32* %99, align 8, !tbaa !13
  %101 = icmp sgt i32 %98, %100
  br i1 %101, label %102, label %107

102:                                              ; preds = %.preheader.i2
  store i32 %100, i32* %97, align 8, !tbaa !13
  store i32 %98, i32* %99, align 8, !tbaa !13
  %103 = getelementptr inbounds %struct.node, %struct.node* %95, i64 0, i32 1
  %104 = load i32, i32* %103, align 4, !tbaa !10
  %105 = getelementptr inbounds %struct.node, %struct.node* %94, i64 0, i32 1
  %106 = load i32, i32* %105, align 4, !tbaa !10
  store i32 %106, i32* %103, align 4, !tbaa !10
  store i32 %104, i32* %105, align 4, !tbaa !10
  br label %107

107:                                              ; preds = %102, %.preheader.i2
  %108 = getelementptr inbounds %struct.node, %struct.node* %95, i64 0, i32 2
  %109 = load %struct.node*, %struct.node** %108, align 8, !tbaa !12
  %110 = add nuw nsw i32 %96, 1
  %111 = getelementptr inbounds %struct.node, %struct.node* %94, i64 0, i32 2
  %112 = load %struct.node*, %struct.node** %111, align 8, !tbaa !12
  %113 = icmp eq i32 %110, %92
  br i1 %113, label %.loopexit.i, label %.preheader.i2

.loopexit.i:                                      ; preds = %107, %90
  %114 = add nuw nsw i32 %91, 1
  %115 = add nsw i32 %92, -1
  %116 = icmp eq i32 %114, %81
  br i1 %116, label %.preheader.i1.preheader, label %90

.preheader.i1.preheader:                          ; preds = %.loopexit.i, %length.exit.i
  br label %.preheader.i1

.preheader.i1:                                    ; preds = %.preheader.i1.preheader, %.preheader.i1
  %117 = phi %struct.node* [ %118, %.preheader.i1 ], [ null, %.preheader.i1.preheader ]
  %118 = phi %struct.node* [ %120, %.preheader.i1 ], [ %79, %.preheader.i1.preheader ]
  %119 = getelementptr inbounds %struct.node, %struct.node* %118, i64 0, i32 2
  %120 = load %struct.node*, %struct.node** %119, align 8, !tbaa !12
  store %struct.node* %117, %struct.node** %119, align 8, !tbaa !12
  %121 = icmp eq %struct.node* %120, null
  br i1 %121, label %reverse.exit, label %.preheader.i1

reverse.exit:                                     ; preds = %.preheader.i1
  %122 = getelementptr inbounds %struct.node, %struct.node* %117, i64 0, i32 2
  %123 = bitcast %struct.node** %122 to i64*
  %124 = load i64, i64* %123, align 8, !tbaa !12
  store i64 %124, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %125 = getelementptr inbounds %struct.node, %struct.node* %117, i64 0, i32 0
  %126 = load i32, i32* %125, align 8, !tbaa !13
  ret i32 %126
}

; Function Attrs:  nounwind uwtable
define internal fastcc void @insertFirst(i32, i32) unnamed_addr #0 {
  %3 = tail call i8* @__cc_malloc(i64 16) #5
  %4 = ptrtoint i8* %3 to i64
  %5 = lshr i64 %4, 58
  %6 = shl i64 1, %5
  %7 = icmp ult i8* %3, inttoptr (i64 1441151880758558720 to i8*)
  br i1 %7, label %12, label %8

8:                                                ; preds = %2
  %9 = add i64 %6, -1
  %10 = and i64 %9, %4
  %11 = icmp eq i64 %10, 0
  br i1 %11, label %13, label %12

12:                                               ; preds = %8, %2
  tail call void @__cc_flag_invalid() #5
  br label %13

13:                                               ; preds = %12, %8
  %14 = getelementptr inbounds i8, i8* %3, i64 %6
  %15 = getelementptr inbounds i8, i8* %14, i64 -8
  %16 = bitcast i8* %15 to i64*
  tail call void @__cc_write_and_poison(i64* nonnull %16, i64 1) #5
  %17 = getelementptr inbounds i8, i8* %3, i64 16
  %18 = tail call i64* @__cc_advise_poison(i8* nonnull %17, i8* nonnull %15) #5
  %19 = icmp eq i64* %18, null
  br i1 %19, label %malloc.exit, label %20

20:                                               ; preds = %13
  %21 = ptrtoint i64* %18 to i64
  %22 = and i64 %21, 7
  %23 = icmp eq i64 %22, 0
  br i1 %23, label %25, label %24

24:                                               ; preds = %20
  tail call void @__cc_flag_invalid() #5
  br label %25

25:                                               ; preds = %24, %20
  %26 = bitcast i64* %18 to i8*
  %27 = icmp ugt i8* %17, %26
  %28 = icmp uge i64* %18, %16
  %29 = or i1 %28, %27
  br i1 %29, label %30, label %31

30:                                               ; preds = %25
  tail call void @__cc_flag_invalid() #5
  br label %31

31:                                               ; preds = %30, %25
  tail call void @__cc_write_and_poison(i64* nonnull %18, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %13, %31
  %32 = getelementptr inbounds i8, i8* %3, i64 4
  %33 = bitcast i8* %32 to i32*
  store i32 %0, i32* %33, align 4, !tbaa !10
  %34 = bitcast i8* %3 to i32*
  store i32 %1, i32* %34, align 8, !tbaa !13
  %35 = load i64, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %36 = getelementptr inbounds i8, i8* %3, i64 8
  %37 = bitcast i8* %36 to i64*
  store i64 %35, i64* %37, align 8, !tbaa !12
  store i8* %3, i8** bitcast (%struct.node** @head to i8**), align 8, !tbaa !8
  ret void
}

declare dso_local i8* @__cc_malloc(i64) local_unnamed_addr #1

declare dso_local void @__cc_flag_invalid() local_unnamed_addr #1

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare dso_local i64* @__cc_advise_poison(i8*, i8*) local_unnamed_addr #1

; Function Attrs:  norecurse nounwind uwtable
define dso_local void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #2 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %.loopexit, label %5

5:                                                ; preds = %3
  %6 = add i64 %2, -1
  %7 = and i64 %2, 3
  %8 = icmp ult i64 %6, 3
  br i1 %8, label %.loopexit2, label %9

9:                                                ; preds = %5
  %10 = sub nuw i64 %2, %7
  br label %21

.loopexit2:                                       ; preds = %21, %5
  %11 = phi i64 [ 0, %5 ], [ %39, %21 ]
  %12 = icmp eq i64 %7, 0
  br i1 %12, label %.loopexit, label %.preheader

.preheader:                                       ; preds = %.loopexit2, %.preheader
  %13 = phi i64 [ %18, %.preheader ], [ %11, %.loopexit2 ]
  %14 = phi i64 [ %19, %.preheader ], [ %7, %.loopexit2 ]
  %15 = getelementptr inbounds i8, i8* %1, i64 %13
  %16 = load i8, i8* %15, align 1, !tbaa !14
  %17 = getelementptr inbounds i8, i8* %0, i64 %13
  store i8 %16, i8* %17, align 1, !tbaa !14
  %18 = add nuw i64 %13, 1
  %19 = add nsw i64 %14, -1
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %.loopexit, label %.preheader, !llvm.loop !15

.loopexit:                                        ; preds = %.preheader, %.loopexit2, %3
  ret void

21:                                               ; preds = %21, %9
  %22 = phi i64 [ 0, %9 ], [ %39, %21 ]
  %23 = phi i64 [ %10, %9 ], [ %40, %21 ]
  %24 = getelementptr inbounds i8, i8* %1, i64 %22
  %25 = load i8, i8* %24, align 1, !tbaa !14
  %26 = getelementptr inbounds i8, i8* %0, i64 %22
  store i8 %25, i8* %26, align 1, !tbaa !14
  %27 = or i64 %22, 1
  %28 = getelementptr inbounds i8, i8* %1, i64 %27
  %29 = load i8, i8* %28, align 1, !tbaa !14
  %30 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %29, i8* %30, align 1, !tbaa !14
  %31 = or i64 %22, 2
  %32 = getelementptr inbounds i8, i8* %1, i64 %31
  %33 = load i8, i8* %32, align 1, !tbaa !14
  %34 = getelementptr inbounds i8, i8* %0, i64 %31
  store i8 %33, i8* %34, align 1, !tbaa !14
  %35 = or i64 %22, 3
  %36 = getelementptr inbounds i8, i8* %1, i64 %35
  %37 = load i8, i8* %36, align 1, !tbaa !14
  %38 = getelementptr inbounds i8, i8* %0, i64 %35
  store i8 %37, i8* %38, align 1, !tbaa !14
  %39 = add nuw i64 %22, 4
  %40 = add i64 %23, -4
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %.loopexit2, label %21
}

; Function Attrs:  norecurse nounwind uwtable writeonly
define dso_local void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #3 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %.loopexit, label %5

5:                                                ; preds = %3
  %6 = add i64 %2, -1
  %7 = and i64 %2, 7
  %8 = icmp ult i64 %6, 7
  br i1 %8, label %.loopexit2, label %9

9:                                                ; preds = %5
  %10 = sub nuw i64 %2, %7
  br label %13

.loopexit2:                                       ; preds = %13, %5
  %11 = phi i64 [ 0, %5 ], [ %31, %13 ]
  %12 = icmp eq i64 %7, 0
  br i1 %12, label %.loopexit, label %.preheader.preheader

.preheader.preheader:                             ; preds = %.loopexit2
  %scevgep = getelementptr i8, i8* %0, i64 %11
  tail call void @llvm.memset.p0i8.i64(i8* align 1 %scevgep, i8 %1, i64 %7, i1 false)
  br label %.loopexit

.loopexit:                                        ; preds = %.preheader.preheader, %.loopexit2, %3
  ret void

13:                                               ; preds = %13, %9
  %14 = phi i64 [ 0, %9 ], [ %31, %13 ]
  %15 = phi i64 [ %10, %9 ], [ %32, %13 ]
  %16 = getelementptr inbounds i8, i8* %0, i64 %14
  store i8 %1, i8* %16, align 1, !tbaa !14
  %17 = or i64 %14, 1
  %18 = getelementptr inbounds i8, i8* %0, i64 %17
  store i8 %1, i8* %18, align 1, !tbaa !14
  %19 = or i64 %14, 2
  %20 = getelementptr inbounds i8, i8* %0, i64 %19
  store i8 %1, i8* %20, align 1, !tbaa !14
  %21 = or i64 %14, 3
  %22 = getelementptr inbounds i8, i8* %0, i64 %21
  store i8 %1, i8* %22, align 1, !tbaa !14
  %23 = or i64 %14, 4
  %24 = getelementptr inbounds i8, i8* %0, i64 %23
  store i8 %1, i8* %24, align 1, !tbaa !14
  %25 = or i64 %14, 5
  %26 = getelementptr inbounds i8, i8* %0, i64 %25
  store i8 %1, i8* %26, align 1, !tbaa !14
  %27 = or i64 %14, 6
  %28 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %1, i8* %28, align 1, !tbaa !14
  %29 = or i64 %14, 7
  %30 = getelementptr inbounds i8, i8* %0, i64 %29
  store i8 %1, i8* %30, align 1, !tbaa !14
  %31 = add nuw i64 %14, 8
  %32 = add i64 %15, -8
  %33 = icmp eq i64 %32, 0
  br i1 %33, label %.loopexit2, label %13
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #4

attributes #0 = {  nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = {  norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { nobuiltin nounwind }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"clang version 9.0.1-12 "}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"ThinLTO", i32 0}
!3 = !{i32 1, !"EnableSplitLTOUnit", i32 0}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!9, !9, i64 0}
!9 = !{!"any pointer", !6, i64 0}
!10 = !{!11, !5, i64 4}
!11 = !{!"node", !5, i64 0, !5, i64 4, !9, i64 8}
!12 = !{!11, !9, i64 8}
!13 = !{!11, !5, i64 0}
!14 = !{!6, !6, i64 0}
!15 = distinct !{!15, !16}
!16 = !{!"llvm.loop.unroll.disable"}
