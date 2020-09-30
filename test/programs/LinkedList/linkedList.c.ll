; ModuleID = 'linkedList.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.node = type { i32, i32, %struct.node* }

@head = internal unnamed_addr global %struct.node* null, align 8
@SECRET_NUMBER = local_unnamed_addr global i32 42, section "__DATA,__secret", align 4

; Function Attrs:  nounwind ssp uwtable
define i32 @main() local_unnamed_addr #0 {
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
  br i1 %5, label %.thread.i, label %.preheader.i

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
  br i1 %15, label %.thread.i, label %20

.thread.i:                                        ; preds = %14, %0
  %16 = getelementptr inbounds %struct.node, %struct.node* %2, i64 0, i32 2
  %17 = bitcast %struct.node** %16 to i64*
  %18 = load i64, i64* %17, align 8, !tbaa !12
  store i64 %18, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %19 = inttoptr i64 %18 to %struct.node*
  br label %delete.exit

20:                                               ; preds = %14
  %21 = getelementptr inbounds %struct.node, %struct.node* %10, i64 0, i32 2
  %22 = getelementptr inbounds %struct.node, %struct.node* %12, i64 0, i32 2
  %23 = bitcast %struct.node** %22 to i64*
  %24 = load i64, i64* %23, align 8, !tbaa !12
  %25 = bitcast %struct.node** %21 to i64*
  store i64 %24, i64* %25, align 8, !tbaa !12
  br label %delete.exit

delete.exit:                                      ; preds = %.preheader.i, %20, %.thread.i
  %.pr1246 = phi %struct.node* [ %19, %.thread.i ], [ %2, %20 ], [ %2, %.preheader.i ]
  %26 = icmp eq %struct.node* %.pr1246, null
  br i1 %26, label %delete.exit6, label %27

27:                                               ; preds = %delete.exit
  %28 = getelementptr inbounds %struct.node, %struct.node* %.pr1246, i64 0, i32 1
  %29 = load i32, i32* %28, align 4, !tbaa !10
  %30 = icmp eq i32 %29, 4
  br i1 %30, label %.thread.i8, label %.preheader.i7

31:                                               ; preds = %.preheader.i7
  %32 = getelementptr inbounds %struct.node, %struct.node* %37, i64 0, i32 1
  %33 = load i32, i32* %32, align 4, !tbaa !10
  %34 = icmp eq i32 %33, 4
  br i1 %34, label %39, label %.preheader.i7

.preheader.i7:                                    ; preds = %27, %31
  %35 = phi %struct.node* [ %37, %31 ], [ %.pr1246, %27 ]
  %36 = getelementptr inbounds %struct.node, %struct.node* %35, i64 0, i32 2
  %37 = load %struct.node*, %struct.node** %36, align 8, !tbaa !12
  %38 = icmp eq %struct.node* %37, null
  br i1 %38, label %delete.exit10, label %31

39:                                               ; preds = %31
  %40 = icmp eq %struct.node* %37, %.pr1246
  br i1 %40, label %.thread.i8, label %45

.thread.i8:                                       ; preds = %39, %27
  %41 = getelementptr inbounds %struct.node, %struct.node* %.pr1246, i64 0, i32 2
  %42 = bitcast %struct.node** %41 to i64*
  %43 = load i64, i64* %42, align 8, !tbaa !12
  store i64 %43, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %44 = inttoptr i64 %43 to %struct.node*
  br label %delete.exit10

45:                                               ; preds = %39
  %46 = getelementptr inbounds %struct.node, %struct.node* %35, i64 0, i32 2
  %47 = getelementptr inbounds %struct.node, %struct.node* %37, i64 0, i32 2
  %48 = bitcast %struct.node** %47 to i64*
  %49 = load i64, i64* %48, align 8, !tbaa !12
  %50 = bitcast %struct.node** %46 to i64*
  store i64 %49, i64* %50, align 8, !tbaa !12
  br label %delete.exit10

delete.exit10:                                    ; preds = %.preheader.i7, %45, %.thread.i8
  %.pr12 = phi %struct.node* [ %44, %.thread.i8 ], [ %.pr1246, %45 ], [ %.pr1246, %.preheader.i7 ]
  %51 = icmp eq %struct.node* %.pr12, null
  br i1 %51, label %delete.exit6, label %52

52:                                               ; preds = %delete.exit10
  %53 = getelementptr inbounds %struct.node, %struct.node* %.pr12, i64 0, i32 1
  %54 = load i32, i32* %53, align 4, !tbaa !10
  %55 = icmp eq i32 %54, 2
  br i1 %55, label %.thread.i4, label %.preheader.i3

56:                                               ; preds = %.preheader.i3
  %57 = getelementptr inbounds %struct.node, %struct.node* %62, i64 0, i32 1
  %58 = load i32, i32* %57, align 4, !tbaa !10
  %59 = icmp eq i32 %58, 2
  br i1 %59, label %64, label %.preheader.i3

.preheader.i3:                                    ; preds = %52, %56
  %60 = phi %struct.node* [ %62, %56 ], [ %.pr12, %52 ]
  %61 = getelementptr inbounds %struct.node, %struct.node* %60, i64 0, i32 2
  %62 = load %struct.node*, %struct.node** %61, align 8, !tbaa !12
  %63 = icmp eq %struct.node* %62, null
  br i1 %63, label %delete.exit6, label %56

64:                                               ; preds = %56
  %65 = icmp eq %struct.node* %62, %.pr12
  br i1 %65, label %.thread.i4, label %70

.thread.i4:                                       ; preds = %64, %52
  %66 = getelementptr inbounds %struct.node, %struct.node* %.pr12, i64 0, i32 2
  %67 = bitcast %struct.node** %66 to i64*
  %68 = load i64, i64* %67, align 8, !tbaa !12
  store i64 %68, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %69 = inttoptr i64 %68 to %struct.node*
  br label %delete.exit6

70:                                               ; preds = %64
  %71 = getelementptr inbounds %struct.node, %struct.node* %60, i64 0, i32 2
  %72 = getelementptr inbounds %struct.node, %struct.node* %62, i64 0, i32 2
  %73 = bitcast %struct.node** %72 to i64*
  %74 = load i64, i64* %73, align 8, !tbaa !12
  %75 = bitcast %struct.node** %71 to i64*
  store i64 %74, i64* %75, align 8, !tbaa !12
  br label %delete.exit6

delete.exit6:                                     ; preds = %.preheader.i3, %delete.exit, %delete.exit10, %70, %.thread.i4
  %76 = phi %struct.node* [ %69, %.thread.i4 ], [ %.pr12, %70 ], [ null, %delete.exit10 ], [ null, %delete.exit ], [ %.pr12, %.preheader.i3 ]
  br label %.preheader.i.i

.preheader.i.i:                                   ; preds = %delete.exit6, %.preheader.i.i
  %77 = phi %struct.node* [ %81, %.preheader.i.i ], [ %76, %delete.exit6 ]
  %78 = phi i32 [ %79, %.preheader.i.i ], [ 0, %delete.exit6 ]
  %79 = add nuw nsw i32 %78, 1
  %80 = getelementptr inbounds %struct.node, %struct.node* %77, i64 0, i32 2
  %81 = load %struct.node*, %struct.node** %80, align 8, !tbaa !8
  %82 = icmp eq %struct.node* %81, null
  br i1 %82, label %length.exit.i, label %.preheader.i.i

length.exit.i:                                    ; preds = %.preheader.i.i
  %83 = icmp eq i32 %78, 0
  br i1 %83, label %.preheader.i1.preheader, label %84

84:                                               ; preds = %length.exit.i
  %85 = getelementptr inbounds %struct.node, %struct.node* %76, i64 0, i32 2
  %86 = load %struct.node*, %struct.node** %85, align 8, !tbaa !12
  br label %87

87:                                               ; preds = %.loopexit.i, %84
  %88 = phi i32 [ 0, %84 ], [ %111, %.loopexit.i ]
  %89 = phi i32 [ %79, %84 ], [ %112, %.loopexit.i ]
  %90 = icmp sgt i32 %89, 1
  br i1 %90, label %.preheader.i2, label %.loopexit.i

.preheader.i2:                                    ; preds = %87, %104
  %91 = phi %struct.node* [ %109, %104 ], [ %86, %87 ]
  %92 = phi %struct.node* [ %106, %104 ], [ %76, %87 ]
  %93 = phi i32 [ %107, %104 ], [ 1, %87 ]
  %94 = getelementptr inbounds %struct.node, %struct.node* %92, i64 0, i32 0
  %95 = load i32, i32* %94, align 8, !tbaa !13
  %96 = getelementptr inbounds %struct.node, %struct.node* %91, i64 0, i32 0
  %97 = load i32, i32* %96, align 8, !tbaa !13
  %98 = icmp sgt i32 %95, %97
  br i1 %98, label %99, label %104

99:                                               ; preds = %.preheader.i2
  store i32 %97, i32* %94, align 8, !tbaa !13
  store i32 %95, i32* %96, align 8, !tbaa !13
  %100 = getelementptr inbounds %struct.node, %struct.node* %92, i64 0, i32 1
  %101 = load i32, i32* %100, align 4, !tbaa !10
  %102 = getelementptr inbounds %struct.node, %struct.node* %91, i64 0, i32 1
  %103 = load i32, i32* %102, align 4, !tbaa !10
  store i32 %103, i32* %100, align 4, !tbaa !10
  store i32 %101, i32* %102, align 4, !tbaa !10
  br label %104

104:                                              ; preds = %99, %.preheader.i2
  %105 = getelementptr inbounds %struct.node, %struct.node* %92, i64 0, i32 2
  %106 = load %struct.node*, %struct.node** %105, align 8, !tbaa !12
  %107 = add nuw nsw i32 %93, 1
  %108 = getelementptr inbounds %struct.node, %struct.node* %91, i64 0, i32 2
  %109 = load %struct.node*, %struct.node** %108, align 8, !tbaa !12
  %110 = icmp eq i32 %107, %89
  br i1 %110, label %.loopexit.i, label %.preheader.i2

.loopexit.i:                                      ; preds = %104, %87
  %111 = add nuw nsw i32 %88, 1
  %112 = add nsw i32 %89, -1
  %113 = icmp eq i32 %111, %78
  br i1 %113, label %.preheader.i1.preheader, label %87

.preheader.i1.preheader:                          ; preds = %.loopexit.i, %length.exit.i
  br label %.preheader.i1

.preheader.i1:                                    ; preds = %.preheader.i1.preheader, %.preheader.i1
  %114 = phi %struct.node* [ %115, %.preheader.i1 ], [ null, %.preheader.i1.preheader ]
  %115 = phi %struct.node* [ %117, %.preheader.i1 ], [ %76, %.preheader.i1.preheader ]
  %116 = getelementptr inbounds %struct.node, %struct.node* %115, i64 0, i32 2
  %117 = load %struct.node*, %struct.node** %116, align 8, !tbaa !12
  store %struct.node* %114, %struct.node** %116, align 8, !tbaa !12
  %118 = icmp eq %struct.node* %117, null
  br i1 %118, label %reverse.exit, label %.preheader.i1

reverse.exit:                                     ; preds = %.preheader.i1
  %119 = getelementptr inbounds %struct.node, %struct.node* %114, i64 0, i32 2
  %120 = bitcast %struct.node** %119 to i64*
  %121 = load i64, i64* %120, align 8, !tbaa !12
  store i64 %121, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %122 = getelementptr inbounds %struct.node, %struct.node* %114, i64 0, i32 0
  %123 = load i32, i32* %122, align 8, !tbaa !13
  ret i32 %123
}

; Function Attrs:  nounwind ssp uwtable
define internal fastcc void @insertFirst(i32, i32) unnamed_addr #0 {
  %3 = tail call i64* @__cc_malloc(i64 16) #5
  %4 = ptrtoint i64* %3 to i64
  %5 = lshr i64 %4, 58
  %6 = shl nuw i64 1, %5
  %7 = icmp ult i64* %3, inttoptr (i64 1441151880758558720 to i64*)
  %8 = add i64 %6, -1
  %9 = and i64 %8, %4
  %10 = icmp ne i64 %9, 0
  %11 = or i1 %7, %10
  br i1 %11, label %12, label %13

12:                                               ; preds = %2
  tail call void @__cc_flag_invalid() #5
  br label %13

13:                                               ; preds = %12, %2
  %14 = getelementptr inbounds i64, i64* %3, i64 %6
  %15 = getelementptr inbounds i64, i64* %14, i64 -1
  tail call void @__cc_write_and_poison(i64* nonnull %15, i64 1) #5
  %16 = getelementptr inbounds i64, i64* %3, i64 16
  %17 = tail call i64* @__cc_advise_poison(i64* nonnull %16, i64* nonnull %15) #5
  %18 = icmp eq i64* %17, null
  br i1 %18, label %malloc.exit, label %19

19:                                               ; preds = %13
  %20 = icmp ugt i64* %16, %17
  %21 = icmp uge i64* %17, %15
  %22 = or i1 %20, %21
  br i1 %22, label %23, label %24

23:                                               ; preds = %19
  tail call void @__cc_flag_invalid() #5
  br label %24

24:                                               ; preds = %23, %19
  tail call void @__cc_write_and_poison(i64* nonnull %17, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %13, %24
  %25 = bitcast i64* %3 to i8*
  %26 = getelementptr inbounds i8, i8* %25, i64 4
  %27 = bitcast i8* %26 to i32*
  store i32 %0, i32* %27, align 4, !tbaa !10
  %28 = bitcast i64* %3 to i32*
  store i32 %1, i32* %28, align 8, !tbaa !13
  %29 = load i64, i64* bitcast (%struct.node** @head to i64*), align 8, !tbaa !8
  %30 = getelementptr inbounds i64, i64* %3, i64 1
  store i64 %29, i64* %30, align 8, !tbaa !12
  store i64* %3, i64** bitcast (%struct.node** @head to i64**), align 8, !tbaa !8
  ret void
}

declare i64* @__cc_malloc(i64) local_unnamed_addr #1

declare void @__cc_flag_invalid() local_unnamed_addr #1

declare void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare i64* @__cc_advise_poison(i64*, i64*) local_unnamed_addr #1

; Function Attrs:  norecurse nounwind ssp uwtable
define void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #2 {
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

; Function Attrs:  norecurse nounwind ssp uwtable writeonly
define void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #3 {
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

attributes #0 = {  nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-builtins" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = {  norecurse nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = {  norecurse nounwind ssp uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { argmemonly nounwind }
attributes #5 = { nobuiltin nounwind "no-builtins" }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"Apple clang version 12.0.0 (clang-1200.0.32.2)"}
!1 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 6]}
!2 = !{i32 1, !"wchar_size", i32 4}
!3 = !{i32 7, !"PIC Level", i32 2}
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
