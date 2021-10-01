; ModuleID = './binaryTree/binaryTree.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, i32, %struct.node*, %struct.node* }

@SECRET_NUMBER = dso_local local_unnamed_addr global i32 15, section "__DATA,__secret", align 4

; Function Attrs:  nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  %1 = tail call fastcc %struct.node* @new_node(i32 20, i32 40)
  %2 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 5, i32 10)
  %3 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 1, i32 2)
  %4 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 15, i32 30)
  %5 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 42, i32 84)
  %6 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  %7 = icmp eq %struct.node* %1, null
  br i1 %7, label %search.exit, label %.preheader.i

.preheader.i:                                     ; preds = %0, %15
  %8 = phi %struct.node* [ %20, %15 ], [ %1, %0 ]
  %9 = getelementptr inbounds %struct.node, %struct.node* %8, i64 0, i32 0
  %10 = load i32, i32* %9, align 8, !tbaa !8
  %11 = icmp eq i32 %10, %6
  br i1 %11, label %12, label %15

12:                                               ; preds = %.preheader.i
  %13 = getelementptr inbounds %struct.node, %struct.node* %8, i64 0, i32 1
  %14 = load i32, i32* %13, align 4, !tbaa !11
  br label %search.exit

15:                                               ; preds = %.preheader.i
  %16 = icmp slt i32 %10, %6
  %17 = getelementptr inbounds %struct.node, %struct.node* %8, i64 0, i32 2
  %18 = getelementptr inbounds %struct.node, %struct.node* %8, i64 0, i32 3
  %19 = select i1 %16, %struct.node** %17, %struct.node** %18
  %20 = load %struct.node*, %struct.node** %19, align 8, !tbaa !12
  %21 = icmp eq %struct.node* %20, null
  br i1 %21, label %search.exit, label %.preheader.i

search.exit:                                      ; preds = %15, %0, %12
  %22 = phi i32 [ %14, %12 ], [ 0, %0 ], [ 0, %15 ]
  ret i32 %22
}

; Function Attrs:  nounwind uwtable
define internal fastcc noalias %struct.node* @new_node(i32, i32) unnamed_addr #0 {
  %3 = tail call i8* @__cc_malloc(i64 24) #5
  %4 = ptrtoint i8* %3 to i64
  %5 = lshr i64 %4, 58
  %6 = shl i64 1, %5
  %7 = icmp ult i8* %3, inttoptr (i64 1441151880758558720 to i8*)
  br i1 %7, label %8, label %9

8:                                                ; preds = %2
  tail call void @__cc_flag_invalid() #5
  br label %9

9:                                                ; preds = %8, %2
  %10 = add i64 %6, -1
  %11 = and i64 %10, %4
  %12 = icmp eq i64 %11, 0
  br i1 %12, label %14, label %13

13:                                               ; preds = %9
  tail call void @__cc_flag_invalid() #5
  br label %14

14:                                               ; preds = %13, %9
  %15 = getelementptr inbounds i8, i8* %3, i64 %6
  %16 = getelementptr inbounds i8, i8* %15, i64 -8
  %17 = bitcast i8* %16 to i64*
  tail call void @__cc_write_and_poison(i64* nonnull %17, i64 1) #5
  %18 = getelementptr inbounds i8, i8* %3, i64 24
  tail call void @__cc_access_valid(i8* %3, i8* nonnull %18) #5
  %19 = ptrtoint i8* %16 to i64
  %20 = ptrtoint i8* %18 to i64
  %21 = sub i64 %19, %20
  %22 = tail call i64 @__cc_advise_poison_offset(i8* nonnull %18, i64 %21) #5
  %23 = icmp ult i64 %22, %21
  br i1 %23, label %24, label %malloc.exit

24:                                               ; preds = %14
  %25 = getelementptr inbounds i8, i8* %18, i64 %22
  %26 = bitcast i8* %25 to i64*
  %27 = ptrtoint i8* %25 to i64
  %28 = and i64 %27, 7
  %29 = icmp eq i64 %28, 0
  br i1 %29, label %31, label %30

30:                                               ; preds = %24
  tail call void @__cc_flag_invalid() #5
  br label %31

31:                                               ; preds = %30, %24
  tail call void @__cc_write_and_poison(i64* nonnull %26, i64 0) #5
  br label %malloc.exit

malloc.exit:                                      ; preds = %14, %31
  %32 = bitcast i8* %3 to %struct.node*
  %33 = bitcast i8* %3 to i32*
  store i32 %0, i32* %33, align 8, !tbaa !8
  %34 = getelementptr inbounds i8, i8* %3, i64 4
  %35 = bitcast i8* %34 to i32*
  store i32 %1, i32* %35, align 4, !tbaa !11
  %36 = getelementptr inbounds i8, i8* %3, i64 8
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %36, i8 0, i64 16, i1 false)
  ret %struct.node* %32
}

; Function Attrs:  nounwind uwtable
define internal fastcc %struct.node* @insert(%struct.node*, i32, i32) unnamed_addr #0 {
  %4 = icmp eq %struct.node* %0, null
  br i1 %4, label %5, label %7

5:                                                ; preds = %3
  %6 = tail call fastcc %struct.node* @new_node(i32 %1, i32 %2)
  ret %struct.node* %6

7:                                                ; preds = %3
  %8 = getelementptr inbounds %struct.node, %struct.node* %0, i64 0, i32 1
  %9 = load i32, i32* %8, align 4, !tbaa !11
  %10 = icmp slt i32 %9, %1
  br i1 %10, label %11, label %15

11:                                               ; preds = %7
  %12 = getelementptr inbounds %struct.node, %struct.node* %0, i64 0, i32 2
  %13 = load %struct.node*, %struct.node** %12, align 8, !tbaa !13
  %14 = tail call fastcc %struct.node* @insert(%struct.node* %13, i32 %1, i32 %2)
  store %struct.node* %14, %struct.node** %12, align 8, !tbaa !13
  ret %struct.node* %0

15:                                               ; preds = %7
  %16 = getelementptr inbounds %struct.node, %struct.node* %0, i64 0, i32 3
  %17 = load %struct.node*, %struct.node** %16, align 8, !tbaa !14
  %18 = tail call fastcc %struct.node* @insert(%struct.node* %17, i32 %1, i32 %2)
  store %struct.node* %18, %struct.node** %16, align 8, !tbaa !14
  ret %struct.node* %0
}

declare dso_local i8* @__cc_malloc(i64) local_unnamed_addr #1

declare dso_local void @__cc_flag_invalid() local_unnamed_addr #1

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare dso_local void @__cc_access_valid(i8*, i8*) local_unnamed_addr #1

declare dso_local i64 @__cc_advise_poison_offset(i8*, i64) local_unnamed_addr #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

; Function Attrs:  norecurse nounwind uwtable
define dso_local void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #3 {
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
  %16 = load i8, i8* %15, align 1, !tbaa !15
  %17 = getelementptr inbounds i8, i8* %0, i64 %13
  store i8 %16, i8* %17, align 1, !tbaa !15
  %18 = add nuw i64 %13, 1
  %19 = add nsw i64 %14, -1
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %.loopexit, label %.preheader, !llvm.loop !16

.loopexit:                                        ; preds = %.preheader, %.loopexit2, %3
  ret void

21:                                               ; preds = %21, %9
  %22 = phi i64 [ 0, %9 ], [ %39, %21 ]
  %23 = phi i64 [ %10, %9 ], [ %40, %21 ]
  %24 = getelementptr inbounds i8, i8* %1, i64 %22
  %25 = load i8, i8* %24, align 1, !tbaa !15
  %26 = getelementptr inbounds i8, i8* %0, i64 %22
  store i8 %25, i8* %26, align 1, !tbaa !15
  %27 = or i64 %22, 1
  %28 = getelementptr inbounds i8, i8* %1, i64 %27
  %29 = load i8, i8* %28, align 1, !tbaa !15
  %30 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %29, i8* %30, align 1, !tbaa !15
  %31 = or i64 %22, 2
  %32 = getelementptr inbounds i8, i8* %1, i64 %31
  %33 = load i8, i8* %32, align 1, !tbaa !15
  %34 = getelementptr inbounds i8, i8* %0, i64 %31
  store i8 %33, i8* %34, align 1, !tbaa !15
  %35 = or i64 %22, 3
  %36 = getelementptr inbounds i8, i8* %1, i64 %35
  %37 = load i8, i8* %36, align 1, !tbaa !15
  %38 = getelementptr inbounds i8, i8* %0, i64 %35
  store i8 %37, i8* %38, align 1, !tbaa !15
  %39 = add nuw i64 %22, 4
  %40 = add i64 %23, -4
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %.loopexit2, label %21
}

; Function Attrs:  norecurse nounwind uwtable writeonly
define dso_local void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #4 {
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
  store i8 %1, i8* %16, align 1, !tbaa !15
  %17 = or i64 %14, 1
  %18 = getelementptr inbounds i8, i8* %0, i64 %17
  store i8 %1, i8* %18, align 1, !tbaa !15
  %19 = or i64 %14, 2
  %20 = getelementptr inbounds i8, i8* %0, i64 %19
  store i8 %1, i8* %20, align 1, !tbaa !15
  %21 = or i64 %14, 3
  %22 = getelementptr inbounds i8, i8* %0, i64 %21
  store i8 %1, i8* %22, align 1, !tbaa !15
  %23 = or i64 %14, 4
  %24 = getelementptr inbounds i8, i8* %0, i64 %23
  store i8 %1, i8* %24, align 1, !tbaa !15
  %25 = or i64 %14, 5
  %26 = getelementptr inbounds i8, i8* %0, i64 %25
  store i8 %1, i8* %26, align 1, !tbaa !15
  %27 = or i64 %14, 6
  %28 = getelementptr inbounds i8, i8* %0, i64 %27
  store i8 %1, i8* %28, align 1, !tbaa !15
  %29 = or i64 %14, 7
  %30 = getelementptr inbounds i8, i8* %0, i64 %29
  store i8 %1, i8* %30, align 1, !tbaa !15
  %31 = add nuw i64 %14, 8
  %32 = add i64 %15, -8
  %33 = icmp eq i64 %32, 0
  br i1 %33, label %.loopexit2, label %13
}

attributes #0 = {  nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind }
attributes #3 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = {  norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nobuiltin nounwind }

!llvm.ident = !{!0, !0, !0, !0}
!llvm.module.flags = !{!1, !2, !3}

!0 = !{!"clang version 9.0.1-16 "}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"ThinLTO", i32 0}
!3 = !{i32 1, !"EnableSplitLTOUnit", i32 0}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = !{!9, !5, i64 0}
!9 = !{!"node", !5, i64 0, !5, i64 4, !10, i64 8, !10, i64 16}
!10 = !{!"any pointer", !6, i64 0}
!11 = !{!9, !5, i64 4}
!12 = !{!10, !10, i64 0}
!13 = !{!9, !10, i64 8}
!14 = !{!9, !10, i64 16}
!15 = !{!6, !6, i64 0}
!16 = distinct !{!16, !17}
!17 = !{!"llvm.loop.unroll.disable"}
