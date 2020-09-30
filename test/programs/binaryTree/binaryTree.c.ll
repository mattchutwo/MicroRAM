; ModuleID = 'binaryTree.c.bc'
source_filename = "llvm-link"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.node = type { i32, i32, %struct.node*, %struct.node* }

@SECRET_NUMBER = local_unnamed_addr global i32 15, section "__DATA,__secret", align 4

; Function Attrs:  nounwind ssp uwtable
define i32 @main() local_unnamed_addr #0 {
  %1 = tail call fastcc %struct.node* @new_node(i32 20, i32 40)
  %2 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 5, i32 10)
  %3 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 1, i32 2)
  %4 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 15, i32 30)
  %5 = tail call fastcc %struct.node* @insert(%struct.node* %1, i32 42, i32 84)
  %6 = load i32, i32* @SECRET_NUMBER, align 4, !tbaa !4
  %7 = icmp eq %struct.node* %1, null
  br i1 %7, label %search.exit, label %.lr.ph.i

.lr.ph.i:                                         ; preds = %0, %tailrecurse.backedge.i
  %.tr5.i = phi %struct.node* [ %.tr.be.i, %tailrecurse.backedge.i ], [ %1, %0 ]
  %8 = getelementptr inbounds %struct.node, %struct.node* %.tr5.i, i64 0, i32 0
  %9 = load i32, i32* %8, align 8, !tbaa !8
  %10 = icmp eq i32 %9, %6
  br i1 %10, label %11, label %tailrecurse.backedge.i

11:                                               ; preds = %.lr.ph.i
  %12 = getelementptr inbounds %struct.node, %struct.node* %.tr5.i, i64 0, i32 1
  %13 = load i32, i32* %12, align 4, !tbaa !11
  br label %search.exit

tailrecurse.backedge.i:                           ; preds = %.lr.ph.i
  %14 = icmp slt i32 %9, %6
  %15 = getelementptr inbounds %struct.node, %struct.node* %.tr5.i, i64 0, i32 2
  %16 = getelementptr inbounds %struct.node, %struct.node* %.tr5.i, i64 0, i32 3
  %.tr.be.in.i = select i1 %14, %struct.node** %15, %struct.node** %16
  %.tr.be.i = load %struct.node*, %struct.node** %.tr.be.in.i, align 8, !tbaa !12
  %17 = icmp eq %struct.node* %.tr.be.i, null
  br i1 %17, label %search.exit, label %.lr.ph.i

search.exit:                                      ; preds = %tailrecurse.backedge.i, %0, %11
  %18 = phi i32 [ %13, %11 ], [ 0, %0 ], [ 0, %tailrecurse.backedge.i ]
  ret i32 %18
}

; Function Attrs:  nounwind ssp uwtable
define internal fastcc noalias %struct.node* @new_node(i32, i32) unnamed_addr #0 {
  %3 = tail call i64* @__cc_malloc(i64 24) #5
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
  %16 = getelementptr inbounds i64, i64* %3, i64 24
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
  %26 = bitcast i64* %3 to %struct.node*
  %27 = bitcast i64* %3 to i32*
  store i32 %0, i32* %27, align 8, !tbaa !8
  %28 = getelementptr inbounds i8, i8* %25, i64 4
  %29 = bitcast i8* %28 to i32*
  store i32 %1, i32* %29, align 4, !tbaa !11
  %30 = getelementptr inbounds i64, i64* %3, i64 1
  %31 = bitcast i64* %30 to i8*
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 8 dereferenceable(16) %31, i8 0, i64 16, i1 false)
  ret %struct.node* %26
}

; Function Attrs:  nounwind ssp uwtable
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

declare i64* @__cc_malloc(i64) local_unnamed_addr #1

declare void @__cc_flag_invalid() local_unnamed_addr #1

declare void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #1

declare i64* @__cc_advise_poison(i64*, i64*) local_unnamed_addr #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

; Function Attrs:  norecurse nounwind ssp uwtable
define void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #3 {
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

; Function Attrs:  norecurse nounwind ssp uwtable writeonly
define void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #4 {
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

attributes #0 = {  nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-builtins" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind }
attributes #3 = {  norecurse nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = {  norecurse nounwind ssp uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-builtins" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
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
