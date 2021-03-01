; ModuleID = 'test/Programs/memcpy.c'
source_filename = "test/Programs/memcpy.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs:  norecurse nounwind uwtable
define dso_local i8* @my_memcpy(i8* returned, i8* readonly, i64) local_unnamed_addr #0 {
  %4 = getelementptr inbounds i8, i8* %1, i64 %2
  %5 = icmp eq i64 %2, 0
  br i1 %5, label %15, label %6

6:                                                ; preds = %3, %6
  %7 = phi i8* [ %13, %6 ], [ %0, %3 ]
  %8 = phi i8* [ %12, %6 ], [ %1, %3 ]
  %9 = load i8, i8* %8, align 1, !tbaa !2
  store i8 %9, i8* %7, align 1, !tbaa !2
  %10 = load i8, i8* %8, align 1, !tbaa !2
  store i8 %10, i8* %7, align 1, !tbaa !2
  %11 = load i8, i8* %8, align 1, !tbaa !2
  store i8 %11, i8* %7, align 1, !tbaa !2
  %12 = getelementptr inbounds i8, i8* %8, i64 1
  %13 = getelementptr inbounds i8, i8* %7, i64 1
  %14 = icmp eq i8* %12, %4
  br i1 %14, label %15, label %6

15:                                               ; preds = %6, %3
  ret i8* %0
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = bitcast i64* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %3) #3
  store i64 123, i64* %1, align 8, !tbaa !5
  %4 = bitcast i64* %2 to i8*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #3
  store i64 0, i64* %2, align 8, !tbaa !5
  %5 = call i8* @my_memcpy(i8* nonnull %4, i8* nonnull %3, i64 8) #4
  %6 = load i64, i64* %2, align 8, !tbaa !5
  %7 = trunc i64 %6 to i32
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #3
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %3) #3
  ret i32 %7
}

attributes #0 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }
attributes #4 = { nobuiltin }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-16 "}
!2 = !{!3, !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
!5 = !{!6, !6, i64 0}
!6 = !{!"long", !3, i64 0}
