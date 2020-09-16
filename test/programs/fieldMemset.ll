; ModuleID = 'test/programs/fieldMemset.c'
source_filename = "test/programs/fieldMemset.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.s = type { i32, [3 x i32], i32 }

; Function Attrs: nofree noinline norecurse nounwind uwtable writeonly
define dso_local void @f(%struct.s* nocapture) local_unnamed_addr #0 {
  %2 = getelementptr inbounds %struct.s, %struct.s* %0, i64 0, i32 0
  store i32 1, i32* %2, align 4, !tbaa !2
  %3 = getelementptr inbounds %struct.s, %struct.s* %0, i64 0, i32 2
  store i32 2, i32* %3, align 4, !tbaa !7
  %4 = getelementptr %struct.s, %struct.s* %0, i64 0, i32 1, i64 0
  %5 = bitcast i32* %4 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 4 %5, i8 0, i64 12, i1 false)
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nounwind uwtable writeonly
define dso_local i32 @main() local_unnamed_addr #2 {
  %1 = alloca %struct.s, align 4
  %2 = bitcast %struct.s* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 20, i8* nonnull %2) #3
  call void @llvm.memset.p0i8.i64(i8* nonnull align 4 %2, i8 0, i64 20, i1 false)
  call void @f(%struct.s* nonnull %1)
  %3 = getelementptr inbounds %struct.s, %struct.s* %1, i64 0, i32 0
  %4 = load i32, i32* %3, align 4, !tbaa !2
  %5 = getelementptr inbounds %struct.s, %struct.s* %1, i64 0, i32 2
  %6 = load i32, i32* %5, align 4, !tbaa !7
  %7 = add nsw i32 %6, %4
  call void @llvm.lifetime.end.p0i8(i64 20, i8* nonnull %2) #3
  ret i32 %7
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

attributes #0 = { nofree noinline norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.1-12 "}
!2 = !{!3, !4, i64 0}
!3 = !{!"s", !4, i64 0, !5, i64 4, !4, i64 16}
!4 = !{!"int", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
!7 = !{!3, !4, i64 16}
