; ModuleID = 'driver-opt.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.CImgFile = type { i32 (...)**, i8, i8*, %struct.CLDIB*, i32, i8* }
%struct.CLDIB = type { i8* }
%class.CBmpFile = type { %class.CImgFile }
%struct.tagBITMAPINFOHEADER = type { i32, i32, i32, i16, i16, i32, i32, i32, i32, i32, i32 }

@_ZTV8CBmpFile = internal unnamed_addr constant { [12 x i8*] } { [12 x i8*] [i8* null, i8* bitcast ({ i8*, i8*, i8* }* @_ZTI8CBmpFile to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD2Ev to i8*), i8* bitcast (void (%class.CBmpFile*)* @_ZN8CBmpFileD0Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFile5ClearEv to i8*), i8* bitcast (%class.CImgFile* (%class.CBmpFile*)* @_ZN8CBmpFile5VMakeEv to i8*), i8* bitcast (i32 (%class.CBmpFile*)* @_ZNK8CBmpFile7GetTypeEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile6GetExtEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile7GetDescEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile9GetFormatEv to i8*), i8* bitcast (i1 (%class.CBmpFile*, i8*)* @_ZN8CBmpFile4LoadEPKc to i8*), i8* bitcast (i1 (%class.CBmpFile*, i8*)* @_ZN8CBmpFile4SaveEPKc to i8*)] }, align 8, !type !0, !type !1, !type !2, !type !3, !type !4, !type !5, !type !6, !type !7, !type !8, !type !9, !type !10, !type !11, !type !12, !type !13, !type !14, !type !15, !type !16, !type !17
@_ZTI8CBmpFile = internal constant { i8*, i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8*, i8** @_ZTVN10__cxxabiv120__si_class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8], [10 x i8]* @_ZTS8CBmpFile, i32 0, i32 0), i8* bitcast ({ i8*, i8* }* @_ZTI8CImgFile to i8*) }, align 8
@_ZTIPKc = external dso_local constant i8*
@.str.6 = private unnamed_addr constant [4 x i8] c"BMP\00", align 1
@.str.5 = private unnamed_addr constant [15 x i8] c"Windows Bitmap\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"bmp\00", align 1
@_ZTVN10__cxxabiv120__si_class_type_infoE = external dso_local global i8*
@_ZTS8CBmpFile = internal constant [10 x i8] c"8CBmpFile\00", align 1
@.str.45 = private unnamed_addr constant [11 x i8] c"All clear!\00", align 1
@.str.1.46 = private unnamed_addr constant [23 x i8] c"Non-descript error :(.\00", align 1
@.str.2.47 = private unnamed_addr constant [25 x i8] c"Memory allocation error.\00", align 1
@.str.3.48 = private unnamed_addr constant [20 x i8] c"File doesn't exist.\00", align 1
@_ZTV8CImgFile = internal unnamed_addr constant { [12 x i8*] } { [12 x i8*] [i8* null, i8* bitcast ({ i8*, i8* }* @_ZTI8CImgFile to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD2Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD0Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFile5ClearEv to i8*), i8* bitcast (%class.CImgFile* (%class.CImgFile*)* @_ZN8CImgFile5VMakeEv to i8*), i8* bitcast (i32 (%class.CImgFile*)* @_ZNK8CImgFile7GetTypeEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile6GetExtEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile7GetDescEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile9GetFormatEv to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*)] }, align 8, !type !9, !type !10, !type !11, !type !12, !type !13, !type !14, !type !15, !type !16, !type !17
@_ZTI8CImgFile = internal constant { i8*, i8* } { i8* bitcast (i8** getelementptr inbounds (i8*, i8** @_ZTVN10__cxxabiv117__class_type_infoE, i64 2) to i8*), i8* getelementptr inbounds ([10 x i8], [10 x i8]* @_ZTS8CImgFile, i32 0, i32 0) }, align 8
@.str.14 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@_ZTVN10__cxxabiv117__class_type_infoE = external dso_local global i8*
@_ZTS8CImgFile = internal constant [10 x i8] c"8CImgFile\00", align 1
@.str.128 = private unnamed_addr constant [13 x i8] c"fromager.bmp\00", align 1
@_ZL14bmp_image_data = internal unnamed_addr constant [256 x i8] zeroinitializer, section ".data.secret", align 16
@llvm.global_ctors = appending global [0 x { i32, void ()*, i8* }] zeroinitializer

; Function Attrs: nofree nounwind
declare dso_local noalias i8* @malloc(i64) local_unnamed_addr #0

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

; Function Attrs: nounwind
declare dso_local void @free(i8* nocapture) local_unnamed_addr #2

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nounwind uwtable
define internal void @_ZN8CImgFileD2Ev(%class.CImgFile* nocapture) unnamed_addr #3 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !22
  %3 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !25
  %5 = icmp eq %struct.CLDIB* %4, null
  br i1 %5, label %10, label %6

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %4, i64 0, i32 0
  %8 = load i8*, i8** %7, align 8, !tbaa !31
  tail call void @free(i8* %8) #14
  %9 = bitcast %struct.CLDIB* %4 to i8*
  tail call void @free(i8* %9) #14
  br label %10

10:                                               ; preds = %6, %1
  store %struct.CLDIB* null, %struct.CLDIB** %3, align 8, !tbaa !25
  %11 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 4
  store i32 8, i32* %11, align 8, !tbaa !33
  %12 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 5
  %13 = load i8*, i8** %12, align 8, !tbaa !34
  tail call void @free(i8* %13) #14
  store i8* null, i8** %12, align 8, !tbaa !34
  %14 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 1
  store i8 0, i8* %14, align 8, !tbaa !35
  ret void
}

; Function Attrs: inlinehint nounwind uwtable
define internal void @_ZN8CBmpFileD0Ev(%class.CBmpFile*) unnamed_addr #4 align 2 personality i32 (...)* @__gxx_personality_v0 {
  %2 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !22
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !25
  %5 = icmp eq %struct.CLDIB* %4, null
  br i1 %5, label %_ZN8CImgFileD2Ev.exit, label %6

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %4, i64 0, i32 0
  %8 = load i8*, i8** %7, align 8, !tbaa !31
  tail call void @free(i8* %8) #14
  %9 = bitcast %struct.CLDIB* %4 to i8*
  tail call void @free(i8* %9) #14
  br label %_ZN8CImgFileD2Ev.exit

_ZN8CImgFileD2Ev.exit:                            ; preds = %1, %6
  store %struct.CLDIB* null, %struct.CLDIB** %3, align 8, !tbaa !25
  %10 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 4
  store i32 8, i32* %10, align 8, !tbaa !33
  %11 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 5
  %12 = load i8*, i8** %11, align 8, !tbaa !34
  tail call void @free(i8* %12) #14
  %13 = bitcast %class.CBmpFile* %0 to i8*
  tail call void @_ZdlPv(i8* %13) #15
  ret void
}

; Function Attrs: uwtable
define internal noalias nonnull %class.CImgFile* @_ZN8CBmpFile5VMakeEv(%class.CBmpFile* nocapture readnone) unnamed_addr #5 align 2 personality i32 (...)* @__gxx_personality_v0 {
  %2 = tail call i8* @_Znwm(i64 48) #16
  %3 = bitcast i8* %2 to i32 (...)***
  %4 = getelementptr inbounds i8, i8* %2, i64 8
  store i8 0, i8* %4, align 8, !tbaa !35
  %5 = getelementptr inbounds i8, i8* %2, i64 16
  %6 = getelementptr inbounds i8, i8* %2, i64 32
  %7 = bitcast i8* %6 to i32*
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %5, i8 0, i64 16, i1 false) #14
  store i32 8, i32* %7, align 8, !tbaa !33
  %8 = getelementptr inbounds i8, i8* %2, i64 40
  %9 = bitcast i8* %8 to i8**
  store i8* null, i8** %9, align 8, !tbaa !34
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CBmpFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !22
  %10 = bitcast i8* %2 to %class.CImgFile*
  ret %class.CImgFile* %10
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i32 @_ZNK8CBmpFile7GetTypeEv(%class.CBmpFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i32 0
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile6GetExtEv(%class.CBmpFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile7GetDescEv(%class.CBmpFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.5, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile9GetFormatEv(%class.CBmpFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.6, i64 0, i64 0)
}

; Function Attrs: uwtable
define internal zeroext i1 @_ZN8CBmpFile4LoadEPKc(%class.CBmpFile* nocapture, i8* nocapture readonly) unnamed_addr #5 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %3 = tail call i32 @strcmp(i8* %1, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.128, i64 0, i64 0)) #17
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %fopen.exit, label %fopen.exit.thread

fopen.exit:                                       ; preds = %2
  %5 = tail call noalias i8* @malloc(i64 8) #14
  %6 = bitcast i8* %5 to i64*
  store i64 0, i64* %6, align 8, !tbaa !36
  %7 = icmp eq i8* %5, null
  br i1 %7, label %fopen.exit.thread, label %12

fopen.exit.thread:                                ; preds = %fopen.exit, %2
  %8 = tail call i8* @__cxa_allocate_exception(i64 8) #14
  %9 = bitcast i8* %8 to i64*
  store i64 ptrtoint ([20 x i8]* @.str.3.48 to i64), i64* %9, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %8, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #18
          to label %85 unwind label %10

10:                                               ; preds = %fopen.exit.thread
  %11 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %52

12:                                               ; preds = %fopen.exit
  store i64 54, i64* %6, align 8, !tbaa !36
  %13 = tail call noalias i8* @malloc(i64 8) #14
  %14 = icmp eq i8* %13, null
  br i1 %14, label %_Z9dib_allociiiPKhb.exit.thread, label %15

15:                                               ; preds = %12
  %16 = tail call noalias i8* @malloc(i64 88) #14
  %17 = bitcast i8* %13 to i8**
  store i8* %16, i8** %17, align 8, !tbaa !31
  %18 = icmp eq i8* %16, null
  br i1 %18, label %19, label %22

19:                                               ; preds = %15
  tail call void @free(i8* nonnull %13) #14
  br label %_Z9dib_allociiiPKhb.exit.thread

_Z9dib_allociiiPKhb.exit.thread:                  ; preds = %12, %19
  %20 = tail call i8* @__cxa_allocate_exception(i64 8) #14
  %21 = bitcast i8* %20 to i64*
  store i64 ptrtoint ([25 x i8]* @.str.2.47 to i64), i64* %21, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %20, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #18
          to label %85 unwind label %50

22:                                               ; preds = %15
  %23 = bitcast i8* %16 to i32*
  store i32 40, i32* %23, align 4, !tbaa !40
  %24 = getelementptr inbounds i8, i8* %16, i64 4
  %25 = bitcast i8* %24 to i32*
  store i32 4, i32* %25, align 4, !tbaa !43
  %26 = getelementptr inbounds i8, i8* %16, i64 8
  %27 = bitcast i8* %26 to i32*
  store i32 -4, i32* %27, align 4, !tbaa !44
  %28 = getelementptr inbounds i8, i8* %16, i64 12
  %29 = bitcast i8* %28 to i16*
  store i16 1, i16* %29, align 4, !tbaa !45
  %30 = getelementptr inbounds i8, i8* %16, i64 14
  %31 = bitcast i8* %30 to i16*
  store i16 24, i16* %31, align 2, !tbaa !46
  %32 = getelementptr inbounds i8, i8* %16, i64 16
  %33 = bitcast i8* %32 to i32*
  store i32 0, i32* %33, align 4, !tbaa !47
  %34 = getelementptr inbounds i8, i8* %16, i64 20
  %35 = bitcast i8* %34 to i32*
  store i32 48, i32* %35, align 4, !tbaa !48
  %36 = getelementptr inbounds i8, i8* %16, i64 24
  %37 = getelementptr inbounds i8, i8* %16, i64 40
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 4 %36, i8 0, i64 16, i1 false)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %37, i8* nonnull align 2 getelementptr inbounds ([256 x i8], [256 x i8]* @_ZL14bmp_image_data, i64 0, i64 54), i64 64, i1 false) #14
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %37, i8* nonnull align 2 getelementptr inbounds ([256 x i8], [256 x i8]* @_ZL14bmp_image_data, i64 0, i64 118), i64 48, i1 false) #14
  store i64 166, i64* %6, align 8, !tbaa !36
  %38 = tail call noalias i8* @malloc(i64 12) #14
  %39 = icmp eq i8* %38, null
  br i1 %39, label %_Z9dib_vflipP5CLDIB.exit, label %.new

.new:                                             ; preds = %22
  %40 = getelementptr inbounds i8, i8* %16, i64 32
  %41 = bitcast i8* %40 to i32*
  %42 = load i32, i32* %41, align 4, !tbaa !49
  %43 = sext i32 %42 to i64
  %44 = shl nsw i64 %43, 2
  %45 = add nsw i64 %44, 40
  %46 = getelementptr inbounds i8, i8* %16, i64 %45
  %47 = getelementptr inbounds i8, i8* %46, i64 36
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %38, i8* nonnull align 1 %46, i64 12, i1 false) #14
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %46, i8* nonnull align 1 %47, i64 12, i1 false) #14
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %47, i8* nonnull align 1 %38, i64 12, i1 false) #14
  %48 = getelementptr inbounds i8, i8* %46, i64 12
  %49 = getelementptr inbounds i8, i8* %47, i64 -12
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %38, i8* nonnull align 1 %48, i64 12, i1 false) #14
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %48, i8* nonnull align 1 %49, i64 12, i1 false) #14
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %49, i8* nonnull align 1 %38, i64 12, i1 false) #14
  tail call void @free(i8* nonnull %38) #14
  br label %_Z9dib_vflipP5CLDIB.exit

50:                                               ; preds = %_Z9dib_allociiiPKhb.exit.thread
  %51 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %52

52:                                               ; preds = %50, %10
  %.sink65 = phi { i8*, i32 } [ %51, %50 ], [ %11, %10 ]
  %53 = phi i1 [ true, %50 ], [ false, %10 ]
  %54 = phi i8* [ %5, %50 ], [ null, %10 ]
  %55 = extractvalue { i8*, i32 } %.sink65, 1
  %56 = tail call i32 @llvm.eh.typeid.for(i8* bitcast (i8** @_ZTIPKc to i8*)) #14
  %57 = icmp eq i32 %55, %56
  br i1 %57, label %58, label %84

58:                                               ; preds = %52
  %59 = extractvalue { i8*, i32 } %.sink65, 0
  %60 = tail call i8* @__cxa_begin_catch(i8* %59) #14
  %61 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* %60, i8** %61, align 8, !tbaa !50
  tail call void @__cxa_end_catch() #14
  br i1 %53, label %62, label %82

62:                                               ; preds = %58
  tail call void @free(i8* %54) #14
  br label %82

_Z9dib_vflipP5CLDIB.exit:                         ; preds = %.new, %22
  tail call void @free(i8* nonnull %5) #14
  %63 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.45, i64 0, i64 0), i8** %63, align 8, !tbaa !50
  %64 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %65 = load %struct.CLDIB*, %struct.CLDIB** %64, align 8, !tbaa !25
  %66 = bitcast %struct.CLDIB** %64 to i8**
  store i8* %13, i8** %66, align 8, !tbaa !25
  %67 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 1
  store i8 1, i8* %67, align 8, !tbaa !35
  %68 = icmp eq %struct.CLDIB* %65, null
  br i1 %68, label %_Z8dib_freeP5CLDIB.exit, label %69

69:                                               ; preds = %_Z9dib_vflipP5CLDIB.exit
  %70 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %65, i64 0, i32 0
  %71 = load i8*, i8** %70, align 8, !tbaa !31
  tail call void @free(i8* %71) #14
  %72 = bitcast %struct.CLDIB* %65 to i8*
  tail call void @free(i8* %72) #14
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %_Z9dib_vflipP5CLDIB.exit, %69
  %73 = bitcast i8* %13 to %struct.tagBITMAPINFOHEADER**
  %74 = load %struct.tagBITMAPINFOHEADER*, %struct.tagBITMAPINFOHEADER** %73, align 8, !tbaa !31
  %75 = getelementptr inbounds %struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* %74, i64 0, i32 4
  %76 = load i16, i16* %75, align 2, !tbaa !46
  %77 = zext i16 %76 to i32
  %78 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 4
  store i32 %77, i32* %78, align 8, !tbaa !33
  %79 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 5
  %80 = load i8*, i8** %79, align 8, !tbaa !34
  tail call void @free(i8* %80) #14
  store i8* null, i8** %79, align 8, !tbaa !34
  %81 = tail call noalias i8* @strdup(i8* %1) #14
  store i8* %81, i8** %79, align 8, !tbaa !34
  br label %82

82:                                               ; preds = %_Z8dib_freeP5CLDIB.exit, %62, %58
  %83 = phi i1 [ true, %_Z8dib_freeP5CLDIB.exit ], [ false, %62 ], [ false, %58 ]
  ret i1 %83

84:                                               ; preds = %52
  resume { i8*, i32 } %.sink65

85:                                               ; preds = %_Z9dib_allociiiPKhb.exit.thread, %fopen.exit.thread
  unreachable
}

; Function Attrs: uwtable
define internal zeroext i1 @_ZN8CBmpFile4SaveEPKc(%class.CBmpFile* nocapture, i8* nocapture readonly) unnamed_addr #5 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !25
  %5 = icmp eq %struct.CLDIB* %4, null
  %6 = tail call i8* @__cxa_allocate_exception(i64 8) #14
  %7 = bitcast i8* %6 to i64*
  br i1 %5, label %8, label %fopen.exit.thread

8:                                                ; preds = %2
  store i64 ptrtoint ([23 x i8]* @.str.1.46 to i64), i64* %7, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %6, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #18
          to label %19 unwind label %9

9:                                                ; preds = %fopen.exit.thread, %8
  %10 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  %11 = extractvalue { i8*, i32 } %10, 1
  %12 = tail call i32 @llvm.eh.typeid.for(i8* bitcast (i8** @_ZTIPKc to i8*)) #14
  %13 = icmp eq i32 %11, %12
  br i1 %13, label %14, label %18

fopen.exit.thread:                                ; preds = %2
  store i64 ptrtoint ([20 x i8]* @.str.3.48 to i64), i64* %7, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %6, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #18
          to label %19 unwind label %9

14:                                               ; preds = %9
  %15 = extractvalue { i8*, i32 } %10, 0
  %16 = tail call i8* @__cxa_begin_catch(i8* %15) #14
  %17 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* %16, i8** %17, align 8, !tbaa !50
  tail call void @__cxa_end_catch() #14
  ret i1 false

18:                                               ; preds = %9
  resume { i8*, i32 } %10

19:                                               ; preds = %fopen.exit.thread, %8
  unreachable
}

declare dso_local i32 @__gxx_personality_v0(...)

declare dso_local i8* @__cxa_allocate_exception(i64) local_unnamed_addr

declare dso_local void @__cxa_throw(i8*, i8*, i8*) local_unnamed_addr

; Function Attrs: nounwind readnone
declare i32 @llvm.eh.typeid.for(i8*) #7

declare dso_local i8* @__cxa_begin_catch(i8*) local_unnamed_addr

declare dso_local void @__cxa_end_catch() local_unnamed_addr

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

; Function Attrs: nobuiltin nofree
declare dso_local noalias nonnull i8* @_Znwm(i64) local_unnamed_addr #8

; Function Attrs: nobuiltin nounwind
declare dso_local void @_ZdlPv(i8*) local_unnamed_addr #9

; Function Attrs: noreturn nounwind uwtable
define internal void @_ZN8CImgFileD0Ev(%class.CImgFile* nocapture readnone) unnamed_addr #10 align 2 {
  tail call void @llvm.trap() #19
  unreachable
}

; Function Attrs: nounwind uwtable
define internal void @_ZN8CImgFile5ClearEv(%class.CImgFile* nocapture) unnamed_addr #3 align 2 {
  %2 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 3
  %3 = load %struct.CLDIB*, %struct.CLDIB** %2, align 8, !tbaa !25
  %4 = icmp eq %struct.CLDIB* %3, null
  br i1 %4, label %_Z8dib_freeP5CLDIB.exit, label %5

5:                                                ; preds = %1
  %6 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %3, i64 0, i32 0
  %7 = load i8*, i8** %6, align 8, !tbaa !31
  tail call void @free(i8* %7) #14
  %8 = bitcast %struct.CLDIB* %3 to i8*
  tail call void @free(i8* %8) #14
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %1, %5
  store %struct.CLDIB* null, %struct.CLDIB** %2, align 8, !tbaa !25
  %9 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 4
  store i32 8, i32* %9, align 8, !tbaa !33
  %10 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 5
  %11 = load i8*, i8** %10, align 8, !tbaa !34
  tail call void @free(i8* %11) #14
  store i8* null, i8** %10, align 8, !tbaa !34
  %12 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 1
  store i8 0, i8* %12, align 8, !tbaa !35
  ret void
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal noalias %class.CImgFile* @_ZN8CImgFile5VMakeEv(%class.CImgFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret %class.CImgFile* null
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i32 @_ZNK8CImgFile7GetTypeEv(%class.CImgFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i32 -1
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile6GetExtEv(%class.CImgFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile7GetDescEv(%class.CImgFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile9GetFormatEv(%class.CImgFile* nocapture readnone) unnamed_addr #6 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

declare dso_local void @__cxa_pure_virtual() unnamed_addr

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #11

; Function Attrs: nofree nounwind
declare dso_local noalias i8* @strdup(i8* nocapture readonly) local_unnamed_addr #0

; Function Attrs: nofree nounwind readonly
declare dso_local i32 @strcmp(i8* nocapture, i8* nocapture) local_unnamed_addr #12

; Function Attrs: norecurse uwtable
define dso_local i32 @main() local_unnamed_addr #13 personality i32 (...)* @__gxx_personality_v0 {
  %1 = alloca %class.CBmpFile, align 8
  %2 = bitcast %class.CBmpFile* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 48, i8* nonnull %2) #14
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 0
  %4 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 1
  store i8 0, i8* %4, align 8, !tbaa !35
  %5 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 2
  %6 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 4
  %7 = bitcast i8** %5 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %7, i8 0, i64 16, i1 false) #14
  store i32 8, i32* %6, align 8, !tbaa !33
  %8 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 5
  store i8* null, i8** %8, align 8, !tbaa !34
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CBmpFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !22
  %9 = invoke zeroext i1 @_ZN8CBmpFile4LoadEPKc(%class.CBmpFile* nonnull %1, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.128, i64 0, i64 0))
          to label %_Z7BmpLoadPKc.exit unwind label %10

10:                                               ; preds = %0
  %11 = landingpad { i8*, i32 }
          cleanup
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !22
  %12 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 3
  %13 = load %struct.CLDIB*, %struct.CLDIB** %12, align 8, !tbaa !25
  %14 = icmp eq %struct.CLDIB* %13, null
  br i1 %14, label %_ZN8CImgFileD2Ev.exit.i, label %15

15:                                               ; preds = %10
  %16 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %13, i64 0, i32 0
  %17 = load i8*, i8** %16, align 8, !tbaa !31
  tail call void @free(i8* %17) #14
  %18 = bitcast %struct.CLDIB* %13 to i8*
  tail call void @free(i8* %18) #14
  br label %_ZN8CImgFileD2Ev.exit.i

_ZN8CImgFileD2Ev.exit.i:                          ; preds = %15, %10
  store %struct.CLDIB* null, %struct.CLDIB** %12, align 8, !tbaa !25
  store i32 8, i32* %6, align 8, !tbaa !33
  %19 = load i8*, i8** %8, align 8, !tbaa !34
  tail call void @free(i8* %19) #14
  call void @llvm.lifetime.end.p0i8(i64 48, i8* nonnull %2) #14
  resume { i8*, i32 } %11

_Z7BmpLoadPKc.exit:                               ; preds = %0
  %20 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 3
  %21 = load %struct.CLDIB*, %struct.CLDIB** %20, align 8, !tbaa !25
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !22
  store %struct.CLDIB* null, %struct.CLDIB** %20, align 8, !tbaa !25
  store i32 8, i32* %6, align 8, !tbaa !33
  %22 = load i8*, i8** %8, align 8, !tbaa !34
  tail call void @free(i8* %22) #14
  call void @llvm.lifetime.end.p0i8(i64 48, i8* nonnull %2) #14
  %23 = icmp eq %struct.CLDIB* %21, null
  br i1 %23, label %_Z8dib_freeP5CLDIB.exit, label %24

24:                                               ; preds = %_Z7BmpLoadPKc.exit
  %25 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %21, i64 0, i32 0
  %26 = load i8*, i8** %25, align 8, !tbaa !31
  tail call void @free(i8* %26) #14
  %27 = bitcast %struct.CLDIB* %21 to i8*
  tail call void @free(i8* %27) #14
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %_Z7BmpLoadPKc.exit, %24
  ret i32 0
}

attributes #0 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { inlinehint nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { nounwind readnone }
attributes #8 = { nobuiltin "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #9 = { nobuiltin nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #10 = { noreturn nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #11 = { cold noreturn nounwind }
attributes #12 = { nounwind readonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #13 = { norecurse uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #14 = { nounwind }
attributes #15 = { builtin nounwind }
attributes #16 = { builtin }
attributes #17 = { nounwind readonly }
attributes #18 = { noreturn }
attributes #19 = { noreturn nounwind }

!llvm.ident = !{!18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18}
!llvm.module.flags = !{!19, !20, !21}

!0 = !{i64 16, !"_ZTS8CBmpFile"}
!1 = !{i64 32, !"_ZTSM8CBmpFileFvvE.virtual"}
!2 = !{i64 40, !"_ZTSM8CBmpFileFP8CImgFilevE.virtual"}
!3 = !{i64 48, !"_ZTSM8CBmpFileKFivE.virtual"}
!4 = !{i64 56, !"_ZTSM8CBmpFileKFPKcvE.virtual"}
!5 = !{i64 64, !"_ZTSM8CBmpFileKFPKcvE.virtual"}
!6 = !{i64 72, !"_ZTSM8CBmpFileKFPKcvE.virtual"}
!7 = !{i64 80, !"_ZTSM8CBmpFileFbPKcE.virtual"}
!8 = !{i64 88, !"_ZTSM8CBmpFileFbPKcE.virtual"}
!9 = !{i64 16, !"_ZTS8CImgFile"}
!10 = !{i64 32, !"_ZTSM8CImgFileFvvE.virtual"}
!11 = !{i64 40, !"_ZTSM8CImgFileFPS_vE.virtual"}
!12 = !{i64 48, !"_ZTSM8CImgFileKFivE.virtual"}
!13 = !{i64 56, !"_ZTSM8CImgFileKFPKcvE.virtual"}
!14 = !{i64 64, !"_ZTSM8CImgFileKFPKcvE.virtual"}
!15 = !{i64 72, !"_ZTSM8CImgFileKFPKcvE.virtual"}
!16 = !{i64 80, !"_ZTSM8CImgFileFbPKcE.virtual"}
!17 = !{i64 88, !"_ZTSM8CImgFileFbPKcE.virtual"}
!18 = !{!"clang version 9.0.1-12 "}
!19 = !{i32 1, !"wchar_size", i32 4}
!20 = !{i32 1, !"ThinLTO", i32 0}
!21 = !{i32 1, !"EnableSplitLTOUnit", i32 0}
!22 = !{!23, !23, i64 0}
!23 = !{!"vtable pointer", !24, i64 0}
!24 = !{!"Simple C++ TBAA"}
!25 = !{!26, !29, i64 24}
!26 = !{!"_ZTS8CImgFile", !27, i64 8, !29, i64 16, !29, i64 24, !30, i64 32, !29, i64 40}
!27 = !{!"bool", !28, i64 0}
!28 = !{!"omnipotent char", !24, i64 0}
!29 = !{!"any pointer", !28, i64 0}
!30 = !{!"int", !28, i64 0}
!31 = !{!32, !29, i64 0}
!32 = !{!"_ZTS5CLDIB", !29, i64 0}
!33 = !{!26, !30, i64 32}
!34 = !{!26, !29, i64 40}
!35 = !{!26, !27, i64 8}
!36 = !{!37, !38, i64 0}
!37 = !{!"_ZTS7my_file", !38, i64 0}
!38 = !{!"long", !28, i64 0}
!39 = !{!29, !29, i64 0}
!40 = !{!41, !30, i64 0}
!41 = !{!"_ZTS19tagBITMAPINFOHEADER", !30, i64 0, !30, i64 4, !30, i64 8, !42, i64 12, !42, i64 14, !30, i64 16, !30, i64 20, !30, i64 24, !30, i64 28, !30, i64 32, !30, i64 36}
!42 = !{!"short", !28, i64 0}
!43 = !{!41, !30, i64 4}
!44 = !{!41, !30, i64 8}
!45 = !{!41, !42, i64 12}
!46 = !{!41, !42, i64 14}
!47 = !{!41, !30, i64 16}
!48 = !{!41, !30, i64 20}
!49 = !{!41, !30, i64 32}
!50 = !{!26, !29, i64 16}
