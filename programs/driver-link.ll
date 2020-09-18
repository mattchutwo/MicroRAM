; ModuleID = 'driver-link.bc'
source_filename = "llvm-link"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.tagBITMAPFILEHEADER = type <{ i16, i32, i16, i16, i32 }>
%struct.tagBITMAPINFOHEADER = type { i32, i32, i32, i16, i16, i32, i32, i32, i32, i32, i32 }
%class.CBmpFile = type { %class.CImgFile }
%class.CImgFile = type { i32 (...)**, i8, i8*, %struct.CLDIB*, i32, i8* }
%struct.CLDIB = type { i8* }
%struct.tagRGBQUAD = type { i8, i8, i8, i8 }

@llvm.global_ctors = appending global [0 x { i32, void ()*, i8* }] zeroinitializer
@bmp_file_header = dso_local local_unnamed_addr global %struct.tagBITMAPFILEHEADER <{ i16 19778, i32 102, i16 0, i16 0, i32 54 }>, section ".data.secret", align 1
@bmp_info_header = dso_local local_unnamed_addr global %struct.tagBITMAPINFOHEADER { i32 40, i32 4, i32 4, i16 1, i16 24, i32 0, i32 48, i32 2835, i32 2835, i32 16, i32 0 }, section ".data.secret", align 4
@bmp_image_data = dso_local local_unnamed_addr global [256 x i8] zeroinitializer, section ".data.secret", align 16
@_ZTV8CBmpFile = internal unnamed_addr constant { [12 x i8*] } { [12 x i8*] [i8* null, i8* null, i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD2Ev to i8*), i8* bitcast (void (%class.CBmpFile*)* @_ZN8CBmpFileD0Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFile5ClearEv to i8*), i8* bitcast (%class.CImgFile* (%class.CBmpFile*)* @_ZN8CBmpFile5VMakeEv to i8*), i8* bitcast (i32 (%class.CBmpFile*)* @_ZNK8CBmpFile7GetTypeEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile6GetExtEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile7GetDescEv to i8*), i8* bitcast (i8* (%class.CBmpFile*)* @_ZNK8CBmpFile9GetFormatEv to i8*), i8* bitcast (i1 (%class.CBmpFile*, i8*)* @_ZN8CBmpFile4LoadEPKc to i8*), i8* bitcast (i1 (%class.CBmpFile*, i8*)* @_ZN8CBmpFile4SaveEPKc to i8*)] }, align 8, !type !0, !type !1, !type !2, !type !3, !type !4, !type !5, !type !6, !type !7, !type !8, !type !9, !type !10, !type !11, !type !12, !type !13, !type !14, !type !15, !type !16, !type !17
@.str.126 = private unnamed_addr constant [13 x i8] c"fromager.bmp\00", align 1
@_ZTV8CImgFile = internal unnamed_addr constant { [12 x i8*] } { [12 x i8*] [i8* null, i8* null, i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD2Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFileD0Ev to i8*), i8* bitcast (void (%class.CImgFile*)* @_ZN8CImgFile5ClearEv to i8*), i8* bitcast (%class.CImgFile* (%class.CImgFile*)* @_ZN8CImgFile5VMakeEv to i8*), i8* bitcast (i32 (%class.CImgFile*)* @_ZNK8CImgFile7GetTypeEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile6GetExtEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile7GetDescEv to i8*), i8* bitcast (i8* (%class.CImgFile*)* @_ZNK8CImgFile9GetFormatEv to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*), i8* bitcast (void ()* @__cxa_pure_virtual to i8*)] }, align 8, !type !9, !type !10, !type !11, !type !12, !type !13, !type !14, !type !15, !type !16, !type !17
@.str.14 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str.3.48 = private unnamed_addr constant [20 x i8] c"File doesn't exist.\00", align 1
@_ZTIPKc = external dso_local constant i8*
@.str.5.50 = private unnamed_addr constant [33 x i8] c"Ack! File format not recognized.\00", align 1
@.str = private unnamed_addr constant [35 x i8] c"Multiple bitplanes are unsupported\00", align 1
@.str.1 = private unnamed_addr constant [28 x i8] c"BMP compression unsupported\00", align 1
@.str.2.47 = private unnamed_addr constant [25 x i8] c"Memory allocation error.\00", align 1
@.str.45 = private unnamed_addr constant [11 x i8] c"All clear!\00", align 1
@.str.1.46 = private unnamed_addr constant [23 x i8] c"Non-descript error :(.\00", align 1
@.str.6 = private unnamed_addr constant [4 x i8] c"BMP\00", align 1
@.str.5 = private unnamed_addr constant [15 x i8] c"Windows Bitmap\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"bmp\00", align 1

; Function Attrs: norecurse uwtable
define dso_local i32 @main() local_unnamed_addr #0 personality i32 (...)* @__gxx_personality_v0 {
  %1 = alloca %class.CBmpFile, align 8
  %2 = bitcast %class.CBmpFile* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 48, i8* nonnull %2) #15
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 0
  %4 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 1
  store i8 0, i8* %4, align 8, !tbaa !22
  %5 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 2
  %6 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 4
  %7 = bitcast i8** %5 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %7, i8 0, i64 16, i1 false) #15
  store i32 8, i32* %6, align 8, !tbaa !29
  %8 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 5
  store i8* null, i8** %8, align 8, !tbaa !30
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CBmpFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !31
  %9 = invoke zeroext i1 @_ZN8CBmpFile4LoadEPKc(%class.CBmpFile* nonnull %1, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.126, i64 0, i64 0))
          to label %_Z7BmpLoadPKc.exit unwind label %10

10:                                               ; preds = %0
  %11 = landingpad { i8*, i32 }
          cleanup
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !31
  %12 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 3
  %13 = load %struct.CLDIB*, %struct.CLDIB** %12, align 8, !tbaa !33
  %14 = icmp eq %struct.CLDIB* %13, null
  br i1 %14, label %_ZN8CImgFileD2Ev.exit.i, label %15

15:                                               ; preds = %10
  %16 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %13, i64 0, i32 0
  %17 = load i8*, i8** %16, align 8, !tbaa !34
  tail call void @free(i8* %17) #15
  %18 = bitcast %struct.CLDIB* %13 to i8*
  tail call void @free(i8* %18) #15
  br label %_ZN8CImgFileD2Ev.exit.i

_ZN8CImgFileD2Ev.exit.i:                          ; preds = %15, %10
  store %struct.CLDIB* null, %struct.CLDIB** %12, align 8, !tbaa !33
  store i32 8, i32* %6, align 8, !tbaa !29
  %19 = load i8*, i8** %8, align 8, !tbaa !30
  tail call void @free(i8* %19) #15
  call void @llvm.lifetime.end.p0i8(i64 48, i8* nonnull %2) #15
  resume { i8*, i32 } %11

_Z7BmpLoadPKc.exit:                               ; preds = %0
  %20 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %1, i64 0, i32 0, i32 3
  %21 = load %struct.CLDIB*, %struct.CLDIB** %20, align 8, !tbaa !33
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !31
  store %struct.CLDIB* null, %struct.CLDIB** %20, align 8, !tbaa !33
  store i32 8, i32* %6, align 8, !tbaa !29
  %22 = load i8*, i8** %8, align 8, !tbaa !30
  tail call void @free(i8* %22) #15
  call void @llvm.lifetime.end.p0i8(i64 48, i8* nonnull %2) #15
  %23 = icmp eq %struct.CLDIB* %21, null
  br i1 %23, label %_Z8dib_freeP5CLDIB.exit, label %24

24:                                               ; preds = %_Z7BmpLoadPKc.exit
  %25 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %21, i64 0, i32 0
  %26 = load i8*, i8** %25, align 8, !tbaa !34
  tail call void @free(i8* %26) #15
  %27 = bitcast %struct.CLDIB* %21 to i8*
  tail call void @free(i8* %27) #15
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %_Z7BmpLoadPKc.exit, %24
  ret i32 0
}

declare dso_local i32 @__gxx_personality_v0(...)

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

; Function Attrs: uwtable
define internal zeroext i1 @_ZN8CBmpFile4LoadEPKc(%class.CBmpFile* nocapture, i8* nocapture readonly) unnamed_addr #2 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %3 = tail call i32 @strcmp(i8* %1, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.126, i64 0, i64 0)) #16
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %fopen.exit, label %fopen.exit.thread

fopen.exit:                                       ; preds = %2
  %5 = tail call noalias i8* @malloc(i64 8) #15
  %6 = bitcast i8* %5 to i64*
  store i64 0, i64* %6, align 8, !tbaa !36
  %7 = icmp eq i8* %5, null
  br i1 %7, label %fopen.exit.thread, label %fread_unlocked.exit

fopen.exit.thread:                                ; preds = %fopen.exit, %2
  %8 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %9 = bitcast i8* %8 to i64*
  store i64 ptrtoint ([20 x i8]* @.str.3.48 to i64), i64* %9, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %8, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %236 unwind label %10

10:                                               ; preds = %fopen.exit.thread
  %11 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %203

fread_unlocked.exit:                              ; preds = %fopen.exit
  %12 = load i16, i16* getelementptr inbounds (%struct.tagBITMAPFILEHEADER, %struct.tagBITMAPFILEHEADER* @bmp_file_header, i64 0, i32 0), align 1, !tbaa !40
  store i64 14, i64* %6, align 8, !tbaa !36
  %13 = icmp eq i16 %12, 19778
  br i1 %13, label %21, label %14

14:                                               ; preds = %fread_unlocked.exit
  %15 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %16 = bitcast i8* %15 to i64*
  store i64 ptrtoint ([33 x i8]* @.str.5.50 to i64), i64* %16, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %15, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %236 unwind label %17

17:                                               ; preds = %14
  %18 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %203

19:                                               ; preds = %34, %29
  %20 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %203

21:                                               ; preds = %fread_unlocked.exit
  %22 = load i32, i32* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 1), align 4, !tbaa !43
  %23 = load i32, i32* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 2), align 4, !tbaa !45
  %24 = load i16, i16* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 3), align 4, !tbaa !46
  %25 = load i16, i16* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 4), align 2, !tbaa !47
  %26 = load i32, i32* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 5), align 4, !tbaa !48
  %27 = load i32, i32* getelementptr inbounds (%struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* @bmp_info_header, i64 0, i32 9), align 4, !tbaa !49
  store i64 54, i64* %6, align 8, !tbaa !36
  %28 = icmp ugt i16 %24, 1
  br i1 %28, label %29, label %32

29:                                               ; preds = %21
  %30 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %31 = bitcast i8* %30 to i64*
  store i64 ptrtoint ([35 x i8]* @.str to i64), i64* %31, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %30, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %236 unwind label %19

32:                                               ; preds = %21
  %33 = icmp eq i32 %26, 0
  br i1 %33, label %37, label %34

34:                                               ; preds = %32
  %35 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %36 = bitcast i8* %35 to i64*
  store i64 ptrtoint ([28 x i8]* @.str.1 to i64), i64* %36, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %35, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %236 unwind label %19

37:                                               ; preds = %32
  %38 = icmp slt i32 %23, 0
  %39 = sub nsw i32 0, %23
  %40 = select i1 %38, i32 %39, i32 %23
  %41 = zext i16 %25 to i32
  %42 = mul nsw i32 %22, %41
  %43 = add nsw i32 %42, 31
  %44 = ashr i32 %43, 5
  %45 = shl nsw i32 %44, 2
  %46 = mul nsw i32 %45, %40
  %47 = icmp ult i16 %25, 9
  %48 = icmp eq i32 %27, 0
  %or.cond = and i1 %47, %48
  %49 = shl i32 1, %41
  %.32.33 = select i1 %or.cond, i32 %49, i32 %27
  switch i16 %25, label %_Z9dib_allociiiPKhb.exit.thread [
    i16 1, label %52
    i16 4, label %52
    i16 8, label %52
    i16 16, label %52
    i16 24, label %52
    i16 32, label %52
  ]

50:                                               ; preds = %_Z9dib_allociiiPKhb.exit.thread
  %51 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  br label %203

52:                                               ; preds = %37, %37, %37, %37, %37, %37
  %53 = icmp ugt i16 %25, 8
  %54 = select i1 %53, i32 0, i32 %49
  %55 = tail call noalias i8* @malloc(i64 8) #15
  %56 = icmp eq i8* %55, null
  br i1 %56, label %_Z9dib_allociiiPKhb.exit.thread, label %57

57:                                               ; preds = %52
  %58 = sext i32 %54 to i64
  %59 = shl nsw i64 %58, 2
  %60 = add nsw i64 %59, 40
  %61 = sext i32 %46 to i64
  %62 = add nsw i64 %60, %61
  %63 = tail call noalias i8* @malloc(i64 %62) #15
  %64 = bitcast i8* %55 to i8**
  store i8* %63, i8** %64, align 8, !tbaa !34
  %65 = icmp eq i8* %63, null
  br i1 %65, label %66, label %67

66:                                               ; preds = %57
  tail call void @free(i8* nonnull %55) #15
  br label %_Z9dib_allociiiPKhb.exit.thread

67:                                               ; preds = %57
  %68 = bitcast i8* %63 to i32*
  store i32 40, i32* %68, align 4, !tbaa !50
  %69 = getelementptr inbounds i8, i8* %63, i64 4
  %70 = bitcast i8* %69 to i32*
  store i32 %22, i32* %70, align 4, !tbaa !43
  %71 = sub nsw i32 0, %40
  %72 = getelementptr inbounds i8, i8* %63, i64 8
  %73 = bitcast i8* %72 to i32*
  store i32 %71, i32* %73, align 4, !tbaa !45
  %74 = getelementptr inbounds i8, i8* %63, i64 12
  %75 = bitcast i8* %74 to i16*
  store i16 1, i16* %75, align 4, !tbaa !46
  %76 = getelementptr inbounds i8, i8* %63, i64 14
  %77 = bitcast i8* %76 to i16*
  store i16 %25, i16* %77, align 2, !tbaa !47
  %78 = getelementptr inbounds i8, i8* %63, i64 16
  %79 = bitcast i8* %78 to i32*
  store i32 0, i32* %79, align 4, !tbaa !48
  %80 = getelementptr inbounds i8, i8* %63, i64 20
  %81 = bitcast i8* %80 to i32*
  store i32 %46, i32* %81, align 4, !tbaa !51
  %82 = getelementptr inbounds i8, i8* %63, i64 24
  %83 = bitcast i8* %82 to i32*
  store i32 0, i32* %83, align 4, !tbaa !52
  %84 = getelementptr inbounds i8, i8* %63, i64 28
  %85 = bitcast i8* %84 to i32*
  store i32 0, i32* %85, align 4, !tbaa !53
  %86 = getelementptr inbounds i8, i8* %63, i64 32
  %87 = bitcast i8* %86 to i32*
  store i32 %54, i32* %87, align 4, !tbaa !49
  %88 = getelementptr inbounds i8, i8* %63, i64 36
  %89 = bitcast i8* %88 to i32*
  store i32 0, i32* %89, align 4, !tbaa !54
  %90 = getelementptr inbounds i8, i8* %63, i64 40
  %91 = bitcast i8* %90 to %struct.tagRGBQUAD*
  %92 = icmp sgt i32 %54, 0
  br i1 %92, label %93, label %fread_unlocked.exit5

93:                                               ; preds = %67
  %94 = zext i32 %54 to i64
  %95 = add nsw i64 %94, -1
  %xtraiter69 = and i64 %94, 7
  %96 = icmp ult i64 %95, 7
  br i1 %96, label %fread_unlocked.exit5.loopexit.unr-lcssa, label %.new68

.new68:                                           ; preds = %93
  %unroll_iter = sub nuw nsw i64 %94, %xtraiter69
  br label %97

97:                                               ; preds = %97, %.new68
  %98 = phi i64 [ 0, %.new68 ], [ %138, %97 ]
  %niter = phi i64 [ %unroll_iter, %.new68 ], [ %niter.nsub.7, %97 ]
  %99 = trunc i64 %98 to i32
  %100 = shl i32 %99, 16
  %101 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %98
  %102 = bitcast %struct.tagRGBQUAD* %101 to i32*
  store i32 %100, i32* %102, align 4, !tbaa !55
  %103 = or i64 %98, 1
  %104 = trunc i64 %103 to i32
  %105 = shl i32 %104, 16
  %106 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %103
  %107 = bitcast %struct.tagRGBQUAD* %106 to i32*
  store i32 %105, i32* %107, align 4, !tbaa !55
  %108 = or i64 %98, 2
  %109 = trunc i64 %108 to i32
  %110 = shl i32 %109, 16
  %111 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %108
  %112 = bitcast %struct.tagRGBQUAD* %111 to i32*
  store i32 %110, i32* %112, align 4, !tbaa !55
  %113 = or i64 %98, 3
  %114 = trunc i64 %113 to i32
  %115 = shl i32 %114, 16
  %116 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %113
  %117 = bitcast %struct.tagRGBQUAD* %116 to i32*
  store i32 %115, i32* %117, align 4, !tbaa !55
  %118 = or i64 %98, 4
  %119 = trunc i64 %118 to i32
  %120 = shl i32 %119, 16
  %121 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %118
  %122 = bitcast %struct.tagRGBQUAD* %121 to i32*
  store i32 %120, i32* %122, align 4, !tbaa !55
  %123 = or i64 %98, 5
  %124 = trunc i64 %123 to i32
  %125 = shl i32 %124, 16
  %126 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %123
  %127 = bitcast %struct.tagRGBQUAD* %126 to i32*
  store i32 %125, i32* %127, align 4, !tbaa !55
  %128 = or i64 %98, 6
  %129 = trunc i64 %128 to i32
  %130 = shl i32 %129, 16
  %131 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %128
  %132 = bitcast %struct.tagRGBQUAD* %131 to i32*
  store i32 %130, i32* %132, align 4, !tbaa !55
  %133 = or i64 %98, 7
  %134 = trunc i64 %133 to i32
  %135 = shl i32 %134, 16
  %136 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %133
  %137 = bitcast %struct.tagRGBQUAD* %136 to i32*
  store i32 %135, i32* %137, align 4, !tbaa !55
  %138 = add nuw nsw i64 %98, 8
  %niter.nsub.7 = add i64 %niter, -8
  %niter.ncmp.7 = icmp eq i64 %niter.nsub.7, 0
  br i1 %niter.ncmp.7, label %fread_unlocked.exit5.loopexit.unr-lcssa, label %97

_Z9dib_allociiiPKhb.exit.thread:                  ; preds = %37, %52, %66
  %139 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %140 = bitcast i8* %139 to i64*
  store i64 ptrtoint ([25 x i8]* @.str.2.47 to i64), i64* %140, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %139, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %236 unwind label %50

fread_unlocked.exit5.loopexit.unr-lcssa:          ; preds = %97, %93
  %.unr70 = phi i64 [ 0, %93 ], [ %138, %97 ]
  %lcmp.mod71 = icmp eq i64 %xtraiter69, 0
  br i1 %lcmp.mod71, label %fread_unlocked.exit5, label %.epil.preheader

.epil.preheader:                                  ; preds = %fread_unlocked.exit5.loopexit.unr-lcssa, %.epil.preheader
  %141 = phi i64 [ %146, %.epil.preheader ], [ %.unr70, %fread_unlocked.exit5.loopexit.unr-lcssa ]
  %epil.iter = phi i64 [ %epil.iter.sub, %.epil.preheader ], [ %xtraiter69, %fread_unlocked.exit5.loopexit.unr-lcssa ]
  %142 = trunc i64 %141 to i32
  %143 = shl i32 %142, 16
  %144 = getelementptr inbounds %struct.tagRGBQUAD, %struct.tagRGBQUAD* %91, i64 %141
  %145 = bitcast %struct.tagRGBQUAD* %144 to i32*
  store i32 %143, i32* %145, align 4, !tbaa !55
  %146 = add nuw nsw i64 %141, 1
  %epil.iter.sub = add nsw i64 %epil.iter, -1
  %epil.iter.cmp = icmp eq i64 %epil.iter.sub, 0
  br i1 %epil.iter.cmp, label %fread_unlocked.exit5, label %.epil.preheader, !llvm.loop !56

fread_unlocked.exit5:                             ; preds = %.epil.preheader, %fread_unlocked.exit5.loopexit.unr-lcssa, %67
  %147 = zext i32 %.32.33 to i64
  %148 = icmp ult i64 %147, 50
  %149 = select i1 %148, i64 %147, i64 50
  %150 = shl nuw nsw i64 %149, 2
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %90, i8* nonnull align 2 getelementptr inbounds ([256 x i8], [256 x i8]* @bmp_image_data, i64 0, i64 54), i64 %150, i1 false) #15
  %151 = add nuw nsw i64 %150, 54
  %152 = getelementptr inbounds i8, i8* %63, i64 %60
  %153 = sub nuw nsw i64 202, %150
  %154 = udiv i64 %153, %61
  %155 = icmp ult i64 %153, %61
  %156 = select i1 %155, i64 %154, i64 1
  %157 = getelementptr inbounds [256 x i8], [256 x i8]* @bmp_image_data, i64 0, i64 %151
  %158 = mul i64 %156, %61
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %152, i8* nonnull align 2 %157, i64 %158, i1 false) #15
  %159 = add i64 %158, %151
  store i64 %159, i64* %6, align 8, !tbaa !36
  %160 = icmp sgt i32 %23, -1
  br i1 %160, label %161, label %_Z9dib_vflipP5CLDIB.exit

161:                                              ; preds = %fread_unlocked.exit5
  %162 = load i32, i32* %70, align 4, !tbaa !43
  %163 = load i16, i16* %77, align 2, !tbaa !47
  %164 = zext i16 %163 to i32
  %165 = mul nsw i32 %162, %164
  %166 = add nsw i32 %165, 31
  %167 = ashr i32 %166, 5
  %168 = shl nsw i32 %167, 2
  %169 = load i32, i32* %73, align 4, !tbaa !45
  %170 = icmp slt i32 %169, 0
  %171 = sub nsw i32 0, %169
  %172 = select i1 %170, i32 %171, i32 %169
  %173 = zext i32 %168 to i64
  %174 = tail call noalias i8* @malloc(i64 %173) #15
  %175 = icmp eq i8* %174, null
  br i1 %175, label %_Z9dib_vflipP5CLDIB.exit, label %176

176:                                              ; preds = %161
  %177 = load i32, i32* %87, align 4, !tbaa !49
  %178 = sext i32 %177 to i64
  %179 = shl nsw i64 %178, 2
  %180 = add nsw i64 %179, 40
  %181 = getelementptr inbounds i8, i8* %63, i64 %180
  %182 = lshr i32 %172, 1
  %183 = icmp eq i32 %182, 0
  br i1 %183, label %.loopexit.i, label %184

184:                                              ; preds = %176
  %185 = add nsw i32 %172, -1
  %186 = mul i32 %168, %185
  %187 = zext i32 %186 to i64
  %188 = getelementptr inbounds i8, i8* %181, i64 %187
  %189 = sub nsw i64 0, %173
  %xtraiter = and i32 %172, 2
  %lcmp.mod = icmp eq i32 %xtraiter, 0
  br i1 %lcmp.mod, label %.prol.loopexit, label %.prol.preheader

.prol.preheader:                                  ; preds = %184
  %190 = add nsw i32 %182, -1
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %174, i8* nonnull align 1 %181, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %181, i8* nonnull align 1 %188, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %188, i8* nonnull align 1 %174, i64 %173, i1 false) #15
  %191 = getelementptr inbounds i8, i8* %181, i64 %173
  %192 = getelementptr inbounds i8, i8* %188, i64 %189
  br label %.prol.loopexit

.prol.loopexit:                                   ; preds = %184, %.prol.preheader
  %.unr = phi i32 [ %182, %184 ], [ %190, %.prol.preheader ]
  %.unr66 = phi i8* [ %181, %184 ], [ %191, %.prol.preheader ]
  %.unr67 = phi i8* [ %188, %184 ], [ %192, %.prol.preheader ]
  %193 = icmp eq i32 %182, 1
  br i1 %193, label %.loopexit.i, label %.new

.new:                                             ; preds = %.prol.loopexit, %.new
  %194 = phi i32 [ %199, %.new ], [ %.unr, %.prol.loopexit ]
  %195 = phi i8* [ %200, %.new ], [ %.unr66, %.prol.loopexit ]
  %196 = phi i8* [ %201, %.new ], [ %.unr67, %.prol.loopexit ]
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %174, i8* align 1 %195, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %195, i8* align 1 %196, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %196, i8* nonnull align 1 %174, i64 %173, i1 false) #15
  %197 = getelementptr inbounds i8, i8* %195, i64 %173
  %198 = getelementptr inbounds i8, i8* %196, i64 %189
  %199 = add nsw i32 %194, -2
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull align 1 %174, i8* align 1 %197, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %197, i8* align 1 %198, i64 %173, i1 false) #15
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %198, i8* nonnull align 1 %174, i64 %173, i1 false) #15
  %200 = getelementptr inbounds i8, i8* %197, i64 %173
  %201 = getelementptr inbounds i8, i8* %198, i64 %189
  %202 = icmp eq i32 %199, 0
  br i1 %202, label %.loopexit.i, label %.new

.loopexit.i:                                      ; preds = %.new, %.prol.loopexit, %176
  tail call void @free(i8* %174) #15
  br label %_Z9dib_vflipP5CLDIB.exit

203:                                              ; preds = %19, %50, %17, %10
  %.sink65 = phi { i8*, i32 } [ %18, %17 ], [ %11, %10 ], [ %51, %50 ], [ %20, %19 ]
  %204 = phi i1 [ true, %17 ], [ false, %10 ], [ true, %50 ], [ true, %19 ]
  %205 = phi i8* [ %5, %17 ], [ null, %10 ], [ %5, %50 ], [ %5, %19 ]
  %206 = extractvalue { i8*, i32 } %.sink65, 1
  %207 = tail call i32 @llvm.eh.typeid.for(i8* bitcast (i8** @_ZTIPKc to i8*)) #15
  %208 = icmp eq i32 %206, %207
  br i1 %208, label %209, label %235

209:                                              ; preds = %203
  %210 = extractvalue { i8*, i32 } %.sink65, 0
  %211 = tail call i8* @__cxa_begin_catch(i8* %210) #15
  %212 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* %211, i8** %212, align 8, !tbaa !58
  tail call void @__cxa_end_catch() #15
  br i1 %204, label %213, label %233

213:                                              ; preds = %209
  tail call void @free(i8* %205) #15
  br label %233

_Z9dib_vflipP5CLDIB.exit:                         ; preds = %.loopexit.i, %161, %fread_unlocked.exit5
  tail call void @free(i8* %5) #15
  %214 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.45, i64 0, i64 0), i8** %214, align 8, !tbaa !58
  %215 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %216 = load %struct.CLDIB*, %struct.CLDIB** %215, align 8, !tbaa !33
  %217 = bitcast %struct.CLDIB** %215 to i8**
  store i8* %55, i8** %217, align 8, !tbaa !33
  %218 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 1
  store i8 1, i8* %218, align 8, !tbaa !22
  %219 = icmp eq %struct.CLDIB* %216, null
  br i1 %219, label %_Z8dib_freeP5CLDIB.exit, label %220

220:                                              ; preds = %_Z9dib_vflipP5CLDIB.exit
  %221 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %216, i64 0, i32 0
  %222 = load i8*, i8** %221, align 8, !tbaa !34
  tail call void @free(i8* %222) #15
  %223 = bitcast %struct.CLDIB* %216 to i8*
  tail call void @free(i8* %223) #15
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %_Z9dib_vflipP5CLDIB.exit, %220
  %224 = bitcast i8* %55 to %struct.tagBITMAPINFOHEADER**
  %225 = load %struct.tagBITMAPINFOHEADER*, %struct.tagBITMAPINFOHEADER** %224, align 8, !tbaa !34
  %226 = getelementptr inbounds %struct.tagBITMAPINFOHEADER, %struct.tagBITMAPINFOHEADER* %225, i64 0, i32 4
  %227 = load i16, i16* %226, align 2, !tbaa !47
  %228 = zext i16 %227 to i32
  %229 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 4
  store i32 %228, i32* %229, align 8, !tbaa !29
  %230 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 5
  %231 = load i8*, i8** %230, align 8, !tbaa !30
  tail call void @free(i8* %231) #15
  store i8* null, i8** %230, align 8, !tbaa !30
  %232 = tail call noalias i8* @strdup(i8* %1) #15
  store i8* %232, i8** %230, align 8, !tbaa !30
  br label %233

233:                                              ; preds = %_Z8dib_freeP5CLDIB.exit, %213, %209
  %234 = phi i1 [ true, %_Z8dib_freeP5CLDIB.exit ], [ false, %213 ], [ false, %209 ]
  ret i1 %234

235:                                              ; preds = %203
  resume { i8*, i32 } %.sink65

236:                                              ; preds = %_Z9dib_allociiiPKhb.exit.thread, %34, %29, %14, %fopen.exit.thread
  unreachable
}

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nounwind uwtable
define internal void @_ZN8CImgFileD2Ev(%class.CImgFile* nocapture) unnamed_addr #3 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !31
  %3 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !33
  %5 = icmp eq %struct.CLDIB* %4, null
  br i1 %5, label %10, label %6

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %4, i64 0, i32 0
  %8 = load i8*, i8** %7, align 8, !tbaa !34
  tail call void @free(i8* %8) #15
  %9 = bitcast %struct.CLDIB* %4 to i8*
  tail call void @free(i8* %9) #15
  br label %10

10:                                               ; preds = %6, %1
  store %struct.CLDIB* null, %struct.CLDIB** %3, align 8, !tbaa !33
  %11 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 4
  store i32 8, i32* %11, align 8, !tbaa !29
  %12 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 5
  %13 = load i8*, i8** %12, align 8, !tbaa !30
  tail call void @free(i8* %13) #15
  store i8* null, i8** %12, align 8, !tbaa !30
  %14 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 1
  store i8 0, i8* %14, align 8, !tbaa !22
  ret void
}

; Function Attrs: noreturn nounwind uwtable
define internal void @_ZN8CImgFileD0Ev(%class.CImgFile* nocapture readnone) unnamed_addr #4 align 2 {
  tail call void @llvm.trap() #18
  unreachable
}

; Function Attrs: nounwind uwtable
define internal void @_ZN8CImgFile5ClearEv(%class.CImgFile* nocapture) unnamed_addr #3 align 2 {
  %2 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 3
  %3 = load %struct.CLDIB*, %struct.CLDIB** %2, align 8, !tbaa !33
  %4 = icmp eq %struct.CLDIB* %3, null
  br i1 %4, label %_Z8dib_freeP5CLDIB.exit, label %5

5:                                                ; preds = %1
  %6 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %3, i64 0, i32 0
  %7 = load i8*, i8** %6, align 8, !tbaa !34
  tail call void @free(i8* %7) #15
  %8 = bitcast %struct.CLDIB* %3 to i8*
  tail call void @free(i8* %8) #15
  br label %_Z8dib_freeP5CLDIB.exit

_Z8dib_freeP5CLDIB.exit:                          ; preds = %1, %5
  store %struct.CLDIB* null, %struct.CLDIB** %2, align 8, !tbaa !33
  %9 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 4
  store i32 8, i32* %9, align 8, !tbaa !29
  %10 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 5
  %11 = load i8*, i8** %10, align 8, !tbaa !30
  tail call void @free(i8* %11) #15
  store i8* null, i8** %10, align 8, !tbaa !30
  %12 = getelementptr inbounds %class.CImgFile, %class.CImgFile* %0, i64 0, i32 1
  store i8 0, i8* %12, align 8, !tbaa !22
  ret void
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal noalias %class.CImgFile* @_ZN8CImgFile5VMakeEv(%class.CImgFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret %class.CImgFile* null
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i32 @_ZNK8CImgFile7GetTypeEv(%class.CImgFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i32 -1
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile6GetExtEv(%class.CImgFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile7GetDescEv(%class.CImgFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CImgFile9GetFormatEv(%class.CImgFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.14, i64 0, i64 0)
}

declare dso_local void @__cxa_pure_virtual() unnamed_addr

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #6

declare dso_local i8* @__cxa_allocate_exception(i64) local_unnamed_addr

declare dso_local void @__cxa_throw(i8*, i8*, i8*) local_unnamed_addr

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

; Function Attrs: nounwind readnone
declare i32 @llvm.eh.typeid.for(i8*) #7

declare dso_local i8* @__cxa_begin_catch(i8*) local_unnamed_addr

declare dso_local void @__cxa_end_catch() local_unnamed_addr

; Function Attrs: inlinehint nounwind uwtable
define internal void @_ZN8CBmpFileD0Ev(%class.CBmpFile*) unnamed_addr #8 align 2 personality i32 (...)* @__gxx_personality_v0 {
  %2 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CImgFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %2, align 8, !tbaa !31
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !33
  %5 = icmp eq %struct.CLDIB* %4, null
  br i1 %5, label %_ZN8CImgFileD2Ev.exit, label %6

6:                                                ; preds = %1
  %7 = getelementptr inbounds %struct.CLDIB, %struct.CLDIB* %4, i64 0, i32 0
  %8 = load i8*, i8** %7, align 8, !tbaa !34
  tail call void @free(i8* %8) #15
  %9 = bitcast %struct.CLDIB* %4 to i8*
  tail call void @free(i8* %9) #15
  br label %_ZN8CImgFileD2Ev.exit

_ZN8CImgFileD2Ev.exit:                            ; preds = %1, %6
  store %struct.CLDIB* null, %struct.CLDIB** %3, align 8, !tbaa !33
  %10 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 4
  store i32 8, i32* %10, align 8, !tbaa !29
  %11 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 5
  %12 = load i8*, i8** %11, align 8, !tbaa !30
  tail call void @free(i8* %12) #15
  %13 = bitcast %class.CBmpFile* %0 to i8*
  tail call void @_ZdlPv(i8* %13) #19
  ret void
}

; Function Attrs: uwtable
define internal noalias nonnull %class.CImgFile* @_ZN8CBmpFile5VMakeEv(%class.CBmpFile* nocapture readnone) unnamed_addr #2 align 2 personality i32 (...)* @__gxx_personality_v0 {
  %2 = tail call i8* @_Znwm(i64 48) #20
  %3 = bitcast i8* %2 to i32 (...)***
  %4 = getelementptr inbounds i8, i8* %2, i64 8
  store i8 0, i8* %4, align 8, !tbaa !22
  %5 = getelementptr inbounds i8, i8* %2, i64 16
  %6 = getelementptr inbounds i8, i8* %2, i64 32
  %7 = bitcast i8* %6 to i32*
  tail call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %5, i8 0, i64 16, i1 false) #15
  store i32 8, i32* %7, align 8, !tbaa !29
  %8 = getelementptr inbounds i8, i8* %2, i64 40
  %9 = bitcast i8* %8 to i8**
  store i8* null, i8** %9, align 8, !tbaa !30
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [12 x i8*] }, { [12 x i8*] }* @_ZTV8CBmpFile, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %3, align 8, !tbaa !31
  %10 = bitcast i8* %2 to %class.CImgFile*
  ret %class.CImgFile* %10
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i32 @_ZNK8CBmpFile7GetTypeEv(%class.CBmpFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i32 0
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile6GetExtEv(%class.CBmpFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile7GetDescEv(%class.CBmpFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.5, i64 0, i64 0)
}

; Function Attrs: norecurse nounwind readnone uwtable
define internal i8* @_ZNK8CBmpFile9GetFormatEv(%class.CBmpFile* nocapture readnone) unnamed_addr #5 align 2 {
  ret i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.6, i64 0, i64 0)
}

; Function Attrs: uwtable
define internal zeroext i1 @_ZN8CBmpFile4SaveEPKc(%class.CBmpFile* nocapture, i8* nocapture readonly) unnamed_addr #2 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %3 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 3
  %4 = load %struct.CLDIB*, %struct.CLDIB** %3, align 8, !tbaa !33
  %5 = icmp eq %struct.CLDIB* %4, null
  %6 = tail call i8* @__cxa_allocate_exception(i64 8) #15
  %7 = bitcast i8* %6 to i64*
  br i1 %5, label %8, label %fopen.exit.thread

8:                                                ; preds = %2
  store i64 ptrtoint ([23 x i8]* @.str.1.46 to i64), i64* %7, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %6, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %19 unwind label %9

9:                                                ; preds = %fopen.exit.thread, %8
  %10 = landingpad { i8*, i32 }
          cleanup
          catch i8* bitcast (i8** @_ZTIPKc to i8*)
  %11 = extractvalue { i8*, i32 } %10, 1
  %12 = tail call i32 @llvm.eh.typeid.for(i8* bitcast (i8** @_ZTIPKc to i8*)) #15
  %13 = icmp eq i32 %11, %12
  br i1 %13, label %14, label %18

fopen.exit.thread:                                ; preds = %2
  store i64 ptrtoint ([20 x i8]* @.str.3.48 to i64), i64* %7, align 16, !tbaa !39
  invoke void @__cxa_throw(i8* %6, i8* bitcast (i8** @_ZTIPKc to i8*), i8* null) #17
          to label %19 unwind label %9

14:                                               ; preds = %9
  %15 = extractvalue { i8*, i32 } %10, 0
  %16 = tail call i8* @__cxa_begin_catch(i8* %15) #15
  %17 = getelementptr inbounds %class.CBmpFile, %class.CBmpFile* %0, i64 0, i32 0, i32 2
  store i8* %16, i8** %17, align 8, !tbaa !58
  tail call void @__cxa_end_catch() #15
  ret i1 false

18:                                               ; preds = %9
  resume { i8*, i32 } %10

19:                                               ; preds = %fopen.exit.thread, %8
  unreachable
}

; Function Attrs: nounwind uwtable
define dso_local i64* @malloc_words(i64) local_unnamed_addr #3 {
  %2 = tail call i64* @__cc_malloc(i64 %0) #21
  %3 = ptrtoint i64* %2 to i64
  %4 = lshr i64 %3, 58
  %5 = shl i64 1, %4
  %6 = add i64 %0, 1
  %7 = icmp ult i64 %5, %6
  br i1 %7, label %13, label %8

8:                                                ; preds = %1
  %9 = add i64 %5, -1
  %10 = and i64 %9, %3
  %11 = icmp eq i64 %10, 0
  %12 = zext i1 %11 to i32
  br label %13

13:                                               ; preds = %8, %1
  %14 = phi i32 [ 0, %1 ], [ %12, %8 ]
  tail call void @__cc_valid_if(i32 %14) #21
  %15 = getelementptr inbounds i64, i64* %2, i64 %5
  %16 = getelementptr inbounds i64, i64* %15, i64 -1
  tail call void @__cc_write_and_poison(i64* nonnull %16, i64 1) #21
  %17 = getelementptr inbounds i64, i64* %2, i64 %0
  %18 = tail call i64* @__cc_advise_poison(i64* %17, i64* nonnull %16) #21
  %19 = icmp eq i64* %18, null
  br i1 %19, label %25, label %20

20:                                               ; preds = %13
  %21 = icmp ule i64* %17, %18
  %22 = icmp ult i64* %18, %16
  %23 = and i1 %22, %21
  %24 = zext i1 %23 to i32
  tail call void @__cc_valid_if(i32 %24) #21
  tail call void @__cc_write_and_poison(i64* nonnull %18, i64 0) #21
  br label %25

25:                                               ; preds = %20, %13
  ret i64* %2
}

declare dso_local i64* @__cc_malloc(i64) local_unnamed_addr #9

declare dso_local void @__cc_valid_if(i32) local_unnamed_addr #9

declare dso_local void @__cc_write_and_poison(i64*, i64) local_unnamed_addr #9

declare dso_local i64* @__cc_advise_poison(i64*, i64*) local_unnamed_addr #9

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
  %9 = icmp ne i64 %8, 0
  %10 = zext i1 %9 to i32
  tail call void @__cc_bug_if(i32 %10) #21
  tail call void @__cc_free(i64* nonnull %0) #21
  %11 = getelementptr inbounds i64, i64* %0, i64 %6
  %12 = getelementptr inbounds i64, i64* %11, i64 -1
  %13 = tail call i64 @__cc_read_poisoned(i64* nonnull %12) #21
  %14 = icmp ne i64 %13, 1
  %15 = zext i1 %14 to i32
  tail call void @__cc_bug_if(i32 %15) #21
  tail call void @__cc_write_poisoned(i64* nonnull %12, i64 0) #21
  %16 = tail call i64* @__cc_advise_poison(i64* nonnull %0, i64* nonnull %12) #21
  %17 = icmp eq i64* %16, null
  br i1 %17, label %27, label %18

18:                                               ; preds = %3
  %19 = ptrtoint i64* %16 to i64
  %20 = and i64 %19, 7
  %21 = icmp eq i64 %20, 0
  %22 = zext i1 %21 to i32
  tail call void @__cc_valid_if(i32 %22) #21
  %23 = icmp uge i64* %16, %0
  %24 = icmp ult i64* %16, %12
  %25 = and i1 %24, %23
  %26 = zext i1 %25 to i32
  tail call void @__cc_valid_if(i32 %26) #21
  tail call void @__cc_write_and_poison(i64* nonnull %16, i64 0) #21
  br label %27

27:                                               ; preds = %18, %3, %1
  ret void
}

declare dso_local void @__cc_bug_if(i32) local_unnamed_addr #9

declare dso_local void @__cc_free(i64*) local_unnamed_addr #9

declare dso_local i64 @__cc_read_poisoned(i64*) local_unnamed_addr #9

declare dso_local void @__cc_write_poisoned(i64*, i64) local_unnamed_addr #9

; Function Attrs:  norecurse nounwind uwtable
define dso_local void @__llvm__memcpy__p0i8__p0i8__i64(i8* nocapture, i8* nocapture readonly, i64) local_unnamed_addr #10 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %5, label %6

5:                                                ; preds = %6, %3
  ret void

6:                                                ; preds = %6, %3
  %7 = phi i64 [ %11, %6 ], [ 0, %3 ]
  %8 = getelementptr inbounds i8, i8* %1, i64 %7
  %9 = load i8, i8* %8, align 1, !tbaa !59
  %10 = getelementptr inbounds i8, i8* %0, i64 %7
  store i8 %9, i8* %10, align 1, !tbaa !59
  %11 = add nuw i64 %7, 1
  %12 = icmp eq i64 %11, %2
  br i1 %12, label %5, label %6
}

; Function Attrs:  norecurse nounwind uwtable writeonly
define dso_local void @__llvm__memset__p0i8__i64(i8* nocapture, i8 zeroext, i64) local_unnamed_addr #11 {
  %4 = icmp eq i64 %2, 0
  br i1 %4, label %5, label %6

5:                                                ; preds = %6, %3
  ret void

6:                                                ; preds = %6, %3
  %7 = phi i64 [ %9, %6 ], [ 0, %3 ]
  %8 = getelementptr inbounds i8, i8* %0, i64 %7
  store i8 %1, i8* %8, align 1, !tbaa !59
  %9 = add nuw i64 %7, 1
  %10 = icmp eq i64 %9, %2
  br i1 %10, label %5, label %6
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

; Function Attrs: norecurse nounwind readonly uwtable
define dso_local i32 @strcmp(i8* nocapture nonnull readonly, i8* nocapture nonnull readonly) local_unnamed_addr #12 {
  %3 = load i8, i8* %0, align 1, !tbaa !59
  %4 = sext i8 %3 to i32
  %5 = load i8, i8* %1, align 1, !tbaa !59
  %6 = sext i8 %5 to i32
  %7 = sub nsw i32 %4, %6
  %8 = icmp ne i32 %7, 0
  %9 = icmp eq i8 %3, 0
  %10 = or i1 %9, %8
  br i1 %10, label %24, label %11

11:                                               ; preds = %11, %2
  %12 = phi i8* [ %14, %11 ], [ %0, %2 ]
  %13 = phi i8* [ %15, %11 ], [ %1, %2 ]
  %14 = getelementptr inbounds i8, i8* %12, i64 1
  %15 = getelementptr inbounds i8, i8* %13, i64 1
  %16 = load i8, i8* %14, align 1, !tbaa !59
  %17 = sext i8 %16 to i32
  %18 = load i8, i8* %15, align 1, !tbaa !59
  %19 = sext i8 %18 to i32
  %20 = sub nsw i32 %17, %19
  %21 = icmp ne i32 %20, 0
  %22 = icmp eq i8 %16, 0
  %23 = or i1 %22, %21
  br i1 %23, label %24, label %11

24:                                               ; preds = %11, %2
  %25 = phi i32 [ %7, %2 ], [ %20, %11 ]
  ret i32 %25
}

; Function Attrs: norecurse nounwind readonly uwtable
define dso_local i64 @strlen(i8* nonnull) local_unnamed_addr #12 {
  br label %2

2:                                                ; preds = %2, %1
  %3 = phi i8* [ %0, %1 ], [ %6, %2 ]
  %4 = load i8, i8* %3, align 1, !tbaa !59
  %5 = icmp eq i8 %4, 0
  %6 = getelementptr inbounds i8, i8* %3, i64 1
  br i1 %5, label %7, label %2

7:                                                ; preds = %2
  %8 = ptrtoint i8* %3 to i64
  %9 = ptrtoint i8* %0 to i64
  %10 = sub i64 %8, %9
  ret i64 %10
}

; Function Attrs: nounwind uwtable
define dso_local noalias i8* @strdup(i8* nonnull) local_unnamed_addr #3 {
  %2 = tail call i64 @strlen(i8* %0) #23
  %3 = add i64 %2, 1
  %4 = tail call noalias i8* @malloc(i64 %3) #21
  %5 = tail call i8* @strcpy(i8* %4, i8* %0) #21
  ret i8* %4
}

; Function Attrs:  norecurse nounwind uwtable
define dso_local nonnull i8* @strcpy(i8* nonnull returned, i8* nocapture nonnull readonly) local_unnamed_addr #10 {
  %3 = load i8, i8* %1, align 1, !tbaa !59
  %4 = icmp eq i8 %3, 0
  br i1 %4, label %13, label %5

5:                                                ; preds = %5, %2
  %6 = phi i8 [ %11, %5 ], [ %3, %2 ]
  %7 = phi i8* [ %9, %5 ], [ %0, %2 ]
  %8 = phi i8* [ %10, %5 ], [ %1, %2 ]
  store i8 %6, i8* %7, align 1, !tbaa !59
  %9 = getelementptr inbounds i8, i8* %7, i64 1
  %10 = getelementptr inbounds i8, i8* %8, i64 1
  %11 = load i8, i8* %10, align 1, !tbaa !59
  %12 = icmp eq i8 %11, 0
  br i1 %12, label %13, label %5

13:                                               ; preds = %5, %2
  ret i8* %0
}

; Function Attrs: nobuiltin  nounwind uwtable
define dso_local noalias i8* @_Znwm(i64) local_unnamed_addr #13 {
  %2 = tail call noalias i8* @malloc(i64 %0) #15
  ret i8* %2
}

; Function Attrs: nobuiltin nounwind uwtable
define dso_local void @_ZdlPv(i8* nocapture) local_unnamed_addr #14 {
  tail call void @free(i8* %0) #15
  ret void
}

attributes #0 = { norecurse uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { norecurse nounwind readnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { cold noreturn nounwind }
attributes #7 = { nounwind readnone }
attributes #8 = { inlinehint nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #9 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #10 = {  norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #11 = {  norecurse nounwind uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #12 = { norecurse nounwind readonly uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #13 = { nobuiltin  nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #14 = { nobuiltin nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "prefer-vector-width"="1" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #15 = { nounwind }
attributes #16 = { nounwind readonly }
attributes #17 = { noreturn }
attributes #18 = { noreturn nounwind }
attributes #19 = { builtin nounwind }
attributes #20 = { builtin }
attributes #21 = { nobuiltin nounwind }
attributes #22 = { nobuiltin }
attributes #23 = { nobuiltin nounwind readonly }

!llvm.ident = !{!18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18, !18}
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
!22 = !{!23, !24, i64 8}
!23 = !{!"_ZTS8CImgFile", !24, i64 8, !27, i64 16, !27, i64 24, !28, i64 32, !27, i64 40}
!24 = !{!"bool", !25, i64 0}
!25 = !{!"omnipotent char", !26, i64 0}
!26 = !{!"Simple C++ TBAA"}
!27 = !{!"any pointer", !25, i64 0}
!28 = !{!"int", !25, i64 0}
!29 = !{!23, !28, i64 32}
!30 = !{!23, !27, i64 40}
!31 = !{!32, !32, i64 0}
!32 = !{!"vtable pointer", !26, i64 0}
!33 = !{!23, !27, i64 24}
!34 = !{!35, !27, i64 0}
!35 = !{!"_ZTS5CLDIB", !27, i64 0}
!36 = !{!37, !38, i64 0}
!37 = !{!"_ZTS7my_file", !38, i64 0}
!38 = !{!"long", !25, i64 0}
!39 = !{!27, !27, i64 0}
!40 = !{!41, !42, i64 0}
!41 = !{!"_ZTS19tagBITMAPFILEHEADER", !42, i64 0, !28, i64 2, !42, i64 6, !42, i64 8, !28, i64 10}
!42 = !{!"short", !25, i64 0}
!43 = !{!44, !28, i64 4}
!44 = !{!"_ZTS19tagBITMAPINFOHEADER", !28, i64 0, !28, i64 4, !28, i64 8, !42, i64 12, !42, i64 14, !28, i64 16, !28, i64 20, !28, i64 24, !28, i64 28, !28, i64 32, !28, i64 36}
!45 = !{!44, !28, i64 8}
!46 = !{!44, !42, i64 12}
!47 = !{!44, !42, i64 14}
!48 = !{!44, !28, i64 16}
!49 = !{!44, !28, i64 32}
!50 = !{!44, !28, i64 0}
!51 = !{!44, !28, i64 20}
!52 = !{!44, !28, i64 24}
!53 = !{!44, !28, i64 28}
!54 = !{!44, !28, i64 36}
!55 = !{!28, !28, i64 0}
!56 = distinct !{!56, !57}
!57 = !{!"llvm.loop.unroll.disable"}
!58 = !{!23, !27, i64 16}
!59 = !{!60, !60, i64 0}
!60 = !{!"omnipotent char", !61, i64 0}
!61 = !{!"Simple C/C++ TBAA"}
