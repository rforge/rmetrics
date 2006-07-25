
CONTENT:

C Interpolation of Irregularly Spaced Data

C###############################################################################

C Package: akima
C Version: 0.3-4
C Title: Interpolation of irregularly spaced data
C Author: Fortran code by H. Akima
C   R port by Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
C Maintainer: Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
C Description: Linear or cubic spline interpolation for irregular gridded data
C License: Fortran code: ACM, free for non-commercial use, R functions GPL

C DW:
C Added - No Debian Package available


C


      SUBROUTINE SDBI3P(MD,NDP,XD,YD,ZD,NIP,XI,YI, ZI,IER, WK,IWK,
     +                  EXTRPI,NEAR,NEXT,DIST)
*
* Scattered-data bivariate interpolation
* (a master subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs bivariate interpolation when the data
* points are scattered in the x-y plane.  It is based on the
* revised Akima method that has the accuracy of a cubic (third-
* degree) polynomial.
*
* The input arguments are
*   MD  = mode of computation
*       = 1 for new XD-YD (default)
*       = 2 for old XD-YD, new ZD
*       = 3 for old XD-YD, old ZD,
*   NDP = number of data points (must be 10 or greater),
*   XD  = array of dimension NDP containing the x coordinates
*         of the data points,
*   YD  = array of dimension NDP containing the y coordinates
*         of the data points,
*   ZD  = array of dimension NDP containing the z values at
*         the data points,
*   NIP = number of output points at which interpolation is
*         to be performed (must be 1 or greater),
*   XI  = array of dimension NIP containing the x coordinates
*         of the output points,
*   YI  = array of dimension NIP containing the y coordinates
*         of the output points.
*
* The output arguments are
*   ZI  = array of dimension NIP, where interpolated z values
*         are to be stored,
*   IER = error flag
*       = 0 for no errors
*       = 1 for NDP = 9 or less
*       = 2 for NDP not equal to NDPPV
*       = 3 for NIP = 0 or less
*       = 9 for errors in SDTRAN called by this subroutine.
*
* The other arguments are
*   WK  = two-dimensional array of dimension NDP*17 used
*         internally as a work area,
*   IWK = two-dimensional integer array of dimension NDP*25
*         used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* The very first call to this subroutine and the call with a new
* NDP value or new XD and YD arrays must be made with MD=1.  The
* call with MD=2 must be preceded by another call with the same
* NDP value and same XD and YD arrays.  The call with MD=3 must
* be preceded by another call with the same NDP value and same
* XD, YD, and ZD arrays.  Between the call with MD=2 and its
* preceding call, the IWK array must not be disturbed.  Between
* the call with MD=3 and its preceding call, the WK and IWK
* arrays must not be disturbed.
*
* The user of this subroutine can save the storage, by NDP*6
* numerical storage units, by placing the statement
*     EQUIVALENCE (WK(1,1),IWK(1,20))
* in the program that calls this subroutine.
*
* The constant in the PARAMETER statement below is
*   NIPIMX = maximum number of output points to be processed
*            at a time.
* The constant value has been selected empirically.
*
* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NIPIMX
      PARAMETER        (NIPIMX=51)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IER,MD,NDP,NIP,NEAR(NDP),NEXT(NDP)

*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             WK(NDP,17),XD(NDP),XI(NIP),YD(NDP),
     +                 YI(NIP),ZD(NDP),ZI(NIP),DIST(NDP)
      INTEGER          IWK(NDP,25)
      LOGICAL          EXTRPI(NIP)
*     ..
*     .. Local Scalars ..
      INTEGER          IERT,IIP,NDPPV,NIPI,NL,NT
*     ..
*     .. Local Arrays ..
      INTEGER          ITLI(NIPIMX),KTLI(NIPIMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLCTN,SDPD3P,SDPLNL,SDTRAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
*     .. Save statement ..
      SAVE             NDPPV,NT,NL
*     ..
* Error check
      IF (NDP.LE.9) GO TO 20
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          NDPPV = NDP
      ELSE
          IF (NDP.NE.NDPPV) GO TO 30
      END IF
      IF (NIP.LE.0) GO TO 40
* Triangulates the x-y plane.  (for MD=1)
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          CALL SDTRAN(NDP,XD,YD, NT,IWK(1,1),NL,IWK(1,7),IERT,
     +                IWK(1,1),IWK(1,7),IWK(1,13),IWK(1,14),IWK(1,9),
     +                NEAR,NEXT,DIST)
*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT,
*    +                LIST,LPTR,LEND,LTRI,ITL)
          IF (IERT.GT.0) GO TO 50
      END IF
* Estimates partial derivatives at all data points.  (for MD=1,2)
      IF (MD.NE.3) THEN
          CALL SDPD3P(NDP,XD,YD,ZD, WK(1,1), WK(1,6),WK(1,15),WK(1,17),
     +                IWK(1,9),IWK(1,10),IWK(1,19))
*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
      END IF
* Locates all points at which interpolation is to be performed
* and interpolates the ZI values.  (for MD=1,2,3)
      DO 10 IIP = 1,NIP,NIPIMX
          NIPI = MIN(NIP-IIP+1,NIPIMX)
          CALL SDLCTN(NDP,XD,YD,NT,IWK(1,1),NL,IWK(1,7),NIPI,XI(IIP),
     +                YI(IIP), KTLI,ITLI)
*         CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
          CALL SDPLNL(NDP,XD,YD,ZD,NT,IWK(1,1),NL,IWK(1,7),WK(1,1),NIPI,
     +                XI(IIP),YI(IIP),KTLI,ITLI, ZI(IIP), EXTRPI(IIP))
*         CALL SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,
*    +                NIP,XI,YI,KTLI,ITLI, ZI)
   10 CONTINUE
* Normal return
      IER = 0
      RETURN
* Error exit
   20 WRITE (*,FMT=9000) MD,NDP
      IER = 1
      RETURN
   30 WRITE (*,FMT=9010) MD,NDP,NDPPV
      IER = 2
      RETURN
   40 WRITE (*,FMT=9020) MD,NDP,NIP
      IER = 3
      RETURN
   50 WRITE (*,FMT=9030)
      IER = 9
      RETURN
* Format statement for error message
 9000 FORMAT (' ',/,'*** SDBI3P Error 1: NDP = 9 or less',/,'    MD =',
     +       I5,',  NDP =',I5,/)
 9010 FORMAT (' ',/,'*** SDBI3P Error 2: NDP not equal to NDPPV',/,
     +       '    MD =',I5,',  NDP =',I5,',  NDPPV =',I5,/)
 9020 FORMAT (' ',/,'*** SDBI3P Error 3: NIP = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,',  NIP =',I5,/)
 9030 FORMAT ('    Error detected in SDTRAN called by SDBI3P',/)
      END


      SUBROUTINE SDSF3P(MD,NDP,XD,YD,ZD,NXI,XI,NYI,YI, ZI,IER, WK,IWK,
     +                  EXTRPI,NEAR,NEXT,DIST)
*
* Scattered-data smooth surface fitting
* (a master subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs smooth surface fitting when the data
* points are scattered in the x-y plane.  It is based on the
* revised Akima method that has the accuracy of a cubic (third-
* degree) polynomial.
*
* The input arguments are
*   MD  = mode of computation
*       = 1 for new XD-YD (default)
*       = 2 for old XD-YD, new ZD
*       = 3 for old XD-YD, old ZD,
*   NDP = number of data points (must be 10 or greater),
*   XD  = array of dimension NDP containing the x coordinates
*         of the data points,
*   YD  = array of dimension NDP containing the y coordinates
*         of the data points,
*   ZD  = array of dimension NDP containing the z values at
*         the data points,
*   NXI = number of output grid points in the x coordinate
*         (must be 1 or greater),
*   XI  = array of dimension NXI containing the x coordinates
*         of the output grid points,
*   NYI = number of output grid points in the y coordinate
*         (must be 1 or greater),
*   YI  = array of dimension NYI containing the y coordinates
*         of the output grid points.
*
* The output arguments are
*   ZI  = two-dimensional array of dimension NXI*NYI, where
*         the interpolated z values at the output grid points
*         are to be stored,
*   IER = error flag
*       = 0 for no errors
*       = 1 for NDP = 9 or less
*       = 2 for NDP not equal to NDPPV
*       = 3 for NXI = 0 or less
*       = 4 for NYI = 0 or less
*       = 9 for errors in SDTRAN called by this subroutine.
*
* The other arguments are
*   WK  = two-dimensional array of dimension NDP*36 used
*         internally as a work area,
*   IWK = two-dimensional integer array of dimension NDP*25
*         used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* The very first call to this subroutine and the call with a new
* NDP value or new XD and YD arrays must be made with MD=1.  The
* call with MD=2 must be preceded by another call with the same
* NDP value and same XD and YD arrays.  The call with MD=3 must
* be preceded by another call with the same NDP value and same
* XD, YD, and ZD arrays.  Between the call with MD=2 and its
* preceding call, the IWK array must not be disturbed.  Between
* the call with MD=3 and its preceding call, the WK and IWK
* arrays must not be disturbed.
*
* The user of this subroutine can save the storage, by NDP*6
* numeric storage units, by placing the statement
*     EQUIVALENCE (WK(1,1),IWK(1,20))
* in the program that calls this subroutine.
*
* The constant in the PARAMETER statement below is
*   NIPIMX = maximum number of output points to be processed
*            at a time.
* The constant value has been selected empirically.
*
* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NIPIMX
      PARAMETER        (NIPIMX=51)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IER,MD,NDP,NXI,NYI,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             WK(NDP,17),XD(NDP),XI(NXI),YD(NDP),
     +                 YI(NYI),ZD(NDP),ZI(NXI,NYI),DIST(NDP)
      INTEGER          IWK(NDP,25)
      LOGICAL          EXTRPI(NXI,NYI)
*     ..
*     .. Local Scalars ..
      INTEGER          IERT,IIP,IXI,IYI,NDPPV,NIPI,NL,NT
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             YII(NIPIMX)
      INTEGER          ITLI(NIPIMX),KTLI(NIPIMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLCTN,SDPD3P,SDPLNL,SDTRAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
*     .. Save statement ..
      SAVE             NDPPV,NT,NL
*     ..
* Error check
      IF (NDP.LE.9) GO TO 40
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          NDPPV = NDP
      ELSE
          IF (NDP.NE.NDPPV) GO TO 50
      END IF
      IF (NXI.LE.0) GO TO 60
      IF (NYI.LE.0) GO TO 70
* Triangulates the x-y plane.  (for MD=1)
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          CALL SDTRAN(NDP,XD,YD, NT,IWK(1,1),NL,IWK(1,7),IERT,
     +                IWK(1,1),IWK(1,7),IWK(1,13),IWK(1,14),IWK(1,9),
     +                NEAR,NEXT,DIST)
*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT,
*    +                LIST,LPTR,LEND,LTRI,ITL)
          IF (IERT.GT.0) GO TO 80
      END IF
* Estimates partial derivatives at all data points.  (for MD=1,2)
      IF (MD.NE.3) THEN
          CALL SDPD3P(NDP,XD,YD,ZD, WK(1,1), WK(1,6),WK(1,15),WK(1,17),
     +                IWK(1,9),IWK(1,10),IWK(1,19))
*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
      END IF
* Locates all grid points at which interpolation is to be
* performed and interpolates the ZI values.  (for MD=1,2,3)
      DO 30 IYI = 1,NYI
          DO 10 IIP = 1,NIPIMX
              YII(IIP) = YI(IYI)
   10     CONTINUE
          DO 20 IXI = 1,NXI,NIPIMX
              NIPI = MIN(NXI-IXI+1,NIPIMX)
              CALL SDLCTN(NDP,XD,YD,NT,IWK(1,1),NL,IWK(1,7),NIPI,
     +                    XI(IXI),YII, KTLI,ITLI)
*             CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
              CALL SDPLNL(NDP,XD,YD,ZD,NT,IWK(1,1),NL,IWK(1,7),WK(1,1),
     +                    NIPI,XI(IXI),YII,KTLI,ITLI, ZI(IXI,IYI),
     +                    EXTRPI(IXI,IYI))
*             CALL SDPLNL(NDP,XD,YD,ZD,NT,ITP,NL,IPL,PDD,
*    +                    NIP,XI,YI,KTLI,ITLI, ZI)
   20     CONTINUE
   30 CONTINUE
* Normal return
      IER = 0
      RETURN
* Error exit
   40 WRITE (*,FMT=9000) MD,NDP
      IER = 1
      RETURN
   50 WRITE (*,FMT=9010) MD,NDP,NDPPV
      IER = 2
      RETURN
   60 WRITE (*,FMT=9020) MD,NDP,NXI,NYI
      IER = 3
      RETURN
   70 WRITE (*,FMT=9030) MD,NDP,NXI,NYI
      IER = 4
      RETURN
   80 WRITE (*,FMT=9040)
      IER = 9
      RETURN
* Format statement for error message
 9000 FORMAT (' ',/,'*** SDSF3P Error 1: NDP = 9 or less',/,'    MD =',
     +       I5,',  NDP =',I5,/)
 9010 FORMAT (' ',/,'*** SDSF3P Error 2: NDP not equal to NDPPV',/,
     +       '    MD =',I5,',  NDP =',I5,'  NDPPV =',I5,/)
 9020 FORMAT (' ',/,'*** SDSF3P Error 3: NXI = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,'  NXI =',I5,',  NYI =',I5,/)
 9030 FORMAT (' ',/,'*** SDSF3P Error 4: NYI = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,'  NXI =',I5,',  NYI =',I5,/)
 9040 FORMAT ('    Error detected in SDTRAN called by SDSF3P',/)
      END


      SUBROUTINE SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT, LIST,LPTR,LEND,
     +                  LTRI,ITL,NEAR,NEXT,DIST)
*
* Triangulation of the data area in a plane with a scattered data
* point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area in the x-y plane with
* a scattered data point set.  It divides the data area into a
* number of triangles and determines line segments that form the
* border of the data area.
*
* This subroutine consists of the following two steps, i.e.,
* (1) basic triangulation in the convex hull of the data points,
* and (2) removal of thin triangles along the border line of the
* data area.  It calls the SDTRCH and SDTRTT subroutines, that
* correspond to Steps (1) and (2), respectively.
*
* The input arguments are
*   NDP  = number of data points (must be greater than 3),
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*
* The output arguments are
*   NT   = number of triangles (its maximum is 2*NDP-5),
*   IPT  = two-dimensional integer array of dimension
*          (3,NT), where the point numbers of the vertexes
*          of the ITth triangle are to be stored counter-
*          clockwise in the ITth column, where IT = 1, 2,
*          ..., NT,
*   NL   = number of border line segments (its maximum is
*          NDP),
*   IPL  = two-dimensional integer array of dimension
*          (2,NL), where the point numbers of the end
*          points of the (IL)th border line segment are to
*          be stored counterclockwise in the ILth column,
*          where IL = 1, 2, ..., NL, with the line segments
*          stored counterclockwise,
*   IERT = error flag
*        = 0 for no errors
*        = 1 for NDP = 3 or less
*        = 2 for identical data points
*        = 3 for all collinear data points.
*
* The other arguments are
*   LIST = integer array of dimension 6*NDP USED internally
*          as a work area,
*   LPTR = integer array of dimension 6*NDP USED internally
*          as a work area,
*   LEND = integer array of dimension NDP USED internally as
*          a work area,
*   LTRI = two-dimensional integer array of dimension 12*NDP
*          used internally as a work area.
*   ITL  = integer array of dimension NDP used internally as
*          a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          IERT,NDP,NL,NT,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP),DIST(NDP)
      INTEGER          IPL(2,*),IPT(3,*),ITL(NDP),LEND(NDP),LIST(6,NDP),
     +                 LPTR(6,NDP),LTRI(12,NDP)
*     ..
*     .. Local Scalars ..
      INTEGER          IERTL,IERTM,IP1
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDTRCH,SDTRTT
*     ..
* Basic triangulation
      CALL SDTRCH(NDP,XD,YD, NT,IPT,NL,IPL,IERTM,IERTL, LIST,LPTR,LEND,
     +            LTRI,NEAR,NEXT,DIST)
      IF (IERTM.NE.0) GO TO 10
      IF (IERTL.NE.0) GO TO 20
      IERT = 0
* Removal of thin triangles that share border line segments
      CALL SDTRTT(NDP,XD,YD, NT,IPT,NL,IPL, ITL)
      RETURN
* Error exit
   10 IF (IERTM.EQ.-1) THEN
          IERT = 1
          WRITE (*,FMT=9000) NDP
      ELSE IF (IERTM.EQ.-2) THEN
          IERT = 2
          WRITE (*,FMT=9010)
      ELSE
          IERT = 3
          IP1 = IERTM
          WRITE (*,FMT=9020) NDP,IP1,XD(IP1),YD(IP1)
      END IF
      RETURN
   20 IF (IERTL.EQ.1) THEN
          IERT = 4
          WRITE (*,FMT=9030) NDP
      ELSE IF (IERTL.EQ.2) THEN
          IERT = 5
          WRITE (*,FMT=9040)
      END IF
      RETURN
* Format statements
 9000 FORMAT (' ',/,'*** SDTRAN Error 1: NDP = 3 or less',/,'    NDP =',
     +       I5)
 9010 FORMAT (' ',/,'*** SDTRAN Error 2: ',
     +       'The first three data points are collinear.',/)
 9020 FORMAT (' ',/,'*** SDTRAN Error 3: Identical data points',/,
     +       '    NDP =',I5,',  IP1 =',I5,',  XD =',E11.3,',  YD =',
     +       E11.3)
 9030 FORMAT (' ',/,'*** SDTRAN Error 4: NDP outside its valid',
     +       ' range',/,'    NDP =',I5)
 9040 FORMAT (' ',/,'*** SDTRAN Error 5: ',
     +       'Invalid data structure (LIST,LPTR,LEND)',/)
      END


      SUBROUTINE SDTRCH(NDP,XD,YD, NT,IPT,NL,IPL,IERTM,IERTL,
     +                  LIST,LPTR,LEND,LTRI,NEAR,NEXT,DIST)
*
* Basic triangulation in the convex hull of a scattered data point
* set in a plane
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area that is a convex hull
* of the scattered data points in the x-y plane.  It divides the
* data area into a number of triangles and determines line segments
* that form the border of the data area.
*
* This subroutine depends on the TRIPACK package of ACM Algorithm
* 751 by R. J. Renka.  It calls the TRMESH and TRLIST subroutines
* included in the package.  The TRMESH subroutine in turn calls
* either directly or indirectly 12 other subprograms included in
* the package.
*
* The input arguments are
*   NDP   = number of data points (must be greater than 3),
*   XD    = array of dimension NDP containing the x
*           coordinates of the data points,
*   YD    = array of dimension NDP containing the y
*           coordinates of the data points.
*
* The output arguments are
*   NT    = number of triangles (its maximum is 2*NDP-5),
*   IPT   = two-dimensional integer array of dimension
*           (3,NT), where the point numbers of the vertexes
*           of the ITth triangle are to be stored counter-
*           clockwise in the ITth column, where IT = 1, 2,
*           ..., NT,
*   NL    = number of border line segments (its maximum is
*           NDP),
*   IPL   = two-dimensional integer array of dimension
*           (2,NL), where the point numbers of the end
*           points of the (IL)th border line segment are to
*           be stored counterclockwise in the ILth column,
*           where IL = 1, 2, ..., NL, with the line segments
*           stored counterclockwise,
*   IERTM = error flag from the TRMESH subroutine,
*         =  0 for no errors
*         = -1 for NDP = 3 or less
*         = -2 for the first three collinear data points,
*         =  L for the Lth data point identical to some
*            Mth data point, M > L.
*   IERTL = error flag from the TRLIST subroutine,
*         = 0 for no errors
*         = 1 for invalid NCC, NDP, or NROW value.
*         = 2 for invalid data structure (LIST,LPTR,LEND).
*
* The other arguments are
*   LIST  = integer array of dimension 6*NDP USED internally
*           as a work area,
*   LPTR  = integer array of dimension 6*NDP USED internally
*           as a work area,
*   LEND  = integer array of dimension NDP USED internally as
*           a work area,
*   LTRI  = two-dimensional integer array of dimension 12*NDP
*           used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* Specification statements
*     .. Parameters ..
      INTEGER          NCC,NROW
      PARAMETER        (NCC=0,NROW=6)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IERTL,IERTM,NDP,NL,NT,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP),DIST(NDP)
      INTEGER          IPL(2,*),IPT(3,*),LEND(NDP),LIST(*),LPTR(*),
     +                 LTRI(NROW,*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,I1,I2,IL,IL1,IL2,IPL11,IPL21,J,LNEW
*     ..
*     .. Local Arrays ..
      INTEGER          LCC(1),LCT(1)
*     ..
*     .. External Subroutines ..
      EXTERNAL         TRLIST,TRMESH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
* Performs basic triangulation.
      CALL TRMESH(NDP,XD,YD, LIST,LPTR,LEND,LNEW,NEAR,NEXT,DIST,IERTM)
      IF (IERTM.NE.0) RETURN
      CALL TRLIST(NCC,LCC,NDP,LIST,LPTR,LEND,NROW, NT,LTRI,LCT,IERTL)
      IF (IERTL.NE.0) RETURN
* Extracts the triangle data from the LTRI array and set the IPT
* array.
      DO 20 J = 1,NT
          DO 10 I = 1,3
              IPT(I,J) = LTRI(I,J)
   10     CONTINUE
   20 CONTINUE
* Extracts the border-line-segment data from the LTRI array and
* set the IPL array.
      IL = 0
      DO 50 J = 1,NT
          DO 30 I = 1,3
              IF (LTRI(I+3,J).LE.0) GO TO 40
   30     CONTINUE
          GO TO 50
   40     IL = IL + 1
          I1 = MOD(I,3) + 1
          I2 = MOD(I+1,3) + 1
          IPL(1,IL) = LTRI(I1,J)
          IPL(2,IL) = LTRI(I2,J)
   50 CONTINUE
      NL = IL
* Sorts the IPL array.
      DO 80 IL1 = 1,NL - 1
          DO 60 IL2 = IL1 + 1,NL
              IF (IPL(1,IL2).EQ.IPL(2,IL1)) GO TO 70
   60     CONTINUE
   70     IPL11 = IPL(1,IL1+1)
          IPL21 = IPL(2,IL1+1)
          IPL(1,IL1+1) = IPL(1,IL2)
          IPL(2,IL1+1) = IPL(2,IL2)
          IPL(1,IL2) = IPL11
          IPL(2,IL2) = IPL21
   80 CONTINUE
      RETURN
      END


      SUBROUTINE SDTRTT(NDP,XD,YD, NT,IPT,NL,IPL, ITL)
*
* Removal of thin triangles along the border line of triangulation
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine removes thin triangles along the border line of
* triangulation.
*
* The input arguments are
*   NDP = number of data points (must be greater than 3),
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points.
*
* The input and output arguments are
*   NT  = number of triangles (its maximum is 2*NDP-5),
*   IPT = two-dimensional integer array of dimension
*         (3,NT), where the point numbers of the vertexes
*         of the ITth triangle are to be stored counter-
*         clockwise in the ITth column, where IT = 1, 2,
*         ..., NT,
*   NL  = number of border line segments (its maximum is
*         NDP),
*   IPL = two-dimensional integer array of dimension
*         (2,NL), where the point numbers of the end
*         points of the (IL)th border line segment are to
*         be stored counterclockwise in the ILth column,
*         where IL = 1, 2, ..., NL, with the line segments
*         stored counterclockwise.
*
* The other argument is
*   ITL = integer array of dimension NDP used internally as
*         a work area.
*
* The constants in the PARAMETER statement below are
*   HBRMN = minimum value of the height-to-bottom ratio of a
*           triangle along the border line of the data area,
*   NRRTT = number of repetitions in thin triangle removal.
* The constant values have been selected empirically.
*
* Specification statements
*     .. Parameters ..
      DOUBLE PRECISION             HBRMN
      INTEGER          NRRTT
      PARAMETER        (HBRMN=0.10,NRRTT=5)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP)
      INTEGER          IPL(2,*),IPT(3,*),ITL(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             HBR,U1,U2,U3,V1,V2,V3
      INTEGER          IL,IL0,IL00,IL1,ILP1,ILR1,IP1,IP2,IP3,IPL1,IPL2,
     +                 IREP,IT,IT0,ITP1,IV,IVP1,MODIF,NL0
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION             DSQF,VPDT
*     ..
* Statement Function definitions 
      DSQF(U1,V1,U2,V2) = (U2-U1)**2 + (V2-V1)**2
      VPDT(U1,V1,U2,V2,U3,V3) = (V3-V1)* (U2-U1) - (U3-U1)* (V2-V1)
*     ..
* Triangle numbers of triangles that share line segments with the
* border line.
      DO 20 IL = 1,NL
          IPL1 = IPL(1,IL)
          IPL2 = IPL(2,IL)
          DO 10 IT = 1,NT
              IF (IPL1.EQ.IPT(1,IT) .OR. IPL1.EQ.IPT(2,IT) .OR.
     +            IPL1.EQ.IPT(3,IT)) THEN
                  IF (IPL2.EQ.IPT(1,IT) .OR. IPL2.EQ.IPT(2,IT) .OR.
     +                IPL2.EQ.IPT(3,IT)) THEN
                      ITL(IL) = IT
                      GO TO 20
                  END IF
              END IF
   10     CONTINUE
   20 CONTINUE
* Removes thin triangles that share line segments with the border
* line.
      DO 130 IREP = 1,NRRTT
          MODIF = 0
          NL0 = NL
          IL = 0
          DO 120 IL0 = 1,NL0
              IL = IL + 1
              IP1 = IPL(1,IL)
              IP2 = IPL(2,IL)
              IT = ITL(IL)
* Calculates the height-to-bottom ratio of the triangle.
              IF (IPT(1,IT).NE.IP1 .AND. IPT(1,IT).NE.IP2) THEN
                  IP3 = IPT(1,IT)
              ELSE IF (IPT(2,IT).NE.IP1 .AND. IPT(2,IT).NE.IP2) THEN
                  IP3 = IPT(2,IT)
              ELSE
                  IP3 = IPT(3,IT)
              END IF
              HBR = VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),
     +              YD(IP3))/DSQF(XD(IP1),YD(IP1),XD(IP2),YD(IP2))
              IF (HBR.LT.HBRMN) THEN
                  MODIF = 1
* Removes this triangle when applicable.
                  ITP1 = IT + 1
                  DO 30 IT0 = ITP1,NT
                      IPT(1,IT0-1) = IPT(1,IT0)
                      IPT(2,IT0-1) = IPT(2,IT0)
                      IPT(3,IT0-1) = IPT(3,IT0)
   30             CONTINUE
                  NT = NT - 1
                  DO 40 IL00 = 1,NL
                      IF (ITL(IL00).GT.IT) ITL(IL00) = ITL(IL00) - 1
   40             CONTINUE
* Replaces the border line segment with two new line segments.
                  IF (IL.LT.NL) THEN
                      ILP1 = IL + 1
                      DO 50 ILR1 = ILP1,NL
                          IL1 = NL + ILP1 - ILR1
                          IPL(1,IL1+1) = IPL(1,IL1)
                          IPL(2,IL1+1) = IPL(2,IL1)
                          ITL(IL1+1) = ITL(IL1)
   50                 CONTINUE
                  END IF
* - Adds the first new line segment.
                  IPL(1,IL) = IP1
                  IPL(2,IL) = IP3
                  DO 70 IT0 = 1,NT
                      DO 60 IV = 1,3
                          IF (IPT(IV,IT0).EQ.IP1 .OR.
     +                        IPT(IV,IT0).EQ.IP3) THEN
                              IVP1 = MOD(IV,3) + 1
                              IF (IPT(IVP1,IT0).EQ.IP1 .OR.
     +                            IPT(IVP1,IT0).EQ.IP3) GO TO 80
                          END IF
   60                 CONTINUE
   70             CONTINUE
   80             ITL(IL) = IT0
* - Adds the second new line segment.
                  IL = IL + 1
                  IPL(1,IL) = IP3
                  IPL(2,IL) = IP2
                  DO 100 IT0 = 1,NT
                      DO 90 IV = 1,3
                          IF (IPT(IV,IT0).EQ.IP3 .OR.
     +                        IPT(IV,IT0).EQ.IP2) THEN
                              IVP1 = MOD(IV,3) + 1
                              IF (IPT(IVP1,IT0).EQ.IP3 .OR.
     +                            IPT(IVP1,IT0).EQ.IP2) GO TO 110
                          END IF
   90                 CONTINUE
  100             CONTINUE
  110             ITL(IL) = IT0
                  NL = NL + 1
              END IF
  120     CONTINUE
          IF (MODIF.EQ.0) RETURN
  130 CONTINUE
      RETURN
      END


      SUBROUTINE SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
*
* Partial derivatives for bivariate interpolation and surface
* fitting for scattered data
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine estimates partial derivatives of the first and
* second orders at the data points for bivariate interpolation
* and surface fitting for scattered data.  In most cases, this
* subroutine has the accuracy of a cubic (third-degree)
* polynomial.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z values
*          at the data points.
*
* The output argument is
*   PDD  = two-dimensional array of dimension 5*NDP, where
*          the estimated zx, zy, zxx, zxy, and zyy values
*          at the IDPth data point are to be stored in the
*          IDPth row, where IDP = 1, 2, ..., NDP.
*
* The other arguments are
*   CF3  = two-dimensional array of dimension 9*NDP used
*          internally as a work area,
*   CFL1 = two-dimensional array of dimension 2*NDP used
*          internally as a work area,
*   DSQ  = array of dimension NDP used internally as a work
*          area,
*   IDSQ = integer array of dimension NDP used internally
*          as a work area,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          used internally as a work area,
*   NCP  = integer array of dimension NDP used internally
*          as a work area.
*
* The constant in the first PARAMETER statement below is
*   NPEMX = maximum number of primary estimates.
* The constant value has been selected empirically.
*
* The constants in the second PARAMETER statement below are
*   NPEAMN = minimum number of primary estimates,
*   NPEAMX = maximum number of primary estimates when
*            additional primary estimates are added.
* The constant values have been selected empirically.
*
* This subroutine calls the SDCLDP, SDCF3P, and SDLS1P
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NPEMX
      PARAMETER        (NPEMX=25)
      INTEGER          NPEAMN,NPEAMX
      PARAMETER        (NPEAMN=3,NPEAMX=6)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CF3(9,NDP),CFL1(2,NDP),DSQ(NDP),
     +                 PDD(5,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IDSQ(NDP),IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A01,A02,A03,A10,A11,A12,A20,A21,A30,
     +                 ALPWT,ANPE,ANPEM1,SMWTF,SMWTI,WTF,WTI,X,Y,ZX,ZY
      INTEGER          IDP1,IDP2,IDPI,IDPPE1,IMN,IPE,IPE1,J,J1,J2,JJ,
     +                 JMN,K,NCP2,NCP2P1,NPE
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             AMPDPE(5),PDDIF(5),PDDII(5),
     +                 PDPE(5,NPEMX),PWT(NPEMX),RVWT(NPEMX),SSPDPE(5)
      INTEGER          IDPPE(NPEMX),IPCPE(10,NPEMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDCF3P,SDCLDP,SDLS1P
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        EXP,DBLE
*     ..
* Calculation
* Selects, at each of the data points, nine data points closest
* to the data point in question.
      CALL SDCLDP(NDP,XD,YD, IPC, DSQ,IDSQ)
* Fits, at each of the data points, a cubic (third-degree)
* polynomial to z values at the 10 data points that consist of
* the data point in question and 9 data points closest to it.
      CALL SDCF3P(NDP,XD,YD,ZD,IPC, CF3,NCP)
* Performs, at each of the data points, the least-squares fit of
* a plane to z values at the 10 data points.
      CALL SDLS1P(NDP,XD,YD,ZD,IPC,NCP, CFL1)
* Outermost DO-loop with respect to the data point
      DO 310 IDP1 = 1,NDP
* Selects data point sets for sets of primary estimates of partial
* derivatives.
* - Selects a candidate.
          NPE = 0
          DO 80 IDP2 = 1,NDP
              NCP2 = NCP(IDP2)
              NCP2P1 = NCP2 + 1
              IF (IDP2.EQ.IDP1) GO TO 20
              DO 10 J = 1,NCP2
                  IF (IPC(J,IDP2).EQ.IDP1) GO TO 20
   10         CONTINUE
              GO TO 80
   20         IPCPE(1,NPE+1) = IDP2
              DO 30 J = 1,NCP2
                  IPCPE(J+1,NPE+1) = IPC(J,IDP2)
   30         CONTINUE
              DO 50 J1 = 1,NCP2
                  JMN = J1
                  IMN = IPCPE(JMN,NPE+1)
                  DO 40 J2 = J1,NCP2P1
                      IF (IPCPE(J2,NPE+1).LT.IMN) THEN
                          JMN = J2
                          IMN = IPCPE(JMN,NPE+1)
                      END IF
   40             CONTINUE
                  IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
                  IPCPE(J1,NPE+1) = IMN
   50         CONTINUE
* - Checks whether or not the candidate has already been included.
              IF (NPE.GT.0) THEN
                  DO 70 IPE1 = 1,NPE
                      IDPPE1 = IDPPE(IPE1)
                      IF (NCP2.NE.NCP(IDPPE1)) GO TO 70
                      DO 60 J = 1,NCP2P1
                          IF (IPCPE(J,NPE+1).NE.
     +                        IPCPE(J,IPE1)) GO TO 70
   60                 CONTINUE
                      GO TO 80
   70             CONTINUE
              END IF
              NPE = NPE + 1
              IDPPE(NPE) = IDP2
              IF (NPE.GE.NPEMX) GO TO 90
   80     CONTINUE
   90     CONTINUE
* Adds additional closest data points when necessary.
          IF (NPE.LT.NPEAMN) THEN
              DO 150 JJ = 1,9
                  IDP2 = IPC(JJ,IDP1)
                  NCP2 = NCP(IDP2)
                  NCP2P1 = NCP2 + 1
                  IPCPE(1,NPE+1) = IDP2
                  DO 100 J = 1,NCP2
                      IPCPE(J+1,NPE+1) = IPC(J,IDP2)
  100             CONTINUE
                  DO 120 J1 = 1,NCP2
                      JMN = J1
                      IMN = IPCPE(JMN,NPE+1)
                      DO 110 J2 = J1,NCP2P1
                          IF (IPCPE(J2,NPE+1).LT.IMN) THEN
                              JMN = J2
                              IMN = IPCPE(JMN,NPE+1)
                          END IF
  110                 CONTINUE
                      IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
                      IPCPE(J1,NPE+1) = IMN
  120             CONTINUE
                  IF (NPE.GT.0) THEN
                      DO 140 IPE1 = 1,NPE
                          IDPPE1 = IDPPE(IPE1)
                          IF (NCP2.NE.NCP(IDPPE1)) GO TO 140
                          DO 130 J = 1,NCP2P1
                              IF (IPCPE(J,NPE+1).NE.
     +                            IPCPE(J,IPE1)) GO TO 140
  130                     CONTINUE
                          GO TO 150
  140                 CONTINUE
                  END IF
                  NPE = NPE + 1
                  IDPPE(NPE) = IDP2
                  IF (NPE.GE.NPEAMX) GO TO 160
  150         CONTINUE
          END IF
  160     CONTINUE
* Calculates the primary estimates of partial derivatives.
          X = XD(IDP1)
          Y = YD(IDP1)
          DO 170 IPE = 1,NPE
              IDPI = IDPPE(IPE)
              A10 = CF3(1,IDPI)
              A20 = CF3(2,IDPI)
              A30 = CF3(3,IDPI)
              A01 = CF3(4,IDPI)
              A11 = CF3(5,IDPI)
              A21 = CF3(6,IDPI)
              A02 = CF3(7,IDPI)
              A12 = CF3(8,IDPI)
              A03 = CF3(9,IDPI)
              PDPE(1,IPE) = A10 + X* (2.0*A20+X*3.0*A30) +
     +                      Y* (A11+2.0*A21*X+A12*Y)
              PDPE(2,IPE) = A01 + Y* (2.0*A02+Y*3.0*A03) +
     +                      X* (A11+2.0*A12*Y+A21*X)
              PDPE(3,IPE) = 2.0*A20 + 6.0*A30*X + 2.0*A21*Y
              PDPE(4,IPE) = A11 + 2.0*A21*X + 2.0*A12*Y
              PDPE(5,IPE) = 2.0*A02 + 6.0*A03*Y + 2.0*A12*X
  170     CONTINUE
          IF (NPE.EQ.1) GO TO 290
* Weighted values of partial derivatives (through the statement
* labeled 280 + 1)
* Calculates the probability weight.
          ANPE = DBLE(NPE)
          ANPEM1 = DBLE(NPE-1)
          DO 190 K = 1,5
              AMPDPE(K) = 0.0
              SSPDPE(K) = 0.0
              DO 180 IPE = 1,NPE
                  AMPDPE(K) = AMPDPE(K) + PDPE(K,IPE)
                  SSPDPE(K) = SSPDPE(K) + PDPE(K,IPE)**2
  180         CONTINUE
              AMPDPE(K) = AMPDPE(K)/ANPE
              SSPDPE(K) = (SSPDPE(K)-ANPE*AMPDPE(K)**2)/ANPEM1
  190     CONTINUE
          DO 210 IPE = 1,NPE
              ALPWT = 0.0
              DO 200 K = 1,5
                  IF (SSPDPE(K).NE.0.0) ALPWT = ALPWT +
     +                ((PDPE(K,IPE)-AMPDPE(K))**2)/SSPDPE(K)
  200         CONTINUE
              PWT(IPE) = EXP(-ALPWT/2.0)
  210     CONTINUE
* Calculates the reciprocal of the volatility weight.
          DO 220 IPE = 1,NPE
              IDPI = IDPPE(IPE)
              ZX = CFL1(1,IDPI)
              ZY = CFL1(2,IDPI)
              RVWT(IPE) = ((PDPE(1,IPE)-ZX)**2+ (PDPE(2,IPE)-ZY)**2)*
     +                    (PDPE(3,IPE)**2+2.0*PDPE(4,IPE)**2+
     +                    PDPE(5,IPE)**2)
*             ZXX=0.0
*             ZXY=0.0
*             ZYY=0.0
*             RVWT(IPE)=((PDPE(1,IPE)-ZX)**2+(PDPE(2,IPE)-ZY)**2)
*    +                 *((PDPE(3,IPE)-ZXX)**2+2.0*(PDPE(4,IPE)-ZXY)**2
*    +                  +(PDPE(5,IPE)-ZYY)**2)
  220     CONTINUE
* Calculates the weighted values of partial derivatives.
          DO 230 K = 1,5
              PDDIF(K) = 0.0
              PDDII(K) = 0.0
  230     CONTINUE
          SMWTF = 0.0
          SMWTI = 0.0
          DO 260 IPE = 1,NPE
              IF (RVWT(IPE).GT.0.0) THEN
                  WTF = PWT(IPE)/RVWT(IPE)
                  DO 240 K = 1,5
                      PDDIF(K) = PDDIF(K) + PDPE(K,IPE)*WTF
  240             CONTINUE
                  SMWTF = SMWTF + WTF
              ELSE
                  WTI = PWT(IPE)
                  DO 250 K = 1,5
                      PDDII(K) = PDDII(K) + PDPE(K,IPE)*WTI
  250             CONTINUE
                  SMWTI = SMWTI + WTI
              END IF
  260     CONTINUE
          IF (SMWTI.LE.0.0) THEN
              DO 270 K = 1,5
                  PDD(K,IDP1) = PDDIF(K)/SMWTF
  270         CONTINUE
          ELSE
              DO 280 K = 1,5
                  PDD(K,IDP1) = PDDII(K)/SMWTI
  280         CONTINUE
          END IF
          GO TO 310
* Only one qualified point set
  290     DO 300 K = 1,5
              PDD(K,IDP1) = PDPE(K,1)
  300     CONTINUE
  310 CONTINUE
      RETURN
      END


      SUBROUTINE SDCLDP(NDP,XD,YD, IPC, DSQ,IDSQ)
*
* Closest data points
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine selects, at each of the data points, nine data
* points closest to it.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*
* The output argument is
*   IPC  = two-dimensional integer array of dimension 9*NDP,
*          where the point numbers of nine data points closest
*          to the IDPth data point, in an ascending order of
*          the distance from the IDPth point, are to be
*          stored in the IDPth column, where IDP = 1, 2,
*          ..., NDP.
*
* The other arguments are
*   DSQ  = array of dimension NDP used as a work area,
*   IDSQ = integer array of dimension NDP used as a work
*          area.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             DSQ(NDP),XD(NDP),YD(NDP)
      INTEGER          IDSQ(NDP),IPC(9,NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             DSQMN
      INTEGER          IDP,IDSQMN,JDP,JDPMN,JDSQMN,JIPC,JIPCMX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
* DO-loop with respect to the data point number
      DO 50 IDP = 1,NDP
* Calculates the distance squared for all data points from the
* IDPth data point and stores the data point number and the
* calculated results in the IDSQ and DSQ arrays, respectively.
          DO 10 JDP = 1,NDP
              IDSQ(JDP) = JDP
              DSQ(JDP) = (XD(JDP)-XD(IDP))**2 + (YD(JDP)-YD(IDP))**2
   10     CONTINUE
* Sorts the IDSQ and DSQ arrays in such a way that the IDPth
* point is in the first element in each array.
          IDSQ(IDP) = 1
          DSQ(IDP) = DSQ(1)
          IDSQ(1) = IDP
          DSQ(1) = 0.0
* Selects nine data points closest to the IDPth data point and
* stores the data point numbers in the IPC array.
          JIPCMX = MIN(NDP-1,10)
          DO 30 JIPC = 2,JIPCMX
              JDSQMN = JIPC
              DSQMN = DSQ(JIPC)
              JDPMN = JIPC + 1
              DO 20 JDP = JDPMN,NDP
                  IF (DSQ(JDP).LT.DSQMN) THEN
                      JDSQMN = JDP
                      DSQMN = DSQ(JDP)
                  END IF
   20         CONTINUE
              IDSQMN = IDSQ(JDSQMN)
              IDSQ(JDSQMN) = IDSQ(JIPC)
              DSQ(JDSQMN) = DSQ(JIPC)
              IDSQ(JIPC) = IDSQMN
   30     CONTINUE
          DO 40 JIPC = 1,9
              IPC(JIPC,IDP) = IDSQ(JIPC+1)
   40     CONTINUE
   50 CONTINUE
      RETURN
      END


      SUBROUTINE SDCF3P(NDP,XD,YD,ZD,IPC, CF,NCP)
*
* Coefficients of the third-degree polynomial for z(x,y)
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine calculates, for each data point, coefficients
* of the third-degree polynomial for z(x,y) fitted to the set of
* 10 data points consisting of the data point in question and
* nine data points closest to it.  When the condition number of
* the matrix associated with the 10 data point set is too large,
* this subroutine calculates coefficients of the second-degree
* polynomial fitted to the set of six data points consisting of
* the data point in question and five data points closest to it.
* When the condition number of the matrix associated with the six
* data point set is too large, this subroutine calculates
* coefficients of the first-degree polynomial fitted to the set of
* three data points closest to the data point in question.  When
* the condition number of the matrix associated with the three data
* point set is too large, this subroutine calculates coefficients
* of the first-degree polynomial fitted to the set of two data
* points consisting of the data point in question and one data
* point closest to it, assuming that the plane represented by the
* polynomial is horizontal in the direction which is at right
* angles to the line connecting the two data points.
*
* The input arguments are
*   NDP = number of data points,
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points,
*   ZD  = array of dimension NDP containing the z values
*         at the data points,
*   IPC = two-dimensional integer array of dimension
*         9*NDP containing the point numbers of 9 data
*         points closest to the IDPth data point in the
*         IDPth column, where IDP = 1, 2, ..., NDP.
*
* The output arguments are
*   CF  = two-dimensional array of dimension 9*NDP,
*         where the coefficients of the polynomial
*         (a10, a20, a30, a01, a11, a21, a02, a12, a03)
*         calculated at the IDPth data point are to be
*         stored in the IDPth column, where IDP = 1, 2,
*         ..., NDP,
*   NCP = integer array of dimension NDP, where the numbers
*         of the closest points used are to be stored.
*
* The constant in the first PARAMETER statement below is
*   CNRMX = maximum value of the ratio of the condition
*           number of the matrix associated with the point
*           set to the number of points.
* The constant value has been selected empirically.
*
* The N1, N2, and N3 constants in the second PARAMETER statement
* are the numbers of the data points used to determine the first-,
* second-, and third-degree polynomials, respectively.
*
* This subroutine calls the SDLEQN subroutine.
*
*
* Specification statements
*     .. Parameters ..
      DOUBLE PRECISION             CNRMX
      PARAMETER        (CNRMX=1.5E+04)
      INTEGER          N1,N2,N3
      PARAMETER        (N1=3,N2=6,N3=10)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CF(9,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             CN,DET,X,X1,X2,Y,Y1,Y2,Z1,Z2
      INTEGER          I,IDP,IDPI,J
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             AA1(N1,N1),AA2(N2,N2),AA3(N3,N3),
     +                 B(N3),CFI(N3),EE(N3,N3),ZZ(N3,N3)
      INTEGER          K(N3)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLEQN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     ..
* Main DO-loop with respect to the data point
      DO 60 IDP = 1,NDP
          DO 10 J = 1,9
              CF(J,IDP) = 0.0
   10     CONTINUE
* Calculates the coefficients of the set of linear equations
* with the 10-point data point set.
          DO 20 I = 1,N3
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              AA3(I,1) = 1.0
              AA3(I,2) = X
              AA3(I,3) = X*X
              AA3(I,4) = X*X*X
              AA3(I,5) = Y
              AA3(I,6) = X*Y
              AA3(I,7) = X*X*Y
              AA3(I,8) = Y*Y
              AA3(I,9) = X*Y*Y
              AA3(I,10) = Y*Y*Y
              B(I) = ZD(IDPI)
   20     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N3,AA3,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* third-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N3)) THEN
                  DO 30 J = 2,N3
                      CF(J-1,IDP) = CFI(J)
   30             CONTINUE
                  NCP(IDP) = N3 - 1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 6-point data point set.
          DO 40 I = 1,N2
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              AA2(I,1) = 1.0
              AA2(I,2) = X
              AA2(I,3) = X*X
              AA2(I,4) = Y
              AA2(I,5) = X*Y
              AA2(I,6) = Y*Y
              B(I) = ZD(IDPI)
   40     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N2,AA2,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* second-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N2)) THEN
                  CF(1,IDP) = CFI(2)
                  CF(2,IDP) = CFI(3)
                  CF(4,IDP) = CFI(4)
                  CF(5,IDP) = CFI(5)
                  CF(7,IDP) = CFI(6)
                  NCP(IDP) = N2 - 1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 3-point data point set.
          DO 50 I = 1,N1
              IDPI = IPC(I,IDP)
              X = XD(IDPI)
              Y = YD(IDPI)
              AA1(I,1) = 1.0
              AA1(I,2) = X
              AA1(I,3) = Y
              B(I) = ZD(IDPI)
   50     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N1,AA1,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* first-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N1)) THEN
                  CF(1,IDP) = CFI(2)
                  CF(4,IDP) = CFI(3)
                  NCP(IDP) = N1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 2-point data point set when applicable.
          IDPI = IDP
          X1 = XD(IDPI)
          Y1 = YD(IDPI)
          Z1 = ZD(IDPI)
          IDPI = IPC(1,IDP)
          X2 = XD(IDPI)
          Y2 = YD(IDPI)
          Z2 = ZD(IDPI)
          CF(1,IDP) = (X2-X1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          CF(4,IDP) = (Y2-Y1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          NCP(IDP) = 1
   60 CONTINUE
      RETURN
      END


      SUBROUTINE SDLEQN(N,AA,B, X,DET,CN, K,EE,ZZ)
*
* Solution of a set of linear equations
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine solves a set of linear equations.
*
* The input arguments are
*   N   = number of linear equations,
*   AA  = two-dimensional array of dimension N*N
*         containing the coefficients of the equations,
*   B   = array of dimension N containing the constant
*         values in the right-hand side of the equations.
*
* The output arguments are
*   X   = array of dimension N, where the solution is
*         to be stored,
*   DET = determinant of the AA array,
*   CN  = condition number of the AA matrix.
*
* The other arguments are
*   K   = integer array of dimension N used internally
*         as the work area,
*   EE  = two-dimensional array of dimension N*N used
*         internally as the work area,
*   ZZ  = two-dimensional array of dimension N*N used
*         internally as the work area.
*
*
* Specification statements
*     .. Scalar Arguments ..
      DOUBLE PRECISION             CN,DET
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             AA(N,N),B(N),EE(N,N),X(N),ZZ(N,N)
      INTEGER          K(N)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             AAIIJ,AAIJIJ,AAIJMX,AAMX,SA,SZ
      INTEGER          I,IJ,IJP1,IJR,J,JJ,JMX,KJMX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,SQRT
*     ..
* Calculation
* Initial setting
      DO 10 J = 1,N
          K(J) = J
   10 CONTINUE
      DO 30 I = 1,N
          DO 20 J = 1,N
              EE(I,J) = 0.0
   20     CONTINUE
          EE(I,I) = 1.0
   30 CONTINUE
* Calculation of inverse matrix of AA
      DO 110 IJ = 1,N
* Finds out the element having the maximum absolute value in the
* IJ th row.
          AAMX = ABS(AA(IJ,IJ))
          JMX = IJ
          DO 40 J = IJ,N
              IF (ABS(AA(IJ,J)).GT.AAMX) THEN
                  AAMX = ABS(AA(IJ,J))
                  JMX = J
              END IF
   40     CONTINUE
* Switches two columns in such a way that the element with the
* maximum value is on the diagonal.
          DO 50 I = 1,N
              AAIJMX = AA(I,IJ)
              AA(I,IJ) = AA(I,JMX)
              AA(I,JMX) = AAIJMX
   50     CONTINUE
          KJMX = K(IJ)
          K(IJ) = K(JMX)
          K(JMX) = KJMX
* Makes the diagonal element to be unity.
          AAIJIJ = AA(IJ,IJ)
          IF (AAIJIJ.EQ.0.0) GO TO 210
          DO 60 J = IJ,N
              AA(IJ,J) = AA(IJ,J)/AAIJIJ
   60     CONTINUE
          DO 70 JJ = 1,N
              EE(IJ,JJ) = EE(IJ,JJ)/AAIJIJ
   70     CONTINUE
* Eliminates the lower left elements.
          IF (IJ.LT.N) THEN
              IJP1 = IJ + 1
              DO 100 I = IJP1,N
                  AAIIJ = AA(I,IJ)
                  DO 80 J = IJP1,N
                      AA(I,J) = AA(I,J) - AA(IJ,J)*AAIIJ
   80             CONTINUE
                  DO 90 JJ = 1,N
                      EE(I,JJ) = EE(I,JJ) - EE(IJ,JJ)*AAIIJ
   90             CONTINUE
  100         CONTINUE
          END IF
* Calculates the determinant.
          IF (IJ.EQ.1) THEN
              DET = 1.0
          END IF
          DET = DET*AAIJIJ* ((-1)** (IJ+JMX))
  110 CONTINUE
* Calculates the elements of the inverse matrix.
      DO 140 IJR = 1,N
          IJ = N + 1 - IJR
          IF (IJ.LT.N) THEN
              IJP1 = IJ + 1
              DO 130 J = IJP1,N
                  DO 120 JJ = 1,N
                      EE(IJ,JJ) = EE(IJ,JJ) - AA(IJ,J)*EE(J,JJ)
  120             CONTINUE
  130         CONTINUE
          END IF
  140 CONTINUE
      DO 160 J = 1,N
          I = K(J)
          DO 150 JJ = 1,N
              ZZ(I,JJ) = EE(J,JJ)
  150     CONTINUE
  160 CONTINUE
* Calculation of the condition number of AA
      SA = 0.0
      SZ = 0.0
      DO 180 I = 1,N
          DO 170 J = 1,N
              SA = SA + AA(I,J)*AA(J,I)
              SZ = SZ + ZZ(I,J)*ZZ(J,I)
  170     CONTINUE
  180 CONTINUE
      CN = SQRT(ABS(SA*SZ))
* Calculation of X vector
      DO 200 I = 1,N
          X(I) = 0.0
          DO 190 J = 1,N
              X(I) = X(I) + ZZ(I,J)*B(J)
  190     CONTINUE
  200 CONTINUE
      RETURN
* Special case where the determinant is zero
  210 DO 220 I = 1,N
          X(I) = 0.0
  220 CONTINUE
      DET = 0.0
      RETURN
      END


      SUBROUTINE SDLS1P(NDP,XD,YD,ZD,IPC,NCP, CFL1)
*
* Least squares fit of a linear surface (plane) to z(x,y) values
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs the least squares fit of a linear
* surface (plane) to a data point set consisting of the data
* point in question and several data points closest to it used
* in the SDCF3P subroutine.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x coordinates
*          of the data points,
*   YD   = array of dimension NDP containing the y coordinates
*          of the data points,
*   ZD   = array of dimension NDP containing the z values at
*          the data points,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          containing, in the IDPth column, point numbers of
*          nine data points closest to the IDPth data point,
*          where IDP = 1, 2, ..., NDP,
*   NCP  = integer array of dimension NDP containing the
*          numbers of the closest points used in the SDCF3P
*          subroutine.
*
* The output argument is
*   CFL1 = two-dimensional array of dimension 2*NDP, where
*          the coefficients (a10, a01) of the least squares
*          fit, first-degree polynomial calculated at the
*          IDPth data point are to be stored in the IDPth
*          column, where IDP = 1, 2, ..., NDP.
*
* Before this subroutine is called, the SDCF3P subroutine must
* have been called.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CFL1(2,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A11,A12,A22,AN,B1,B2,DLT,SX,SXX,SXY,
     +                 SXZ,SY,SYY,SYZ,SZ,X,X1,X2,Y,Y1,Y2,Z,Z1,Z2
      INTEGER          I,IDP,IDPI,NPLS
*     ..
* DO-loop with respect to the data point
      DO 30 IDP = 1,NDP
          NPLS = NCP(IDP) + 1
          IF (NPLS.EQ.2) GO TO 20
* Performs the least squares fit of a plane.
          SX = 0.0
          SY = 0.0
          SXX = 0.0
          SXY = 0.0
          SYY = 0.0
          SZ = 0.0
          SXZ = 0.0
          SYZ = 0.0
          DO 10 I = 1,NPLS
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              Z = ZD(IDPI)
              SX = SX + X
              SY = SY + Y
              SXX = SXX + X*X
              SXY = SXY + X*Y
              SYY = SYY + Y*Y
              SZ = SZ + Z
              SXZ = SXZ + X*Z
              SYZ = SYZ + Y*Z
   10     CONTINUE
          AN = NPLS
          A11 = AN*SXX - SX*SX
          A12 = AN*SXY - SX*SY
          A22 = AN*SYY - SY*SY
          B1 = AN*SXZ - SX*SZ
          B2 = AN*SYZ - SY*SZ
          DLT = A11*A22 - A12*A12
          CFL1(1,IDP) = (B1*A22-B2*A12)/DLT
          CFL1(2,IDP) = (B2*A11-B1*A12)/DLT
          GO TO 30
   20     IDPI = IDP
          X1 = XD(IDPI)
          Y1 = YD(IDPI)
          Z1 = ZD(IDPI)
          IDPI = IPC(1,IDP)
          X2 = XD(IDPI)
          Y2 = YD(IDPI)
          Z2 = ZD(IDPI)
          CFL1(1,IDP) = (X2-X1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          CFL1(2,IDP) = (Y2-Y1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
   30 CONTINUE
      RETURN
      END


      SUBROUTINE SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
*
* Locating points in a scattered data point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine locates points in a scattered data point set in
* the x-y plane, i.e., determines to which triangle each of the
* points to be located belongs.  When a point to be located does
* not lie inside the data area, this subroutine determines the
* border line segment when the point lies in an outside rectangle,
* in an outside triangle, or in the overlap of two outside
* rectangles.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   NIP  = number of points to be located,
*   XI   = array of dimension NIP containing the x
*          coordinates of the points to be located,
*   YI   = array of dimension NIP containing the y
*          coordinates of the points to be located.
*
* The output arguments are
*   KTLI = integer array of dimension NIP, where the code
*          for the type of the piece of plane in which each
*          interpolated point lies is to be stored
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border line
*            segments
*        = 4 for a triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP, where the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the points to
*          be located are to be stored.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP,NIP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),XI(NIP),YD(NDP),YI(NIP)
      INTEGER          IPL(2,NL),IPT(3,NT),ITLI(NIP),KTLI(NIP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             U1,U2,U3,V1,V2,V3,X0,X1,X2,X3,Y0,Y1,
     +                             Y2,Y3
      INTEGER          IIP,IL1,IL2,ILII,IP1,IP2,IP3,ITII,ITLIPV,KTLIPV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION             SPDT,VPDT
*     ..
* Statement Function definitions 
      SPDT(U1,V1,U2,V2,U3,V3) = (U1-U3)* (U2-U3) + (V1-V3)* (V2-V3)
      VPDT(U1,V1,U2,V2,U3,V3) = (U1-U3)* (V2-V3) - (V1-V3)* (U2-U3)
*     ..
* Outermost DO-loop with respect to the points to be located
      DO 40 IIP = 1,NIP
          X0 = XI(IIP)
          Y0 = YI(IIP)
          IF (IIP.EQ.1) THEN
              KTLIPV = 0
              ITLIPV = 0
          ELSE
              KTLIPV = KTLI(IIP-1)
              ITLIPV = ITLI(IIP-1)
          END IF
* Checks if in the same inside triangle as previous.
          IF (KTLIPV.EQ.1) THEN
              ITII = ITLIPV
              IP1 = IPT(1,ITII)
              IP2 = IPT(2,ITII)
              IP3 = IPT(3,ITII)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF ((VPDT(X1,Y1,X2,Y2,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X2,Y2,X3,Y3,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X3,Y3,X1,Y1,X0,Y0).GE.0.0)) THEN
                  KTLI(IIP) = 1
                  ITLI(IIP) = ITII
                  GO TO 40
              END IF
          END IF
* Locates inside the data area.
          DO 10 ITII = 1,NT
              IP1 = IPT(1,ITII)
              IP2 = IPT(2,ITII)
              IP3 = IPT(3,ITII)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF ((VPDT(X1,Y1,X2,Y2,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X2,Y2,X3,Y3,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X3,Y3,X1,Y1,X0,Y0).GE.0.0)) THEN
                  KTLI(IIP) = 1
                  ITLI(IIP) = ITII
                  GO TO 40
              END IF
   10     CONTINUE
* Locates outside the data area.
          DO 20 ILII = 1,NL
              IL1 = ILII
              IL2 = MOD(IL1,NL) + 1
              IP1 = IPL(1,IL1)
              IP2 = IPL(1,IL2)
              IP3 = IPL(2,IL2)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF (VPDT(X1,Y1,X3,Y3,X0,Y0).LE.0.0) THEN
                  IF (VPDT(X1,Y1,X3,Y3,X2,Y2).LE.0.0) THEN
                      IF ((SPDT(X1,Y1,X0,Y0,X2,Y2).LE.0.0) .AND.
     +                    (SPDT(X3,Y3,X0,Y0,X2,Y2).LE.0.0)) THEN
                          KTLI(IIP) = 3
                          ITLI(IIP) = IL2
                          GO TO 40
                      END IF
                  END IF
                  IF (VPDT(X1,Y1,X3,Y3,X2,Y2).GE.0.0) THEN
                      IF ((SPDT(X1,Y1,X0,Y0,X2,Y2).GE.0.0) .AND.
     +                    (SPDT(X3,Y3,X0,Y0,X2,Y2).GE.0.0)) THEN
                          KTLI(IIP) = 4
                          ITLI(IIP) = IL2
                          GO TO 40
                      END IF
                  END IF
              END IF
   20     CONTINUE
          DO 30 ILII = 1,NL
              IL2 = ILII
              IP2 = IPL(1,IL2)
              IP3 = IPL(2,IL2)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF (VPDT(X2,Y2,X3,Y3,X0,Y0).LE.0.0) THEN
                  IF ((SPDT(X3,Y3,X0,Y0,X2,Y2).GE.0.0) .AND.
     +                (SPDT(X2,Y2,X0,Y0,X3,Y3).GE.0.0)) THEN
                      KTLI(IIP) = 2
                      ITLI(IIP) = IL2
                      GO TO 40
                  END IF
              END IF
   30     CONTINUE
   40 CONTINUE
      END


      SUBROUTINE SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,NIP,XI,YI,KTLI,
     +                  ITLI, ZI, EXTRPI)
*
* Polynomials
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine determines a polynomial in x and y for each
* triangle or rectangle in the x-y plane and calculates the z
* value by evaluating the polynomial for the desired points,
* for bivariate interpolation and surface fitting for scattered
* data.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z
*          values at the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   PDD  = two-dimensional array of dimension 5*NDP
*          containing the partial derivatives at the data
*          points,
*   NIP  = number of output points at which interpolation is
*          to be performed,
*   XI   = array of dimension NIP containing the x
*          coordinates of the output points,
*   YI   = array of dimension NIP containing the y
*          coordinates of the output points,
*   KTLI = integer array of dimension NIP, each element
*          containing the code for the type of the piece of
*          the plane in which each output point lies
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border
*            line segments
*        = 4 for the triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP containing the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the output
*          points.
*
* The output argument is
*   ZI   = array of dimension NIP, where the calculated z
*          values are to be stored.
*   EXTRPI = logical array of dimension NIP, indicating 
*            if a point resides outside the convex hull (and its Z value
*            has been extrapolated)
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP,NIP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             PDD(5,NDP),XD(NDP),XI(NIP),YD(NDP),
     +                 YI(NIP),ZD(NDP),ZI(NIP)
      INTEGER          IPL(2,NL),IPT(3,NT),ITLI(NIP),KTLI(NIP)
      LOGICAL          EXTRPI(NIP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A,AA,AB,ACT2,AD,ADBC,AP,B,BB,BC,BDT2,
     +                 BP,C,CC,CD,
     +                 CP,D,DD,DLT,DP,DX,DY,E1,E2,G1,G2,H1,H2,H3,LUSQ,
     +                 LVSQ,P0,P00,P01,P02,P03,P04,P05,P1,P10,P11,P12,
     +                 P13,P14,P2,P20,P21,P22,P23,P3,P30,P31,P32,P4,P40,
     +                 P41,P5,P50,SPUV,U,V,WT1,WT2,X0,XII,Y0,YII,Z0,ZII,
     +                 ZII1,ZII2
      INTEGER          I,IDP,IIP,ILI,IR,ITLII,ITLIPV,K,KTLII,KTLIPV
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             PD(5,3),X(3),Y(3),Z(3),ZU(3),ZUU(3),
     +                 ZUV(3),ZV(3),ZVV(3)
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
* Outermost DO-loop with respect to the output point
      DO 120 IIP = 1,NIP
          XII = XI(IIP)
          YII = YI(IIP)
          KTLII = KTLI(IIP)
          ITLII = ITLI(IIP)
          IF (IIP.EQ.1) THEN
              KTLIPV = 0
              ITLIPV = 0
          ELSE
              KTLIPV = KTLI(IIP-1)
              ITLIPV = ITLI(IIP-1)
          END IF
* Part 1.  Calculation of ZII by interpolation
          IF (KTLII.EQ.1) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the
* vertexes.
                  DO 20 I = 1,3
                      IDP = IPT(I,ITLII)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 10 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   10                 CONTINUE
   20             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = X(2) - X0
                  B = X(3) - X0
                  C = Y(2) - Y0
                  D = Y(3) - Y0
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -C/DLT
                  DP = A/DLT
* Converts the partial derivatives at the vertexes of the
* triangle for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 30 I = 1,3
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
   30             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P10 - P20
                  H2 = ZU(2) - P10 - ZUU(1)
                  H3 = ZUU(2) - ZUU(1)
                  P30 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P40 = -15.0*H1 + 7.0*H2 - H3
                  P50 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = Z(3) - P00 - P01 - P02
                  H2 = ZV(3) - P01 - ZVV(1)
                  H3 = ZVV(3) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  LUSQ = AA + CC
                  LVSQ = BB + DD
                  SPUV = AB + CD
                  P41 = 5.0*SPUV/LUSQ*P50
                  P14 = 5.0*SPUV/LVSQ*P05
                  H1 = ZV(2) - P01 - P11 - P41
                  H2 = ZUV(2) - P11 - 4.0*P41
                  P21 = 3.0*H1 - H2
                  P31 = -2.0*H1 + H2
                  H1 = ZU(3) - P10 - P11 - P14
                  H2 = ZUV(3) - P11 - 4.0*P14
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  E1 = (LVSQ-SPUV)/ ((LVSQ-SPUV)+ (LUSQ-SPUV))
                  E2 = 1.0 - E1
                  G1 = 5.0*E1 - 2.0
                  G2 = 1.0 - G1
                  H1 = 5.0* (E1* (P50-P41)+E2* (P05-P14)) + (P41+P14)
                  H2 = 0.5*ZVV(2) - P02 - P12
                  H3 = 0.5*ZUU(3) - P20 - P21
                  P22 = H1 + G1*H2 + G2*H3
                  P32 = H2 - P22
                  P23 = H3 - P22
              END IF
* Converts XII and YII to u-v system.
              DX = XII - X0
              DY = YII - Y0
              U = AP*DX + BP*DY
              V = CP*DX + DP*DY
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
              P1 = P10 + V* (P11+V* (P12+V* (P13+V*P14)))
              P2 = P20 + V* (P21+V* (P22+V*P23))
              P3 = P30 + V* (P31+V*P32)
              P4 = P40 + V*P41
              P5 = P50
              ZI(IIP) = P0 + U* (P1+U* (P2+U* (P3+U* (P4+U*P5))))
              EXTRPI(IIP) = .FALSE.
          END IF
* Part 2.  Calculation of ZII by extrapolation in the rectangle
          IF (KTLII.EQ.2) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the end
* points of the border line segment.
                  DO 50 I = 1,2
                      IDP = IPL(I,ITLII)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 40 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   40                 CONTINUE
   50             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = Y(2) - Y(1)
                  B = X(2) - X(1)
                  C = -B
                  D = A
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -BP
                  DP = AP
* Converts the partial derivatives at the end points of the
* border line segment for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 60 I = 1,2
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
   60             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P01 - P02
                  H2 = ZV(2) - P01 - ZVV(1)
                  H3 = ZVV(2) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = ZU(2) - P10 - P11
                  H2 = ZUV(2) - P11
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  P21 = 0.5* (ZUU(2)-ZUU(1))
              END IF
* Converts XII and YII to u-v system.
              DX = XII - X0
              DY = YII - Y0
              U = AP*DX + BP*DY
              V = CP*DX + DP*DY
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
              P1 = P10 + V* (P11+V* (P12+V*P13))
              P2 = P20 + V*P21
              ZI(IIP) = P0 + U* (P1+U*P2)
              EXTRPI(IIP) = .TRUE.
          END IF
* Part 3.  Calculation of ZII by extrapolation in the triangle
          IF (KTLII.EQ.3) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the vertex
* of the triangle.
                  IDP = IPL(1,ITLII)
                  X0 = XD(IDP)
                  Y0 = YD(IDP)
                  Z0 = ZD(IDP)
                  DO 70 K = 1,5
                      PD(K,1) = PDD(K,IDP)
   70             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z0
                  P10 = PD(1,1)
                  P01 = PD(2,1)
                  P20 = 0.5*PD(3,1)
                  P11 = PD(4,1)
                  P02 = 0.5*PD(5,1)
              END IF
* Converts XII and YII to U-V system.
              U = XII - X0
              V = YII - Y0
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V*P02)
              P1 = P10 + V*P11
              ZI(IIP) = P0 + U* (P1+U*P20)
              EXTRPI(IIP) = .TRUE.
          END IF
* Part 4.  Calculation of ZII by extrapolation in the triangle
*          which is an overlap of two rectangles.
          IF (KTLII.EQ.4) THEN
* Calculates the coefficients.
              DO 110 IR = 1,2
                  IF (IR.EQ.1) THEN
                      ILI = MOD(ITLII+NL-2,NL) + 1
                  ELSE
                      ILI = ITLII
                  END IF
* Loads coordinate and partial derivative values at the end
* points of the border line segment.
                  DO 90 I = 1,2
                      IDP = IPL(I,ILI)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 80 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   80                 CONTINUE
   90             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = Y(2) - Y(1)
                  B = X(2) - X(1)
                  C = -B
                  D = A
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -BP
                  DP = AP
* Converts the partial derivatives at the end points of the
* border line segment for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 100 I = 1,2
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
  100             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P01 - P02
                  H2 = ZV(2) - P01 - ZVV(1)
                  H3 = ZVV(2) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = ZU(2) - P10 - P11
                  H2 = ZUV(2) - P11
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  P21 = 0.5* (ZUU(2)-ZUU(1))
* Converts XII and YII to u-v system.
                  DX = XII - X0
                  DY = YII - Y0
                  U = AP*DX + BP*DY
                  V = CP*DX + DP*DY
* Evaluates the polynomial.
                  P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
                  P1 = P10 + V* (P11+V* (P12+V*P13))
                  P2 = P20 + V*P21
                  ZII = P0 + U* (P1+U*P2)
                  IF (IR.EQ.1) THEN
                      ZII1 = ZII
                      WT2 = ((X(1)-X(2))* (XII-X(2))+
     +                      (Y(1)-Y(2))* (YII-Y(2)))**2
                  ELSE
                      ZII2 = ZII
                      WT1 = ((X(2)-X(1))* (XII-X(1))+
     +                      (Y(2)-Y(1))* (YII-Y(1)))**2
                  END IF
  110         CONTINUE
              ZI(IIP) = (WT1*ZII1+WT2*ZII2)/ (WT1+WT2)
              EXTRPI(IIP) = .TRUE.
          END IF
  120 CONTINUE
      END


      SUBROUTINE  IDBVIP(MD,NCP,NDP,XD,YD,ZD,NIP,XI,YI,ZI,              ID001340
     1                   IWK,WK,MISSI)
C THIS SUBROUTINE PERFORMS BIVARIATE INTERPOLATION WHEN THE PRO-
C JECTIONS OF THE DATA POINTS IN THE X-Y PLANE ARE IRREGULARLY
C DISTRIBUTED IN THE PLANE.
C THE INPUT PARAMETERS ARE
C     MD  = MODE OF COMPUTATION (MUST BE 1, 2, OR 3),
C         = 1 FOR NEW NCP AND/OR NEW XD-YD,
C         = 2 FOR OLD NCP, OLD XD-YD, NEW XI-YI,
C         = 3 FOR OLD NCP, OLD XD-YD, OLD XI-YI,
C     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI-
C           MATING PARTIAL DERIVATIVES AT EACH DATA POINT
C           (MUST BE 2 OR GREATER, BUT SMALLER THAN NDP),
C     NDP = NUMBER OF DATA POINTS (MUST BE 4 OR GREATER),
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE X
C           COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE Y
C           COORDINATES OF THE DATA POINTS,
C     ZD  = ARRAY OF DIMENSION NDP CONTAINING THE Z
C           COORDINATES OF THE DATA POINTS,
C     NIP = NUMBER OF OUTPUT POINTS AT WHICH INTERPOLATION
C           IS TO BE PERFORMED (MUST BE 1 OR GREATER),
C     XI  = ARRAY OF DIMENSION NIP CONTAINING THE X
C           COORDINATES OF THE OUTPUT POINTS,
C     YI  = ARRAY OF DIMENSION NIP CONTAINING THE Y
C           COORDINATES OF THE OUTPUT POINTS.
C THE OUTPUT PARAMETER IS
C     ZI  = ARRAY OF DIMENSION NIP WHERE INTERPOLATED Z
C           VALUES ARE TO BE STORED.
C     MISSI = LOCICAL ARRAY, INDICATING IF EXTRAPOLATION OR MISSING VALUES
C            OUTSIDE CONVEX HULL WANTED
C THE OTHER PARAMETERS ARE
C     IWK = INTEGER ARRAY OF DIMENSION
C              MAX0(31,27+NCP)*NDP+NIP
C           USED INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A
C           WORK AREA.
C THE VERY FIRST CALL TO THIS SUBROUTINE AND THE CALL WITH A NEW
C NCP VALUE, A NEW NDP VALUE, AND/OR NEW CONTENTS OF THE XD AND
C YD ARRAYS MUST BE MADE WITH MD=1.  THE CALL WITH MD=2 MUST BE
C PRECEDED BY ANOTHER CALL WITH THE SAME NCP AND NDP VALUES AND
C WITH THE SAME CONTENTS OF THE XD AND YD ARRAYS.  THE CALL WITH
C MD=3 MUST BE PRECEDED BY ANOTHER CALL WITH THE SAME NCP, NDP,
C AND NIP VALUES AND WITH THE SAME CONTENTS OF THE XD, YD, XI,
C AND YI ARRAYS.  BETWEEN THE CALL WITH MD=2 OR MD=3 AND ITS
C PRECEDING CALL, THE IWK AND WK ARRAYS MUST NOT BE DISTURBED.
C USE OF A VALUE BETWEEN 3 AND 5 (INCLUSIVE) FOR NCP IS RECOM-
C MENDED UNLESS THERE ARE EVIDENCES THAT DICTATE OTHERWISE.
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,
C THEREFORE, SYSTEM DEPENDENT.
C THIS SUBROUTINE CALLS THE IDCLDP, IDLCTN, IDPDRV, IDPTIP, AND
C IDTANG SUBROUTINES.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      LOGICAL MISSI, LINEAR
      DIMENSION   XD(NDP),YD(NDP),ZD(NDP),XI(NIP),YI(NIP),
     1            ZI(NIP),MISSI(NIP),IWK((31+NCP)*NDP+NIP),WK(8*NDP)
      COMMON/IDLC/NIT, idummy
      COMMON/IDPI/ITPV
      DATA  LUN/6/
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NCP0=NCP
      NDP0=NDP
      NIP0=NIP
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF(MD0.LT.1.OR.MD0.GT.3)           GO TO 90
      IF(NCP0.EQ.0) THEN
         LINEAR=.TRUE.
         DO 21 I=1,NIP
            MISSI(I)=.TRUE.
   21    CONTINUE
      END IF
      IF(NCP0.EQ.1.OR.NCP0.GE.NDP0)      GO TO 90
      IF(NDP0.LT.4)                      GO TO 90
      IF(NIP0.LT.1)                      GO TO 90
      IF(MD0.GE.2)        GO TO 22
      IWK(1)=NCP0
      IWK(2)=NDP0
      GO TO 23
   22 NCPPV=IWK(1)
      NDPPV=IWK(2)
      IF(NCP0.NE.NCPPV)   GO TO 90
      IF(NDP0.NE.NDPPV)   GO TO 90
   23 IF(MD0.GE.3)        GO TO 24
      IWK(3)=NIP
      GO TO 30
   24 NIPPV=IWK(3)
      IF(NIP0.NE.NIPPV)   GO TO 90
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWIWK=JWIWL
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIPC=27*NDP0+1
      JWIT0=MAX0(31,27+NCP0)*NDP0
C TRIANGULATES THE X-Y PLANE.  (FOR MD=1)
   40 IF(MD0.GT.1)   GO TO 50
      CALL IDTANG(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            IWK(JWIWL),IWK(JWIWP),WK)
      IWK(5)=NT
      IWK(6)=NL
      IF(NT.EQ.0)    RETURN
C DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.  (FOR MD=1)
   50 IF(MD0.GT.1 .OR. LINEAR)   GO TO 60
      CALL IDCLDP(NDP0,XD,YD,NCP0,IWK(JWIPC))
      IF(IWK(JWIPC).EQ.0)      RETURN
C LOCATES ALL POINTS AT WHICH INTERPOLATION IS TO BE PERFORMED.
C (FOR MD=1,2)
   60 IF(MD0.EQ.3)   GO TO 70
      NIT=0
      JWIT=JWIT0
      DO 61  IIP=1,NIP0
         JWIT=JWIT+1
        CALL IDLCTN(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            XI(IIP),YI(IIP),IWK(JWIT),IWK(JWIWK),WK)
   61 CONTINUE
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   70 IF (.NOT.LINEAR) CALL IDPDRV(NDP0,XD,YD,ZD,NCP0,IWK(JWIPC),WK)
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   80 ITPV=0
      JWIT=JWIT0
      DO 81  IIP=1,NIP0
        JWIT=JWIT+1
        IF (LINEAR) THEN
           CALL IDPTLI(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1                 IWK(JWIT),XI(IIP),YI(IIP),ZI(IIP),MISSI(IIP))
        ELSE
           CALL IDPTIP(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1                 IWK(JWIT),XI(IIP),YI(IIP),ZI(IIP),MISSI(IIP))
        END IF
   81 CONTINUE
      RETURN
C ERROR EXIT
   90 WRITE (LUN,2090) MD0,NCP0,NDP0,NIP0
      RETURN
C FORMAT STATEMENT FOR ERROR MESSAGE
 2090 FORMAT(1X/41H ***   IMPROPER INPUT PARAMETER VALUE(S)./
     1   7H   MD =,I4,10X,5HNCP =,I6,10X,5HNDP =,I6,
     2   10X,5HNIP =,I6/
     3   35H ERROR DETECTED IN ROUTINE   IDBVIP/)
      END

      SUBROUTINE  IDCLDP(NDP,XD,YD,NCP,IPC)                         
C THIS SUBROUTINE SELECTS SEVERAL DATA POINTS THAT ARE CLOSEST
C TO EACH OF THE DATA POINT.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS,
C     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA
C           POINTS.
C THE OUTPUT PARAMETER IS
C     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP, WHERE THE
C           POINT NUMBERS OF NCP DATA POINTS CLOSEST TO
C           EACH OF THE NDP DATA POINTS ARE TO BE STORED.
C THIS SUBROUTINE ARBITRARILY SETS A RESTRICTION THAT NCP MUST
C NOT EXCEED 25.
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,
C THEREFORE, SYSTEM DEPENDENT.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   XD(NDP),YD(NDP),IPC(NCP*NDP)
      DIMENSION   DSQ0(25),IPC0(25)
      DATA  NCPMX/25/, LUN/6/
C STATEMENT FUNCTION
      DSQF(U1,V1,U2,V2)=(U2-U1)**2+(V2-V1)**2
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NCP0=NCP
      IF(NDP0.LT.2)  GO TO 90
      IF(NCP0.LT.1.OR.NCP0.GT.NCPMX.OR.NCP0.GE.NDP0)    GO TO 90
C CALCULATION
   20 DO 59  IP1=1,NDP0
C - SELECTS NCP POINTS.
        X1=XD(IP1)
        Y1=YD(IP1)
        J1=0
        DSQMX=0.0
        DO 22  IP2=1,NDP0
          IF(IP2.EQ.IP1)  GO TO 22
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))
          J1=J1+1
          DSQ0(J1)=DSQI
          IPC0(J1)=IP2
          IF(DSQI.LE.DSQMX)    GO TO 21
          DSQMX=DSQI
          JMX=J1
   21     IF(J1.GE.NCP0)  GO TO 23
   22   CONTINUE
   23   IP2MN=IP2+1
        IF(IP2MN.GT.NDP0)      GO TO 30
        DO 25  IP2=IP2MN,NDP0
          IF(IP2.EQ.IP1)  GO TO 25
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))
          IF(DSQI.GE.DSQMX)    GO TO 25
          DSQ0(JMX)=DSQI
          IPC0(JMX)=IP2
          DSQMX=0.0
          DO 24  J1=1,NCP0
            IF(DSQ0(J1).LE.DSQMX)   GO TO 24
            DSQMX=DSQ0(J1)
            JMX=J1
   24     CONTINUE
   25   CONTINUE
C - CHECKS IF ALL THE NCP+1 POINTS ARE COLLINEAR.
   30   IP2=IPC0(1)
        DX12=XD(IP2)-X1
        DY12=YD(IP2)-Y1
        DO 31  J3=2,NCP0
          IP3=IPC0(J3)
          DX13=XD(IP3)-X1
          DY13=YD(IP3)-Y1
          IF((DY13*DX12-DX13*DY12).NE.0.0)    GO TO 50
   31   CONTINUE
C - SEARCHES FOR THE CLOSEST NONCOLLINEAR POINT.
   40   NCLPT=0
        DO 43  IP3=1,NDP0
          IF(IP3.EQ.IP1)       GO TO 43
          DO 41  J4=1,NCP0
            IF(IP3.EQ.IPC0(J4))     GO TO 43
   41     CONTINUE
          DX13=XD(IP3)-X1
          DY13=YD(IP3)-Y1
          IF((DY13*DX12-DX13*DY12).EQ.0.0)    GO TO 43
          DSQI=DSQF(X1,Y1,XD(IP3),YD(IP3))
          IF(NCLPT.EQ.0)       GO TO 42
          IF(DSQI.GE.DSQMN)    GO TO 43
   42     NCLPT=1
          DSQMN=DSQI
          IP3MN=IP3
   43   CONTINUE
        IF(NCLPT.EQ.0)    GO TO 91
        DSQMX=DSQMN
        IPC0(JMX)=IP3MN
C - REPLACES THE LOCAL ARRAY FOR THE OUTPUT ARRAY.
   50   J1=(IP1-1)*NCP0
        DO 51  J2=1,NCP0
          J1=J1+1
          IPC(J1)=IPC0(J2)
   51   CONTINUE
   59 CONTINUE
      RETURN
C ERROR EXIT
   90 WRITE (LUN,2090)
      GO TO 92
   91 WRITE (LUN,2091)
   92 WRITE (LUN,2092)  NDP0,NCP0
      IPC(1)=0
      RETURN
C FORMAT STATEMENTS FOR ERROR MESSAGES
 2090 FORMAT(1X/41H ***   IMPROPER INPUT PARAMETER VALUE(S).)
 2091 FORMAT(1X/33H ***   ALL COLLINEAR DATA POINTS.)
 2092 FORMAT(8H   NDP =,I5,5X,5HNCP =,I5/
     1   35H ERROR DETECTED IN ROUTINE   IDCLDP/)
      END

      SUBROUTINE IDGRID(XD, YD, NDP, NT, IPT, NL, IPL, NXI, NYI, XI, YI,  IDG   10
     *  NGP, IGP)
C THIS SUBROUTINE ORGANIZES GRID POINTS FOR SURFACE FITTING BY
C SORTING THEM IN ASCENDING ORDER OF TRIANGLE NUMBERS AND OF THE
C BORDER LINE SEGMENT NUMBER.
C THE INPUT PARAMETERS ARE
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS, WHERE NDP IS THE
C           NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     NXI = NUMBER OF GRID POINTS IN THE X COORDINATE,
C     NYI = NUMBER OF GRID POINTS IN THE Y COORDINATE,
C     XI,YI = ARRAYS OF DIMENSION NXI AND NYI CONTAINING
C           THE X AND Y COORDINATES OF THE GRID POINTS,
C           RESPECTIVELY.
C THE OUTPUT PARAMETERS ARE
C     NGP = INTEGER ARRAY OF DIMENSION 2*(NT+2*NL) WHERE THE
C           NUMBER OF GRID POINTS THAT BELONG TO EACH OF THE
C           TRIANGLES OR OF THE BORDER LINE SEGMENTS ARE TO
C           BE STORED,
C     IGP = INTEGER ARRAY OF DIMENSION NXI*NYI WHERE THE
C           GRID POINT NUMBERS ARE TO BE STORED IN ASCENDING
C           ORDER OF THE TRIANGLE NUMBER AND THE BORDER LINE
C           SEGMENT NUMBER.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION XD(NDP), YD(NDP), IPT(3*NT), IPL(3*NL), XI(NXI),
     *  YI(NYI), NGP(2*(NT+2*NL)), IGP(NXI*NYI)
C STATEMENT FUNCTIONS
      SIDE(U1,V1,U2,V2,U3,V3) = (U1-U3)*(V2-V3) - (V1-V3)*(U2-U3)
      SPDT(U1,V1,U2,V2,U3,V3) = (U1-U2)*(U3-U2) + (V1-V2)*(V3-V2)
C PRELIMINARY PROCESSING
      NT0 = NT
      NL0 = NL
      NXI0 = NXI
      NYI0 = NYI
      NXINYI = NXI0*NYI0
      XIMN = DMIN1(XI(1),XI(NXI0))
      XIMX = DMAX1(XI(1),XI(NXI0))
      YIMN = DMIN1(YI(1),YI(NYI0))
      YIMX = DMAX1(YI(1),YI(NYI0))
C DETERMINES GRID POINTS INSIDE THE DATA AREA.
      JNGP0 = 0
      JNGP1 = 2*(NT0+2*NL0) + 1
      JIGP0 = 0
      JIGP1 = NXINYI + 1
      DO 160 IT0=1,NT0
        NGP0 = 0
        NGP1 = 0
        IT0T3 = IT0*3
        IP1 = IPT(IT0T3-2)
        IP2 = IPT(IT0T3-1)
        IP3 = IPT(IT0T3)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        XMN = DMIN1(X1,X2,X3)
        XMX = DMAX1(X1,X2,X3)
        YMN = DMIN1(Y1,Y2,Y3)
        YMX = DMAX1(Y1,Y2,Y3)
        INSD = 0
        DO 20 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 10
          IF (INSD.EQ.0) GO TO 20
          IXIMX = IXI - 1
          GO TO 30
   10     IF (INSD.EQ.1) GO TO 20
          INSD = 1
          IXIMN = IXI
   20   CONTINUE
        IF (INSD.EQ.0) GO TO 150
        IXIMX = NXI0
   30   DO 140 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 140
          DO 130 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SIDE(X1,Y1,X2,Y2,XII,YII)) 130, 40, 50
   40       L = 1
   50       IF (SIDE(X2,Y2,X3,Y3,XII,YII)) 130, 60, 70
   60       L = 1
   70       IF (SIDE(X3,Y3,X1,Y1,XII,YII)) 130, 80, 90
   80       L = 1
   90       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 100
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
            IGP(JIGP0) = IZI
            GO TO 130
  100       IF (JIGP1.GT.NXINYI) GO TO 120
            DO 110 JIGP1I=JIGP1,NXINYI
              IF (IZI.EQ.IGP(JIGP1I)) GO TO 130
  110       CONTINUE
  120       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
            IGP(JIGP1) = IZI
  130     CONTINUE
  140   CONTINUE
  150   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
  160 CONTINUE
C DETERMINES GRID POINTS OUTSIDE THE DATA AREA.
C - IN SEMI-INFINITE RECTANGULAR AREA.
      DO 450 IL0=1,NL0
        NGP0 = 0
        NGP1 = 0
        IL0T3 = IL0*3
        IP1 = IPL(IL0T3-2)
        IP2 = IPL(IL0T3-1)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        XMN = XIMN
        XMX = XIMX
        YMN = YIMN
        YMX = YIMX
        IF (Y2.GE.Y1) XMN = DMIN1(X1,X2)
        IF (Y2.LE.Y1) XMX = DMAX1(X1,X2)
        IF (X2.LE.X1) YMN = DMIN1(Y1,Y2)
        IF (X2.GE.X1) YMX = DMAX1(Y1,Y2)
        INSD = 0
        DO 180 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 170
          IF (INSD.EQ.0) GO TO 180
          IXIMX = IXI - 1
          GO TO 190
  170     IF (INSD.EQ.1) GO TO 180
          INSD = 1
          IXIMN = IXI
  180   CONTINUE
        IF (INSD.EQ.0) GO TO 310
        IXIMX = NXI0
  190   DO 300 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 300
          DO 290 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SIDE(X1,Y1,X2,Y2,XII,YII)) 210, 200, 290
  200       L = 1
  210       IF (SPDT(X2,Y2,X1,Y1,XII,YII)) 290, 220, 230
  220       L = 1
  230       IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 290, 240, 250
  240       L = 1
  250       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 260
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
            IGP(JIGP0) = IZI
            GO TO 290
  260       IF (JIGP1.GT.NXINYI) GO TO 280
            DO 270 JIGP1I=JIGP1,NXINYI
              IF (IZI.EQ.IGP(JIGP1I)) GO TO 290
  270       CONTINUE
  280       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
            IGP(JIGP1) = IZI
  290     CONTINUE
  300   CONTINUE
  310   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
C - IN SEMI-INFINITE TRIANGULAR AREA.
        NGP0 = 0
        NGP1 = 0
        ILP1 = MOD(IL0,NL0) + 1
        ILP1T3 = ILP1*3
        IP3 = IPL(ILP1T3-1)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        XMN = XIMN
        XMX = XIMX
        YMN = YIMN
        YMX = YIMX
        IF (Y3.GE.Y2 .AND. Y2.GE.Y1) XMN = X2
        IF (Y3.LE.Y2 .AND. Y2.LE.Y1) XMX = X2
        IF (X3.LE.X2 .AND. X2.LE.X1) YMN = Y2
        IF (X3.GE.X2 .AND. X2.GE.X1) YMX = Y2
        INSD = 0
        DO 330 IXI=1,NXI0
          IF (XI(IXI).GE.XMN .AND. XI(IXI).LE.XMX) GO TO 320
          IF (INSD.EQ.0) GO TO 330
          IXIMX = IXI - 1
          GO TO 340
  320     IF (INSD.EQ.1) GO TO 330
          INSD = 1
          IXIMN = IXI
  330   CONTINUE
        IF (INSD.EQ.0) GO TO 440
        IXIMX = NXI0
  340   DO 430 IYI=1,NYI0
          YII = YI(IYI)
          IF (YII.LT.YMN .OR. YII.GT.YMX) GO TO 430
          DO 420 IXI=IXIMN,IXIMX
            XII = XI(IXI)
            L = 0
            IF (SPDT(X1,Y1,X2,Y2,XII,YII)) 360, 350, 420
  350       L = 1
  360       IF (SPDT(X3,Y3,X2,Y2,XII,YII)) 380, 370, 420
  370       L = 1
  380       IZI = NXI0*(IYI-1) + IXI
            IF (L.EQ.1) GO TO 390
            NGP0 = NGP0 + 1
            JIGP0 = JIGP0 + 1
            IGP(JIGP0) = IZI
            GO TO 420
  390       IF (JIGP1.GT.NXINYI) GO TO 410
            DO 400 JIGP1I=JIGP1,NXINYI
              IF (IZI.EQ.IGP(JIGP1I)) GO TO 420
  400       CONTINUE
  410       NGP1 = NGP1 + 1
            JIGP1 = JIGP1 - 1
            IGP(JIGP1) = IZI
  420     CONTINUE
  430   CONTINUE
  440   JNGP0 = JNGP0 + 1
        NGP(JNGP0) = NGP0
        JNGP1 = JNGP1 - 1
        NGP(JNGP1) = NGP1
  450 CONTINUE
      RETURN
      END

      SUBROUTINE IDLCTN(NDP, XD, YD, NT, IPT, NL, IPL, XII, YII, ITI,   IDL   10
     *  IWK, WK)
C THIS SUBROUTINE LOCATES A POINT, I.E., DETERMINES TO WHAT TRI-
C ANGLE A GIVEN POINT (XII,YII) BELONGS.  WHEN THE GIVEN POINT
C DOES NOT LIE INSIDE THE DATA AREA, THIS SUBROUTINE DETERMINES
C THE BORDER LINE SEGMENT WHEN THE POINT LIES IN AN OUTSIDE
C RECTANGULAR AREA, AND TWO BORDER LINE SEGMENTS WHEN THE POINT
C LIES IN AN OUTSIDE TRIANGULAR AREA.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
C           COORDINATES OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     XII,YII = X AND Y COORDINATES OF THE POINT TO BE
C           LOCATED.
C THE OUTPUT PARAMETER IS
C     ITI = TRIANGLE NUMBER, WHEN THE POINT IS INSIDE THE
C           DATA AREA, OR
C           TWO BORDER LINE SEGMENT NUMBERS, IL1 AND IL2,
C           CODED TO IL1*(NT+NL)+IL2, WHEN THE POINT IS
C           OUTSIDE THE DATA AREA.
C THE OTHER PARAMETERS ARE
C     IWK = INTEGER ARRAY OF DIMENSION 18*NDP USED INTER-
C           NALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION XD(NDP), YD(NDP), IPT(3*NT), IPL(3*NL), IWK(18*NDP),
     *  WK(8*NDP)
      DIMENSION NTSC(9), IDSC(9)
      COMMON /IDLC/ NIT, ITIPV
C STATEMENT FUNCTIONS
      SIDE(U1,V1,U2,V2,U3,V3) = (U1-U3)*(V2-V3) - (V1-V3)*(U2-U3)
      SPDT(U1,V1,U2,V2,U3,V3) = (U1-U2)*(U3-U2) + (V1-V2)*(V3-V2)
C PRELIMINARY PROCESSING
      NDP0 = NDP
      NT0 = NT
      NL0 = NL
      NTL = NT0 + NL0
      X0 = XII
      Y0 = YII
C PROCESSING FOR A NEW SET OF DATA POINTS
      IF (NIT.NE.0) GO TO 80
      NIT = 1
C - DIVIDES THE X-Y PLANE INTO NINE RECTANGULAR SECTIONS.
      XMN = XD(1)
      XMX = XMN
      YMN = YD(1)
      YMX = YMN
      DO 10 IDP=2,NDP0
        XI = XD(IDP)
        YI = YD(IDP)
        XMN = DMIN1(XI,XMN)
        XMX = DMAX1(XI,XMX)
        YMN = DMIN1(YI,YMN)
        YMX = DMAX1(YI,YMX)
   10 CONTINUE
      XS1 = (XMN+XMN+XMX)/3.0
      XS2 = (XMN+XMX+XMX)/3.0
      YS1 = (YMN+YMN+YMX)/3.0
      YS2 = (YMN+YMX+YMX)/3.0
C - DETERMINES AND STORES IN THE IWK ARRAY TRIANGLE NUMBERS OF
C - THE TRIANGLES ASSOCIATED WITH EACH OF THE NINE SECTIONS.
      DO 20 ISC=1,9
        NTSC(ISC) = 0
        IDSC(ISC) = 0
   20 CONTINUE
      IT0T3 = 0
      JWK = 0
      DO 70 IT0=1,NT0
        IT0T3 = IT0T3 + 3
        I1 = IPT(IT0T3-2)
        I2 = IPT(IT0T3-1)
        I3 = IPT(IT0T3)
        XMN = DMIN1(XD(I1),XD(I2),XD(I3))
        XMX = DMAX1(XD(I1),XD(I2),XD(I3))
        YMN = DMIN1(YD(I1),YD(I2),YD(I3))
        YMX = DMAX1(YD(I1),YD(I2),YD(I3))
        IF (YMN.GT.YS1) GO TO 30
        IF (XMN.LE.XS1) IDSC(1) = 1
        IF (XMX.GE.XS1 .AND. XMN.LE.XS2) IDSC(2) = 1
        IF (XMX.GE.XS2) IDSC(3) = 1
   30   IF (YMX.LT.YS1 .OR. YMN.GT.YS2) GO TO 40
        IF (XMN.LE.XS1) IDSC(4) = 1
        IF (XMX.GE.XS1 .AND. XMN.LE.XS2) IDSC(5) = 1
        IF (XMX.GE.XS2) IDSC(6) = 1
   40   IF (YMX.LT.YS2) GO TO 50
        IF (XMN.LE.XS1) IDSC(7) = 1
        IF (XMX.GE.XS1 .AND. XMN.LE.XS2) IDSC(8) = 1
        IF (XMX.GE.XS2) IDSC(9) = 1
   50   DO 60 ISC=1,9
          IF (IDSC(ISC).EQ.0) GO TO 60
          JIWK = 9*NTSC(ISC) + ISC
          IWK(JIWK) = IT0
          NTSC(ISC) = NTSC(ISC) + 1
          IDSC(ISC) = 0
   60   CONTINUE
C - STORES IN THE WK ARRAY THE MINIMUM AND MAXIMUM OF THE X AND
C - Y COORDINATE VALUES FOR EACH OF THE TRIANGLE.
        JWK = JWK + 4
        WK(JWK-3) = XMN
        WK(JWK-2) = XMX
        WK(JWK-1) = YMN
        WK(JWK) = YMX
   70 CONTINUE
      GO TO 110
C CHECKS IF IN THE SAME TRIANGLE AS PREVIOUS.
   80 IT0 = ITIPV
      IF (IT0.GT.NT0) GO TO 90
      IT0T3 = IT0*3
      IP1 = IPT(IT0T3-2)
      X1 = XD(IP1)
      Y1 = YD(IP1)
      IP2 = IPT(IT0T3-1)
      X2 = XD(IP2)
      Y2 = YD(IP2)
      IF (SIDE(X1,Y1,X2,Y2,X0,Y0).LT.0.0) GO TO 110
      IP3 = IPT(IT0T3)
      X3 = XD(IP3)
      Y3 = YD(IP3)
      IF (SIDE(X2,Y2,X3,Y3,X0,Y0).LT.0.0) GO TO 110
      IF (SIDE(X3,Y3,X1,Y1,X0,Y0).LT.0.0) GO TO 110
      GO TO 170
C CHECKS IF ON THE SAME BORDER LINE SEGMENT.
   90 IL1 = IT0/NTL
      IL2 = IT0 - IL1*NTL
      IL1T3 = IL1*3
      IP1 = IPL(IL1T3-2)
      X1 = XD(IP1)
      Y1 = YD(IP1)
      IP2 = IPL(IL1T3-1)
      X2 = XD(IP2)
      Y2 = YD(IP2)
      IF (IL2.NE.IL1) GO TO 100
      IF (SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0) GO TO 110
      IF (SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0) GO TO 110
      IF (SIDE(X1,Y1,X2,Y2,X0,Y0).GT.0.0) GO TO 110
      GO TO 170
C CHECKS IF BETWEEN THE SAME TWO BORDER LINE SEGMENTS.
  100 IF (SPDT(X1,Y1,X2,Y2,X0,Y0).GT.0.0) GO TO 110
      IP3 = IPL(3*IL2-1)
      X3 = XD(IP3)
      Y3 = YD(IP3)
      IF (SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0) GO TO 170
C LOCATES INSIDE THE DATA AREA.
C - DETERMINES THE SECTION IN WHICH THE POINT IN QUESTION LIES.
 110  ISC = 1
      IF (X0.GE.XS1) ISC = ISC + 1
      IF (X0.GE.XS2) ISC = ISC + 1
      IF (Y0.GE.YS1) ISC = ISC + 3
      IF (Y0.GE.YS2) ISC = ISC + 3
C - SEARCHES THROUGH THE TRIANGLES ASSOCIATED WITH THE SECTION.
      NTSCI = NTSC(ISC)
      IF (NTSCI.LE.0) GO TO 130
      JIWK = -9 + ISC
      DO 120 ITSC=1,NTSCI
        JIWK = JIWK + 9
        IT0 = IWK(JIWK)
        JWK = IT0*4
        IF (X0.LT.WK(JWK-3)) GO TO 120
        IF (X0.GT.WK(JWK-2)) GO TO 120
        IF (Y0.LT.WK(JWK-1)) GO TO 120
        IF (Y0.GT.WK(JWK)) GO TO 120
        IT0T3 = IT0*3
        IP1 = IPT(IT0T3-2)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        IP2 = IPT(IT0T3-1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        IF (SIDE(X1,Y1,X2,Y2,X0,Y0).LT.0.0) GO TO 120
        IP3 = IPT(IT0T3)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        IF (SIDE(X2,Y2,X3,Y3,X0,Y0).LT.0.0) GO TO 120
        IF (SIDE(X3,Y3,X1,Y1,X0,Y0).LT.0.0) GO TO 120
        GO TO 170
  120 CONTINUE
C LOCATES OUTSIDE THE DATA AREA.
  130 DO 150 IL1=1,NL0
        IL1T3 = IL1*3
        IP1 = IPL(IL1T3-2)
        X1 = XD(IP1)
        Y1 = YD(IP1)
        IP2 = IPL(IL1T3-1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        IF (SPDT(X2,Y2,X1,Y1,X0,Y0).LT.0.0) GO TO 150
        IF (SPDT(X1,Y1,X2,Y2,X0,Y0).LT.0.0) GO TO 140
        IF (SIDE(X1,Y1,X2,Y2,X0,Y0).GT.0.0) GO TO 150
        IL2 = IL1
        GO TO 160
  140   IL2 = MOD(IL1,NL0) + 1
        IP3 = IPL(3*IL2-1)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        IF (SPDT(X3,Y3,X2,Y2,X0,Y0).LE.0.0) GO TO 160
  150 CONTINUE
      IT0 = 1
      GO TO 170
  160 IT0 = IL1*NTL + IL2
C NORMAL EXIT
  170 ITI = IT0
      ITIPV = IT0
      RETURN
      END

      SUBROUTINE  IDPDRV(NDP,XD,YD,ZD,NCP,IPC,PD)                       ID008940
C THIS SUBROUTINE ESTIMATES PARTIAL DERIVATIVES OF THE FIRST AND
C SECOND ORDER AT THE DATA POINTS.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
C           Y, AND Z COORDINATES OF THE DATA POINTS,
C     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI-
C           MATING PARTIAL DERIVATIVES AT EACH DATA POINT,
C     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP CONTAINING
C           THE POINT NUMBERS OF NCP DATA POINTS CLOSEST TO
C           EACH OF THE NDP DATA POINTS.
C THE OUTPUT PARAMETER IS
C     PD  = ARRAY OF DIMENSION 5*NDP, WHERE THE ESTIMATED
C           ZX, ZY, ZXX, ZXY, AND ZYY VALUES AT THE DATA
C           POINTS ARE TO BE STORED.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   XD(NDP),YD(NDP),ZD(NDP),IPC(NCP*NDP),PD(5*NDP)
      DOUBLE PRECISION        NMX,NMY,NMZ,NMXX,NMXY,NMYX,NMYY
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NCP0=NCP
      NCPM1=NCP0-1
C ESTIMATION OF ZX AND ZY
  20  DO 24  IP0=1,NDP0
        X0=XD(IP0)
        Y0=YD(IP0)
        Z0=ZD(IP0)
        NMX=0.0
        NMY=0.0
        NMZ=0.0
        JIPC0=NCP0*(IP0-1)
        DO 23  IC1=1,NCPM1
          JIPC=JIPC0+IC1
          IPI=IPC(JIPC)
          DX1=XD(IPI)-X0
          DY1=YD(IPI)-Y0
          DZ1=ZD(IPI)-Z0
          IC2MN=IC1+1
          DO 22  IC2=IC2MN,NCP0
            JIPC=JIPC0+IC2
            IPI=IPC(JIPC)
            DX2=XD(IPI)-X0
            DY2=YD(IPI)-Y0
            DNMZ=DX1*DY2-DY1*DX2
            IF(DNMZ.EQ.0.0)    GO TO 22
            DZ2=ZD(IPI)-Z0
            DNMX=DY1*DZ2-DZ1*DY2
            DNMY=DZ1*DX2-DX1*DZ2
            IF(DNMZ.GE.0.0)    GO TO 21
            DNMX=-DNMX
            DNMY=-DNMY
            DNMZ=-DNMZ
   21       NMX=NMX+DNMX
            NMY=NMY+DNMY
            NMZ=NMZ+DNMZ
   22     CONTINUE
   23   CONTINUE
        JPD0=5*IP0
        PD(JPD0-4)=-NMX/NMZ
        PD(JPD0-3)=-NMY/NMZ
   24 CONTINUE
C ESTIMATION OF ZXX, ZXY, AND ZYY
   30 DO 34  IP0=1,NDP0
        JPD0=JPD0+5
        X0=XD(IP0)
        JPD0=5*IP0
        Y0=YD(IP0)
        ZX0=PD(JPD0-4)
        ZY0=PD(JPD0-3)
        NMXX=0.0
        NMXY=0.0
        NMYX=0.0
        NMYY=0.0
        NMZ =0.0
        JIPC0=NCP0*(IP0-1)
        DO 33  IC1=1,NCPM1
          JIPC=JIPC0+IC1
          IPI=IPC(JIPC)
          DX1=XD(IPI)-X0
          DY1=YD(IPI)-Y0
          JPD=5*IPI
          DZX1=PD(JPD-4)-ZX0
          DZY1=PD(JPD-3)-ZY0
          IC2MN=IC1+1
          DO 32  IC2=IC2MN,NCP0
            JIPC=JIPC0+IC2
            IPI=IPC(JIPC)
            DX2=XD(IPI)-X0
            DY2=YD(IPI)-Y0
            DNMZ =DX1*DY2 -DY1*DX2
            IF(DNMZ.EQ.0.0)    GO TO 32
            JPD=5*IPI
            DZX2=PD(JPD-4)-ZX0
            DZY2=PD(JPD-3)-ZY0
            DNMXX=DY1*DZX2-DZX1*DY2
            DNMXY=DZX1*DX2-DX1*DZX2
            DNMYX=DY1*DZY2-DZY1*DY2
            DNMYY=DZY1*DX2-DX1*DZY2
            IF(DNMZ.GE.0.0)    GO TO 31
            DNMXX=-DNMXX
            DNMXY=-DNMXY
            DNMYX=-DNMYX
            DNMYY=-DNMYY
            DNMZ =-DNMZ
   31       NMXX=NMXX+DNMXX
            NMXY=NMXY+DNMXY
            NMYX=NMYX+DNMYX
            NMYY=NMYY+DNMYY
            NMZ =NMZ +DNMZ
   32     CONTINUE
   33   CONTINUE
        PD(JPD0-2)=-NMXX/NMZ
        PD(JPD0-1)=-(NMXY+NMYX)/(2.0*NMZ)
        PD(JPD0)  =-NMYY/NMZ
   34 CONTINUE
      RETURN
      END

      SUBROUTINE  IDPTIP(XD,YD,ZD,NDP,NT,IPT,NL,IPL,PDD,ITI,XII,YII,        ID010190
     1                   ZII,MISSII)
C THIS SUBROUTINE PERFORMS PUNCTUAL INTERPOLATION OR EXTRAPOLA-
C TION, I.E., DETERMINES THE Z VALUE AT A POINT.
C THE INPUT PARAMETERS ARE
C     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
C           Y, AND Z COORDINATES OF THE DATA POINTS, WHERE
C           NDP IS THE NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     PDD = ARRAY OF DIMENSION 5*NDP CONTAINING THE PARTIAL
C           DERIVATIVES AT THE DATA POINTS,
C     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES
C           THE POINT FOR WHICH INTERPOLATION IS TO BE
C           PERFORMED,
C     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH
C           INTERPOLATION IS TO BE PERFORMED.
C THE OUTPUT PARAMETER IS
C     ZII = INTERPOLATED Z VALUE.
C     MISSII = LOCIGAL INDICATING MISSING VALUE
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      LOGICAL MISSII
      DIMENSION   XD(NDP),YD(NDP),ZD(NDP),IPT(3*NT),IPL(3*NL),
     1            PDD(5*NDP)
      COMMON/IDPI/ITPV
      DIMENSION   X(3),Y(3),Z(3),PD(15),
     1            ZU(3),ZV(3),ZUU(3),ZUV(3),ZVV(3)
      DOUBLE PRECISION        LU,LV
      EQUIVALENCE (P5,P50)
C PRELIMINARY PROCESSING
   10 IT0=ITI
      NTL=NT+NL
      IF(IT0.LE.NTL)      GO TO 20
C EXTRAPOLATION OR MISSING VALUE WANTED?
      IF (MISSII) THEN
         ZII=0
         RETURN
      END IF
      IL1=IT0/NTL
      IL2=IT0-IL1*NTL
      IF(IL1.EQ.IL2)      GO TO 40
      GO TO 60
C CALCULATION OF ZII BY INTERPOLATION.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   20 IF(IT0.EQ.ITPV)     GO TO 30
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE
C VERTEXES.
   21 JIPT=3*(IT0-1)
      JPD=0
      DO 23  I=1,3
        JIPT=JIPT+1
        IDP=IPT(JIPT)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        JPDD=5*(IDP-1)
        DO 22  KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
   22   CONTINUE
   23 CONTINUE
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
   24 X0=X(1)
      Y0=Y(1)
      A=X(2)-X0
      B=X(3)-X0
      C=Y(2)-Y0
      D=Y(3)-Y0
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-C/DLT
      DP= A/DLT
C CONVERTS THE PARTIAL DERIVATIVES AT THE VERTEXES OF THE
C TRIANGLE FOR THE U-V COORDINATE SYSTEM.
   25 AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 26  I=1,3
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
   26 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   27 P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P10-P20
      H2=ZU(2)-P10-ZUU(1)
      H3=ZUU(2)-ZUU(1)
      P30= 10.0*H1-4.0*H2+0.5*H3
      P40=-15.0*H1+7.0*H2    -H3
      P50=  6.0*H1-3.0*H2+0.5*H3
      H1=Z(3)-P00-P01-P02
      H2=ZV(3)-P01-ZVV(1)
      H3=ZVV(3)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      LU=SQRT(AA+CC)
      LV=SQRT(BB+DD)
      THXU=ATAN2(C,A)
      THUV=ATAN2(D,B)-THXU
      CSUV=COS(THUV)
      P41=5.0*LV*CSUV/LU*P50
      P14=5.0*LU*CSUV/LV*P05
      H1=ZV(2)-P01-P11-P41
      H2=ZUV(2)-P11-4.0*P41
      P21= 3.0*H1-H2
      P31=-2.0*H1+H2
      H1=ZU(3)-P10-P11-P14
      H2=ZUV(3)-P11-4.0*P14
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      THUS=ATAN2(D-C,B-A)-THXU
      THSV=THUV-THUS
      AA= SIN(THSV)/LU
      BB=-COS(THSV)/LU
      CC= SIN(THUS)/LV
      DD= COS(THUS)/LV
      AC=AA*CC
      AD=AA*DD
      BC=BB*CC
      G1=AA*AC*(3.0*BC+2.0*AD)
      G2=CC*AC*(3.0*AD+2.0*BC)
      H1=-AA*AA*AA*(5.0*AA*BB*P50+(4.0*BC+AD)*P41)
     1   -CC*CC*CC*(5.0*CC*DD*P05+(4.0*AD+BC)*P14)
      H2=0.5*ZVV(2)-P02-P12
      H3=0.5*ZUU(3)-P20-P21
      P22=(G1*H2+G2*H3-H1)/(G1+G2)
      P32=H2-P22
      P23=H3-P22
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   30 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C EVALUATES THE POLYNOMIAL.
   31 P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*(P13+V*P14)))
      P2=P20+V*(P21+V*(P22+V*P23))
      P3=P30+V*(P31+V*P32)
      P4=P40+V*P41
      ZII=P0+U*(P1+U*(P2+U*(P3+U*(P4+U*P5))))
      MISSII=.FALSE.
      RETURN
C CALCULATION OF ZII BY EXTRAPOLATION IN THE RECTANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   40 IF(IT0.EQ.ITPV)     GO TO 50
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE END
C POINTS OF THE BORDER LINE SEGMENT.
   41 JIPL=3*(IL1-1)
      JPD=0
      DO 43  I=1,2
        JIPL=JIPL+1
        IDP=IPL(JIPL)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
        JPDD=5*(IDP-1)
        DO 42  KPD=1,5
          JPD=JPD+1
          JPDD=JPDD+1
          PD(JPD)=PDD(JPDD)
   42   CONTINUE
   43 CONTINUE
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
   44 X0=X(1)
      Y0=Y(1)
      A=Y(2)-Y(1)
      B=X(2)-X(1)
      C=-B
      D=A
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-BP
      DP= AP
C CONVERTS THE PARTIAL DERIVATIVES AT THE END POINTS OF THE
C BORDER LINE SEGMENT FOR THE U-V COORDINATE SYSTEM.
   45 AA=A*A
      ACT2=2.0*A*C
      CC=C*C
      AB=A*B
      ADBC=AD+BC
      CD=C*D
      BB=B*B
      BDT2=2.0*B*D
      DD=D*D
      DO 46  I=1,2
        JPD=5*I
        ZU(I)=A*PD(JPD-4)+C*PD(JPD-3)
        ZV(I)=B*PD(JPD-4)+D*PD(JPD-3)
        ZUU(I)=AA*PD(JPD-2)+ACT2*PD(JPD-1)+CC*PD(JPD)
        ZUV(I)=AB*PD(JPD-2)+ADBC*PD(JPD-1)+CD*PD(JPD)
        ZVV(I)=BB*PD(JPD-2)+BDT2*PD(JPD-1)+DD*PD(JPD)
   46 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   47 P00=Z(1)
      P10=ZU(1)
      P01=ZV(1)
      P20=0.5*ZUU(1)
      P11=ZUV(1)
      P02=0.5*ZVV(1)
      H1=Z(2)-P00-P01-P02
      H2=ZV(2)-P01-ZVV(1)
      H3=ZVV(2)-ZVV(1)
      P03= 10.0*H1-4.0*H2+0.5*H3
      P04=-15.0*H1+7.0*H2    -H3
      P05=  6.0*H1-3.0*H2+0.5*H3
      H1=ZU(2)-P10-P11
      H2=ZUV(2)-P11
      P12= 3.0*H1-H2
      P13=-2.0*H1+H2
      P21=0.0
      P23=-ZUU(2)+ZUU(1)
      P22=-1.5*P23
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   50 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C EVALUATES THE POLYNOMIAL.
   51 P0=P00+V*(P01+V*(P02+V*(P03+V*(P04+V*P05))))
      P1=P10+V*(P11+V*(P12+V*P13))
      P2=P20+V*(P21+V*(P22+V*P23))
      ZII=P0+U*(P1+U*P2)
      RETURN
C CALCULATION OF ZII BY EXTRAPOLATION IN THE TRIANGLE.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   60 IF(IT0.EQ.ITPV)     GO TO 70
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE VERTEX
C OF THE TRIANGLE.
   61 JIPL=3*IL2-2
      IDP=IPL(JIPL)
      X(1)=XD(IDP)
      Y(1)=YD(IDP)
      Z(1)=ZD(IDP)
      JPDD=5*(IDP-1)
      DO 62  KPD=1,5
        JPDD=JPDD+1
        PD(KPD)=PDD(JPDD)
   62 CONTINUE
C CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL.
   67 P00=Z(1)
      P10=PD(1)
      P01=PD(2)
      P20=0.5*PD(3)
      P11=PD(4)
      P02=0.5*PD(5)
      ITPV=IT0
C CONVERTS XII AND YII TO U-V SYSTEM.
   70 U=XII-X(1)
      V=YII-Y(1)
C EVALUATES THE POLYNOMIAL.
   71 P0=P00+V*(P01+V*P02)
      P1=P10+V*P11
      ZII=P0+U*(P1+U*P20)
      RETURN
      END

      SUBROUTINE  IDPTLI(XD,YD,ZD,NDP,NT,IPT,NL,IPL,ITI,XII,YII,ZII,
     1                   MISSII)                                        ID010191
C THIS SUBROUTINE PERFORMS LINEAR PUNCTUAL INTERPOLATION,
C I.E., DETERMINES THE Z VALUE AT A POINT.
C THE INPUT PARAMETERS ARE
C     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
C           Y, AND Z COORDINATES OF THE DATA POINTS, WHERE
C           NDP IS THE NUMBER OF THE DATA POINTS,
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE
C           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE
C           POINT NUMBERS OF THE END POINTS OF THE BORDER
C           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE
C           NUMBERS,
C     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES
C           THE POINT FOR WHICH INTERPOLATION IS TO BE
C           PERFORMED,
C     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH
C           INTERPOLATION IS TO BE PERFORMED.
C THE OUTPUT PARAMETERS ARE
C     ZII = INTERPOLATED Z VALUE.
C     MISSII = LOCIGAL INDICATING MISSING VALUE
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      LOGICAL MISSII
      DIMENSION   XD(NDP),YD(NDP),ZD(NDP),IPT(3*NT),IPL(3*NL)
      COMMON/IDPI/ITPV
      DIMENSION   X(3),Y(3),Z(3)
      EQUIVALENCE (P5,P50)
C PRELIMINARY PROCESSING
   10 IT0=ITI
      NTL=NT+NL
      IF(IT0.LE.NTL)      GO TO 20
      GO TO 40
C CALCULATION OF ZII BY INTERPOLATION.
C CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED.
   20 IF(IT0.EQ.ITPV)     GO TO 30
C LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE
C VERTEXES.
   21 JIPT=3*(IT0-1)
      JPD=0
      DO 23  I=1,3
        JIPT=JIPT+1
        IDP=IPT(JIPT)
        X(I)=XD(IDP)
        Y(I)=YD(IDP)
        Z(I)=ZD(IDP)
   23 CONTINUE
C DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
C TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
C AND VICE VERSA.
   24 X0=X(1)
      Y0=Y(1)
      A=X(2)-X0
      B=X(3)-X0
      C=Y(2)-Y0
      D=Y(3)-Y0
      AD=A*D
      BC=B*C
      DLT=AD-BC
      AP= D/DLT
      BP=-B/DLT
      CP=-C/DLT
      DP= A/DLT
C CONVERTS XII AND YII TO U-V SYSTEM.
   30 DX=XII-X0
      DY=YII-Y0
      U=AP*DX+BP*DY
      V=CP*DX+DP*DY
C EVALUATES THE INTERPOLATED PLANE
C ACCORDING TO
C |  U   V  ZII-Z1 |
C |  1   0   Z2-Z1 | = 0
C |  0   1   Z3-Z1 |
C
      ZII=Z(1)+U*(Z(2)-Z(1))+V*(Z(3)-Z(1))
      MISSII=.FALSE.
      RETURN
C NO EXTRAPOLATION! 
 40   ZII=0
      MISSII=.TRUE.
      RETURN
      END

      SUBROUTINE  IDSFFT(MD,NCP,NDP,XD,YD,ZD,NXI,NYI,XI,YI,ZI,          ID013070
     1                   IWK,WK,MISSI)
C THIS SUBROUTINE PERFORMS SMOOTH SURFACE FITTING WHEN THE PRO-
C JECTIONS OF THE DATA POINTS IN THE X-Y PLANE ARE IRREGULARLY
C DISTRIBUTED IN THE PLANE.
C THE INPUT PARAMETERS ARE
C     MD  = MODE OF COMPUTATION (MUST BE 1, 2, OR 3),
C         = 1 FOR NEW NCP AND/OR NEW XD-YD,
C         = 2 FOR OLD NCP, OLD XD-YD, NEW XI-YI,
C         = 3 FOR OLD NCP, OLD XD-YD, OLD XI-YI,
C     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI-
C           MATING PARTIAL DERIVATIVES AT EACH DATA POINT
C           (MUST BE 2 OR GREATER, BUT SMALLER THAN NDP),
C     NDP = NUMBER OF DATA POINTS (MUST BE 4 OR GREATER),
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE X
C           COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE Y
C           COORDINATES OF THE DATA POINTS,
C     ZD  = ARRAY OF DIMENSION NDP CONTAINING THE Z
C           COORDINATES OF THE DATA POINTS,
C     NXI = NUMBER OF OUTPUT GRID POINTS IN THE X COORDINATE
C           (MUST BE 1 OR GREATER),
C     NYI = NUMBER OF OUTPUT GRID POINTS IN THE Y COORDINATE
C           (MUST BE 1 OR GREATER),
C     XI  = ARRAY OF DIMENSION NXI CONTAINING THE X
C           COORDINATES OF THE OUTPUT GRID POINTS,
C     YI  = ARRAY OF DIMENSION NYI CONTAINING THE Y
C           COORDINATES OF THE OUTPUT GRID POINTS.
C THE OUTPUT PARAMETER IS
C     ZI  = DOUBLY-DIMENSIONED ARRAY OF DIMENSION (NXI,NYI),
C           WHERE THE INTERPOLATED Z VALUES AT THE OUTPUT
C           GRID POINTS ARE TO BE STORED.
C THE OTHER PARAMETERS ARE
C     IWK = INTEGER ARRAY OF DIMENSION
C              MAX0(31,27+NCP)*NDP+NXI*NYI
C           USED INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION 5*NDP USED INTERNALLY AS A
C           WORK AREA.
C     MISSI = LOCICAL ARRAY, INDICATING IF EXTRAPOLATION OR MISSING VALUES
C            OUTSIDE CONVEX HULL WANTED
C THE VERY FIRST CALL TO THIS SUBROUTINE AND THE CALL WITH A NEW
C NCP VALUE, A NEW NDP VALUE, AND/OR NEW CONTENTS OF THE XD AND
C YD ARRAYS MUST BE MADE WITH MD=1.  THE CALL WITH MD=2 MUST BE
C PRECEDED BY ANOTHER CALL WITH THE SAME NCP AND NDP VALUES AND
C WITH THE SAME CONTENTS OF THE XD AND YD ARRAYS.  THE CALL WITH
C MD=3 MUST BE PRECEDED BY ANOTHER CALL WITH THE SAME NCP, NDP,
C NXI, AND NYI VALUES AND WITH THE SAME CONTENTS OF THE XD, YD,
C XI, AND YI ARRAYS.  BETWEEN THE CALL WITH MD=2 OR MD=3 AND ITS
C PRECEDING CALL, THE IWK AND WK ARRAYS MUST NOT BE DISTURBED.
C USE OF A VALUE BETWEEN 3 AND 5 (INCLUSIVE) FOR NCP IS RECOM-
C MENDED UNLESS THERE ARE EVIDENCES THAT DICTATE OTHERWISE.
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,
C THEREFORE, SYSTEM DEPENDENT.
C THIS SUBROUTINE CALLS THE IDCLDP, IDGRID, IDPDRV, IDPTIP, AND
C IDTANG SUBROUTINES.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      LOGICAL MISSI, LINEAR
      DIMENSION   XD(NDP),YD(NDP),ZD(NDP),XI(NXI),YI(NYI),
     1            ZI(NXI*NYI),MISSI(NXI*NYI),IWK((31+NCP)*NDP+NXI*NYI),
     2            WK(5*NDP)
      COMMON/IDPI/ITPV
      DATA  LUN/6/
      LINEAR=.FALSE.
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NCP0=NCP
      NDP0=NDP
      NXI0=NXI
      NYI0=NYI
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF(MD0.LT.1.OR.MD0.GT.3)           GO TO 90
      IF(NCP0.EQ.0) THEN
         LINEAR=.TRUE.
         DO 21 I=1,NXI*NYI
            MISSI(I)=.TRUE.
   21    CONTINUE
      END IF
      IF(NCP0.EQ.1.OR.NCP0.GE.NDP0)      GO TO 90
      IF(NDP0.LT.4)                      GO TO 90
      IF(NXI0.LT.1.OR.NYI0.LT.1)         GO TO 90
      IF(MD0.GE.2)        GO TO 23
      IWK(1)=NCP0
      IWK(2)=NDP0
      GO TO 24
   23 NCPPV=IWK(1)
      NDPPV=IWK(2)
      IF(NCP0.NE.NCPPV)   GO TO 90
      IF(NDP0.NE.NDPPV)   GO TO 90
   24 IF(MD0.GE.3)        GO TO 25
      IWK(3)=NXI0
      IWK(4)=NYI0
      GO TO 30
   25 NXIPV=IWK(3)
      NYIPV=IWK(4)
      IF(NXI0.NE.NXIPV)   GO TO 90
      IF(NYI0.NE.NYIPV)   GO TO 90
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWNGP0=JWIWL-1
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIPC=27*NDP0+1
      JWIGP0=MAX0(31,27+NCP0)*NDP0
C TRIANGULATES THE X-Y PLANE.  (FOR MD=1)
   40 IF(MD0.GT.1)   GO TO 50
      CALL IDTANG(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            IWK(JWIWL),IWK(JWIWP),WK)
      IWK(5)=NT
      IWK(6)=NL
      IF(NT.EQ.0)    RETURN
C DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.  (FOR MD=1)
   50 IF(MD0.GT.1 .OR. LINEAR)   GO TO 60
      CALL IDCLDP(NDP0,XD,YD,NCP0,IWK(JWIPC))
      IF(IWK(JWIPC).EQ.0)      RETURN
C SORTS OUTPUT GRID POINTS IN ASCENDING ORDER OF THE TRIANGLE
C NUMBER AND THE BORDER LINE SEGMENT NUMBER.  (FOR MD=1,2)
   60 IF(MD0.EQ.3)   GO TO 70
      CALL IDGRID(XD,YD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),NXI0,NYI0,
     1            XI,YI,IWK(JWNGP0+1),IWK(JWIGP0+1))
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   70 IF (.NOT.LINEAR) CALL IDPDRV(NDP0,XD,YD,ZD,NCP0,IWK(JWIPC),WK)
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   80 ITPV=0
      JIG0MX=0
      JIG1MN=NXI0*NYI0+1
      NNGP=NT+2*NL
      DO 89  JNGP=1,NNGP
        ITI=JNGP
        IF(JNGP.LE.NT)    GO TO 81
        IL1=(JNGP-NT+1)/2
        IL2=(JNGP-NT+2)/2
        IF(IL2.GT.NL)     IL2=1
        ITI=IL1*(NT+NL)+IL2
   81   JWNGP=JWNGP0+JNGP
        NGP0=IWK(JWNGP)
        IF(NGP0.EQ.0)     GO TO 86
        JIG0MN=JIG0MX+1
        JIG0MX=JIG0MX+NGP0
        DO 82  JIGP=JIG0MN,JIG0MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          IF (LINEAR) THEN
          CALL IDPTLI(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1                ITI,XI(IXI),YI(IYI),ZI(IZI),MISSI(IZI))
          ELSE
          CALL IDPTIP(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1                ITI,XI(IXI),YI(IYI),ZI(IZI),MISSI(IZI))
          END IF          
   82   CONTINUE
   86   JWNGP=JWNGP0+2*NNGP+1-JNGP
        NGP1=IWK(JWNGP)
        IF(NGP1.EQ.0)     GO TO 89
        JIG1MX=JIG1MN-1
        JIG1MN=JIG1MN-NGP1
        DO 87  JIGP=JIG1MN,JIG1MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          IF (LINEAR) THEN
          CALL IDPTLI(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1                ITI,XI(IXI),YI(IYI),ZI(IZI),MISSI(IZI))
          ELSE
          CALL IDPTIP(XD,YD,ZD,NDP,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1                ITI,XI(IXI),YI(IYI),ZI(IZI),MISSI(IZI))
          END IF
   87   CONTINUE
   89 CONTINUE
      RETURN
C ERROR EXIT
   90 WRITE (LUN,2090) MD0,NCP0,NDP0,NXI0,NYI0
      RETURN
C FORMAT STATEMENT FOR ERROR MESSAGE
 2090 FORMAT(1X/41H ***   IMPROPER INPUT PARAMETER VALUE(S)./
     1   7H   MD =,I4,10X,5HNCP =,I6,10X,5HNDP =,I6,
     2   10X,5HNXI =,I6,10X,5HNYI =,I6/
     3   35H ERROR DETECTED IN ROUTINE   IDSFFT/)
      END

      SUBROUTINE  IDTANG(NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)            ID014770
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
C CORRESPONDING TO THE BORDER LINE SEGMENTS.
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,
C THEREFORE, SYSTEM DEPENDENT.
C THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           X COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           Y COORDINATES OF THE DATA POINTS.
C THE OUTPUT PARAMETERS ARE
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE
C           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH
C           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND,
C           (3*IT-1)ST, AND (3*IT)TH ELEMENTS,
C           IT=1,2,...,NT,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE
C           POINT NUMBERS OF THE END POINTS OF THE (IL)TH
C           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE
C           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,
C           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,
C           IL=1,2,..., NL.
C THE OTHER PARAMETERS ARE
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
C           INTERNALLY AS A WORK AREA,
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
C           INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   XD(NDP),YD(NDP),IPT(6*NDP-15),IPL(6*NDP),
     1            IWL(18*NDP),IWP(NDP),WK(NDP)
      DIMENSION   ITF(2)
      DATA  RATIO/1.0E-6/, NREP/100/, LUN/6/
C STATEMENT FUNCTIONS
      DSQF(U1,V1,U2,V2)=(U2-U1)**2+(V2-V1)**2
      SIDE(U1,V1,U2,V2,U3,V3)=(V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)
C PRELIMINARY PROCESSING
   10 NDP0=NDP
      NDPM1=NDP0-1
      IF(NDP0.LT.4)       GO TO 90
C DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.
   20 DSQMN=DSQF(XD(1),YD(1),XD(2),YD(2))
      IPMN1=1
      IPMN2=2
      DO 22  IP1=1,NDPM1
        X1=XD(IP1)
        Y1=YD(IP1)
        IP1P1=IP1+1
        DO 21  IP2=IP1P1,NDP0
          DSQI=DSQF(X1,Y1,XD(IP2),YD(IP2))
          IF(DSQI.EQ.0.0)      GO TO 91
          IF(DSQI.GE.DSQMN)    GO TO 21
          DSQMN=DSQI
          IPMN1=IP1
          IPMN2=IP2
   21   CONTINUE
   22 CONTINUE
      DSQ12=DSQMN
      XDMP=(XD(IPMN1)+XD(IPMN2))/2.0
      YDMP=(YD(IPMN1)+YD(IPMN2))/2.0
C SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF
C DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT
C NUMBERS IN THE IWP ARRAY.
   30 JP1=2
      DO 31  IP1=1,NDP0
        IF(IP1.EQ.IPMN1.OR.IP1.EQ.IPMN2)      GO TO 31
        JP1=JP1+1
        IWP(JP1)=IP1
        WK(JP1)=DSQF(XDMP,YDMP,XD(IP1),YD(IP1))
   31 CONTINUE
      DO 33  JP1=3,NDPM1
        DSQMN=WK(JP1)
        JPMN=JP1
        DO 32  JP2=JP1,NDP0
          IF(WK(JP2).GE.DSQMN)      GO TO 32
          DSQMN=WK(JP2)
          JPMN=JP2
   32   CONTINUE
        ITS=IWP(JP1)
        IWP(JP1)=IWP(JPMN)
        IWP(JPMN)=ITS
        WK(JPMN)=WK(JP1)
   33 CONTINUE
C IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE
C FIRST THREE DATA POINTS ARE NOT COLLINEAR.
   35 AR=DSQ12*RATIO
      X1=XD(IPMN1)
      Y1=YD(IPMN1)
      DX21=XD(IPMN2)-X1
      DY21=YD(IPMN2)-Y1
      DO 36  JP=3,NDP0
        IP=IWP(JP)
        IF(ABS((YD(IP)-Y1)*DX21-(XD(IP)-X1)*DY21).GT.AR)
     1               GO TO 37
   36 CONTINUE
      GO TO 92
   37 IF(JP.EQ.3)    GO TO 40
      JPMX=JP
      JP=JPMX+1
      DO 38  JPC=4,JPMX
        JP=JP-1
        IWP(JP)=IWP(JP-1)
   38 CONTINUE
      IWP(3)=IP
C FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-
C TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-
C BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN
C THE IPL ARRAY.
   40 IP1=IPMN1
      IP2=IPMN2
      IP3=IWP(3)
      IF(SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     1     .GE.0.0)       GO TO 41
      IP1=IPMN2
      IP2=IPMN1
   41 NT0=1
      NTT3=3
      IPT(1)=IP1
      IPT(2)=IP2
      IPT(3)=IP3
      NL0=3
      NLT3=9
      IPL(1)=IP1
      IPL(2)=IP2
      IPL(3)=1
      IPL(4)=IP2
      IPL(5)=IP3
      IPL(6)=1
      IPL(7)=IP3
      IPL(8)=IP1
      IPL(9)=1
C ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.
   50 DO 79  JP1=4,NDP0
        IP1=IWP(JP1)
        X1=XD(IP1)
        Y1=YD(IP1)
C - DETERMINES THE VISIBLE BORDER LINE SEGMENTS.
        IP2=IPL(1)
        JPMN=1
        DXMN=XD(IP2)-X1
        DYMN=YD(IP2)-Y1
        DSQMN=DXMN**2+DYMN**2
        ARMN=DSQMN*RATIO
        JPMX=1
        DXMX=DXMN
        DYMX=DYMN
        DSQMX=DSQMN
        ARMX=ARMN
        DO 52  JP2=2,NL0
          IP2=IPL(3*JP2-2)
          DX=XD(IP2)-X1
          DY=YD(IP2)-Y1
          AR=DY*DXMN-DX*DYMN
          IF(AR.GT.ARMN)       GO TO 51
          DSQI=DX**2+DY**2
          IF(AR.GE.(-ARMN).AND.DSQI.GE.DSQMN)      GO TO 51
          JPMN=JP2
          DXMN=DX
          DYMN=DY
          DSQMN=DSQI
          ARMN=DSQMN*RATIO
   51     AR=DY*DXMX-DX*DYMX
          IF(AR.LT.(-ARMX))    GO TO 52
          DSQI=DX**2+DY**2
          IF(AR.LE.ARMX.AND.DSQI.GE.DSQMX)    GO TO 52
          JPMX=JP2
          DXMX=DX
          DYMX=DY
          DSQMX=DSQI
          ARMX=DSQMX*RATIO
   52   CONTINUE
        IF(JPMX.LT.JPMN)  JPMX=JPMX+NL0
        NSH=JPMN-1
        IF(NSH.LE.0)      GO TO 60
C - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER
C - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.
        NSHT3=NSH*3
        DO 53  JP2T3=3,NSHT3,3
          JP3T3=JP2T3+NLT3
          IPL(JP3T3-2)=IPL(JP2T3-2)
          IPL(JP3T3-1)=IPL(JP2T3-1)
          IPL(JP3T3)  =IPL(JP2T3)
   53   CONTINUE
        DO 54  JP2T3=3,NLT3,3
          JP3T3=JP2T3+NSHT3
          IPL(JP2T3-2)=IPL(JP3T3-2)
          IPL(JP2T3-1)=IPL(JP3T3-1)
          IPL(JP2T3)  =IPL(JP3T3)
   54   CONTINUE
        JPMX=JPMX-NSH
C - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE
C - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER
C - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.
   60   JWL=0
        DO 64  JP2=JPMX,NL0
          JP2T3=JP2*3
          IPL1=IPL(JP2T3-2)
          IPL2=IPL(JP2T3-1)
          IT  =IPL(JP2T3)
C - - ADDS A TRIANGLE TO THE IPT ARRAY.
          NT0=NT0+1
          NTT3=NTT3+3
          IPT(NTT3-2)=IPL2
          IPT(NTT3-1)=IPL1
          IPT(NTT3)  =IP1
C - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.
          IF(JP2.NE.JPMX)      GO TO 61
          IPL(JP2T3-1)=IP1
          IPL(JP2T3)  =NT0
   61     IF(JP2.NE.NL0)       GO TO 62
          NLN=JPMX+1
          NLNT3=NLN*3
          IPL(NLNT3-2)=IP1
          IPL(NLNT3-1)=IPL(1)
          IPL(NLNT3)  =NT0
C - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER
C - - LINE SEGMENTS.
   62     ITT3=IT*3
          IPTI=IPT(ITT3-2)
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63
          IPTI=IPT(ITT3-1)
          IF(IPTI.NE.IPL1.AND.IPTI.NE.IPL2)   GO TO 63
          IPTI=IPT(ITT3)
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
   63     IF(IDXCHG(XD,YD,NDP,IP1,IPTI,IPL1,IPL2).EQ.0)     GO TO 64
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
          IPT(ITT3-2)=IPTI
          IPT(ITT3-1)=IPL1
          IPT(ITT3)  =IP1
          IPT(NTT3-1)=IPTI
          IF(JP2.EQ.JPMX)      IPL(JP2T3)=IT
          IF(JP2.EQ.NL0.AND.IPL(3).EQ.IT)     IPL(3)=NT0
C - - SETS FLAGS IN THE IWL ARRAY.
          JWL=JWL+4
          IWL(JWL-3)=IPL1
          IWL(JWL-2)=IPTI
          IWL(JWL-1)=IPTI
          IWL(JWL)  =IPL2
   64   CONTINUE
        NL0=NLN
        NLT3=NLNT3
        NLF=JWL/2
        IF(NLF.EQ.0)      GO TO 79
C - IMPROVES TRIANGULATION.
   70   NTT3P3=NTT3+3
        DO 78  IREP=1,NREP
          DO 76  ILF=1,NLF
            ILFT2=ILF*2
            IPL1=IWL(ILFT2-1)
            IPL2=IWL(ILFT2)
C - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
C - - THE FLAGGED LINE SEGMENT.
            NTF=0
            DO 71  ITT3R=3,NTT3,3
              ITT3=NTT3P3-ITT3R
              IPT1=IPT(ITT3-2)
              IPT2=IPT(ITT3-1)
              IPT3=IPT(ITT3)
              IF(IPL1.NE.IPT1.AND.IPL1.NE.IPT2.AND.
     1           IPL1.NE.IPT3)      GO TO 71
              IF(IPL2.NE.IPT1.AND.IPL2.NE.IPT2.AND.
     1           IPL2.NE.IPT3)      GO TO 71
              NTF=NTF+1
              ITF(NTF)=ITT3/3
              IF(NTF.EQ.2)     GO TO 72
   71       CONTINUE
            IF(NTF.LT.2)       GO TO 76
C - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE
C - - ON THE LINE SEGMENT.
   72       IT1T3=ITF(1)*3
            IPTI1=IPT(IT1T3-2)
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73
            IPTI1=IPT(IT1T3-1)
            IF(IPTI1.NE.IPL1.AND.IPTI1.NE.IPL2)    GO TO 73
            IPTI1=IPT(IT1T3)
   73       IT2T3=ITF(2)*3
            IPTI2=IPT(IT2T3-2)
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74
            IPTI2=IPT(IT2T3-1)
            IF(IPTI2.NE.IPL1.AND.IPTI2.NE.IPL2)    GO TO 74
            IPTI2=IPT(IT2T3)
C - - CHECKS IF THE EXCHANGE IS NECESSARY.
   74       IF(IDXCHG(XD,YD,NDP,IPTI1,IPTI2,IPL1,IPL2).EQ.0)
     1         GO TO 76
C - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
            IPT(IT1T3-2)=IPTI1
            IPT(IT1T3-1)=IPTI2
            IPT(IT1T3)  =IPL1
            IPT(IT2T3-2)=IPTI2
            IPT(IT2T3-1)=IPTI1
            IPT(IT2T3)  =IPL2
C - - SETS NEW FLAGS.
            JWL=JWL+8
            IWL(JWL-7)=IPL1
            IWL(JWL-6)=IPTI1
            IWL(JWL-5)=IPTI1
            IWL(JWL-4)=IPL2
            IWL(JWL-3)=IPL2
            IWL(JWL-2)=IPTI2
            IWL(JWL-1)=IPTI2
            IWL(JWL)  =IPL1
            DO 75  JLT3=3,NLT3,3
              IPLJ1=IPL(JLT3-2)
              IPLJ2=IPL(JLT3-1)
              IF((IPLJ1.EQ.IPL1.AND.IPLJ2.EQ.IPTI2).OR.
     1           (IPLJ2.EQ.IPL1.AND.IPLJ1.EQ.IPTI2))
     2                         IPL(JLT3)=ITF(1)
              IF((IPLJ1.EQ.IPL2.AND.IPLJ2.EQ.IPTI1).OR.
     1           (IPLJ2.EQ.IPL2.AND.IPLJ1.EQ.IPTI1))
     2                         IPL(JLT3)=ITF(2)
   75       CONTINUE
   76     CONTINUE
          NLFC=NLF
          NLF=JWL/2
          IF(NLF.EQ.NLFC)      GO TO 79
C - - RESETS THE IWL ARRAY FOR THE NEXT ROUND.
          JWL=0
          JWL1MN=(NLFC+1)*2
          NLFT2=NLF*2
          DO 77  JWL1=JWL1MN,NLFT2,2
            JWL=JWL+2
            IWL(JWL-1)=IWL(JWL1-1)
            IWL(JWL)  =IWL(JWL1)
   77     CONTINUE
          NLF=JWL/2
   78   CONTINUE
   79 CONTINUE
C REARRANGES THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.
   80 DO 81  ITT3=3,NTT3,3
        IP1=IPT(ITT3-2)
        IP2=IPT(ITT3-1)
        IP3=IPT(ITT3)
        IF(SIDE(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),YD(IP3))
     1       .GE.0.0)     GO TO 81
        IPT(ITT3-2)=IP2
        IPT(ITT3-1)=IP1
   81 CONTINUE
      NT=NT0
      NL=NL0
      RETURN
C ERROR EXIT
   90 WRITE (LUN,2090)  NDP0
      GO TO 93
   91 WRITE (LUN,2091)  NDP0,IP1,IP2,X1,Y1
      GO TO 93
   92 WRITE (LUN,2092)  NDP0
   93 WRITE (LUN,2093)
      NT=0
      RETURN
C FORMAT STATEMENTS
 2090 FORMAT(1X/23H ***   NDP LESS THAN 4./8H   NDP =,I5)
 2091 FORMAT(1X/29H ***   IDENTICAL DATA POINTS./
     1   8H   NDP =,I5,5X,5HIP1 =,I5,5X,5HIP2 =,I5,
     2   5X,4HXD =,E12.4,5X,4HYD =,E12.4)
 2092 FORMAT(1X/33H ***   ALL COLLINEAR DATA POINTS./
     1   8H   NDP =,I5)
 2093 FORMAT(35H ERROR DETECTED IN ROUTINE   IDTANG/)
      END

      FUNCTION  IDXCHG(X,Y,NDP,I1,I2,I3,I4)                                 ID018560
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
C BY C. L. LAWSON.
C THE INPUT PARAMETERS ARE
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
C           POINTS,
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
C           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3
C           AND P4 CONNECTED DIAGONALLY.
C THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-
C CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.
C DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   X(NDP),Y(NDP)
      EQUIVALENCE (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),
     1            (A4SQ,B1SQ),(B4SQ,A2SQ),(C4SQ,C3SQ)
C PRELIMINARY PROCESSING
   10 X1=X(I1)
      Y1=Y(I1)
      X2=X(I2)
      Y2=Y(I2)
      X3=X(I3)
      Y3=Y(I3)
      X4=X(I4)
      Y4=Y(I4)
C CALCULATION
   20 IDX=0
      U3=(Y2-Y3)*(X1-X3)-(X2-X3)*(Y1-Y3)
      U4=(Y1-Y4)*(X2-X4)-(X1-X4)*(Y2-Y4)
      IF(U3*U4.LE.0.0)    GO TO 30
      U1=(Y3-Y1)*(X4-X1)-(X3-X1)*(Y4-Y1)
      U2=(Y4-Y2)*(X3-X2)-(X4-X2)*(Y3-Y2)
      A1SQ=(X1-X3)**2+(Y1-Y3)**2
      B1SQ=(X4-X1)**2+(Y4-Y1)**2
      C1SQ=(X3-X4)**2+(Y3-Y4)**2
      A2SQ=(X2-X4)**2+(Y2-Y4)**2
      B2SQ=(X3-X2)**2+(Y3-Y2)**2
      C3SQ=(X2-X1)**2+(Y2-Y1)**2
      S1SQ=U1*U1/(C1SQ*DMAX1(A1SQ,B1SQ))
      S2SQ=U2*U2/(C2SQ*DMAX1(A2SQ,B2SQ))
      S3SQ=U3*U3/(C3SQ*DMAX1(A3SQ,B3SQ))
      S4SQ=U4*U4/(C4SQ*DMAX1(A4SQ,B4SQ))
      IF(DMIN1(S1SQ,S2SQ).LT.DMIN1(S3SQ,S4SQ))     IDX=1
   30 IDXCHG=IDX
      RETURN
      END
C      ALGORITHM 751, COLLECTED ALGORITHMS FROM ACM.
C      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
C      VOL. 22, NO. 1, March, 1996, P.  1--8.
C      ####### With remark from renka (to appear) 4/dec/1998
C
C      modifications for R:
C        REAL -> DOUBLE PRECISION  albrecht.gebhardt@uni-klu.ac.at
C
      SUBROUTINE ADDCST (NCC,LCC,N,X,Y, LWK,IWK,LIST,LPTR,
     .                   LEND, IER)
      INTEGER NCC, LCC(*), N, LWK, IWK(LWK), LIST(*),
     .        LPTR(*), LEND(N), IER
      DOUBLE PRECISION X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/12/94
C
C   This subroutine provides for creation of a constrained
C Delaunay triangulation which, in some sense, covers an
C arbitrary connected region R rather than the convex hull
C of the nodes.  This is achieved simply by forcing the
C presence of certain adjacencies (triangulation arcs) cor-
C responding to constraint curves.  The union of triangles
C coincides with the convex hull of the nodes, but triangles
C in R can be distinguished from those outside of R.  The
C only modification required to generalize the definition of
C the Delaunay triangulation is replacement of property 5
C (refer to TRMESH) by the following:
C
C  5')  If a node is contained in the interior of the cir-
C       cumcircle of a triangle, then every interior point
C       of the triangle is separated from the node by a
C       constraint arc.
C
C   In order to be explicit, we make the following defini-
C tions.  A constraint region is the open interior of a
C simple closed positively oriented polygonal curve defined
C by an ordered sequence of three or more distinct nodes
C (constraint nodes) P(1),P(2),...,P(K), such that P(I) is
C adjacent to P(I+1) for I = 1,...,K with P(K+1) = P(1).
C Thus, the constraint region is on the left (and may have
C nonfinite area) as the sequence of constraint nodes is
C traversed in the specified order.  The constraint regions
C must not contain nodes and must not overlap.  The region
C R is the convex hull of the nodes with constraint regions
C excluded.
C
C   Note that the terms boundary node and boundary arc are
C reserved for nodes and arcs on the boundary of the convex
C hull of the nodes.
C
C   The algorithm is as follows:  given a triangulation
C which includes one or more sets of constraint nodes, the
C corresponding adjacencies (constraint arcs) are forced to
C be present (Subroutine EDGE).  Any additional new arcs
C required are chosen to be locally optimal (satisfy the
C modified circumcircle property).
C
C
C On input:
C
C       NCC = Number of constraint curves (constraint re-
C             gions).  NCC .GE. 0.
C
C       LCC = Array of length NCC (or dummy array of length
C             1 if NCC = 0) containing the index (for X, Y,
C             and LEND) of the first node of constraint I in
C             LCC(I) for I = 1 to NCC.  Thus, constraint I
C             contains K = LCC(I+1) - LCC(I) nodes, K .GE.
C             3, stored in (X,Y) locations LCC(I), ...,
C             LCC(I+1)-1, where LCC(NCC+1) = N+1.
C
C       N = Number of nodes in the triangulation, including
C           constraint nodes.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes with non-constraint nodes in the
C             first LCC(1)-1 locations, followed by NCC se-
C             quences of constraint nodes.  Only one of
C             these sequences may be specified in clockwise
C             order to represent an exterior constraint
C             curve (a constraint region with nonfinite
C             area).
C
C The above parameters are not altered by this routine.
C
C       LWK = Length of IWK.  This must be at least 2*NI
C             where NI is the maximum number of arcs which
C             intersect a constraint arc to be added.  NI
C             is bounded by N-3.
C
C       IWK = Integer work array of length LWK (used by
C             Subroutine EDGE to add constraint arcs).
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LWK = Required length of IWK unless IER = 1 or IER =
C             3.  In the case of IER = 1, LWK is not altered
C             from its input value.
C
C       IWK = Array containing the endpoint indexes of the
C             new arcs which were swapped in by the last
C             call to Subroutine EDGE.
C
C       LIST,LPTR,LEND = Triangulation data structure with
C                        all constraint arcs present unless
C                        IER .NE. 0.  These arrays are not
C                        altered if IER = 1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if NCC, N, or an LCC entry is outside
C                     its valid range, or LWK .LT. 0 on
C                     input.
C             IER = 2 if more space is required in IWK.
C             IER = 3 if the triangulation data structure is
C                     invalid, or failure (in EDGE or OPTIM)
C                     was caused by collinear nodes on the
C                     convex hull boundary.  An error mes-
C                     sage is written to logical unit 6 in
C                     this case.
C             IER = 4 if intersecting constraint arcs were
C                     encountered.
C             IER = 5 if a constraint region contains a
C                     node.
C
C Modules required by ADDCST:  EDGE, LEFT, LSTPTR, OPTIM,
C                                SWAP, SWPTST
C
C Intrinsic functions called by ADDCST:  ABS, MAX
C
C***********************************************************
C
      INTEGER I, IFRST, ILAST, K, KBAK, KFOR, KN, LCCIP1,
     .        LP, LPB, LPF, LPL, LW, LWD2, N1, N2
      LWD2 = LWK/2
C
C Test for errors in input parameters.
C
      IER = 1
      IF (NCC .LT. 0  .OR.  LWK .LT. 0) RETURN
      IF (NCC .EQ. 0) THEN
        IF (N .LT. 3) RETURN
        LWK = 0
        GO TO 9
      ELSE
        LCCIP1 = N+1
        DO 1 I = NCC,1,-1
          IF (LCCIP1 - LCC(I) .LT. 3) RETURN
          LCCIP1 = LCC(I)
    1     CONTINUE
        IF (LCCIP1 .LT. 1) RETURN
      ENDIF
C
C Force the presence of constraint arcs.  The outer loop is
C   on constraints in reverse order.  IFRST and ILAST are
C   the first and last nodes of constraint I.
C
      LWK = 0
      IFRST = N+1
      DO 3 I = NCC,1,-1
        ILAST = IFRST - 1
        IFRST = LCC(I)
C
C   Inner loop on constraint arcs N1-N2 in constraint I.
C
        N1 = ILAST
        DO 2 N2 = IFRST,ILAST
          LW = LWD2
          CALL EDGE (N1,N2,X,Y, LW,IWK,LIST,LPTR,LEND, IER)
          LWK = MAX(LWK,2*LW)
          IF (IER .EQ. 4) IER = 3
          IF (IER .NE. 0) RETURN
          N1 = N2
    2     CONTINUE
    3   CONTINUE
C
C Test for errors.  The outer loop is on constraint I with
C   first and last nodes IFRST and ILAST, and the inner loop
C   is on constraint nodes K with (KBAK,K,KFOR) a subse-
C   quence of constraint I.
C
      IER = 4
      IFRST = N+1
      DO 8 I = NCC,1,-1
        ILAST = IFRST - 1
        IFRST = LCC(I)
        KBAK = ILAST
        DO 7 K = IFRST,ILAST
          KFOR = K + 1
          IF (K .EQ. ILAST) KFOR = IFRST
C
C   Find the LIST pointers LPF and LPB of KFOR and KBAK as
C     neighbors of K.
C
          LPF = 0
          LPB = 0
          LPL = LEND(K)
          LP = LPL
C
    4     LP = LPTR(LP)
            KN = ABS(LIST(LP))
            IF (KN .EQ. KFOR) LPF = LP
            IF (KN .EQ. KBAK) LPB = LP
            IF (LP .NE. LPL) GO TO 4
C
C   A pair of intersecting constraint arcs was encountered
C     if and only if a constraint arc is missing (introduc-
C     tion of the second caused the first to be swapped out).
C
          IF (LPF .EQ. 0  .OR.  LPB .EQ. 0) RETURN
C
C   Loop on neighbors KN of node K which follow KFOR and
C     precede KBAK.  The constraint region contains no nodes
C     if and only if all such nodes KN are in constraint I.
C
          LP = LPF
    5     LP = LPTR(LP)
            IF (LP .EQ. LPB) GO TO 6
            KN = ABS(LIST(LP))
            IF (KN .LT. IFRST  .OR.  KN .GT. ILAST) GO TO 10
            GO TO 5
C
C   Bottom of loop.
C
    6     KBAK = K
    7     CONTINUE
    8   CONTINUE
C
C No errors encountered.
C
    9 IER = 0
      RETURN
C
C A constraint region contains a node.
C
   10 IER = 5
      RETURN
      END
      SUBROUTINE ADDNOD (K,XK,YK,IST,NCC, LCC,N,X,Y,LIST,
     .                   LPTR,LEND,LNEW, IER)
      INTEGER K, IST, NCC, LCC(*), N, LIST(*), LPTR(*),
     .        LEND(*), LNEW, IER
      DOUBLE PRECISION XK, YK, X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/27/98
C
C   Given a triangulation of N nodes in the plane created by
C Subroutine TRMESH or TRMSHR, this subroutine updates the
C data structure with the addition of a new node in position
C K.  If node K is inserted into X and Y (K .LE. N) rather
C than appended (K = N+1), then a corresponding insertion
C must be performed in any additional arrays associated
C with the nodes.  For example, an array of data values Z
C must be shifted down to open up position K for the new
C value:  set Z(I+1) to Z(I) for I = N,N-1,...,K.  For
C optimal efficiency, new nodes should be appended whenever
C possible.  Insertion is necessary, however, to add a non-
C constraint node when constraints are present (refer to
C Subroutine ADDCST).
C
C   Note that a constraint node cannot be added by this
C routine.  In order to insert a constraint node, it is
C necessary to add the node with no constraints present
C (call this routine with NCC = 0), update LCC by increment-
C ing the appropriate entries, and then create (or restore)
C the constraints by a call to ADDCST.
C
C   The algorithm consists of the following steps:  node K
C is located relative to the triangulation (TRFIND), its
C index is added to the data structure (INTADD or BDYADD),
C and a sequence of swaps (SWPTST and SWAP) are applied to
C the arcs opposite K so that all arcs incident on node K
C and opposite node K (excluding constraint arcs) are local-
C ly optimal (satisfy the circumcircle test).  Thus, if a
C (constrained) Delaunay triangulation is input, a (con-
C strained) Delaunay triangulation will result.  All indexes
C are incremented as necessary for an insertion.
C
C
C On input:
C
C       K = Nodal index (index for X, Y, and LEND) of the
C           new node to be added.  1 .LE. K .LE. LCC(1).
C           (K .LE. N+1 if NCC=0).
C
C       XK,YK = Cartesian coordinates of the new node (to be
C               stored in X(K) and Y(K)).  The node must not
C               lie in a constraint region.
C
C       IST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to node K.  1 .LE. IST .LE. N.
C
C       NCC = Number of constraint curves.  NCC .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             Subroutine ADDCST.
C
C       N = Number of nodes in the triangulation before K is
C           added.  N .GE. 3.  Note that N will be incre-
C           mented following the addition of node K.
C
C       X,Y = Arrays of length at least N+1 containing the
C             Cartesian coordinates of the nodes in the
C             first N positions with non-constraint nodes
C             in the first LCC(1)-1 locations if NCC > 0.
C
C       LIST,LPTR,LEND,LNEW = Data structure associated with
C                             the triangulation of nodes 1
C                             to N.  The arrays must have
C                             sufficient length for N+1
C                             nodes.  Refer to TRMESH.
C
C On output:
C
C       LCC = List of constraint curve starting indexes in-
C             cremented by 1 to reflect the insertion of K
C             unless NCC = 0 or (IER .NE. 0 and IER .NE.
C             -4).
C
C       N = Number of nodes in the triangulation including K
C           unless IER .NE. 0 and IER .NE. -4.  Note that
C           all comments refer to the input value of N.
C
C       X,Y = Arrays updated with the insertion of XK and YK
C             in the K-th positions (node I+1 was node I be-
C             fore the insertion for I = K to N if K .LE. N)
C             unless IER .NE. 0 and IER .NE. -4.
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node K unless
C                             IER .NE. 0 and IER .NE. -4.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if K, IST, NCC, N, or an LCC entry is
C                      outside its valid range on input.
C             IER = -2 if all nodes (including K) are col-
C                      linear.
C             IER =  L if nodes L and K coincide for some L.
C             IER = -3 if K lies in a constraint region.
C             IER = -4 if an error flag is returned by SWAP
C                      implying that the triangulation
C                      (geometry) was bad on input.
C
C             The errors conditions are tested in the order
C             specified.
C
C Modules required by ADDNOD:  BDYADD, CRTRI, INDXCC,
C                                INSERT, INTADD, JRAND,
C                                LEFT, LSTPTR, SWAP,
C                                SWPTST, TRFIND
C
C Intrinsic function called by ADDNOD:  ABS
C
C***********************************************************
C
      INTEGER INDXCC, LSTPTR
      INTEGER I, I1, I2, I3, IBK, IO1, IO2, IN1, KK, L,
     .        LCCIP1, LP, LPF, LPO1, NM1
      LOGICAL CRTRI, SWPTST
      KK = K
C
C Test for an invalid input parameter.
C
      IF (KK .LT. 1  .OR.  IST .LT. 1  .OR.  IST .GT. N
     .    .OR.  NCC .LT. 0  .OR.  N .LT. 3) GO TO 7
      LCCIP1 = N+1
      DO 1 I = NCC,1,-1
        IF (LCCIP1-LCC(I) .LT. 3) GO TO 7
        LCCIP1 = LCC(I)
    1   CONTINUE
      IF (KK .GT. LCCIP1) GO TO 7
C
C Find a triangle (I1,I2,I3) containing K or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from node K.
C
      CALL TRFIND (IST,XK,YK,N,X,Y,LIST,LPTR,LEND, I1,I2,I3)
C
C Test for collinear nodes, duplicate nodes, and K lying in
C   a constraint region.
C
      IF (I1 .EQ. 0) GO TO 8
      IF (I3 .NE. 0) THEN
        L = I1
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        L = I2
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        L = I3
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        IF (NCC .GT. 0  .AND.  CRTRI(NCC,LCC,I1,I2,I3) )
     .    GO TO 10
      ELSE
C
C   K is outside the convex hull of the nodes and lies in a
C     constraint region iff an exterior constraint curve is
C     present.
C
        IF (NCC .GT. 0  .AND.  INDXCC(NCC,LCC,N,LIST,LEND)
     .      .NE. 0) GO TO 10
      ENDIF
C
C No errors encountered.
C
      IER = 0
      NM1 = N
      N = N + 1
      IF (KK .LT. N) THEN
C
C Open a slot for K in X, Y, and LEND, and increment all
C   nodal indexes which are greater than or equal to K.
C   Note that LIST, LPTR, and LNEW are not yet updated with
C   either the neighbors of K or the edges terminating on K.
C
        DO 2 IBK = NM1,KK,-1
          X(IBK+1) = X(IBK)
          Y(IBK+1) = Y(IBK)
          LEND(IBK+1) = LEND(IBK)
    2     CONTINUE
        DO 3 I = 1,NCC
          LCC(I) = LCC(I) + 1
    3     CONTINUE
        L = LNEW - 1
        DO 4 I = 1,L
          IF (LIST(I) .GE. KK) LIST(I) = LIST(I) + 1
          IF (LIST(I) .LE. -KK) LIST(I) = LIST(I) - 1
    4     CONTINUE
        IF (I1 .GE. KK) I1 = I1 + 1
        IF (I2 .GE. KK) I2 = I2 + 1
        IF (I3 .GE. KK) I3 = I3 + 1
      ENDIF
C
C Insert K into X and Y, and update LIST, LPTR, LEND, and
C   LNEW with the arcs containing node K.
C
      X(KK) = XK
      Y(KK) = YK
      IF (I3 .EQ. 0) THEN
        CALL BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
      ELSE
        CALL INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      ENDIF
C
C Initialize variables for optimization of the triangula-
C   tion.
C
      LP = LEND(KK)
      LPF = LPTR(LP)
      IO2 = LIST(LPF)
      LPO1 = LPTR(LPF)
      IO1 = ABS(LIST(LPO1))
C
C Begin loop:  find the node opposite K.
C
    5 LP = LSTPTR(LEND(IO1),IO2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 6
        LP = LPTR(LP)
        IN1 = ABS(LIST(LP))
        IF ( CRTRI(NCC,LCC,IO1,IO2,IN1) ) GO TO 6
C
C Swap test:  if a swap occurs, two new arcs are
C             opposite K and must be tested.
C
        IF ( .NOT. SWPTST(IN1,KK,IO1,IO2,X,Y) ) GO TO 6
        CALL SWAP (IN1,KK,IO1,IO2, LIST,LPTR,LEND, LPO1)
        IF (LPO1 .EQ. 0) GO TO 11
        IO1 = IN1
        GO TO 5
C
C No swap occurred.  Test for termination and reset
C   IO2 and IO1.
C
    6   IF (LPO1 .EQ. LPF  .OR.  LIST(LPO1) .LT. 0) RETURN
        IO2 = IO1
        LPO1 = LPTR(LPO1)
        IO1 = ABS(LIST(LPO1))
        GO TO 5
C
C A parameter is outside its valid range on input.
C
    7 IER = -1
      RETURN
C
C All nodes are collinear.
C
    8 IER = -2
      RETURN
C
C Nodes L and K coincide.
C
    9 IER = L
      RETURN
C
C Node K lies in a constraint region.
C
   10 IER = -3
      RETURN
C
C Zero pointer returned by SWAP.
C
   11 IER = -4
      RETURN
      END
      DOUBLE PRECISION FUNCTION AREAP (X,Y,NB,NODES)
      INTEGER NB, NODES(NB)
      DOUBLE PRECISION X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/21/90
C
C   Given a sequence of NB points in the plane, this func-
C tion computes the signed area bounded by the closed poly-
C gonal curve which passes through the points in the
C specified order.  Each simple closed curve is positively
C oriented (bounds positive area) if and only if the points
C are specified in counterclockwise order.  The last point
C of the curve is taken to be the first point specified, and
C this point should therefore not be specified twice.
C
C   The area of a triangulation may be computed by calling
C AREAP with values of NB and NODES determined by Subroutine
C BNODES.
C
C
C On input:
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of a set of points in the plane
C             for some N .GE. NB.
C
C       NB = Length of NODES.
C
C       NODES = Array of length NB containing the ordered
C               sequence of nodal indexes (in the range
C               1 to N) which define the polygonal curve.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       AREAP = Signed area bounded by the polygonal curve,
C              or zero if NB < 3.
C
C Modules required by AREAP:  None
C
C***********************************************************
C
      INTEGER I, ND1, ND2, NNB
      DOUBLE PRECISION A
C
C Local parameters:
C
C A =       Partial sum of signed (and doubled) trapezoid
C             areas
C I =       DO-loop and NODES index
C ND1,ND2 = Elements of NODES
C NNB =     Local copy of NB
C
      NNB = NB
      A = 0.
      IF (NNB .LT. 3) GO TO 2
      ND2 = NODES(NNB)
C
C Loop on line segments NODES(I-1) -> NODES(I), where
C   NODES(0) = NODES(NB), adding twice the signed trapezoid
C   areas (integrals of the linear interpolants) to A.
C
      DO 1 I = 1,NNB
        ND1 = ND2
        ND2 = NODES(I)
        A = A + (X(ND2)-X(ND1))*(Y(ND1)+Y(ND2))
    1   CONTINUE
C
C A contains twice the negative signed area of the region.
C
    2 AREAP = -A/2.
      RETURN
      END
      SUBROUTINE BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, LIST(*), LPTR(*), LEND(*), LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   02/22/91
C
C   This subroutine adds a boundary node to a triangulation
C of a set of points in the plane.  The data structure is
C updated with the insertion of node KK, but no optimization
C is performed.
C
C
C On input:
C
C       KK = Index of a node to be connected to the sequence
C            of all visible boundary nodes.  KK .GE. 1 and
C            KK must not be equal to I1 or I2.
C
C       I1 = First (rightmost as viewed from KK) boundary
C            node in the triangulation which is visible from
C            node KK (the line segment KK-I1 intersects no
C            arcs.
C
C       I2 = Last (leftmost) boundary node which is visible
C            from node KK.  I1 and I2 may be determined by
C            Subroutine TRFIND.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by TRMESH or TRMSHR.
C                             Nodes I1 and I2 must be in-
C                             cluded in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  Node
C                             KK is connected to I1, I2, and
C                             all boundary nodes in between.
C
C Module required by BDYADD:  INSERT
C
C***********************************************************
C
      INTEGER K, LP, LSAV, N1, N2, NEXT, NSAV
      K = KK
      N1 = I1
      N2 = I2
C
C Add K as the last neighbor of N1.
C
      LP = LEND(N1)
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = -K
      LPTR(LNEW) = LSAV
      LEND(N1) = LNEW
      LNEW = LNEW + 1
      NEXT = -LIST(LP)
      LIST(LP) = NEXT
      NSAV = NEXT
C
C Loop on the remaining boundary nodes between N1 and N2,
C   adding K as the first neighbor.
C
    1 LP = LEND(NEXT)
        CALL INSERT (K,LP,LIST,LPTR,LNEW)
        IF (NEXT .EQ. N2) GO TO 2
        NEXT = -LIST(LP)
        LIST(LP) = NEXT
        GO TO 1
C
C Add the boundary nodes between N1 and N2 as neighbors
C   of node K.
C
    2 LSAV = LNEW
      LIST(LNEW) = N1
      LPTR(LNEW) = LNEW + 1
      LNEW = LNEW + 1
      NEXT = NSAV
C
    3 IF (NEXT .EQ. N2) GO TO 4
        LIST(LNEW) = NEXT
        LPTR(LNEW) = LNEW + 1
        LNEW = LNEW + 1
        LP = LEND(NEXT)
        NEXT = LIST(LP)
        GO TO 3
C
    4 LIST(LNEW) = -N2
      LPTR(LNEW) = LSAV
      LEND(K) = LNEW
      LNEW = LNEW + 1
      RETURN
      END
      SUBROUTINE BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT)
      INTEGER N, LIST(*), LPTR(*), LEND(N), NODES(*), NB,
     .        NA, NT
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   Given a triangulation of N points in the plane, this
C subroutine returns an array containing the indexes, in
C counterclockwise order, of the nodes on the boundary of
C the convex hull of the set of points.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C The above parameters are not altered by this routine.
C
C       NODES = Integer array of length at least NB
C               (NB .LE. N).
C
C On output:
C
C       NODES = Ordered sequence of boundary node indexes
C               in the range 1 to N.
C
C       NB = Number of boundary nodes.
C
C       NA,NT = Number of arcs and triangles, respectively,
C               in the triangulation.
C
C Modules required by BNODES:  None
C
C***********************************************************
C
      INTEGER K, LP, N0, NST
C
C Set NST to the first boundary node encountered.
C
      NST = 1
    1 LP = LEND(NST)
        IF (LIST(LP) .LT. 0) GO TO 2
        NST = NST + 1
        GO TO 1
C
C Initialization.
C
    2 NODES(1) = NST
      K = 1
      N0 = NST
C
C Traverse the boundary in counterclockwise order.
C
    3 LP = LEND(N0)
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (N0 .EQ. NST) GO TO 4
        K = K + 1
        NODES(K) = N0
        GO TO 3
C
C Termination.
C
    4 NB = K
      NT = 2*N - NB - 2
      NA = NT + N - 1
      RETURN
      END
      SUBROUTINE CIRCUM (X1,Y1,X2,Y2,X3,Y3,RATIO, XC,YC,CR,
     .                   SA,AR)
      LOGICAL RATIO
      DOUBLE PRECISION X1, Y1, X2, Y2, X3, Y3, XC, YC, CR, 
     .           SA, AR
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   12/10/96
C
C   Given three vertices defining a triangle, this subrou-
C tine returns the circumcenter, circumradius, signed
C triangle area, and, optionally, the aspect ratio of the
C triangle.
C
C
C On input:
C
C       X1,...,Y3 = Cartesian coordinates of the vertices.
C
C       RATIO = Logical variable with value TRUE if and only
C               if the aspect ratio is to be computed.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       XC,YC = Cartesian coordinates of the circumcenter
C               (center of the circle defined by the three
C               points) unless SA = 0, in which XC and YC
C               are not altered.
C
C       CR = Circumradius (radius of the circle defined by
C            the three points) unless SA = 0 (infinite
C            radius), in which case CR is not altered.
C
C       SA = Signed triangle area with positive value if
C            and only if the vertices are specified in
C            counterclockwise order:  (X3,Y3) is strictly
C            to the left of the directed line from (X1,Y1)
C            toward (X2,Y2).
C
C       AR = Aspect ratio r/CR, where r is the radius of the
C            inscribed circle, unless RATIO = FALSE, in
C            which case AR is not altered.  AR is in the
C            range 0 to .5, with value 0 iff SA = 0 and
C            value .5 iff the vertices define an equilateral
C            triangle.
C
C Modules required by CIRCUM:  None
C
C Intrinsic functions called by CIRCUM:  ABS, SQRT
C
C***********************************************************
C
      INTEGER I
      DOUBLE PRECISION DS(3), FX, FY, U(3), V(3)
C
C Set U(K) and V(K) to the x and y components, respectively,
C   of the directed edge opposite vertex K.
C
      U(1) = X3 - X2
      U(2) = X1 - X3
      U(3) = X2 - X1
      V(1) = Y3 - Y2
      V(2) = Y1 - Y3
      V(3) = Y2 - Y1
C
C Set SA to the signed triangle area.
C
      SA = (U(1)*V(2) - U(2)*V(1))/2.
      IF (SA .EQ. 0.) THEN
        IF (RATIO) AR = 0.
        RETURN
      ENDIF
C
C Set DS(K) to the squared distance from the origin to
C   vertex K.
C
      DS(1) = X1*X1 + Y1*Y1
      DS(2) = X2*X2 + Y2*Y2
      DS(3) = X3*X3 + Y3*Y3
C
C Compute factors of XC and YC.
C
      FX = 0.
      FY = 0.
      DO 1 I = 1,3
        FX = FX - DS(I)*V(I)
        FY = FY + DS(I)*U(I)
    1   CONTINUE
      XC = FX/(4.*SA)
      YC = FY/(4.*SA)
      CR = SQRT( (XC-X1)**2 + (YC-Y1)**2 )
      IF (.NOT. RATIO) RETURN
C
C Compute the squared edge lengths and aspect ratio.
C
      DO 2 I = 1,3
        DS(I) = U(I)*U(I) + V(I)*V(I)
    2   CONTINUE
      AR = 2.*ABS(SA)/
     .     ( (SQRT(DS(1)) + SQRT(DS(2)) + SQRT(DS(3)))*CR )
      RETURN
      END
      LOGICAL FUNCTION CRTRI (NCC,LCC,I1,I2,I3)
      INTEGER NCC, LCC(*), I1, I2, I3
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   08/14/91
C
C   This function returns TRUE if and only if triangle (I1,
C I2,I3) lies in a constraint region.
C
C
C On input:
C
C       NCC,LCC = Constraint data structure.  Refer to Sub-
C                 routine ADDCST.
C
C       I1,I2,I3 = Nodal indexes of the counterclockwise-
C                  ordered vertices of a triangle.
C
C Input parameters are altered by this function.
C
C       CRTRI = TRUE iff (I1,I2,I3) is a constraint region
C               triangle.
C
C Note that input parameters are not tested for validity.
C
C Modules required by CRTRI:  None
C
C Intrinsic functions called by CRTRI:  MAX, MIN
C
C***********************************************************
C
      INTEGER I, IMAX, IMIN
      IMAX = MAX(I1,I2,I3)
C
C   Find the index I of the constraint containing IMAX.
C
      I = NCC + 1
    1 I = I - 1
        IF (I .LE. 0) GO TO 2
        IF (IMAX .LT. LCC(I)) GO TO 1
      IMIN = MIN(I1,I2,I3)
C
C P lies in a constraint region iff I1, I2, and I3 are nodes
C   of the same constraint (IMIN >= LCC(I)), and (IMIN,IMAX)
C   is (I1,I3), (I2,I1), or (I3,I2).
C
      CRTRI = IMIN .GE. LCC(I)  .AND.  ((IMIN .EQ. I1 .AND.
     .        IMAX .EQ. I3)  .OR.  (IMIN .EQ. I2  .AND.
     .        IMAX .EQ. I1)  .OR.  (IMIN .EQ. I3  .AND.
     .        IMAX .EQ. I2))
      RETURN
C
C NCC .LE. 0 or all vertices are non-constraint nodes.
C
    2 CRTRI = .FALSE.
      RETURN
      END
      SUBROUTINE DELARC (N,IO1,IO2, LIST,LPTR,LEND,
     .                   LNEW, IER)
      INTEGER N, IO1, IO2, LIST(*), LPTR(*), LEND(N), LNEW,
     .        IER
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/12/94
C
C   This subroutine deletes a boundary arc from a triangula-
C tion.  It may be used to remove a null triangle from the
C convex hull boundary.  Note, however, that if the union of
C triangles is rendered nonconvex, Subroutines DELNOD, EDGE,
C and TRFIND may fail.  Thus, Subroutines ADDCST, ADDNOD,
C DELNOD, EDGE, and NEARND should not be called following
C an arc deletion.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 4.
C
C       IO1,IO2 = Indexes (in the range 1 to N) of a pair of
C                 adjacent boundary nodes defining the arc
C                 to be removed.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by TRMESH or TRMSHR.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the removal of arc IO1-IO2
C                             unless IER > 0.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N, IO1, or IO2 is outside its valid
C                     range, or IO1 = IO2.
C             IER = 2 if IO1-IO2 is not a boundary arc.
C             IER = 3 if the node opposite IO1-IO2 is al-
C                     ready a boundary node, and thus IO1
C                     or IO2 has only two neighbors or a
C                     deletion would result in two triangu-
C                     lations sharing a single node.
C             IER = 4 if one of the nodes is a neighbor of
C                     the other, but not vice versa, imply-
C                     ing an invalid triangulation data
C                     structure.
C
C Modules required by DELARC:  DELNB, LSTPTR
C
C Intrinsic function called by DELARC:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER LP, LPH, LPL, N1, N2, N3
      N1 = IO1
      N2 = IO2
C
C Test for errors, and set N1->N2 to the directed boundary
C   edge associated with IO1-IO2:  (N1,N2,N3) is a triangle
C   for some N3.
C
      IF (N .LT. 4  .OR.  N1 .LT. 1  .OR.  N1 .GT. N  .OR.
     .    N2 .LT. 1  .OR.  N2 .GT. N  .OR.  N1 .EQ. N2) THEN
        IER = 1
        RETURN
      ENDIF
C
      LPL = LEND(N2)
      IF (-LIST(LPL) .NE. N1) THEN
        N1 = N2
        N2 = IO1
        LPL = LEND(N2)
        IF (-LIST(LPL) .NE. N1) THEN
          IER = 2
          RETURN
        ENDIF
      ENDIF
C
C Set N3 to the node opposite N1->N2 (the second neighbor
C   of N1), and test for error 3 (N3 already a boundary
C   node).
C
      LPL = LEND(N1)
      LP = LPTR(LPL)
      LP = LPTR(LP)
      N3 = ABS(LIST(LP))
      LPL = LEND(N3)
      IF (LIST(LPL) .LE. 0) THEN
        IER = 3
        RETURN
      ENDIF
C
C Delete N2 as a neighbor of N1, making N3 the first
C   neighbor, and test for error 4 (N2 not a neighbor
C   of N1).  Note that previously computed pointers may
C   no longer be valid following the call to DELNB.
C
      CALL DELNB (N1,N2,N, LIST,LPTR,LEND,LNEW, LPH)
      IF (LPH .LT. 0) THEN
        IER = 4
        RETURN
      ENDIF
C
C Delete N1 as a neighbor of N2, making N3 the new last
C   neighbor.
C
      CALL DELNB (N2,N1,N, LIST,LPTR,LEND,LNEW, LPH)
C
C Make N3 a boundary node with first neighbor N2 and last
C   neighbor N1.
C
      LP = LSTPTR(LEND(N3),N1,LIST,LPTR)
      LEND(N3) = LP
      LIST(LP) = -N1
C
C No errors encountered.
C
      IER = 0
      RETURN
      END
      SUBROUTINE DELNB (N0,NB,N, LIST,LPTR,LEND,LNEW, LPH)
      INTEGER N0, NB, N, LIST(*), LPTR(*), LEND(N), LNEW,
     .        LPH
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/30/98
C
C   This subroutine deletes a neighbor NB from the adjacency
C list of node N0 (but N0 is not deleted from the adjacency
C list of NB) and, if NB is a boundary node, makes N0 a
C boundary node.  For pointer (LIST index) LPH to NB as a
C neighbor of N0, the empty LIST,LPTR location LPH is filled
C in with the values at LNEW-1, pointer LNEW-1 (in LPTR and
C possibly in LEND) is changed to LPH, and LNEW is decremen-
C ted.  This requires a search of LEND and LPTR entailing an
C expected operation count of O(N).
C
C
C On input:
C
C       N0,NB = Indexes, in the range 1 to N, of a pair of
C               nodes such that NB is a neighbor of N0.
C               (N0 need not be a neighbor of NB.)
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the removal of NB from the ad-
C                             jacency list of N0 unless
C                             LPH < 0.
C
C       LPH = List pointer to the hole (NB as a neighbor of
C             N0) filled in by the values at LNEW-1 or error
C             indicator:
C             LPH > 0 if no errors were encountered.
C             LPH = -1 if N0, NB, or N is outside its valid
C                      range.
C             LPH = -2 if NB is not a neighbor of N0.
C
C Modules required by DELNB:  None
C
C Intrinsic function called by DELNB:  ABS
C
C***********************************************************
C
      INTEGER I, LNW, LP, LPB, LPL, LPP, NN
C
C Local parameters:
C
C I =   DO-loop index
C LNW = LNEW-1 (output value of LNEW)
C LP =  LIST pointer of the last neighbor of NB
C LPB = Pointer to NB as a neighbor of N0
C LPL = Pointer to the last neighbor of N0
C LPP = Pointer to the neighbor of N0 that precedes NB
C NN =  Local copy of N
C
      NN = N
C
C Test for error 1.
C
      IF (N0 .LT. 1  .OR.  N0 .GT. NN  .OR.  NB .LT. 1  .OR.
     .    NB .GT. NN  .OR.  NN .LT. 3) THEN
        LPH = -1
        RETURN
      ENDIF
C
C   Find pointers to neighbors of N0:
C
C     LPL points to the last neighbor,
C     LPP points to the neighbor NP preceding NB, and
C     LPB points to NB.
C
      LPL = LEND(N0)
      LPP = LPL
      LPB = LPTR(LPP)
    1 IF (LIST(LPB) .EQ. NB) GO TO 2
        LPP = LPB
        LPB = LPTR(LPP)
        IF (LPB .NE. LPL) GO TO 1
C
C   Test for error 2 (NB not found).
C
      IF (ABS(LIST(LPB)) .NE. NB) THEN
        LPH = -2
        RETURN
      ENDIF
C
C   NB is the last neighbor of N0.  Make NP the new last
C     neighbor and, if NB is a boundary node, then make N0
C     a boundary node.
C
      LEND(N0) = LPP
      LP = LEND(NB)
      IF (LIST(LP) .LT. 0) LIST(LPP) = -LIST(LPP)
      GO TO 3
C
C   NB is not the last neighbor of N0.  If NB is a boundary
C     node and N0 is not, then make N0 a boundary node with
C     last neighbor NP.
C
    2 LP = LEND(NB)
      IF (LIST(LP) .LT. 0  .AND.  LIST(LPL) .GT. 0) THEN
        LEND(N0) = LPP
        LIST(LPP) = -LIST(LPP)
      ENDIF
C
C   Update LPTR so that the neighbor following NB now fol-
C     lows NP, and fill in the hole at location LPB.
C
    3 LPTR(LPP) = LPTR(LPB)
      LNW = LNEW-1
      LIST(LPB) = LIST(LNW)
      LPTR(LPB) = LPTR(LNW)
      DO 4 I = NN,1,-1
        IF (LEND(I) .EQ. LNW) THEN
          LEND(I) = LPB
          GO TO 5
        ENDIF
    4   CONTINUE
C
    5 DO 6 I = 1,LNW-1
        IF (LPTR(I) .EQ. LNW) THEN
          LPTR(I) = LPB
        ENDIF
    6   CONTINUE
C
C No errors encountered.
C
      LNEW = LNW
      LPH = LPB
      RETURN
      END
      SUBROUTINE DELNOD (K,NCC, LCC,N,X,Y,LIST,LPTR,LEND,
     .                   LNEW,LWK,IWK, IER)
      INTEGER K, NCC, LCC(*), N, LIST(*), LPTR(*),
     .        LEND(*), LNEW, LWK, IWK(2,*), IER
      DOUBLE PRECISION X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/28/98
C
C   This subroutine deletes node K (along with all arcs
C incident on node K) from a triangulation of N nodes in the
C plane, and inserts arcs as necessary to produce a triangu-
C lation of the remaining N-1 nodes.  If a Delaunay triangu-
C lation is input, a Delaunay triangulation will result, and
C thus, DELNOD reverses the effect of a call to Subroutine
C ADDNOD.
C
C   Note that a constraint node cannot be deleted by this
C routine.  In order to delete a constraint node, it is
C necessary to call this routine with NCC = 0, decrement the
C appropriate LCC entries (LCC(I) such that LCC(I) > K), and
C then create (or restore) the constraints by a call to Sub-
C routine ADDCST.
C
C
C On input:
C
C       K = Index (for X and Y) of the node to be deleted.
C           1 .LE. K .LT. LCC(1).  (K .LE. N if NCC=0).
C
C       NCC = Number of constraint curves.  NCC .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             Subroutine ADDCST.
C
C       N = Number of nodes in the triangulation on input.
C           N .GE. 4.  Note that N will be decremented
C           following the deletion.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes with non-constraint nodes in the
C             first LCC(1)-1 locations if NCC > 0.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine TRMESH.
C
C       LWK = Number of columns reserved for IWK.  LWK must
C             be at least NNB-3, where NNB is the number of
C             neighbors of node K, including an extra
C             pseudo-node if K is a boundary node.
C
C       IWK = Integer work array dimensioned 2 by LWK (or
C             array of length .GE. 2*LWK).
C
C On output:
C
C       LCC = List of constraint curve starting indexes de-
C             cremented by 1 to reflect the deletion of K
C             unless NCC = 0 or 1 .LE. IER .LE. 4.
C
C       N = New number of nodes (input value minus one) un-
C           less 1 .LE. IER .LE. 4.
C
C       X,Y = Updated arrays of length N-1 containing nodal
C             coordinates (with elements K+1,...,N shifted
C             up a position and thus overwriting element K)
C             unless 1 .LE. IER .LE. 4.  (N here denotes the
C             input value.)
C
C       LIST,LPTR,LEND,LNEW = Updated triangulation data
C                             structure reflecting the dele-
C                             tion unless IER .NE. 0.  Note
C                             that the data structure may
C                             have been altered if IER .GE.
C                             3.
C
C       LWK = Number of IWK columns required unless IER = 1
C             or IER = 3.
C
C       IWK = Indexes of the endpoints of the new arcs added
C             unless LWK = 0 or 1 .LE. IER .LE. 4.  (Arcs
C             are associated with columns, or pairs of
C             adjacent elements if IWK is declared as a
C             singly-subscripted array.)
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if K, NCC, N, or an LCC entry is out-
C                     side its valid range or LWK < 0 on
C                     input.
C             IER = 2 if more space is required in IWK.
C                     Refer to LWK.
C             IER = 3 if the triangulation data structure is
C                     invalid on input.
C             IER = 4 if K is an interior node with 4 or
C                     more neighbors, and the number of
C                     neighbors could not be reduced to 3
C                     by swaps.  This could be caused by
C                     floating point errors with collinear
C                     nodes or by an invalid data structure.
C             IER = 5 if an error flag was returned by
C                     OPTIM.  An error message is written
C                     to the standard output unit in this
C                     event.
C
C   Note that the deletion may result in all remaining nodes
C being collinear.  This situation is not flagged.
C
C Modules required by DELNOD:  DELNB, LEFT, LSTPTR, NBCNT,
C                                OPTIM, SWAP, SWPTST
C
C Intrinsic function called by DELNOD:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR, NBCNT
      LOGICAL LEFT
      INTEGER I, IERR, IWL, J, LCCIP1, LNW, LP, LP21, LPF,
     .        LPH, LPL, LPL2, LPN, LWKL, N1, N2, NFRST, NIT,
     .        NL, NN, NNB, NR
      LOGICAL BDRY
      DOUBLE PRECISION X1, X2, XL, XR, Y1, Y2, YL, YR
C
C Set N1 to K and NNB to the number of neighbors of N1 (plus
C   one if N1 is a boundary node), and test for errors.  LPF
C   and LPL are LIST indexes of the first and last neighbors
C   of N1, IWL is the number of IWK columns containing arcs,
C   and BDRY is TRUE iff N1 is a boundary node.
C
      N1 = K
      NN = N
      IF (NCC .LT. 0  .OR.  N1 .LT. 1  .OR.  NN .LT. 4  .OR.
     .    LWK .LT. 0) GO TO 21
      LCCIP1 = NN+1
      DO 1 I = NCC,1,-1
        IF (LCCIP1-LCC(I) .LT. 3) GO TO 21
        LCCIP1 = LCC(I)
    1   CONTINUE
      IF (N1 .GE. LCCIP1) GO TO 21
      LPL = LEND(N1)
      LPF = LPTR(LPL)
      NNB = NBCNT(LPL,LPTR)
      BDRY = LIST(LPL) .LT. 0
      IF (BDRY) NNB = NNB + 1
      IF (NNB .LT. 3) GO TO 23
      LWKL = LWK
      LWK = NNB - 3
      IF (LWKL .LT. LWK) GO TO 22
      IWL = 0
      IF (NNB .EQ. 3) GO TO 5
C
C Initialize for loop on arcs N1-N2 for neighbors N2 of N1,
C   beginning with the second neighbor.  NR and NL are the
C   neighbors preceding and following N2, respectively, and
C   LP indexes NL.  The loop is exited when all possible
C   swaps have been applied to arcs incident on N1.  If N1
C   is interior, the number of neighbors will be reduced
C   to 3.
C
      X1 = X(N1)
      Y1 = Y(N1)
      NFRST = LIST(LPF)
      NR = NFRST
      XR = X(NR)
      YR = Y(NR)
      LP = LPTR(LPF)
      N2 = LIST(LP)
      X2 = X(N2)
      Y2 = Y(N2)
      LP = LPTR(LP)
C
C Top of loop:  set NL to the neighbor following N2.
C
    2 NL = ABS(LIST(LP))
      IF (NL .EQ. NFRST  .AND.  BDRY) GO TO 5
      XL = X(NL)
      YL = Y(NL)
C
C   Test for a convex quadrilateral.  To avoid an incorrect
C     test caused by collinearity, use the fact that if N1
C     is a boundary node, then N1 LEFT NR->NL and if N2 is
C     a boundary node, then N2 LEFT NL->NR.
C
      LPL2 = LEND(N2)
      IF ( (BDRY  .OR.  LEFT(XR,YR,XL,YL,X1,Y1))  .AND.
     .     (LIST(LPL2) .LT. 0  .OR.
     .      LEFT(XL,YL,XR,YR,X2,Y2)) ) GO TO 3
C
C   Nonconvex quadrilateral -- no swap is possible.
C
      NR = N2
      XR = X2
      YR = Y2
      GO TO 4
C
C   The quadrilateral defined by adjacent triangles
C     (N1,N2,NL) and (N2,N1,NR) is convex.  Swap in
C     NL-NR and store it in IWK.  Indexes larger than N1
C     must be decremented since N1 will be deleted from
C     X and Y.
C
    3 CALL SWAP (NL,NR,N1,N2, LIST,LPTR,LEND, LP21)
      IWL = IWL + 1
      IF (NL .LE. N1) THEN
        IWK(1,IWL) = NL
      ELSE
        IWK(1,IWL) = NL - 1
      ENDIF
      IF (NR .LE. N1) THEN
        IWK(2,IWL) = NR
      ELSE
        IWK(2,IWL) = NR - 1
      ENDIF
C
C   Recompute the LIST indexes LPL,LP and decrement NNB.
C
      LPL = LEND(N1)
      NNB = NNB - 1
      IF (NNB .EQ. 3) GO TO 5
      LP = LSTPTR(LPL,NL,LIST,LPTR)
      IF (NR .EQ. NFRST) GO TO 4
C
C   NR is not the first neighbor of N1.
C     Back up and test N1-NR for a swap again:  Set N2 to
C     NR and NR to the previous neighbor of N1 -- the
C     neighbor of NR which follows N1.  LP21 points to NL
C     as a neighbor of NR.
C
      N2 = NR
      X2 = XR
      Y2 = YR
      LP21 = LPTR(LP21)
      LP21 = LPTR(LP21)
      NR = ABS(LIST(LP21))
      XR = X(NR)
      YR = Y(NR)
      GO TO 2
C
C   Bottom of loop -- test for invalid termination.
C
    4 IF (N2 .EQ. NFRST) GO TO 24
      N2 = NL
      X2 = XL
      Y2 = YL
      LP = LPTR(LP)
      GO TO 2
C
C Delete N1 from the adjacency list of N2 for all neighbors
C   N2 of N1.  LPL points to the last neighbor of N1.
C   LNEW is stored in local variable LNW.
C
    5 LP = LPL
      LNW = LNEW
C
C Loop on neighbors N2 of N1, beginning with the first.
C
    6 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        CALL DELNB (N2,N1,N, LIST,LPTR,LEND,LNW, LPH)
        IF (LPH .LT. 0) GO TO 23
C
C   LP and LPL may require alteration.
C
        IF (LPL .EQ. LNW) LPL = LPH
        IF (LP .EQ. LNW) LP = LPH
        IF (LP .NE. LPL) GO TO 6
C
C Delete N1 from X, Y, and LEND, and remove its adjacency
C   list from LIST and LPTR.  LIST entries (nodal indexes)
C   which are larger than N1 must be decremented.
C
      NN = NN - 1
      IF (N1 .GT. NN) GO TO 9
      DO 7 I = N1,NN
        X(I) = X(I+1)
        Y(I) = Y(I+1)
        LEND(I) = LEND(I+1)
    7   CONTINUE
C
      DO 8 I = 1,LNW-1
        IF (LIST(I) .GT. N1) LIST(I) = LIST(I) - 1
        IF (LIST(I) .LT. -N1) LIST(I) = LIST(I) + 1
    8   CONTINUE
C
C   For LPN = first to last neighbors of N1, delete the
C     preceding neighbor (indexed by LP).
C
C   Each empty LIST,LPTR location LP is filled in with the
C     values at LNW-1, and LNW is decremented.  All pointers
C     (including those in LPTR and LEND) with value LNW-1
C     must be changed to LP.
C
C  LPL points to the last neighbor of N1.
C
    9 IF (BDRY) NNB = NNB - 1
      LPN = LPL
      DO 13 J = 1,NNB
        LNW = LNW - 1
        LP = LPN
        LPN = LPTR(LP)
        LIST(LP) = LIST(LNW)
        LPTR(LP) = LPTR(LNW)
        IF (LPTR(LPN) .EQ. LNW) LPTR(LPN) = LP
        IF (LPN .EQ. LNW) LPN = LP
        DO 10 I = NN,1,-1
          IF (LEND(I) .EQ. LNW) THEN
            LEND(I) = LP
            GO TO 11
          ENDIF
   10     CONTINUE
C
   11   DO 12 I = LNW-1,1,-1
          IF (LPTR(I) .EQ. LNW) LPTR(I) = LP
   12     CONTINUE
   13   CONTINUE
C
C Decrement LCC entries.
C
      DO 14 I = 1,NCC
        LCC(I) = LCC(I) - 1
   14   CONTINUE
C
C Update N and LNEW, and optimize the patch of triangles
C   containing K (on input) by applying swaps to the arcs
C   in IWK.
C
      N = NN
      LNEW = LNW
      IF (IWL .GT. 0) THEN
        NIT = 4*IWL
        CALL OPTIM (X,Y,IWL, LIST,LPTR,LEND,NIT,IWK, IERR)
        IF (IERR .NE. 0) GO TO 25
      ENDIF
C
C Successful termination.
C
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   21 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   22 IER = 2
      RETURN
C
C Invalid triangulation data structure.  NNB < 3 on input or
C   N2 is a neighbor of N1 but N1 is not a neighbor of N2.
C
   23 IER = 3
      RETURN
C
C K is an interior node with 4 or more neighbors, but the
C   number of neighbors could not be reduced.
C
   24 IER = 4
      RETURN
C
C Error flag returned by OPTIM.
C
   25 IER = 5
      WRITE (*,100) NIT, IERR
      RETURN
  100 FORMAT (//5X,'*** Error in OPTIM:  NIT = ',I4,
     .        ', IER = ',I1,' ***'/)
      END
      SUBROUTINE EDGE (IN1,IN2,X,Y, LWK,IWK,LIST,LPTR,
     .                 LEND, IER)
      INTEGER IN1, IN2, LWK, IWK(2,*), LIST(*), LPTR(*),
     .        LEND(*), IER
      DOUBLE PRECISION X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/23/98
C
C   Given a triangulation of N nodes and a pair of nodal
C indexes IN1 and IN2, this routine swaps arcs as necessary
C to force IN1 and IN2 to be adjacent.  Only arcs which
C intersect IN1-IN2 are swapped out.  If a Delaunay triangu-
C lation is input, the resulting triangulation is as close
C as possible to a Delaunay triangulation in the sense that
C all arcs other than IN1-IN2 are locally optimal.
C
C   A sequence of calls to EDGE may be used to force the
C presence of a set of edges defining the boundary of a non-
C convex and/or multiply connected region (refer to Subrou-
C tine ADDCST), or to introduce barriers into the triangula-
C tion.  Note that Subroutine GETNP will not necessarily
C return closest nodes if the triangulation has been con-
C strained by a call to EDGE.  However, this is appropriate
C in some applications, such as triangle-based interpolation
C on a nonconvex domain.
C
C
C On input:
C
C       IN1,IN2 = Indexes (of X and Y) in the range 1 to N
C                 defining a pair of nodes to be connected
C                 by an arc.
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of the nodes.
C
C The above parameters are not altered by this routine.
C
C       LWK = Number of columns reserved for IWK.  This must
C             be at least NI -- the number of arcs which
C             intersect IN1-IN2.  (NI is bounded by N-3.)
C
C       IWK = Integer work array of length at least 2*LWK.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LWK = Number of arcs which intersect IN1-IN2 (but
C             not more than the input value of LWK) unless
C             IER = 1 or IER = 3.  LWK = 0 if and only if
C             IN1 and IN2 were adjacent (or LWK=0) on input.
C
C       IWK = Array containing the indexes of the endpoints
C             of the new arcs other than IN1-IN2 unless IER
C             .GT. 0 or LWK = 0.  New arcs to the left of
C             IN2-IN1 are stored in the first K-1 columns
C             (left portion of IWK), column K contains
C             zeros, and new arcs to the right of IN2-IN1
C             occupy columns K+1,...,LWK.  (K can be deter-
C             mined by searching IWK for the zeros.)
C
C       LIST,LPTR,LEND = Data structure updated if necessary
C                        to reflect the presence of an arc
C                        connecting IN1 and IN2 unless IER
C                        .NE. 0.  The data structure has
C                        been altered if IER = 4.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if IN1 .LT. 1, IN2 .LT. 1, IN1 = IN2,
C                     or LWK .LT. 0 on input.
C             IER = 2 if more space is required in IWK.
C             IER = 3 if IN1 and IN2 could not be connected
C                     due to either an invalid data struc-
C                     ture or collinear nodes (and floating
C                     point error).
C             IER = 4 if an error flag was returned by
C                     OPTIM.
C
C   An error message is written to the standard output unit
C in the case of IER = 3 or IER = 4.
C
C Modules required by EDGE:  LEFT, LSTPTR, OPTIM, SWAP,
C                              SWPTST
C
C Intrinsic function called by EDGE:  ABS
C
C***********************************************************
C
      LOGICAL LEFT
      INTEGER I, IERR, IWC, IWCP1, IWEND, IWF, IWL, LFT, LP,
     .        LPL, LP21, NEXT, NIT, NL, NR, N0, N1, N2,
     .        N1FRST, N1LST
      DOUBLE PRECISION DX, DY, X0, Y0, X1, Y1, X2, Y2
C
C Local parameters:
C
C DX,DY =   Components of arc N1-N2
C I =       DO-loop index and column index for IWK
C IERR =    Error flag returned by Subroutine OPTIM
C IWC =     IWK index between IWF and IWL -- NL->NR is
C             stored in IWK(1,IWC)->IWK(2,IWC)
C IWCP1 =   IWC + 1
C IWEND =   Input or output value of LWK
C IWF =     IWK (column) index of the first (leftmost) arc
C             which intersects IN1->IN2
C IWL =     IWK (column) index of the last (rightmost) are
C             which intersects IN1->IN2
C LFT =     Flag used to determine if a swap results in the
C             new arc intersecting IN1-IN2 -- LFT = 0 iff
C             N0 = IN1, LFT = -1 implies N0 LEFT IN1->IN2,
C             and LFT = 1 implies N0 LEFT IN2->IN1
C LP21 =    Unused parameter returned by SWAP
C LP =      List pointer (index) for LIST and LPTR
C LPL =     Pointer to the last neighbor of IN1 or NL
C N0 =      Neighbor of N1 or node opposite NR->NL
C N1,N2 =   Local copies of IN1 and IN2
C N1FRST =  First neighbor of IN1
C N1LST =   (Signed) last neighbor of IN1
C NEXT =    Node opposite NL->NR
C NIT =     Flag or number of iterations employed by OPTIM
C NL,NR =   Endpoints of an arc which intersects IN1-IN2
C             with NL LEFT IN1->IN2
C X0,Y0 =   Coordinates of N0
C X1,Y1 =   Coordinates of IN1
C X2,Y2 =   Coordinates of IN2
C
C
C Store IN1, IN2, and LWK in local variables and test for
C   errors.
C
      N1 = IN1
      N2 = IN2
      IWEND = LWK
      IF (N1 .LT. 1  .OR.  N2 .LT. 1  .OR.  N1 .EQ. N2  .OR.
     .    IWEND .LT. 0) GO TO 31
C
C Test for N2 as a neighbor of N1.  LPL points to the last
C   neighbor of N1.
C
      LPL = LEND(N1)
      N0 = ABS(LIST(LPL))
      LP = LPL
    1 IF (N0 .EQ. N2) GO TO 30
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (LP .NE. LPL) GO TO 1
C
C Initialize parameters.
C
      IWL = 0
      NIT = 0
C
C Store the coordinates of N1 and N2.
C
    2 X1 = X(N1)
      Y1 = Y(N1)
      X2 = X(N2)
      Y2 = Y(N2)
C
C Set NR and NL to adjacent neighbors of N1 such that
C   NR LEFT N2->N1 and NL LEFT N1->N2,
C   (NR Forward N1->N2 or NL Forward N1->N2), and
C   (NR Forward N2->N1 or NL Forward N2->N1).
C
C   Initialization:  Set N1FRST and N1LST to the first and
C     (signed) last neighbors of N1, respectively, and
C     initialize NL to N1FRST.
C
      LPL = LEND(N1)
      N1LST = LIST(LPL)
      LP = LPTR(LPL)
      N1FRST = LIST(LP)
      NL = N1FRST
      IF (N1LST .LT. 0) GO TO 4
C
C   N1 is an interior node.  Set NL to the first candidate
C     for NR (NL LEFT N2->N1).
C
    3 IF ( LEFT(X2,Y2,X1,Y1,X(NL),Y(NL)) ) GO TO 4
        LP = LPTR(LP)
        NL = LIST(LP)
        IF (NL .NE. N1FRST) GO TO 3
C
C   All neighbors of N1 are strictly left of N1->N2.
C
      GO TO 5
C
C   NL = LIST(LP) LEFT N2->N1.  Set NR to NL and NL to the
C     following neighbor of N1.
C
    4 NR = NL
        LP = LPTR(LP)
        NL = ABS(LIST(LP))
        IF ( LEFT(X1,Y1,X2,Y2,X(NL),Y(NL)) ) THEN
C
C   NL LEFT N1->N2 and NR LEFT N2->N1.  The Forward tests
C     are employed to avoid an error associated with
C     collinear nodes.
C
          DX = X2-X1
          DY = Y2-Y1
          IF ((DX*(X(NL)-X1)+DY*(Y(NL)-Y1) .GE. 0.  .OR.
     .         DX*(X(NR)-X1)+DY*(Y(NR)-Y1) .GE. 0.)  .AND.
     .        (DX*(X(NL)-X2)+DY*(Y(NL)-Y2) .LE. 0.  .OR.
     .         DX*(X(NR)-X2)+DY*(Y(NR)-Y2) .LE. 0.)) GO TO 6
C
C   NL-NR does not intersect N1-N2.  However, there is
C     another candidate for the first arc if NL lies on
C     the line N1-N2.
C
          IF ( .NOT. LEFT(X2,Y2,X1,Y1,X(NL),Y(NL)) ) GO TO 5
        ENDIF
C
C   Bottom of loop.
C
        IF (NL .NE. N1FRST) GO TO 4
C
C Either the triangulation is invalid or N1-N2 lies on the
C   convex hull boundary and an edge NR->NL (opposite N1 and
C   intersecting N1-N2) was not found due to floating point
C   error.  Try interchanging N1 and N2 -- NIT > 0 iff this
C   has already been done.
C
    5 IF (NIT .GT. 0) GO TO 33
      NIT = 1
      N1 = N2
      N2 = IN1
      GO TO 2
C
C Store the ordered sequence of intersecting edges NL->NR in
C   IWK(1,IWL)->IWK(2,IWL).
C
    6 IWL = IWL + 1
      IF (IWL .GT. IWEND) GO TO 32
      IWK(1,IWL) = NL
      IWK(2,IWL) = NR
C
C   Set NEXT to the neighbor of NL which follows NR.
C
      LPL = LEND(NL)
      LP = LPTR(LPL)
C
C   Find NR as a neighbor of NL.  The search begins with
C     the first neighbor.
C
    7 IF (LIST(LP) .EQ. NR) GO TO 8
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 7
C
C   NR must be the last neighbor, and NL->NR cannot be a
C     boundary edge.
C
      IF (LIST(LP) .NE. NR) GO TO 33
C
C   Set NEXT to the neighbor following NR, and test for
C     termination of the store loop.
C
    8 LP = LPTR(LP)
      NEXT = ABS(LIST(LP))
      IF (NEXT .EQ. N2) GO TO 9
C
C   Set NL or NR to NEXT.
C
      IF ( LEFT(X1,Y1,X2,Y2,X(NEXT),Y(NEXT)) ) THEN
        NL = NEXT
      ELSE
        NR = NEXT
      ENDIF
      GO TO 6
C
C IWL is the number of arcs which intersect N1-N2.
C   Store LWK.
C
    9 LWK = IWL
      IWEND = IWL
C
C Initialize for edge swapping loop -- all possible swaps
C   are applied (even if the new arc again intersects
C   N1-N2), arcs to the left of N1->N2 are stored in the
C   left portion of IWK, and arcs to the right are stored in
C   the right portion.  IWF and IWL index the first and last
C   intersecting arcs.
C
      IWF = 1
C
C Top of loop -- set N0 to N1 and NL->NR to the first edge.
C   IWC points to the arc currently being processed.  LFT
C   .LE. 0 iff N0 LEFT N1->N2.
C
   10 LFT = 0
      N0 = N1
      X0 = X1
      Y0 = Y1
      NL = IWK(1,IWF)
      NR = IWK(2,IWF)
      IWC = IWF
C
C   Set NEXT to the node opposite NL->NR unless IWC is the
C     last arc.
C
   11 IF (IWC .EQ. IWL) GO TO 21
      IWCP1 = IWC + 1
      NEXT = IWK(1,IWCP1)
      IF (NEXT .NE. NL) GO TO 16
      NEXT = IWK(2,IWCP1)
C
C   NEXT RIGHT N1->N2 and IWC .LT. IWL.  Test for a possible
C     swap.
C
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )
     .   GO TO 14
      IF (LFT .GE. 0) GO TO 12
      IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )
     .   GO TO 14
C
C   Replace NL->NR with N0->NEXT.
C
      CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = N0
      IWK(2,IWC) = NEXT
      GO TO 15
C
C   Swap NL-NR for N0-NEXT, shift columns IWC+1,...,IWL to
C     the left, and store N0-NEXT in the right portion of
C     IWK.
C
   12 CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 13 I = IWCP1,IWL
        IWK(1,I-1) = IWK(1,I)
        IWK(2,I-1) = IWK(2,I)
   13   CONTINUE
      IWK(1,IWL) = N0
      IWK(2,IWL) = NEXT
      IWL = IWL - 1
      NR = NEXT
      GO TO 11
C
C   A swap is not possible.  Set N0 to NR.
C
   14 N0 = NR
      X0 = X(N0)
      Y0 = Y(N0)
      LFT = 1
C
C   Advance to the next arc.
C
   15 NR = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   NEXT LEFT N1->N2, NEXT .NE. N2, and IWC .LT. IWL.
C     Test for a possible swap.
C
   16 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )
     .   GO TO 19
      IF (LFT .LE. 0) GO TO 17
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )
     .   GO TO 19
C
C   Replace NL->NR with NEXT->N0.
C
      CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = NEXT
      IWK(2,IWC) = N0
      GO TO 20
C
C   Swap NL-NR for N0-NEXT, shift columns IWF,...,IWC-1 to
C     the right, and store N0-NEXT in the left portion of
C     IWK.
C
   17 CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 18 I = IWC-1,IWF,-1
        IWK(1,I+1) = IWK(1,I)
        IWK(2,I+1) = IWK(2,I)
   18   CONTINUE
      IWK(1,IWF) = N0
      IWK(2,IWF) = NEXT
      IWF = IWF + 1
      GO TO 20
C
C   A swap is not possible.  Set N0 to NL.
C
   19 N0 = NL
      X0 = X(N0)
      Y0 = Y(N0)
      LFT = -1
C
C   Advance to the next arc.
C
   20 NL = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   N2 is opposite NL->NR (IWC = IWL).
C
   21 IF (N0 .EQ. N1) GO TO 24
      IF (LFT .LT. 0) GO TO 22
C
C   N0 RIGHT N1->N2.  Test for a possible swap.
C
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X2,Y2) ) GO TO 10
C
C   Swap NL-NR for N0-N2 and store N0-N2 in the right
C     portion of IWK.
C
      CALL SWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWL) = N0
      IWK(2,IWL) = N2
      IWL = IWL - 1
      GO TO 10
C
C   N0 LEFT N1->N2.  Test for a possible swap.
C
   22 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X2,Y2) ) GO TO 10
C
C   Swap NL-NR for N0-N2, shift columns IWF,...,IWL-1 to the
C     right, and store N0-N2 in the left portion of IWK.
C
      CALL SWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      I = IWL
   23 IWK(1,I) = IWK(1,I-1)
      IWK(2,I) = IWK(2,I-1)
      I = I - 1
      IF (I .GT. IWF) GO TO 23
      IWK(1,IWF) = N0
      IWK(2,IWF) = N2
      IWF = IWF + 1
      GO TO 10
C
C IWF = IWC = IWL.  Swap out the last arc for N1-N2 and
C   store zeros in IWK.
C
   24 CALL SWAP (N2,N1,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = 0
      IWK(2,IWC) = 0
C
C Optimization procedure --
C
      IF (IWC .GT. 1) THEN
C
C   Optimize the set of new arcs to the left of IN1->IN2.
C
        NIT = 3*(IWC-1)
        CALL OPTIM (X,Y,IWC-1, LIST,LPTR,LEND,NIT,IWK, IERR)
        IF (IERR .NE. 0) GO TO 34
      ENDIF
      IF (IWC .LT. IWEND) THEN
C
C   Optimize the set of new arcs to the right of IN1->IN2.
C
        NIT = 3*(IWEND-IWC)
        CALL OPTIM (X,Y,IWEND-IWC, LIST,LPTR,LEND,NIT,
     .              IWK(1,IWC+1), IERR)
        IF (IERR .NE. 0) GO TO 34
      ENDIF
C
C Successful termination.
C
      IER = 0
      RETURN
C
C IN1 and IN2 were adjacent on input.
C
   30 IER = 0
      RETURN
C
C Invalid input parameter.
C
   31 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   32 IER = 2
      RETURN
C
C Invalid triangulation data structure or collinear nodes
C   on convex hull boundary.
C
   33 IER = 3
      WRITE (*,130) IN1, IN2
  130 FORMAT (//5X,'*** Error in EDGE:  Invalid triangula',
     .        'tion or null triangles on boundary'/
     .        9X,'IN1 =',I4,', IN2=',I4/)
      RETURN
C
C Error flag returned by OPTIM.
C
   34 IER = 4
      WRITE (*,140) NIT, IERR
  140 FORMAT (//5X,'*** Error in OPTIM:  NIT = ',I4,
     .        ', IER = ',I1,' ***'/)
      RETURN
      END
      SUBROUTINE GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
     .                  L, NPTS,DS, IER)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
     .        L, NPTS(L), IER
      DOUBLE PRECISION X(N), Y(N), DS(L)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/12/94
C
C   Given a triangulation of N nodes and an array NPTS con-
C taining the indexes of L-1 nodes ordered by distance from
C NPTS(1), this subroutine sets NPTS(L) to the index of the
C next node in the sequence -- the node, other than NPTS(1),
C ...,NPTS(L-1), which is closest to NPTS(1).  Thus, the
C ordered sequence of K closest nodes to N1 (including N1)
C may be determined by K-1 calls to GETNP with NPTS(1) = N1
C and L = 2,3,...,K for K .GE. 2.  Note that NPTS must in-
C clude constraint nodes as well as non-constraint nodes.
C Thus, a sequence of K1 closest non-constraint nodes to N1
C must be obtained as a subset of the closest K2 nodes to N1
C for some K2 .GE. K1.
C
C   The terms closest and distance have special definitions
C when constraint nodes are present in the triangulation.
C Nodes N1 and N2 are said to be visible from each other if
C and only if the line segment N1-N2 intersects no con-
C straint arc (except possibly itself) and is not an interi-
C or constraint arc (arc whose interior lies in a constraint
C region).  A path from N1 to N2 is an ordered sequence of
C nodes, with N1 first and N2 last, such that adjacent path
C elements are visible from each other.  The path length is
C the sum of the Euclidean distances between adjacent path
C nodes.  Finally, the distance from N1 to N2 is defined to
C be the length of the shortest path from N1 to N2.
C
C   The algorithm uses the property of a Delaunay triangula-
C tion that the K-th closest node to N1 is a neighbor of one
C of the K-1 closest nodes to N1.  With the definition of
C distance used here, this property holds when constraints
C are present as long as non-constraint arcs are locally
C optimal.
C
C
C On input:
C
C       NCC = Number of constraints.  NCC .GE. 0.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             Subroutine ADDCST.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes with non-constraint nodes in the
C             first LCC(1)-1 locations if NCC > 0.
C
C       LIST,LPTR,LEND = Triangulation data structure.  Re-
C                        fer to Subroutine TRMESH.
C
C       L = Number of nodes in the sequence on output.  2
C           .LE. L .LE. N.
C
C       NPTS = Array of length .GE. L containing the indexes
C              of the L-1 closest nodes to NPTS(1) in the
C              first L-1 locations.
C
C       DS = Array of length .GE. L containing the distance
C            (defined above) between NPTS(1) and NPTS(I) in
C            the I-th position for I = 1,...,L-1.  Thus,
C            DS(1) = 0.
C
C Input parameters other than NPTS(L) and DS(L) are not
C   altered by this routine.
C
C On output:
C
C       NPTS = Array updated with the index of the L-th
C              closest node to NPTS(1) in position L unless
C              IER .NE. 0.
C
C       DS = Array updated with the distance between NPTS(1)
C            and NPTS(L) in position L unless IER .NE. 0.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if NCC, N, L, or an LCC entry is
C                      outside its valid range on input.
C             IER =  K if NPTS(K) is not a valid index in
C                      the range 1 to N.
C
C Module required by GETNP:  INTSEC
C
C Intrinsic functions called by GETNP:  ABS, MIN, SQRT
C
C***********************************************************
C
      LOGICAL INTSEC
      INTEGER I, IFRST, ILAST, J, K, KM1, LCC1, LM1, LP,
     .        LPCL, LPK, LPKL, N1, NC, NF1, NF2, NJ, NK,
     .        NKBAK, NKFOR, NL, NN
      LOGICAL ISW, VIS, NCF, NJF, SKIP, SKSAV, LFT1, LFT2,
     .        LFT12
      DOUBLE PRECISION DC, DL, X1, XC, XJ, XK, Y1, YC, YJ, 
     .        YK
C
C Store parameters in local variables and test for errors.
C   LCC1 indexes the first constraint node.
C
      IER = -1
      NN = N
      LCC1 = NN+1
      LM1 = L-1
      IF (NCC .LT. 0  .OR.  LM1 .LT. 1  .OR.  LM1 .GE. NN)
     .   RETURN
      IF (NCC .EQ. 0) THEN
        IF (NN .LT. 3) RETURN
      ELSE
        DO 1 I = NCC,1,-1
          IF (LCC1 - LCC(I) .LT. 3) RETURN
          LCC1 = LCC(I)
    1     CONTINUE
        IF (LCC1 .LT. 1) RETURN
      ENDIF
C
C Test for an invalid index in NPTS.
C
      DO 2 K = 1,LM1
        NK = NPTS(K)
        IF (NK .LT. 1  .OR.  NK .GT. NN) THEN
          IER = K
          RETURN
        ENDIF
    2   CONTINUE
C
C Store N1 = NPTS(1) and mark the elements of NPTS.
C
      N1 = NPTS(1)
      X1 = X(N1)
      Y1 = Y(N1)
      DO 3 K = 1,LM1
        NK = NPTS(K)
        LEND(NK) = -LEND(NK)
    3   CONTINUE
C
C Candidates NC for NL = NPTS(L) are the unmarked visible
C   neighbors of nodes NK in NPTS.  ISW is an initialization
C   switch set to .TRUE. when NL and its distance DL from N1
C   have been initialized with the first candidate encount-
C   ered.
C
      ISW = .FALSE.
      DL = 0.
C
C Loop on marked nodes NK = NPTS(K).  LPKL indexes the last
C   neighbor of NK in LIST.
C
      DO 16 K = 1,LM1
        KM1 = K - 1
        NK = NPTS(K)
        XK = X(NK)
        YK = Y(NK)
        LPKL = -LEND(NK)
        NKFOR = 0
        NKBAK = 0
        VIS = .TRUE.
        IF (NK .GE. LCC1) THEN
C
C   NK is a constraint node.  Set NKFOR and NKBAK to the
C     constraint nodes which follow and precede NK.  IFRST
C     and ILAST are set to the first and last nodes in the
C     constraint containing NK.
C
          IFRST = NN + 1
          DO 4 I = NCC,1,-1
            ILAST = IFRST - 1
            IFRST = LCC(I)
            IF (NK .GE. IFRST) GO TO 5
    4       CONTINUE
C
    5     IF (NK .LT. ILAST) THEN
            NKFOR = NK + 1
          ELSE
            NKFOR = IFRST
          ENDIF
          IF (NK .GT. IFRST) THEN
            NKBAK = NK - 1
          ELSE
            NKBAK = ILAST
          ENDIF
C
C   Initialize VIS to TRUE iff NKFOR precedes NKBAK in the
C     adjacency list for NK -- the first neighbor is visi-
C     ble and is not NKBAK.
C
          LPK = LPKL
    6     LPK = LPTR(LPK)
            NC = ABS(LIST(LPK))
            IF (NC .NE. NKFOR  .AND.  NC .NE. NKBAK) GO TO 6
          VIS = NC .EQ. NKFOR
        ENDIF
C
C Loop on neighbors NC of NK, bypassing marked and nonvis-
C   ible neighbors.
C
        LPK = LPKL
    7   LPK = LPTR(LPK)
          NC = ABS(LIST(LPK))
          IF (NC .EQ. NKBAK) VIS = .TRUE.
C
C   VIS = .FALSE. iff NK-NC is an interior constraint arc
C     (NK is a constraint node and NC lies strictly between
C     NKFOR and NKBAK).
C
          IF (.NOT. VIS) GO TO 15
          IF (NC .EQ. NKFOR) VIS = .FALSE.
          IF (LEND(NC) .LT. 0) GO TO 15
C
C Initialize distance DC between N1 and NC to Euclidean
C   distance.
C
          XC = X(NC)
          YC = Y(NC)
          DC = SQRT((XC-X1)*(XC-X1) + (YC-Y1)*(YC-Y1))
          IF (ISW  .AND.  DC .GE. DL) GO TO 15
          IF (K .EQ. 1) GO TO 14
C
C K .GE. 2.  Store the pointer LPCL to the last neighbor
C   of NC.
C
          LPCL = LEND(NC)
C
C Set DC to the length of the shortest path from N1 to NC
C   which has not previously been encountered and which is
C   a viable candidate for the shortest path from N1 to NL.
C   This is Euclidean distance iff NC is visible from N1.
C   Since the shortest path from N1 to NL contains only ele-
C   ments of NPTS which are constraint nodes (in addition to
C   N1 and NL), only these need be considered for the path
C   from N1 to NC.  Thus, for distance function D(A,B) and
C   J = 1,...,K, DC = min(D(N1,NJ) + D(NJ,NC)) over con-
C   straint nodes NJ = NPTS(J) which are visible from NC.
C
          DO 13 J = 1,KM1
            NJ = NPTS(J)
            IF (J .GT. 1  .AND.  NJ .LT. LCC1) GO TO 13
C
C If NC is a visible neighbor of NJ, a path from N1 to NC
C   containing NJ has already been considered.  Thus, NJ may
C   be bypassed if it is adjacent to NC.
C
            LP = LPCL
    8       LP = LPTR(LP)
              IF ( NJ .EQ. ABS(LIST(LP)) ) GO TO 12
              IF (LP .NE. LPCL) GO TO 8
C
C NJ is a constraint node (unless J=1) not adjacent to NC,
C   and is visible from NC iff NJ-NC is not intersected by
C   a constraint arc.  Loop on constraints I in reverse
C   order --
C
            XJ = X(NJ)
            YJ = Y(NJ)
            IFRST = NN+1
            DO 11 I = NCC,1,-1
              ILAST = IFRST - 1
              IFRST = LCC(I)
              NF1 = ILAST
              NCF = NF1 .EQ. NC
              NJF = NF1 .EQ. NJ
              SKIP = NCF  .OR.  NJF
C
C Loop on boundary constraint arcs NF1-NF2 which contain
C   neither NC nor NJ.  NCF and NJF are TRUE iff NC (or NJ)
C   has been encountered in the constraint, and SKIP =
C   .TRUE. iff NF1 = NC or NF1 = NJ.
C
              DO 10 NF2 = IFRST,ILAST
                IF (NF2 .EQ. NC) NCF = .TRUE.
                IF (NF2 .EQ. NJ) NJF = .TRUE.
                SKSAV = SKIP
                SKIP = NF2 .EQ. NC  .OR.  NF2 .EQ. NJ
C
C   The last constraint arc in the constraint need not be
C     tested if none of the arcs have been skipped.
C
                IF ( SKSAV  .OR.  SKIP  .OR.
     .               (NF2 .EQ. ILAST  .AND.
     .               .NOT. NCF  .AND.  .NOT. NJF) ) GO TO 9
                IF ( INTSEC(X(NF1),Y(NF1),X(NF2),Y(NF2),
     .                      XC,YC,XJ,YJ) ) GO TO 12
    9           NF1 = NF2
   10           CONTINUE
              IF (.NOT. NCF  .OR.  .NOT. NJF) GO TO 11
C
C NC and NJ are constraint nodes in the same constraint.
C   NC-NJ is intersected by an interior constraint arc iff
C   1)  NC LEFT NF2->NF1 and (NJ LEFT NF1->NC and NJ LEFT
C         NC->NF2) or
C   2)  NC .NOT. LEFT NF2->NF1 and (NJ LEFT NF1->NC or
C         NJ LEFT NC->NF2),
C   where NF1, NC, NF2 are consecutive constraint nodes.
C
              IF (NC .NE. IFRST) THEN
                NF1 = NC - 1
              ELSE
                NF1 = ILAST
              ENDIF
              IF (NC .NE. ILAST) THEN
                NF2 = NC + 1
              ELSE
                NF2 = IFRST
              ENDIF
              LFT1 = (XC-X(NF1))*(YJ-Y(NF1)) .GE.
     .               (XJ-X(NF1))*(YC-Y(NF1))
              LFT2 = (X(NF2)-XC)*(YJ-YC) .GE.
     .               (XJ-XC)*(Y(NF2)-YC)
              LFT12 = (X(NF1)-X(NF2))*(YC-Y(NF2)) .GE.
     .                (XC-X(NF2))*(Y(NF1)-Y(NF2))
              IF ( (LFT1  .AND.  LFT2)  .OR.  (.NOT. LFT12
     .             .AND.  (LFT1  .OR.  LFT2)) ) GO TO 12
   11         CONTINUE
C
C NJ is visible from NC.  Exit the loop with DC = Euclidean
C   distance if J = 1.
C
            IF (J .EQ. 1) GO TO 14
            DC = MIN(DC,DS(J) + SQRT((XC-XJ)*(XC-XJ) +
     .                  (YC-YJ)*(YC-YJ)))
            GO TO 13
C
C NJ is not visible from NC or is adjacent to NC.  Initial-
C   ize DC with D(N1,NK) + D(NK,NC) if J = 1.
C
   12       IF (J .EQ. 1) DC = DS(K) + SQRT((XC-XK)*(XC-XK)
     .                         + (YC-YK)*(YC-YK))
   13       CONTINUE
C
C Compare DC with DL.
C
          IF (ISW  .AND.  DC .GE. DL) GO TO 15
C
C The first (or a closer) candidate for NL has been
C   encountered.
C
   14     NL = NC
          DL = DC
          ISW = .TRUE.
   15     IF (LPK .NE. LPKL) GO TO 7
   16   CONTINUE
C
C Unmark the elements of NPTS and store NL and DL.
C
      DO 17 K = 1,LM1
        NK = NPTS(K)
        LEND(NK) = -LEND(NK)
   17   CONTINUE
      NPTS(L) = NL
      DS(L) = DL
      IER = 0
      RETURN
      END
      INTEGER FUNCTION INDXCC (NCC,LCC,N,LIST,LEND)
      INTEGER NCC, LCC(*), N, LIST(*), LEND(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   08/25/91
C
C   Given a constrained Delaunay triangulation, this func-
C tion returns the index, if any, of an exterior constraint
C curve (an unbounded constraint region).  An exterior con-
C straint curve is assumed to be present if and only if the
C clockwise-ordered sequence of boundary nodes is a subse-
C quence of a constraint node sequence.  The triangulation
C adjacencies corresponding to constraint edges may or may
C not have been forced by a call to ADDCST, and the con-
C straint region may or may not be valid (contain no nodes).
C
C
C On input:
C
C       NCC = Number of constraints.  NCC .GE. 0.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             Subroutine ADDCST.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LEND = Data structure defining the triangula-
C                   tion.  Refer to Subroutine TRMESH.
C
C   Input parameters are not altered by this function.  Note
C that the parameters are not tested for validity.
C
C On output:
C
C       INDXCC = Index of the exterior constraint curve, if
C                present, or 0 otherwise.
C
C Modules required by INDXCC:  None
C
C***********************************************************
C
      INTEGER I, IFRST, ILAST, LP, N0, NST, NXT
      INDXCC = 0
      IF (NCC .LT. 1) RETURN
C
C Set N0 to the boundary node with smallest index.
C
      N0 = 0
    1 N0 = N0 + 1
        LP = LEND(N0)
        IF (LIST(LP) .GT. 0) GO TO 1
C
C Search in reverse order for the constraint I, if any, that
C   contains N0.  IFRST and ILAST index the first and last
C   nodes in constraint I.
C
      I = NCC
      ILAST = N
    2 IFRST = LCC(I)
        IF (N0 .GE. IFRST) GO TO 3
        IF (I .EQ. 1) RETURN
        I = I - 1
        ILAST = IFRST - 1
        GO TO 2
C
C N0 is in constraint I which indexes an exterior constraint
C   curve iff the clockwise-ordered sequence of boundary
C   node indexes beginning with N0 is increasing and bounded
C   above by ILAST.
C
    3 NST = N0
C
    4 NXT = -LIST(LP)
        IF (NXT .EQ. NST) GO TO 5
        IF (NXT .LE. N0  .OR.  NXT .GT. ILAST) RETURN
        N0 = NXT
        LP = LEND(N0)
        GO TO 4
C
C Constraint I contains the boundary node sequence as a
C   subset.
C
    5 INDXCC = I
      RETURN
      END
      SUBROUTINE INSERT (K,LP, LIST,LPTR,LNEW )
      INTEGER K, LP, LIST(*), LPTR(*), LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   This subroutine inserts K as a neighbor of N1 following
C N2, where LP is the LIST pointer of N2 as a neighbor of
C N1.  Note that, if N2 is the last neighbor of N1, K will
C become the first neighbor (even if N1 is a boundary node).
C
C
C On input:
C
C       K = Index of the node to be inserted.
C
C       LP = LIST pointer of N2 as a neighbor of N1.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LNEW = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LNEW = Data structure updated with the
C                        addition of node K.
C
C Modules required by INSERT:  None
C
C***********************************************************
C
      INTEGER LSAV
C
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = K
      LPTR(LNEW) = LSAV
      LNEW = LNEW + 1
      RETURN
      END
      SUBROUTINE INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, I3, LIST(*), LPTR(*), LEND(*),
     .        LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   02/22/91
C
C   This subroutine adds an interior node to a triangulation
C of a set of points in the plane.  The data structure is
C updated with the insertion of node KK into the triangle
C whose vertices are I1, I2, and I3.  No optimization of the
C triangulation is performed.
C
C
C On input:
C
C       KK = Index of the node to be inserted.  KK .GE. 1
C            and KK must not be equal to I1, I2, or I3.
C
C       I1,I2,I3 = Indexes of the counterclockwise-ordered
C                  sequence of vertices of a triangle which
C                  contains node KK.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine TRMESH.  Triangle
C                             (I1,I2,I3) must be included
C                             in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  KK
C                             will be connected to nodes I1,
C                             I2, and I3.
C
C Modules required by INTADD:  INSERT, LSTPTR
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER K, LP, N1, N2, N3
      K = KK
C
C Initialization.
C
      N1 = I1
      N2 = I2
      N3 = I3
C
C Add K as a neighbor of I1, I2, and I3.
C
      LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
      LP = LSTPTR(LEND(N2),N3,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
      LP = LSTPTR(LEND(N3),N1,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
C
C Add I1, I2, and I3 as neighbors of K.
C
      LIST(LNEW) = N1
      LIST(LNEW+1) = N2
      LIST(LNEW+2) = N3
      LPTR(LNEW) = LNEW + 1
      LPTR(LNEW+1) = LNEW + 2
      LPTR(LNEW+2) = LNEW
      LEND(K) = LNEW + 2
      LNEW = LNEW + 3
      RETURN
      END
      LOGICAL FUNCTION INTSEC (X1,Y1,X2,Y2,X3,Y3,X4,Y4)
      DOUBLE PRECISION X1, Y1, X2, Y2, X3, Y3, X4, Y4
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   Given a pair of line segments P1-P2 and P3-P4, this
C function returns the value .TRUE. if and only if P1-P2
C shares one or more points with P3-P4.  The line segments
C include their endpoints, and the four points need not be
C distinct.  Thus, either line segment may consist of a
C single point, and the segments may meet in a V (which is
C treated as an intersection).  Note that an incorrect
C decision may result from floating point error if the four
C endpoints are nearly collinear.
C
C
C On input:
C
C       X1,Y1 = Coordinates of P1.
C
C       X2,Y2 = Coordinates of P2.
C
C       X3,Y3 = Coordinates of P3.
C
C       X4,Y4 = Coordinates of P4.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       INTSEC = Logical value defined above.
C
C Modules required by INTSEC:  None
C
C***********************************************************
C
      DOUBLE PRECISION A, B, D, DX12, DX31, DX34, DY12,
     .        DY31, DY34
C
C Test for overlap between the smallest rectangles that
C   contain the line segments and have sides parallel to
C   the axes.
C
      IF ((X1 .LT. X3  .AND.  X1 .LT. X4  .AND.  X2 .LT. X3
     .     .AND.  X2 .LT. X4)  .OR.
     .    (X1 .GT. X3  .AND.  X1 .GT. X4  .AND.  X2 .GT. X3
     .     .AND.  X2 .GT. X4)  .OR.
     .    (Y1 .LT. Y3  .AND.  Y1 .LT. Y4  .AND.  Y2 .LT. Y3
     .     .AND.  Y2 .LT. Y4)  .OR.
     .    (Y1 .GT. Y3  .AND.  Y1 .GT. Y4  .AND.  Y2 .GT. Y3
     .     .AND.  Y2 .GT. Y4)) THEN
        INTSEC = .FALSE.
        RETURN
      ENDIF
C
C Compute A = P4-P3 X P1-P3, B = P2-P1 X P1-P3, and
C   D = P2-P1 X P4-P3 (Z components).
C
      DX12 = X2 - X1
      DY12 = Y2 - Y1
      DX34 = X4 - X3
      DY34 = Y4 - Y3
      DX31 = X1 - X3
      DY31 = Y1 - Y3
      A = DX34*DY31 - DX31*DY34
      B = DX12*DY31 - DX31*DY12
      D = DX12*DY34 - DX34*DY12
      IF (D .EQ. 0.) GO TO 1
C
C D .NE. 0 and the point of intersection of the lines de-
C   fined by the line segments is P = P1 + (A/D)*(P2-P1) =
C   P3 + (B/D)*(P4-P3).
C
      INTSEC = A/D .GE. 0.  .AND.  A/D .LE. 1.  .AND.
     .         B/D .GE. 0.  .AND.  B/D .LE. 1.
      RETURN
C
C D .EQ. 0 and thus either the line segments are parallel,
C   or one (or both) of them is a single point.
C
    1 INTSEC = A .EQ. 0.  .AND.  B .EQ. 0.
      RETURN
      END
      INTEGER FUNCTION JRAND (N, IX,IY,IZ )
      INTEGER N, IX, IY, IZ
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   This function returns a uniformly distributed pseudo-
C random integer in the range 1 to N.
C
C
C On input:
C
C       N = Maximum value to be returned.
C
C N is not altered by this function.
C
C       IX,IY,IZ = Integer seeds initialized to values in
C                  the range 1 to 30,000 before the first
C                  call to JRAND, and not altered between
C                  subsequent calls (unless a sequence of
C                  random numbers is to be repeated by
C                  reinitializing the seeds).
C
C On output:
C
C       IX,IY,IZ = Updated integer seeds.
C
C       JRAND = Random integer in the range 1 to N.
C
C Reference:  B. A. Wichmann and I. D. Hill, "An Efficient
C             and Portable Pseudo-random Number Generator",
C             Applied Statistics, Vol. 31, No. 2, 1982,
C             pp. 188-190.
C
C Modules required by JRAND:  None
C
C Intrinsic functions called by JRAND:  INT, MOD, DBLE
C
C***********************************************************
C
      DOUBLE PRECISION U, X
C
C Local parameters:
C
C U = Pseudo-random number uniformly distributed in the
C     interval (0,1).
C X = Pseudo-random number in the range 0 to 3 whose frac-
C       tional part is U.
C
      IX = MOD(171*IX,30269)
      IY = MOD(172*IY,30307)
      IZ = MOD(170*IZ,30323)
      X = (DBLE(IX)/30269.) + (DBLE(IY)/30307.) +
     .    (DBLE(IZ)/30323.)
      U = X - INT(X)
      JRAND = DBLE(N)*U + 1.
      RETURN
      END
      LOGICAL FUNCTION LEFT (X1,Y1,X2,Y2,X0,Y0)
      DOUBLE PRECISION X1, Y1, X2, Y2, X0, Y0
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   This function determines whether node N0 is to the left
C or to the right of the line through N1-N2 as viewed by an
C observer at N1 facing N2.
C
C
C On input:
C
C       X1,Y1 = Coordinates of N1.
C
C       X2,Y2 = Coordinates of N2.
C
C       X0,Y0 = Coordinates of N0.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LEFT = .TRUE. if and only if (X0,Y0) is on or to the
C              left of the directed line N1->N2.
C
C Modules required by LEFT:  None
C
C***********************************************************
C
      DOUBLE PRECISION DX1, DY1, DX2, DY2
C
C Local parameters:
C
C DX1,DY1 = X,Y components of the vector N1->N2
C DX2,DY2 = X,Y components of the vector N1->N0
C
      DX1 = X2-X1
      DY1 = Y2-Y1
      DX2 = X0-X1
      DY2 = Y0-Y1
C
C If the sign of the vector cross product of N1->N2 and
C   N1->N0 is positive, then sin(A) > 0, where A is the
C   angle between the vectors, and thus A is in the range
C   (0,180) degrees.
C
      LEFT = DX1*DY2 .GE. DX2*DY1
      RETURN
      END
      INTEGER FUNCTION LSTPTR (LPL,NB,LIST,LPTR)
      INTEGER LPL, NB, LIST(*), LPTR(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   This function returns the index (LIST pointer) of NB in
C the adjacency list for N0, where LPL = LEND(N0).
C
C
C On input:
C
C       LPL = LEND(N0)
C
C       NB = Index of the node whose pointer is to be re-
C            turned.  NB must be connected to N0.
C
C       LIST,LPTR = Data structure defining the triangula-
C                   tion.  Refer to Subroutine TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LSTPTR = Pointer such that LIST(LSTPTR) = NB or
C                LIST(LSTPTR) = -NB, unless NB is not a
C                neighbor of N0, in which case LSTPTR = LPL.
C
C Modules required by LSTPTR:  None
C
C***********************************************************
C
      INTEGER LP, ND
C
      LP = LPTR(LPL)
    1 ND = LIST(LP)
        IF (ND .EQ. NB) GO TO 2
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 1
C
    2 LSTPTR = LP
      RETURN
      END
      INTEGER FUNCTION NBCNT (LPL,LPTR)
      INTEGER LPL, LPTR(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   This function returns the number of neighbors of a node
C N0 in a triangulation created by Subroutine TRMESH (or
C TRMSHR).
C
C
C On input:
C
C       LPL = LIST pointer to the last neighbor of N0 --
C             LPL = LEND(N0).
C
C       LPTR = Array of pointers associated with LIST.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NBCNT = Number of neighbors of N0.
C
C Modules required by NBCNT:  None
C
C***********************************************************
C
      INTEGER K, LP
C
      LP = LPL
      K = 1
C
    1 LP = LPTR(LP)
        IF (LP .EQ. LPL) GO TO 2
        K = K + 1
        GO TO 1
C
    2 NBCNT = K
      RETURN
      END
      INTEGER FUNCTION NEARND (XP,YP,IST,N,X,Y,LIST,LPTR,
     .                         LEND, DSQ)
      INTEGER IST, N, LIST(*), LPTR(*), LEND(N)
      DOUBLE PRECISION XP, YP, X(N), Y(N), DSQ
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/27/98
C
C   Given a point P in the plane and a Delaunay triangula-
C tion created by Subroutine TRMESH or TRMSHR, this function
C returns the index of the nearest triangulation node to P.
C
C   The algorithm consists of implicitly adding P to the
C triangulation, finding the nearest neighbor to P, and
C implicitly deleting P from the triangulation.  Thus, it
C is based on the fact that, if P is a node in a Delaunay
C triangulation, the nearest node to P is a neighbor of P.
C
C
C On input:
C
C       XP,YP = Cartesian coordinates of the point P to be
C               located relative to the triangulation.
C
C       IST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of the nodes.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NEARND = Nodal index of the nearest node to P, or 0
C                if N < 3 or the triangulation data struc-
C                ture is invalid.
C
C       DSQ = Squared distance between P and NEARND unless
C             NEARND = 0.
C
C       Note that the number of candidates for NEARND
C       (neighbors of P) is limited to LMAX defined in
C       the PARAMETER statement below.
C
C Modules required by NEARND:  JRAND, LEFT, LSTPTR, TRFIND
C
C Intrinsic function called by NEARND:  ABS
C
C***********************************************************
C
      INTEGER   LSTPTR
      INTEGER   LMAX
      PARAMETER (LMAX=25)
      INTEGER   I1, I2, I3, L, LISTP(LMAX), LP, LP1, LP2,
     .          LPL, LPTRP(LMAX), N1, N2, N3, NR, NST
      DOUBLE PRECISION   COS1, COS2, DS1, DSR, DX11, DX12, 
     .          DX21, DX22, DY11, DY12, DY21, DY22, SIN1, 
     .          SIN2
C
C Store local parameters and test for N invalid.
C
      IF (N .LT. 3) GO TO 7
      NST = IST
      IF (NST .LT. 1  .OR.  NST .GT. N) NST = 1
C
C Find a triangle (I1,I2,I3) containing P, or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from P.
C
      CALL TRFIND (NST,XP,YP,N,X,Y,LIST,LPTR,LEND, I1,I2,I3)
C
C Test for collinear nodes.
C
      IF (I1 .EQ. 0) GO TO 7
C
C Store the linked list of 'neighbors' of P in LISTP and
C   LPTRP.  I1 is the first neighbor, and 0 is stored as
C   the last neighbor if P is not contained in a triangle.
C   L is the length of LISTP and LPTRP, and is limited to
C   LMAX.
C
      IF (I3 .NE. 0) THEN
        LISTP(1) = I1
        LPTRP(1) = 2
        LISTP(2) = I2
        LPTRP(2) = 3
        LISTP(3) = I3
        LPTRP(3) = 1
        L = 3
      ELSE
        N1 = I1
        L = 1
        LP1 = 2
        LISTP(L) = N1
        LPTRP(L) = LP1
C
C   Loop on the ordered sequence of visible boundary nodes
C     N1 from I1 to I2.
C
    1   LPL = LEND(N1)
          N1 = -LIST(LPL)
          L = LP1
          LP1 = L+1
          LISTP(L) = N1
          LPTRP(L) = LP1
          IF (N1 .NE. I2  .AND.  LP1 .LT. LMAX) GO TO 1
        L = LP1
        LISTP(L) = 0
        LPTRP(L) = 1
      ENDIF
C
C Initialize variables for a loop on arcs N1-N2 opposite P
C   in which new 'neighbors' are 'swapped' in.  N1 follows
C   N2 as a neighbor of P, and LP1 and LP2 are the LISTP
C   indexes of N1 and N2.
C
      LP2 = 1
      N2 = I1
      LP1 = LPTRP(1)
      N1 = LISTP(LP1)
C
C Begin loop:  find the node N3 opposite N1->N2.
C
    2 LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 4
        LP = LPTR(LP)
        N3 = ABS(LIST(LP))
C
C Swap test:  Exit the loop if L = LMAX.
C
        IF (L .EQ. LMAX) GO TO 5
        DX11 = X(N1) - X(N3)
        DX12 = X(N2) - X(N3)
        DX22 = X(N2) - XP
        DX21 = X(N1) - XP
C
        DY11 = Y(N1) - Y(N3)
        DY12 = Y(N2) - Y(N3)
        DY22 = Y(N2) - YP
        DY21 = Y(N1) - YP
C
        COS1 = DX11*DX12 + DY11*DY12
        COS2 = DX22*DX21 + DY22*DY21
        IF (COS1 .GE. 0.  .AND.  COS2 .GE. 0.) GO TO 4
        IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 3
C
        SIN1 = DX11*DY12 - DX12*DY11
        SIN2 = DX22*DY21 - DX21*DY22
        IF (SIN1*COS2 + COS1*SIN2 .GE. 0.) GO TO 4
C
C Swap:  Insert N3 following N2 in the adjacency list for P.
C        The two new arcs opposite P must be tested.
C
    3   L = L+1
        LPTRP(LP2) = L
        LISTP(L) = N3
        LPTRP(L) = LP1
        LP1 = L
        N1 = N3
        GO TO 2
C
C No swap:  Advance to the next arc and test for termination
C           on N1 = I1 (LP1 = 1) or N1 followed by 0.
C
    4   IF (LP1 .EQ. 1) GO TO 5
        LP2 = LP1
        N2 = N1
        LP1 = LPTRP(LP1)
        N1 = LISTP(LP1)
        IF (N1 .EQ. 0) GO TO 5
        GO TO 2
C
C Set NR and DSR to the index of the nearest node to P and
C   its squared distance from P, respectively.
C
    5 NR = I1
      DSR = (X(NR)-XP)**2 + (Y(NR)-YP)**2
      DO 6 LP = 2,L
        N1 = LISTP(LP)
        IF (N1 .EQ. 0) GO TO 6
        DS1 = (X(N1)-XP)**2 + (Y(N1)-YP)**2
        IF (DS1 .LT. DSR) THEN
          NR = N1
          DSR = DS1
        ENDIF
    6   CONTINUE
      DSQ = DSR
      NEARND = NR
      RETURN
C
C Invalid input.
C
    7 NEARND = 0
      RETURN
      END
      SUBROUTINE OPTIM (X,Y,NA, LIST,LPTR,LEND,NIT,IWK, IER)
      INTEGER NA, LIST(*), LPTR(*), LEND(*), NIT, IWK(2,NA),
     .        IER
      DOUBLE PRECISION X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/27/98
C
C   Given a set of NA triangulation arcs, this subroutine
C optimizes the portion of the triangulation consisting of
C the quadrilaterals (pairs of adjacent triangles) which
C have the arcs as diagonals by applying the circumcircle
C test and appropriate swaps to the arcs.
C
C   An iteration consists of applying the swap test and
C swaps to all NA arcs in the order in which they are
C stored.  The iteration is repeated until no swap occurs
C or NIT iterations have been performed.  The bound on the
C number of iterations may be necessary to prevent an
C infinite loop caused by cycling (reversing the effect of a
C previous swap) due to floating point inaccuracy when four
C or more nodes are nearly cocircular.
C
C
C On input:
C
C       X,Y = Arrays containing the nodal coordinates.
C
C       NA = Number of arcs in the set.  NA .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       NIT = Maximum number of iterations to be performed.
C             A reasonable value is 3*NA.  NIT .GE. 1.
C
C       IWK = Integer array dimensioned 2 by NA containing
C             the nodal indexes of the arc endpoints (pairs
C             of endpoints are stored in columns).
C
C On output:
C
C       LIST,LPTR,LEND = Updated triangulation data struc-
C                        ture reflecting the swaps.
C
C       NIT = Number of iterations performed.
C
C       IWK = Endpoint indexes of the new set of arcs
C             reflecting the swaps.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if a swap occurred on the last of
C                     MAXIT iterations, where MAXIT is the
C                     value of NIT on input.  The new set
C                     of arcs in not necessarily optimal
C                     in this case.
C             IER = 2 if NA < 0 or NIT < 1 on input.
C             IER = 3 if IWK(2,I) is not a neighbor of
C                     IWK(1,I) for some I in the range 1
C                     to NA.  A swap may have occurred in
C                     this case.
C             IER = 4 if a zero pointer was returned by
C                     Subroutine SWAP.
C
C Modules required by OPTIM:  LSTPTR, SWAP, SWPTST
C
C Intrinsic function called by OPTIM:  ABS
C
C***********************************************************
C
      LOGICAL SWPTST
      INTEGER I, IO1, IO2, ITER, LP, LP21, LPL, LPP, MAXIT,
     .        N1, N2, NNA
      LOGICAL SWP
C
C Local parameters:
C
C I =       Column index for IWK
C IO1,IO2 = Nodal indexes of the endpoints of an arc in IWK
C ITER =    Iteration count
C LP =      LIST pointer
C LP21 =    Parameter returned by SWAP (not used)
C LPL =     Pointer to the last neighbor of IO1
C LPP =     Pointer to the node preceding IO2 as a neighbor
C             of IO1
C MAXIT =   Input value of NIT
C N1,N2 =   Nodes opposite IO1->IO2 and IO2->IO1,
C             respectively
C NNA =     Local copy of NA
C SWP =     Flag set to TRUE iff a swap occurs in the
C             optimization loop
C
      NNA = NA
      MAXIT = NIT
      IF (NNA .LT. 0  .OR.  MAXIT .LT. 1) GO TO 7
C
C Initialize iteration count ITER and test for NA = 0.
C
      ITER = 0
      IF (NNA .EQ. 0) GO TO 5
C
C Top of loop --
C   SWP = TRUE iff a swap occurred in the current iteration.
C
    1 IF (ITER .EQ. MAXIT) GO TO 6
      ITER = ITER + 1
      SWP = .FALSE.
C
C   Inner loop on arcs IO1-IO2 --
C
      DO 4 I = 1,NNA
        IO1 = IWK(1,I)
        IO2 = IWK(2,I)
C
C   Set N1 and N2 to the nodes opposite IO1->IO2 and
C     IO2->IO1, respectively.  Determine the following:
C
C     LPL = pointer to the last neighbor of IO1,
C     LP = pointer to IO2 as a neighbor of IO1, and
C     LPP = pointer to the node N2 preceding IO2.
C
        LPL = LEND(IO1)
        LPP = LPL
        LP = LPTR(LPP)
    2   IF (LIST(LP) .EQ. IO2) GO TO 3
          LPP = LP
          LP = LPTR(LPP)
          IF (LP .NE. LPL) GO TO 2
C
C   IO2 should be the last neighbor of IO1.  Test for no
C     arc and bypass the swap test if IO1 is a boundary
C     node.
C
        IF (ABS(LIST(LP)) .NE. IO2) GO TO 8
        IF (LIST(LP) .LT. 0) GO TO 4
C
C   Store N1 and N2, or bypass the swap test if IO1 is a
C     boundary node and IO2 is its first neighbor.
C
    3   N2 = LIST(LPP)
        IF (N2 .LT. 0) GO TO 4
        LP = LPTR(LP)
        N1 = ABS(LIST(LP))
C
C   Test IO1-IO2 for a swap, and update IWK if necessary.
C
        IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y) ) GO TO 4
        CALL SWAP (N1,N2,IO1,IO2, LIST,LPTR,LEND, LP21)
        IF (LP21 .EQ. 0) GO TO 9
        SWP = .TRUE.
        IWK(1,I) = N1
        IWK(2,I) = N2
    4   CONTINUE
      IF (SWP) GO TO 1
C
C Successful termination.
C
    5 NIT = ITER
      IER = 0
      RETURN
C
C MAXIT iterations performed without convergence.
C
    6 NIT = MAXIT
      IER = 1
      RETURN
C
C Invalid input parameter.
C
    7 NIT = 0
      IER = 2
      RETURN
C
C IO2 is not a neighbor of IO1.
C
    8 NIT = ITER
      IER = 3
      RETURN
C
C Zero pointer returned by SWAP.
C
    9 NIT = ITER
      IER = 4
      RETURN
      END
      DOUBLE PRECISION FUNCTION STORE (X)
      DOUBLE PRECISION X
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   03/18/90
C
C   This function forces its argument X to be stored in a
C memory location, thus providing a means of determining
C floating point number characteristics (such as the machine
C precision) when it is necessary to avoid computation in
C high precision registers.
C
C
C On input:
C
C       X = Value to be stored.
C
C X is not altered by this function.
C
C On output:
C
C       STORE = Value of X after it has been stored and
C               possibly truncated or rounded to the single
C               precision word length.
C
C Modules required by STORE:  None
C
C***********************************************************
C
      DOUBLE PRECISION Y
      COMMON/STCOM/Y
C
      Y = X
      STORE = Y
      RETURN
      END
      SUBROUTINE SWAP (IN1,IN2,IO1,IO2, LIST,LPTR,
     .                 LEND, LP21)
      INTEGER IN1, IN2, IO1, IO2, LIST(*), LPTR(*), LEND(*),
     .        LP21
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/22/98
C
C   Given a triangulation of a set of points on the unit
C sphere, this subroutine replaces a diagonal arc in a
C strictly convex quadrilateral (defined by a pair of adja-
C cent triangles) with the other diagonal.  Equivalently, a
C pair of adjacent triangles is replaced by another pair
C having the same union.
C
C
C On input:
C
C       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of
C                         the quadrilateral.  IO1-IO2 is re-
C                         placed by IN1-IN2.  (IO1,IO2,IN1)
C                         and (IO2,IO1,IN2) must be trian-
C                         gles on input.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LEND = Data structure updated with the
C                        swap -- triangles (IO1,IO2,IN1) and
C                        (IO2,IO1,IN2) are replaced by
C                        (IN1,IN2,IO2) and (IN2,IN1,IO1)
C                        unless LP21 = 0.
C
C       LP21 = Index of IN1 as a neighbor of IN2 after the
C              swap is performed unless IN1 and IN2 are
C              adjacent on input, in which case LP21 = 0.
C
C Module required by SWAP:  LSTPTR
C
C Intrinsic function called by SWAP:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER LP, LPH, LPSAV
C
C Local parameters:
C
C LP,LPH,LPSAV = LIST pointers
C
C
C Test for IN1 and IN2 adjacent.
C
      LP = LSTPTR(LEND(IN1),IN2,LIST,LPTR)
      IF (ABS(LIST(LP)) .EQ. IN2) THEN
        LP21 = 0
        RETURN
      ENDIF
C
C Delete IO2 as a neighbor of IO1.
C
      LP = LSTPTR(LEND(IO1),IN2,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO2 is the last neighbor of IO1, make IN2 the
C   last neighbor.
C
      IF (LEND(IO1) .EQ. LPH) LEND(IO1) = LP
C
C Insert IN2 as a neighbor of IN1 following IO1
C   using the hole created above.
C
      LP = LSTPTR(LEND(IN1),IO1,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN2
      LPTR(LPH) = LPSAV
C
C Delete IO1 as a neighbor of IO2.
C
      LP = LSTPTR(LEND(IO2),IN1,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO1 is the last neighbor of IO2, make IN1 the
C   last neighbor.
C
      IF (LEND(IO2) .EQ. LPH) LEND(IO2) = LP
C
C Insert IN1 as a neighbor of IN2 following IO2.
C
      LP = LSTPTR(LEND(IN2),IO2,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN1
      LPTR(LPH) = LPSAV
      LP21 = LPH
      RETURN
      END
      LOGICAL FUNCTION SWPTST (IN1,IN2,IO1,IO2,X,Y)
      INTEGER IN1, IN2, IO1, IO2
      DOUBLE PRECISION X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/01/88
C
C   This function applies the circumcircle test to a quadri-
C lateral defined by a pair of adjacent triangles.  The
C diagonal arc (shared triangle side) should be swapped for
C the other diagonl if and only if the fourth vertex is
C strictly interior to the circumcircle of one of the
C triangles (the decision is independent of the choice of
C triangle).  Equivalently, the diagonal is chosen to maxi-
C mize the smallest of the six interior angles over the two
C pairs of possible triangles (the decision is for no swap
C if the quadrilateral is not strictly convex).
C
C   When the four vertices are nearly cocircular (the
C neutral case), the preferred decision is no swap -- in
C order to avoid unnecessary swaps and, more important, to
C avoid cycling in Subroutine OPTIM which is called by
C DELNOD and EDGE.  Thus, a tolerance SWTOL (stored in
C SWPCOM by TRMESH or TRMSHR) is used to define 'nearness'
C to the neutral case.
C
C
C On input:
C
C       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of
C                         the quadrilateral.  IO1-IO2 is the
C                         triangulation arc (shared triangle
C                         side) to be replaced by IN1-IN2 if
C                         the decision is to swap.  The
C                         triples (IO1,IO2,IN1) and (IO2,
C                         IO1,IN2) must define triangles (be
C                         in counterclockwise order) on in-
C                         put.
C
C       X,Y = Arrays containing the nodal coordinates.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       SWPTST = .TRUE. if and only if the arc connecting
C                IO1 and IO2 is to be replaced.
C
C Modules required by SWPTST:  None
C
C***********************************************************
C
      DOUBLE PRECISION DX11, DX12, DX22, DX21, DY11, DY12, DY22, DY21,
     .     SIN1, SIN2, COS1, COS2, SIN12, SWTOL
C
C Tolerance stored by TRMESH or TRMSHR.
C
      COMMON/SWPCOM/SWTOL
C
C Local parameters:
C
C DX11,DY11 = X,Y components of the vector IN1->IO1
C DX12,DY12 = X,Y components of the vector IN1->IO2
C DX22,DY22 = X,Y components of the vector IN2->IO2
C DX21,DY21 = X,Y components of the vector IN2->IO1
C SIN1 =      Cross product of the vectors IN1->IO1 and
C               IN1->IO2 -- proportional to sin(T1), where
C               T1 is the angle at IN1 formed by the vectors
C COS1 =      Inner product of the vectors IN1->IO1 and
C               IN1->IO2 -- proportional to cos(T1)
C SIN2 =      Cross product of the vectors IN2->IO2 and
C               IN2->IO1 -- proportional to sin(T2), where
C               T2 is the angle at IN2 formed by the vectors
C COS2 =      Inner product of the vectors IN2->IO2 and
C               IN2->IO1 -- proportional to cos(T2)
C SIN12 =     SIN1*COS2 + COS1*SIN2 -- proportional to
C               sin(T1+T2)
C
C
C Compute the vectors containing the angles T1 and T2.
C
      DX11 = X(IO1) - X(IN1)
      DX12 = X(IO2) - X(IN1)
      DX22 = X(IO2) - X(IN2)
      DX21 = X(IO1) - X(IN2)
C
      DY11 = Y(IO1) - Y(IN1)
      DY12 = Y(IO2) - Y(IN1)
      DY22 = Y(IO2) - Y(IN2)
      DY21 = Y(IO1) - Y(IN2)
C
C Compute inner products.
C
      COS1 = DX11*DX12 + DY11*DY12
      COS2 = DX22*DX21 + DY22*DY21
C
C The diagonals should be swapped iff (T1+T2) > 180
C   degrees.  The following two tests ensure numerical
C   stability:  the decision must be FALSE when both
C   angles are close to 0, and TRUE when both angles
C   are close to 180 degrees.
C
      IF (COS1 .GE. 0.  .AND.  COS2 .GE. 0.) GO TO 2
      IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 1
C
C Compute vector cross products (Z-components).
C
      SIN1 = DX11*DY12 - DX12*DY11
      SIN2 = DX22*DY21 - DX21*DY22
      SIN12 = SIN1*COS2 + COS1*SIN2
      IF (SIN12 .GE. -SWTOL) GO TO 2
C
C Swap.
C
    1 SWPTST = .TRUE.
      RETURN
C
C No swap.
C
    2 SWPTST = .FALSE.
      RETURN
      END
      SUBROUTINE TRFIND (NST,PX,PY,N,X,Y,LIST,LPTR,LEND, I1,
     .                   I2,I3)
      INTEGER NST, N, LIST(*), LPTR(*), LEND(N), I1, I2, I3
      DOUBLE PRECISION PX, PY, X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   This subroutine locates a point P relative to a triangu-
C lation created by Subroutine TRMESH or TRMSHR.  If P is
C contained in a triangle, the three vertex indexes are
C returned.  Otherwise, the indexes of the rightmost and
C leftmost visible boundary nodes are returned.
C
C
C On input:
C
C       NST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       PX,PY = X and y coordinates of the point P to be
C               located.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes in the triangulation.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       I1,I2,I3 = Nodal indexes, in counterclockwise order,
C                  of the vertices of a triangle containing
C                  P if P is contained in a triangle.  If P
C                  is not in the convex hull of the nodes,
C                  I1 indexes the rightmost visible boundary
C                  node, I2 indexes the leftmost visible
C                  boundary node, and I3 = 0.  Rightmost and
C                  leftmost are defined from the perspective
C                  of P, and a pair of points are visible
C                  from each other if and only if the line
C                  segment joining them intersects no trian-
C                  gulation arc.  If P and all of the nodes
C                  lie on a common line, then I1 = I2 = I3 =
C                  0 on output.
C
C Modules required by TRFIND:  JRAND, LEFT, LSTPTR, STORE
C
C Intrinsic function called by TRFIND:  ABS
C
C***********************************************************
C
      INTEGER JRAND, LSTPTR
      LOGICAL LEFT
      DOUBLE PRECISION STORE
      INTEGER IX, IY, IZ, LP, N0, N1, N1S, N2, N2S, N3, N4,
     .        NB, NF, NL, NP, NPP
      LOGICAL FRWRD
      DOUBLE PRECISION B1, B2, XA, XB, XC, XP, YA, YB, YC, 
     .        YP
C
      SAVE    IX, IY, IZ
      DATA    IX/1/, IY/2/, IZ/3/
C
C Local parameters:
C
C B1,B2 =    Unnormalized barycentric coordinates of P with
C              respect to (N1,N2,N3)
C IX,IY,IZ = Integer seeds for JRAND
C LP =       LIST pointer
C N0,N1,N2 = Nodes in counterclockwise order defining a
C              cone (with vertex N0) containing P
C N1S,N2S =  Saved values of N1 and N2
C N3,N4 =    Nodes opposite N1->N2 and N2->N1, respectively
C NB =       Index of a boundary node -- first neighbor of
C              NF or last neighbor of NL in the boundary
C              traversal loops
C NF,NL =    First and last neighbors of N0, or first
C              (rightmost) and last (leftmost) nodes
C              visible from P when P is exterior to the
C              triangulation
C NP,NPP =   Indexes of boundary nodes used in the boundary
C              traversal loops
C XA,XB,XC = Dummy arguments for FRWRD
C YA,YB,YC = Dummy arguments for FRWRD
C XP,YP =    Local variables containing the components of P
C
C Statement function:
C
C FRWRD = TRUE iff C is forward of A->B
C              iff <A->B,A->C> .GE. 0.
C
      FRWRD(XA,YA,XB,YB,XC,YC) = (XB-XA)*(XC-XA) +
     .                           (YB-YA)*(YC-YA) .GE. 0.
C
C Initialize variables.
C
      XP = PX
      YP = PY
      N0 = NST
      IF (N0 .LT. 1  .OR.  N0 .GT. N)
     .  N0 = JRAND(N, IX,IY,IZ )
C
C Set NF and NL to the first and last neighbors of N0, and
C   initialize N1 = NF.
C
    1 LP = LEND(N0)
      NL = LIST(LP)
      LP = LPTR(LP)
      NF = LIST(LP)
      N1 = NF
C
C Find a pair of adjacent neighbors N1,N2 of N0 that define
C   a wedge containing P:  P LEFT N0->N1 and P RIGHT N0->N2.
C
      IF (NL .GT. 0) GO TO 2
C
C   N0 is a boundary node.  Test for P exterior.
C
      NL = -NL
      IF ( .NOT. LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) ) THEN
        NL = N0
        GO TO 9
      ENDIF
      IF ( .NOT. LEFT(X(NL),Y(NL),X(N0),Y(N0),XP,YP) ) THEN
        NB = NF
        NF = N0
        NP = NL
        NPP = N0
        GO TO 11
      ENDIF
      GO TO 3
C
C   N0 is an interior node.  Find N1.
C
    2 IF ( LEFT(X(N0),Y(N0),X(N1),Y(N1),XP,YP) ) GO TO 3
        LP = LPTR(LP)
        N1 = LIST(LP)
        IF (N1 .EQ. NL) GO TO 6
        GO TO 2
C
C   P is to the left of edge N0->N1.  Initialize N2 to the
C     next neighbor of N0.
C
    3 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        IF ( .NOT. LEFT(X(N0),Y(N0),X(N2),Y(N2),XP,YP) )
     .    GO TO 7
        N1 = N2
        IF (N1 .NE. NL) GO TO 3
      IF ( .NOT. LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) )
     .  GO TO 6
      IF (XP .EQ. X(N0) .AND. YP .EQ. Y(N0)) GO TO 5
C
C   P is left of or on edges N0->NB for all neighbors NB
C     of N0.
C   All points are collinear iff P is left of NB->N0 for
C     all neighbors NB of N0.  Search the neighbors of N0.
C     NOTE -- N1 = NL and LP points to NL.
C
    4 IF ( .NOT. LEFT(X(N1),Y(N1),X(N0),Y(N0),XP,YP) )
     .  GO TO 5
        LP = LPTR(LP)
        N1 = ABS(LIST(LP))
        IF (N1 .EQ. NL) GO TO 17
        GO TO 4
C
C   P is to the right of N1->N0, or P=N0.  Set N0 to N1 and
C     start over.
C
    5 N0 = N1
      GO TO 1
C
C   P is between edges N0->N1 and N0->NF.
C
    6 N2 = NF
C
C P is contained in the wedge defined by line segments
C   N0->N1 and N0->N2, where N1 is adjacent to N2.  Set
C   N3 to the node opposite N1->N2, and save N1 and N2 to
C   test for cycling.
C
    7 N3 = N0
      N1S = N1
      N2S = N2
C
C Top of edge hopping loop.  Test for termination.
C
    8 IF ( LEFT(X(N1),Y(N1),X(N2),Y(N2),XP,YP) ) THEN
C
C   P LEFT N1->N2 and hence P is in (N1,N2,N3) unless an
C     error resulted from floating point inaccuracy and
C     collinearity.  Compute the unnormalized barycentric
C     coordinates of P with respect to (N1,N2,N3).
C
        B1 = (X(N3)-X(N2))*(YP-Y(N2)) -
     .       (XP-X(N2))*(Y(N3)-Y(N2))
        B2 = (X(N1)-X(N3))*(YP-Y(N3)) -
     .       (XP-X(N3))*(Y(N1)-Y(N3))
        IF (STORE(B1+1.) .GE. 1.  .AND.
     .      STORE(B2+1.) .GE. 1.) GO TO 16
C
C   Restart with N0 randomly selected.
C
        N0 = JRAND(N, IX,IY,IZ )
        GO TO 1
      ENDIF
C
C   Set N4 to the neighbor of N2 which follows N1 (node
C     opposite N2->N1) unless N1->N2 is a boundary edge.
C
      LP = LSTPTR(LEND(N2),N1,LIST,LPTR)
      IF (LIST(LP) .LT. 0) THEN
        NF = N2
        NL = N1
        GO TO 9
      ENDIF
      LP = LPTR(LP)
      N4 = ABS(LIST(LP))
C
C   Select the new edge N1->N2 which intersects the line
C     segment N0-P, and set N3 to the node opposite N1->N2.
C
      IF ( LEFT(X(N0),Y(N0),X(N4),Y(N4),XP,YP) ) THEN
        N3 = N1
        N1 = N4
        N2S = N2
        IF (N1 .NE. N1S  .AND.  N1 .NE. N0) GO TO 8
      ELSE
        N3 = N2
        N2 = N4
        N1S = N1
        IF (N2 .NE. N2S  .AND.  N2 .NE. N0) GO TO 8
      ENDIF
C
C   The starting node N0 or edge N1-N2 was encountered
C     again, implying a cycle (infinite loop).  Restart
C     with N0 randomly selected.
C
      N0 = JRAND(N, IX,IY,IZ )
      GO TO 1
C
C Boundary traversal loops.  NL->NF is a boundary edge and
C   P RIGHT NL->NF.  Save NL and NF.

    9 NP = NL
      NPP = NF
C
C Find the first (rightmost) visible boundary node NF.  NB
C   is set to the first neighbor of NF, and NP is the last
C   neighbor.
C
   10 LP = LEND(NF)
      LP = LPTR(LP)
      NB = LIST(LP)
      IF ( .NOT. LEFT(X(NF),Y(NF),X(NB),Y(NB),XP,YP) )
     .  GO TO 12
C
C   P LEFT NF->NB and thus NB is not visible unless an error
C     resulted from floating point inaccuracy and collinear-
C     ity of the 4 points NP, NF, NB, and P.
C
   11 IF ( FRWRD(X(NF),Y(NF),X(NP),Y(NP),XP,YP)  .OR.
     .     FRWRD(X(NF),Y(NF),X(NP),Y(NP),X(NB),Y(NB)) ) THEN
        I1 = NF
        GO TO 13
      ENDIF
C
C   Bottom of loop.
C
   12 NP = NF
      NF = NB
      GO TO 10
C
C Find the last (leftmost) visible boundary node NL.  NB
C   is set to the last neighbor of NL, and NPP is the first
C   neighbor.
C
   13 LP = LEND(NL)
      NB = -LIST(LP)
      IF ( .NOT. LEFT(X(NB),Y(NB),X(NL),Y(NL),XP,YP) )
     .  GO TO 14
C
C   P LEFT NB->NL and thus NB is not visible unless an error
C     resulted from floating point inaccuracy and collinear-
C     ity of the 4 points P, NB, NL, and NPP.
C
      IF ( FRWRD(X(NL),Y(NL),X(NPP),Y(NPP),XP,YP)  .OR.
     .     FRWRD(X(NL),Y(NL),X(NPP),Y(NPP),X(NB),Y(NB)) )
     .  GO TO 15
C
C   Bottom of loop.
C
   14 NPP = NL
      NL = NB
      GO TO 13
C
C NL is the leftmost visible boundary node.
C
   15 I2 = NL
      I3 = 0
      RETURN
C
C P is in the triangle (N1,N2,N3).
C
   16 I1 = N1
      I2 = N2
      I3 = N3
      RETURN
C
C All points are collinear.
C
   17 I1 = 0
      I2 = 0
      I3 = 0
      RETURN
      END
      SUBROUTINE TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,
     .                   LTRI,LCT,IER)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
     .        NROW, NT, LTRI(NROW,*), LCT(*), IER
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   03/22/97
C
C   This subroutine converts a triangulation data structure
C from the linked list created by Subroutine TRMESH or
C TRMSHR to a triangle list.
C
C On input:
C
C       NCC = Number of constraints.  NCC .GE. 0.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             Subroutine ADDCST.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Linked list data structure defin-
C                        ing the triangulation.  Refer to
C                        Subroutine TRMESH.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are to be
C              stored, or 9 if arc indexes are also to be
C              assigned and stored.  Refer to LTRI.
C
C The above parameters are not altered by this routine.
C
C       LTRI = Integer array of length at least NROW*NT,
C              where NT is at most 2N-5.  (A sufficient
C              length is 12N if NROW=6 or 18N if NROW=9.)
C
C       LCT = Integer array of length NCC or dummy array of
C             length 1 if NCC = 0.
C
C On output:
C
C       NT = Number of triangles in the triangulation unless
C            IER .NE. 0, in which case NT = 0.  NT = 2N - NB
C            - 2, where NB is the number of boundary nodes.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.  The vertices are ordered
C              counterclockwise with the first vertex taken
C              to be the one with smallest index.  Thus,
C              LTRI(2,J) and LTRI(3,J) are larger than
C              LTRI(1,J) and index adjacent neighbors of
C              node LTRI(1,J).  For I = 1,2,3, LTRI(I+3,J)
C              and LTRI(I+6,J) index the triangle and arc,
C              respectively, which are opposite (not shared
C              by) node LTRI(I,J), with LTRI(I+3,J) = 0 if
C              LTRI(I+6,J) indexes a boundary arc.  Vertex
C              indexes range from 1 to N, triangle indexes
C              from 0 to NT, and, if included, arc indexes
C              from 1 to NA = NT+N-1.  The triangles are or-
C              dered on first (smallest) vertex indexes,
C              except that the sets of constraint triangles
C              (triangles contained in the closure of a con-
C              straint region) follow the non-constraint
C              triangles.
C
C       LCT = Array of length NCC containing the triangle
C             index of the first triangle of constraint J in
C             LCT(J).  Thus, the number of non-constraint
C             triangles is LCT(1)-1, and constraint J con-
C             tains LCT(J+1)-LCT(J) triangles, where
C             LCT(NCC+1) = NT+1.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if NCC, N, NROW, or an LCC entry is
C                     outside its valid range on input.
C             IER = 2 if the triangulation data structure
C                     (LIST,LPTR,LEND) is invalid.  Note,
C                     however, that these arrays are not
C                     completely tested for validity.
C
C Modules required by TRLIST:  None
C
C Intrinsic function called by TRLIST:  ABS
C
C***********************************************************
C
      INTEGER I, I1, I2, I3, ISV, J, JLAST, KA, KN, KT, L,
     .        LCC1, LP, LP2, LPL, LPLN1, N1, N1ST, N2, N3,
     .        NM2, NN
      LOGICAL ARCS, CSTRI, PASS2
C
C Test for invalid input parameters and store the index
C   LCC1 of the first constraint node (if any).
C
      NN = N
      IF (NCC .LT. 0  .OR.  (NROW .NE. 6  .AND.
     .    NROW .NE. 9)) GO TO 12
      LCC1 = NN+1
      IF (NCC .EQ. 0) THEN
        IF (NN .LT. 3) GO TO 12
      ELSE
        DO 1 I = NCC,1,-1
          IF (LCC1-LCC(I) .LT. 3) GO TO 12
          LCC1 = LCC(I)
    1     CONTINUE
        IF (LCC1 .LT. 1) GO TO 12
      ENDIF
C
C Initialize parameters for loop on triangles KT = (N1,N2,
C   N3), where N1 < N2 and N1 < N3.  This requires two
C   passes through the nodes with all non-constraint
C   triangles stored on the first pass, and the constraint
C   triangles stored on the second.
C
C   ARCS = TRUE iff arc indexes are to be stored.
C   KA,KT = Numbers of currently stored arcs and triangles.
C   N1ST = Starting index for the loop on nodes (N1ST = 1 on
C            pass 1, and N1ST = LCC1 on pass 2).
C   NM2 = Upper bound on candidates for N1.
C   PASS2 = TRUE iff constraint triangles are to be stored.
C
      ARCS = NROW .EQ. 9
      KA = 0
      KT = 0
      N1ST = 1
      NM2 = NN-2
      PASS2 = .FALSE.
C
C Loop on nodes N1:  J = constraint containing N1,
C                    JLAST = last node in constraint J.
C
    2 J = 0
      JLAST = LCC1 - 1
      DO 11 N1 = N1ST,NM2
        IF (N1 .GT. JLAST) THEN
C
C N1 is the first node in constraint J+1.  Update J and
C   JLAST, and store the first constraint triangle index
C   if in pass 2.
C
          J = J + 1
          IF (J .LT. NCC) THEN
            JLAST = LCC(J+1) - 1
          ELSE
            JLAST = NN
          ENDIF
          IF (PASS2) LCT(J) = KT + 1
        ENDIF
C
C Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points
C   to the last neighbor of N1, and LP2 points to N2.
C
        LPLN1 = LEND(N1)
        LP2 = LPLN1
    3     LP2 = LPTR(LP2)
          N2 = LIST(LP2)
          LP = LPTR(LP2)
          N3 = ABS(LIST(LP))
          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 10
C
C (N1,N2,N3) is a constraint triangle iff the three nodes
C   are in the same constraint and N2 < N3.  Bypass con-
C   straint triangles on pass 1 and non-constraint triangles
C   on pass 2.
C
          CSTRI = N1 .GE. LCC1  .AND.  N2 .LT. N3  .AND.
     .            N3 .LE. JLAST
          IF ((CSTRI  .AND.  .NOT. PASS2)  .OR.
     .        (.NOT. CSTRI  .AND.  PASS2)) GO TO 10
C
C Add a new triangle KT = (N1,N2,N3).
C
          KT = KT + 1
          LTRI(1,KT) = N1
          LTRI(2,KT) = N2
          LTRI(3,KT) = N3
C
C Loop on triangle sides (I1,I2) with neighboring triangles
C   KN = (I1,I2,I3).
C
          DO 9 I = 1,3
            IF (I .EQ. 1) THEN
              I1 = N3
              I2 = N2
            ELSEIF (I .EQ. 2) THEN
              I1 = N1
              I2 = N3
            ELSE
              I1 = N2
              I2 = N1
            ENDIF
C
C Set I3 to the neighbor of I1 which follows I2 unless
C   I2->I1 is a boundary arc.
C
            LPL = LEND(I1)
            LP = LPTR(LPL)
    4       IF (LIST(LP) .EQ. I2) GO TO 5
              LP = LPTR(LP)
              IF (LP .NE. LPL) GO TO 4
C
C   I2 is the last neighbor of I1 unless the data structure
C     is invalid.  Bypass the search for a neighboring
C     triangle if I2->I1 is a boundary arc.
C
            IF (ABS(LIST(LP)) .NE. I2) GO TO 13
            KN = 0
            IF (LIST(LP) .LT. 0) GO TO 8
C
C   I2->I1 is not a boundary arc, and LP points to I2 as
C     a neighbor of I1.
C
    5       LP = LPTR(LP)
            I3 = ABS(LIST(LP))
C
C Find L such that LTRI(L,KN) = I3 (not used if KN > KT),
C   and permute the vertex indexes of KN so that I1 is
C   smallest.
C
            IF (I1 .LT. I2  .AND.  I1 .LT. I3) THEN
              L = 3
            ELSEIF (I2 .LT. I3) THEN
              L = 2
              ISV = I1
              I1 = I2
              I2 = I3
              I3 = ISV
            ELSE
              L = 1
              ISV = I1
              I1 = I3
              I3 = I2
              I2 = ISV
            ENDIF
C
C Test for KN > KT (triangle index not yet assigned).
C
            IF (I1 .GT. N1  .AND.  .NOT. PASS2) GO TO 9
C
C Find KN, if it exists, by searching the triangle list in
C   reverse order.
C
            DO 6 KN = KT-1,1,-1
              IF (LTRI(1,KN) .EQ. I1  .AND.  LTRI(2,KN) .EQ.
     .            I2  .AND.  LTRI(3,KN) .EQ. I3) GO TO 7
    6         CONTINUE
            GO TO 9
C
C Store KT as a neighbor of KN.
C
    7       LTRI(L+3,KN) = KT
C
C Store KN as a neighbor of KT, and add a new arc KA.
C
    8       LTRI(I+3,KT) = KN
            IF (ARCS) THEN
              KA = KA + 1
              LTRI(I+6,KT) = KA
              IF (KN .NE. 0) LTRI(L+6,KN) = KA
            ENDIF
    9       CONTINUE
C
C Bottom of loop on triangles.
C
   10     IF (LP2 .NE. LPLN1) GO TO 3
   11     CONTINUE
C
C Bottom of loop on nodes.
C
      IF (.NOT. PASS2  .AND.  NCC .GT. 0) THEN
        PASS2 = .TRUE.
        N1ST = LCC1
        GO TO 2
      ENDIF
C
C No errors encountered.
C
      NT = KT
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   12 NT = 0
      IER = 1
      RETURN
C
C Invalid triangulation data structure:  I1 is a neighbor of
C   I2, but I2 is not a neighbor of I1.
C
   13 NT = 0
      IER = 2
      RETURN
      END
      SUBROUTINE TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,
     .                   PRNTX)
      INTEGER NCC, LCT(*), N, NROW, NT, LTRI(NROW,NT),
     .        LOUT
      LOGICAL PRNTX
      DOUBLE PRECISION X(N), Y(N)
C
C***********************************************************
C
C                                               From TRLPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/02/98
C
C   Given a triangulation of a set of points in the plane,
C this subroutine prints the triangle list created by
C Subroutine TRLIST and, optionally, the nodal coordinates
C on logical unit LOUT.  The numbers of boundary nodes,
C triangles, and arcs, and the constraint region triangle
C indexes, if any, are also printed.
C
C   All parameters other than LOUT and PRNTX should be
C unaltered from their values on output from TRLIST.
C
C
C On input:
C
C       NCC = Number of constraints.
C
C       LCT = List of constraint triangle starting indexes
C             (or dummy array of length 1 if NCC = 0).
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes in the triangulation -- not used
C             unless PRNTX = TRUE.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are stored, or 9
C              if arc indexes are also stored.
C
C       NT = Number of triangles in the triangulation.
C            1 .LE. NT .LE. 9999.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.
C
C       LOUT = Logical unit number for output.  0 .LE. LOUT
C              .LE. 99.  Output is printed on unit 6 if LOUT
C              is outside its valid range on input.
C
C       PRNTX = Logical variable with value TRUE if and only
C               if X and Y are to be printed (to 6 decimal
C               places).
C
C None of the parameters are altered by this routine.
C
C Modules required by TRLPRT:  None
C
C***********************************************************
C
      INTEGER I, K, LUN, NA, NB, NL, NLMAX, NMAX
      DATA    NMAX/9999/,  NLMAX/60/
C
C Local parameters:
C
C   I = DO-loop, nodal index, and row index for LTRI
C   K = DO-loop and triangle index
C   LUN = Logical unit number for output
C   NA = Number of triangulation arcs
C   NB = Number of boundary nodes
C   NL = Number of lines printed on the current page
C   NLMAX = Maximum number of print lines per page
C   NMAX = Maximum value of N and NT (4-digit format)
C
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test for invalid input.
C
      WRITE (LUN,100)
      NL = 1
      IF (N .LT. 3  .OR.  N .GT. NMAX  .OR.
     .    (NROW .NE. 6  .AND.  NROW .NE. 9)  .OR.
     .    NT .LT. 1  .OR.  NT .GT. NMAX) THEN
C
C Print an error message and bypass the loops.
C
        WRITE (LUN,110) N, NROW, NT
        GO TO 3
      ENDIF
      IF (PRNTX) THEN
C
C Print X and Y.
C
        WRITE (LUN,101)
        NL = 6
        DO 1 I = 1,N
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,106)
            NL = 0
          ENDIF
          WRITE (LUN,102) I, X(I), Y(I)
          NL = NL + 1
    1     CONTINUE
      ENDIF
C
C Print the triangulation LTRI.
C
      IF (NL .GT. NLMAX/2) THEN
        WRITE (LUN,106)
        NL = 0
      ENDIF
      IF (NROW .EQ. 6) THEN
        WRITE (LUN,103)
      ELSE
        WRITE (LUN,104)
      ENDIF
      NL = NL + 5
      DO 2 K = 1,NT
        IF (NL .GE. NLMAX) THEN
          WRITE (LUN,106)
          NL = 0
        ENDIF
        WRITE (LUN,105) K, (LTRI(I,K), I = 1,NROW)
        NL = NL + 1
    2   CONTINUE
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NB = 2*N - NT - 2
      NA = NT + N - 1
      IF (NL .GT. NLMAX-6) WRITE (LUN,106)
      WRITE (LUN,107) NB, NA, NT
C
C Print NCC and LCT.
C
    3 WRITE (LUN,108) NCC
      IF (NCC .GT. 0) WRITE (LUN,109) (LCT(I), I = 1,NCC)
      RETURN
C
C Print formats:
C
  100 FORMAT (///,24X,'TRIPACK (TRLIST) Output')
  101 FORMAT (//16X,'Node',7X,'X(Node)',10X,'Y(Node)'//)
  102 FORMAT (16X,I4,2E17.6)
  103 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3'/)
  104 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors',
     .        14X,'Arcs'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3',4X,'KA1',4X,'KA2',4X,'KA3'/)
  105 FORMAT (2X,I4,2X,6(3X,I4),3(2X,I5))
  106 FORMAT (///)
  107 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  108 FORMAT (/1X,'NCC =',I3,' Constraint Curves')
  109 FORMAT (1X,9X,14I5)
  110 FORMAT (//1X,10X,'*** Invalid Parameter:  N =',I5,
     .        ', NROW =',I5,', NT =',I5,' ***')
      END
      SUBROUTINE TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NEAR,
     .                   NEXT,DIST,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), LNEW, NEAR(N),
     .        NEXT(N), IER
      DOUBLE PRECISION X(N), Y(N), DIST(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/28/98
C
C   This subroutine creates a Delaunay triangulation of a
C set of N arbitrarily distributed points in the plane re-
C ferred to as nodes.  The Delaunay triangulation is defined
C as a set of triangles with the following five properties:
C
C  1)  The triangle vertices are nodes.
C  2)  No triangle contains a node other than its vertices.
C  3)  The interiors of the triangles are pairwise disjoint.
C  4)  The union of triangles is the convex hull of the set
C        of nodes (the smallest convex set which contains
C        the nodes).
C  5)  The interior of the circumcircle of each triangle
C        contains no node.
C
C The first four properties define a triangulation, and the
C last property results in a triangulation which is as close
C as possible to equiangular in a certain sense and which is
C uniquely defined unless four or more nodes lie on a common
C circle.  This property makes the triangulation well-suited
C for solving closest point problems and for triangle-based
C interpolation.
C
C   The triangulation can be generalized to a constrained
C Delaunay triangulation by a call to Subroutine ADDCST.
C This allows for user-specified boundaries defining a non-
C convex and/or multiply connected region.
C
C   The algorithm for constructing the triangulation has
C expected time complexity O(N*log(N)) for most nodal dis-
C tributions.  Also, since the algorithm proceeds by adding
C nodes incrementally, the triangulation may be updated with
C the addition (or deletion) of a node very efficiently.
C The adjacency information representing the triangulation
C is stored as a linked list requiring approximately 13N
C storage locations.
C
C
C   The following is a list of the software package modules
C which a user may wish to call directly:
C
C  ADDCST - Generalizes the Delaunay triangulation to allow
C             for user-specified constraints.
C
C  ADDNOD - Updates the triangulation by appending or
C             inserting a new node.
C
C  AREAP  - Computes the area bounded by a closed polygonal
C             curve such as the boundary of the triangula-
C             tion or of a constraint region.
C
C  BNODES - Returns an array containing the indexes of the
C             boundary nodes in counterclockwise order.
C             Counts of boundary nodes, triangles, and arcs
C             are also returned.
C
C  CIRCUM - Computes the area, circumcenter, circumradius,
C             and, optionally, the aspect ratio of a trian-
C             gle defined by user-specified vertices.
C
C  DELARC - Deletes a boundary arc from the triangulation.
C
C  DELNOD - Updates the triangulation with the deletion of a
C             node.
C
C  EDGE   - Forces a pair of nodes to be connected by an arc
C             in the triangulation.
C
C  GETNP  - Determines the ordered sequence of L closest
C             nodes to a given node, along with the associ-
C             ated distances.  The distance between nodes is
C             taken to be the length of the shortest connec-
C             ting path which intersects no constraint
C             region.
C
C  INTSEC - Determines whether or not an arbitrary pair of
C             line segments share a common point.
C
C  JRAND  - Generates a uniformly distributed pseudo-random
C             integer.
C
C  LEFT   - Locates a point relative to a line.
C
C  NEARND - Returns the index of the nearest node to an
C             arbitrary point, along with its squared
C             distance.
C
C  STORE  - Forces a value to be stored in main memory so
C             that the precision of floating point numbers
C             in memory locations rather than registers is
C             computed.
C
C  TRLIST - Converts the triangulation data structure to a
C             triangle list more suitable for use in a fin-
C             ite element code.
C
C  TRLPRT - Prints the triangle list created by Subroutine
C             TRLIST.
C
C  TRMESH - Creates a Delaunay triangulation of a set of
C             nodes.
C
C  TRMSHR - Creates a Delaunay triangulation (more effici-
C             ently than TRMESH) of a set of nodes lying at
C             the vertices of a (possibly skewed) rectangu-
C             lar grid.
C
C  TRPLOT - Creates a level-2 Encapsulated Postscript (EPS)
C             file containing a triangulation plot.
C
C  TRPRNT - Prints the triangulation data structure and,
C             optionally, the nodal coordinates.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of the nodes.  (X(K),Y(K)) is re-
C             ferred to as node K, and K is referred to as
C             a nodal index.  The first three nodes must not
C             be collinear.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C       NEAR,NEXT,DIST = Work space arrays of length at
C                        least N.  The space is used to
C                        efficiently determine the nearest
C                        triangulation node to each un-
C                        processed node for use by ADDNOD.
C
C On output:
C
C       LIST = Set of nodal indexes which, along with LPTR,
C              LEND, and LNEW, define the triangulation as a
C              set of N adjacency lists -- counterclockwise-
C              ordered sequences of neighboring nodes such
C              that the first and last neighbors of a bound-
C              ary node are boundary nodes (the first neigh-
C              bor of an interior node is arbitrary).  In
C              order to distinguish between interior and
C              boundary nodes, the last neighbor of each
C              boundary node is represented by the negative
C              of its index.
C
C       LPTR = Set of pointers (LIST indexes) in one-to-one
C              correspondence with the elements of LIST.
C              LIST(LPTR(I)) indexes the node which follows
C              LIST(I) in cyclical counterclockwise order
C              (the first neighbor follows the last neigh-
C              bor).
C
C       LEND = Set of pointers to adjacency lists.  LEND(K)
C              points to the last neighbor of node K for
C              K = 1,...,N.  Thus, LIST(LEND(K)) < 0 if and
C              only if K is a boundary node.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).  LIST, LPTR,
C              LEND, and LNEW are not altered if IER < 0,
C              and are incomplete if IER > 0.
C
C       NEAR,NEXT,DIST = Garbage.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if N < 3 on input.
C             IER = -2 if the first three nodes are
C                      collinear.
C             IER = -4 if an error flag was returned by a
C                      call to SWAP in ADDNOD.  This is an
C                      internal error and should be reported
C                      to the programmer.
C             IER =  L if nodes L and M coincide for some
C                      M > L.  The linked list represents
C                      a triangulation of nodes 1 to M-1
C                      in this case.
C
C Modules required by TRMESH:  ADDNOD, BDYADD, INSERT,
C                                INTADD, JRAND, LEFT,
C                                LSTPTR, STORE, SWAP,
C                                SWPTST, TRFIND
C
C Intrinsic function called by TRMESH:  ABS
C
C***********************************************************
C
      LOGICAL LEFT
      DOUBLE PRECISION STORE
      INTEGER I, I0, J, K, KM1, LCC(1), LP, LPL, NCC, NEXTI,
     .        NN
      DOUBLE PRECISION D, D1, D2, D3, EPS, SWTOL
      COMMON/SWPCOM/SWTOL
C
C Local parameters:
C
C D =        Squared distance from node K to node I
C D1,D2,D3 = Squared distances from node K to nodes 1, 2,
C              and 3, respectively
C EPS =      Half the machine precision
C I,J =      Nodal indexes
C I0 =       Index of the node preceding I in a sequence of
C              unprocessed nodes:  I = NEXT(I0)
C K =        Index of node to be added and DO-loop index:
C              K > 3
C KM1 =      K-1
C LCC(1) =   Dummy array
C LP =       LIST index (pointer) of a neighbor of K
C LPL =      Pointer to the last neighbor of K
C NCC =      Number of constraint curves
C NEXTI =    NEXT(I)
C NN =       Local copy of N
C SWTOL =    Tolerance for function SWPTST
C
      NN = N
      IF (NN .LT. 3) THEN
        IER = -1
        RETURN
      ENDIF
C
C Compute a tolerance for function SWPTST:  SWTOL = 10*
C   (machine precision)
C
      EPS = 1.
    1 EPS = EPS/2.
        SWTOL = STORE(EPS + 1.)
        IF (SWTOL .GT. 1.) GO TO 1
      SWTOL = EPS*20.
C
C Store the first triangle in the linked list.
C
      IF ( .NOT. LEFT(X(1),Y(1),X(2),Y(2),X(3),Y(3)) ) THEN
C
C   The initial triangle is (3,2,1) = (2,1,3) = (1,3,2).
C
        LIST(1) = 3
        LPTR(1) = 2
        LIST(2) = -2
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 1
        LPTR(3) = 4
        LIST(4) = -3
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 2
        LPTR(5) = 6
        LIST(6) = -1
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSEIF ( .NOT. LEFT(X(2),Y(2),X(1),Y(1),X(3),Y(3)) )
     .       THEN
C
C   The initial triangle is (1,2,3).
C
        LIST(1) = 2
        LPTR(1) = 2
        LIST(2) = -3
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 3
        LPTR(3) = 4
        LIST(4) = -1
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 1
        LPTR(5) = 6
        LIST(6) = -2
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSE
C
C   The first three nodes are collinear.
C
        IER = -2
        RETURN
      ENDIF
C
C Initialize LNEW and test for N = 3.
C
      LNEW = 7
      IF (NN .EQ. 3) THEN
        IER = 0
        RETURN
      ENDIF
C
C A nearest-node data structure (NEAR, NEXT, and DIST) is
C   used to obtain an expected-time (N*log(N)) incremental
C   algorithm by enabling constant search time for locating
C   each new node in the triangulation.
C
C For each unprocessed node K, NEAR(K) is the index of the
C   triangulation node closest to K (used as the starting
C   point for the search in Subroutine TRFIND) and DIST(K)
C   is an increasing function of the distance between nodes
C   K and NEAR(K).
C
C Since it is necessary to efficiently find the subset of
C   unprocessed nodes associated with each triangulation
C   node J (those that have J as their NEAR entries), the
C   subsets are stored in NEAR and NEXT as follows:  for
C   each node J in the triangulation, I = NEAR(J) is the
C   first unprocessed node in J's set (with I = 0 if the
C   set is empty), L = NEXT(I) (if I > 0) is the second,
C   NEXT(L) (if L > 0) is the third, etc.  The nodes in each
C   set are initially ordered by increasing indexes (which
C   maximizes efficiency) but that ordering is not main-
C   tained as the data structure is updated.
C
C Initialize the data structure for the single triangle.
C
      NEAR(1) = 0
      NEAR(2) = 0
      NEAR(3) = 0
      DO 2 K = NN,4,-1
        D1 = (X(K)-X(1))**2 + (Y(K)-Y(1))**2
        D2 = (X(K)-X(2))**2 + (Y(K)-Y(2))**2
        D3 = (X(K)-X(3))**2 + (Y(K)-Y(3))**2
        IF (D1 .LE. D2  .AND.  D1 .LE. D3) THEN
          NEAR(K) = 1
          DIST(K) = D1
          NEXT(K) = NEAR(1)
          NEAR(1) = K
        ELSEIF (D2 .LE. D1  .AND.  D2 .LE. D3) THEN
          NEAR(K) = 2
          DIST(K) = D2
          NEXT(K) = NEAR(2)
          NEAR(2) = K
        ELSE
          NEAR(K) = 3
          DIST(K) = D3
          NEXT(K) = NEAR(3)
          NEAR(3) = K
        ENDIF
    2   CONTINUE
C
C Add the remaining nodes.  Parameters for ADDNOD are as
C   follows:
C
C   K = Index of the node to be added.
C   NEAR(K) = Index of the starting node for the search in
C             TRFIND.
C   NCC = Number of constraint curves.
C   LCC = Dummy array (since NCC = 0).
C   KM1 = Number of nodes in the triangulation.
C
      NCC = 0
      DO 7 K = 4,NN
        KM1 = K-1
        CALL ADDNOD (K,X(K),Y(K),NEAR(K),NCC, LCC,KM1,X,Y,
     .               LIST,LPTR,LEND,LNEW, IER)
        IF (IER .NE. 0) RETURN
C
C Remove K from the set of unprocessed nodes associated
C   with NEAR(K).
C
        I = NEAR(K)
        IF (NEAR(I) .EQ. K) THEN
          NEAR(I) = NEXT(K)
        ELSE
          I = NEAR(I)
    3     I0 = I
            I = NEXT(I0)
            IF (I .NE. K) GO TO 3
          NEXT(I0) = NEXT(K)
        ENDIF
        NEAR(K) = 0
C
C Loop on neighbors J of node K.
C
        LPL = LEND(K)
        LP = LPL
    4   LP = LPTR(LP)
          J = ABS(LIST(LP))
C
C Loop on elements I in the sequence of unprocessed nodes
C   associated with J:  K is a candidate for replacing J
C   as the nearest triangulation node to I.  The next value
C   of I in the sequence, NEXT(I), must be saved before I
C   is moved because it is altered by adding I to K's set.
C
          I = NEAR(J)
    5     IF (I .EQ. 0) GO TO 6
          NEXTI = NEXT(I)
C
C Test for the distance from I to K less than the distance
C   from I to J.
C
          D = (X(K)-X(I))**2 + (Y(K)-Y(I))**2
          IF (D .LT. DIST(I)) THEN
C
C Replace J by K as the nearest triangulation node to I:
C   update NEAR(I) and DIST(I), and remove I from J's set
C   of unprocessed nodes and add it to K's set.
C
            NEAR(I) = K
            DIST(I) = D
            IF (I .EQ. NEAR(J)) THEN
              NEAR(J) = NEXTI
            ELSE
              NEXT(I0) = NEXTI
            ENDIF
            NEXT(I) = NEAR(K)
            NEAR(K) = I
          ELSE
            I0 = I
          ENDIF
C
C Bottom of loop on I.
C
          I = NEXTI
          GO TO 5
C
C Bottom of loop on neighbors J.
C
    6     IF (LP .NE. LPL) GO TO 4
    7   CONTINUE
      RETURN
      END
      SUBROUTINE TRMSHR (N,NX,X,Y, NIT, LIST,LPTR,LEND,LNEW,
     .                   IER)
      INTEGER  N, NX, NIT, LIST(*), LPTR(*), LEND(N), LNEW,
     .         IER
      DOUBLE PRECISION  X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/27/98
C
C   This subroutine creates a Delaunay triangulation of a
C set of N nodes in the plane, where the nodes are the vert-
C ices of an NX by NY skewed rectangular grid with the
C natural ordering.  Thus, N = NX*NY, and the nodes are
C ordered from left to right beginning at the top row so
C that adjacent nodes have indexes which differ by 1 in the
C x-direction and by NX in the y-direction.  A skewed rec-
C tangular grid is defined as one in which each grid cell is
C a strictly convex quadrilateral (and is thus the convex
C hull of its four vertices).  Equivalently, any transfor-
C mation from a rectangle to a grid cell which is bilinear
C in both components has an invertible Jacobian.
C
C   If the nodes are not distributed and ordered as defined
C above, Subroutine TRMESH must be called in place of this
C routine.  Refer to Subroutine ADDCST for the treatment of
C constraints.
C
C   The first phase of the algorithm consists of construc-
C ting a triangulation by choosing a diagonal arc in each
C grid cell.  If NIT = 0, all diagonals connect lower left
C to upper right corners and no error checking or additional
C computation is performed.  Otherwise, each diagonal arc is
C chosen to be locally optimal, and boundary arcs are added
C where necessary in order to cover the convex hull of the
C nodes.  (This is the first iteration.)  If NIT > 1 and no
C error was detected, the triangulation is then optimized by
C a sequence of up to NIT-1 iterations in which interior
C arcs of the triangulation are tested and swapped if appro-
C priate.  The algorithm terminates when an iteration
C results in no swaps and/or when the allowable number of
C iterations has been performed.  NIT = 0 is sufficient to
C produce a Delaunay triangulation if the original grid is
C actually rectangular, and NIT = 1 is sufficient if it is
C close to rectangular.  Note, however, that the ordering
C and distribution of nodes is not checked for validity in
C the case NIT = 0, and the triangulation will not be valid
C unless the rectangular grid covers the convex hull of the
C nodes.
C
C
C On input:
C
C       N = Number of nodes in the grid.  N = NX*NY for some
C           NY .GE. 2.
C
C       NX = Number of grid points in the x-direction.  NX
C            .GE. 2.
C
C       X,Y = Arrays of length N containing coordinates of
C             the nodes with the ordering and distribution
C             defined in the header comments above.
C             (X(K),Y(K)) is referred to as node K.
C
C The above parameters are not altered by this routine.
C
C       NIT = Nonnegative integer specifying the maximum
C             number of iterations to be employed.  Refer
C             to the header comments above.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C On output:
C
C       NIT = Number of iterations employed.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine TRMESH.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = K if the grid element with upper left
C                     corner at node K is not a strictly
C                     convex quadrilateral.  The algorithm
C                     is terminated when the first such
C                     occurrence is detected.  Note that
C                     this test is not performed if NIT = 0
C                     on input.
C             IER = -1 if N, NX, or NIT is outside its valid
C                      range on input.
C             IER = -2 if NIT > 1 on input, and the optimi-
C                      zation loop failed to converge within
C                      the allowable number of iterations.
C                      The triangulation is valid but not
C                      optimal in this case.
C
C Modules required by TRMSHR:  INSERT, LEFT, LSTPTR, NBCNT,
C                                STORE, SWAP, SWPTST
C
C Intrinsic function called by TRMSHR:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR, NBCNT
      LOGICAL LEFT, SWPTST
      DOUBLE PRECISION STORE
      INTEGER I, ITER, J, K, KP1, LP, LPF, LPK, LPL, LPP,
     .        M1, M2, M3, M4, MAXIT, N0, N1, N2, N3, N4, NI,
     .        NJ, NM1, NN, NNB
      LOGICAL TST
      DOUBLE PRECISION EPS, SWTOL
      COMMON/SWPCOM/SWTOL
C
C Store local variables and test for errors in input
C   parameters.
C
      NI = NX
      NJ = N/NI
      NN = NI*NJ
      MAXIT = NIT
      NIT = 0
      IF (N .NE. NN  .OR.  NJ .LT. 2  .OR.  NI .LT. 2  .OR.
     .    MAXIT .LT. 0) THEN
        IER = -1
        RETURN
      ENDIF
      IER = 0
C
C Compute a tolerance for function SWPTST:  SWTOL = 10*
C   (machine precision)
C
      EPS = 1.
    1 EPS = EPS/2.
        SWTOL = STORE(EPS + 1.)
        IF (SWTOL .GT. 1.) GO TO 1
      SWTOL = EPS*20.
C
C Loop on grid points (I,J) corresponding to nodes K =
C   (J-1)*NI + I.  TST = TRUE iff diagonals are to be
C   chosen by the swap test.  M1, M2, M3, and M4 are the
C   slopes (-1, 0, or 1) of the diagonals in quadrants 1
C   to 4 (counterclockwise beginning with the upper right)
C   for a coordinate system with origin at node K.
C
      TST = MAXIT .GT. 0
      M1 = 0
      M4 = 0
      LP = 0
      KP1 = 1
      DO 6 J = 1,NJ
        DO 5 I = 1,NI
          M2 = M1
          M3 = M4
          K = KP1
          KP1 = K + 1
          LPF = LP + 1
          IF (J .EQ. NJ  .AND.  I .NE. NI) GO TO 2
          IF (I .NE. 1) THEN
            IF (J .NE. 1) THEN
C
C   K is not in the top row, leftmost column, or bottom row
C     (unless K is the lower right corner).  Take the first
C     neighbor to be the node above K.
C
              LP = LP + 1
              LIST(LP) = K - NI
              LPTR(LP) = LP + 1
              IF (M2 .LE. 0) THEN
                LP = LP + 1
                LIST(LP) = K - 1 - NI
                LPTR(LP) = LP + 1
              ENDIF
            ENDIF
C
C   K is not in the leftmost column.  The next (or first)
C     neighbor is to the left of K.
C
            LP = LP + 1
            LIST(LP) = K - 1
            LPTR(LP) = LP + 1
            IF (J .EQ. NJ) GO TO 3
            IF (M3 .GE. 0) THEN
              LP = LP + 1
              LIST(LP) = K - 1 + NI
              LPTR(LP) = LP + 1
            ENDIF
          ENDIF
C
C   K is not in the bottom row.  The next (or first)
C     neighbor is below K.
C
          LP = LP + 1
          LIST(LP) = K + NI
          LPTR(LP) = LP + 1
C
C   Test for a negative diagonal in quadrant 4 unless K is
C     in the rightmost column.  The quadrilateral associated
C     with the quadrant is tested for strict convexity un-
C     less NIT = 0 on input.
C
          IF (I .EQ. NI) GO TO 3
          M4 = 1
          IF (.NOT. TST) GO TO 2
          IF ( LEFT(X(KP1),Y(KP1),X(K+NI),Y(K+NI),X(K),Y(K))
     .         .OR.  LEFT(X(K),Y(K),X(KP1+NI),Y(KP1+NI),
     .                    X(K+NI),Y(K+NI))
     .         .OR.  LEFT(X(K+NI),Y(K+NI),X(KP1),Y(KP1),
     .                    X(KP1+NI),Y(KP1+NI))
     .         .OR.  LEFT(X(KP1+NI),Y(KP1+NI),X(K),Y(K),
     .                    X(KP1),Y(KP1)) )          GO TO 12
          IF ( SWPTST(KP1,K+NI,K,KP1+NI,X,Y) ) GO TO 2
          M4 = -1
          LP = LP + 1
          LIST(LP) = KP1 + NI
          LPTR(LP) = LP + 1
C
C   The next (or first) neighbor is to the right of K.
C
    2     LP = LP + 1
          LIST(LP) = KP1
          LPTR(LP) = LP + 1
C
C   Test for a positive diagonal in quadrant 1 (the neighbor
C     of K-NI which follows K is not K+1) unless K is in the
C     top row.
C
          IF (J .EQ. 1) GO TO 3
          IF (TST) THEN
            M1 = -1
            LPK = LSTPTR(LEND(K-NI),K,LIST,LPTR)
            LPK = LPTR(LPK)
            IF (LIST(LPK) .NE. KP1) THEN
              M1 = 1
              LP = LP + 1
              LIST(LP) = KP1 - NI
              LPTR(LP) = LP + 1
            ENDIF
          ENDIF
C
C   If K is in the leftmost column (and not the top row) or
C     in the bottom row (and not the rightmost column), then
C     the next neighbor is the node above K.
C
          IF (I .NE. 1  .AND.  J .NE. NJ) GO TO 4
          LP = LP + 1
          LIST(LP) = K - NI
          LPTR(LP) = LP + 1
          IF (I .EQ. 1) GO TO 3
C
C   K is on the bottom row (and not the leftmost or right-
C     most column).
C
          IF (M2 .LE. 0) THEN
            LP = LP + 1
            LIST(LP) = K - 1 - NI
            LPTR(LP) = LP + 1
          ENDIF
          LP = LP + 1
          LIST(LP) = K - 1
          LPTR(LP) = LP + 1
C
C   K is a boundary node.
C
    3     LIST(LP) = -LIST(LP)
C
C   Bottom of loop.  Store LEND and correct LPTR(LP).
C     LPF and LP point to the first and last neighbors
C     of K.
C
    4     LEND(K) = LP
          LPTR(LP) = LPF
    5     CONTINUE
    6   CONTINUE
C
C Store LNEW, and terminate the algorithm if NIT = 0 on
C   input.
C
      LNEW = LP + 1
      IF (MAXIT .EQ. 0) RETURN
C
C Add boundary arcs where necessary in order to cover the
C   convex hull of the nodes.  N1, N2, and N3 are consecu-
C   tive boundary nodes in counterclockwise order, and N0
C   is the starting point for each loop around the boundary.
C
      N0 = 1
      N1 = N0
      N2 = NI + 1
C
C   TST is set to TRUE if an arc is added.  The boundary
C     loop is repeated until a traversal results in no
C     added arcs.
C
    7 TST = .FALSE.
C
C   Top of boundary loop.  Set N3 to the first neighbor of
C     N2, and test for N3 LEFT N1 -> N2.
C
    8   LPL = LEND(N2)
          LP = LPTR(LPL)
          N3 = LIST(LP)
          IF ( LEFT(X(N1),Y(N1),X(N2),Y(N2),X(N3),Y(N3)) )
     .       N1 = N2
          IF (N1 .NE. N2) THEN
C
C   Add the boundary arc N1-N3.  If N0 = N2, the starting
C     point is changed to N3, since N2 will be removed from
C     the boundary.  N3 is inserted as the first neighbor of
C     N1, N2 is changed to an interior node, and N1 is
C     inserted as the last neighbor of N3.
C
            TST = .TRUE.
            IF (N2 .EQ. N0) N0 = N3
            LP = LEND(N1)
            CALL INSERT (N3,LP, LIST,LPTR,LNEW )
            LIST(LPL) = -LIST(LPL)
            LP = LEND(N3)
            LIST(LP) = N2
            CALL INSERT (-N1,LP, LIST,LPTR,LNEW )
            LEND(N3) = LNEW - 1
          ENDIF
C
C   Bottom of loops.  Test for termination.
C
          N2 = N3
          IF (N1 .NE. N0) GO TO 8
        IF (TST) GO TO 7
C
C Terminate the algorithm if NIT = 1 on input.
C
      NIT = 1
      IF (MAXIT .EQ. 1) RETURN
C
C Optimize the triangulation by applying the swap test and
C   appropriate swaps to the interior arcs.  The loop is
C   repeated until no swaps are performed or MAXIT itera-
C   tions have been applied.  ITER is the current iteration,
C   and TST is set to TRUE if a swap occurs.
C
      ITER = 1
      NM1 = NN - 1
    9 ITER = ITER + 1
        TST = .FALSE.
C
C   Loop on interior arcs N1-N2, where N2 > N1 and
C     (N1,N2,N3) and (N2,N1,N4) are adjacent triangles.
C
C   Top of loop on nodes N1.
C
        DO 11 N1 = 1,NM1
          LPL = LEND(N1)
          N4 = LIST(LPL)
          LPF = LPTR(LPL)
          N2 = LIST(LPF)
          LP = LPTR(LPF)
          N3 = LIST(LP)
          NNB = NBCNT(LPL,LPTR)
C
C   Top of loop on neighbors N2 of N1.  NNB is the number of
C                                       neighbors of N1.
C
          DO 10 I = 1,NNB
C
C   Bypass the swap test if N1 is a boundary node and N2 is
C     the first neighbor (N4 < 0), N2 < N1, or N1-N2 is a
C     diagonal arc (already locally optimal) when ITER = 2.
C
            IF ( N4 .GT. 0  .AND.  N2 .GT. N1  .AND.
     .          (ITER .NE. 2  .OR.  ABS(N1+NI-N2) .NE. 1) )
     .          THEN
              IF (SWPTST(N3,N4,N1,N2,X,Y) ) THEN
C
C   Swap diagonal N1-N2 for N3-N4, set TST to TRUE, and set
C     N2 to N4 (the neighbor preceding N3).
C
                CALL SWAP (N3,N4,N1,N2, LIST,LPTR,LEND, LPP)
                IF (LPP .NE. 0) THEN
                  TST = .TRUE.
                  N2 = N4
                ENDIF
              ENDIF
            ENDIF
C
C   Bottom of neighbor loop.
C
            IF (LIST(LPL) .EQ. -N3) GO TO 11
            N4 = N2
            N2 = N3
            LP = LSTPTR(LPL,N2,LIST,LPTR)
            LP = LPTR(LP)
            N3 = ABS(LIST(LP))
   10       CONTINUE
   11     CONTINUE
C
C   Test for termination.
C
        IF (TST  .AND.  ITER .LT. MAXIT) GO TO 9
      NIT = ITER
      IF (TST) IER = -2
      RETURN
C
C Invalid grid cell encountered.
C
   12 IER = K
      RETURN
      END
      SUBROUTINE TRPLOT (LUN,PLTSIZ,WX1,WX2,WY1,WY2,NCC,LCC,
     .                   N,X,Y,LIST,LPTR,LEND,TITLE,
     .                   NUMBR, IER)
      CHARACTER*(*) TITLE
      INTEGER LUN, NCC, LCC(*), N, LIST(*), LPTR(*),
     .        LEND(N), IER
      LOGICAL NUMBR
      DOUBLE PRECISION PLTSIZ, WX1, WX2, WY1, WY2, X(N), 
     .        Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/98
C
C   This subroutine creates a level-2 Encapsulated Post-
C script (EPS) file containing a triangulation plot.
C
C
C On input:
C
C       LUN = Logical unit number in the range 0 to 99.
C             The unit should be opened with an appropriate
C             file name before the call to this routine.
C
C       PLTSIZ = Plot size in inches.  The window is mapped,
C                with aspect ratio preserved, to a rectangu-
C                lar viewport with maximum side-length equal
C                to .88*PLTSIZ (leaving room for labels out-
C                side the viewport).  The viewport is
C                centered on the 8.5 by 11 inch page, and
C                its boundary is drawn.  1.0 .LE. PLTSIZ
C                .LE. 8.5.
C
C       WX1,WX2,WY1,WY2 = Parameters defining a rectangular
C                         window against which the triangu-
C                         lation is clipped.  (Only the
C                         portion of the triangulation that
C                         lies in the window is drawn.)
C                         (WX1,WY1) and (WX2,WY2) are the
C                         lower left and upper right cor-
C                         ners, respectively.  WX1 < WX2 and
C                         WY1 < WY2.
C
C       NCC = Number of constraint curves.  Refer to Subrou-
C             tine ADDCST.  NCC .GE. 0.
C
C       LCC = Array of length NCC (or dummy parameter if
C             NCC = 0) containing the index of the first
C             node of constraint I in LCC(I).  For I = 1 to
C             NCC, LCC(I+1)-LCC(I) .GE. 3, where LCC(NCC+1)
C             = N+1.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes with non-constraint nodes in the
C             first LCC(1)-1 locations.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       TITLE = Type CHARACTER variable or constant contain-
C               ing a string to be centered above the plot.
C               The string must be enclosed in parentheses;
C               i.e., the first and last characters must be
C               '(' and ')', respectively, but these are not
C               displayed.  TITLE may have at most 80 char-
C               acters including the parentheses.
C
C       NUMBR = Option indicator:  If NUMBR = TRUE, the
C               nodal indexes are plotted next to the nodes.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if LUN, PLTSIZ, NCC, or N is outside
C                     its valid range.  LCC is not tested
C                     for validity.
C             IER = 2 if WX1 >= WX2 or WY1 >= WY2.
C             IER = 3 if an error was encountered in writing
C                     to unit LUN.
C
C   Various plotting options can be controlled by altering
C the data statement below.
C
C Modules required by TRPLOT:  None
C
C Intrinsic functions called by TRPLOT:  ABS, CHAR, NINT,
C                                          DBLE
C
C***********************************************************
C
      INTEGER I, IFRST, IH, ILAST, IPX1, IPX2, IPY1, IPY2,
     .        IW, LP, LPL, N0, N0BAK, N0FOR, N1, NLS
      LOGICAL ANNOT, CNSTR, PASS1
      DOUBLE PRECISION DASHL, DX, DY, FSIZN, FSIZT, R, SFX, 
     .        SFY, T, TX, TY, X0, Y0
C
      DATA    ANNOT/.TRUE./,  DASHL/4.0/,  FSIZN/10.0/,
     .        FSIZT/16.0/
C
C Local parameters:
C
C ANNOT =     Logical variable with value TRUE iff the plot
C               is to be annotated with the values of WX1,
C               WX2, WY1, and WY2
C CNSTR       Logical variable used to flag constraint arcs:
C               TRUE iff N0-N1 lies in a constraint region
C DASHL =     Length (in points, at 72 points per inch) of
C               dashes and spaces in a dashed line pattern
C               used for drawing constraint arcs
C DX =        Window width WX2-WX1
C DY =        Window height WY2-WY1
C FSIZN =     Font size in points for labeling nodes with
C               their indexes if NUMBR = TRUE
C FSIZT =     Font size in points for the title (and
C               annotation if ANNOT = TRUE)
C I =         Constraint index (1 to NCC)
C IFRST =     Index of the first node in constraint I
C IH =        Height of the viewport in points
C ILAST =     Index of the last node in constraint I
C IPX1,IPY1 = X and y coordinates (in points) of the lower
C               left corner of the bounding box or viewport
C IPX2,IPY2 = X and y coordinates (in points) of the upper
C               right corner of the bounding box or viewport
C IW =        Width of the viewport in points
C LP =        LIST index (pointer)
C LPL =       Pointer to the last neighbor of N0
C N0 =        Nodal index and DO-loop index
C N0BAK =     Predecessor of N0 in a constraint curve
C               (sequence of adjacent constraint nodes)
C N0FOR =     Successor to N0 in a constraint curve
C N1 =        Index of a neighbor of N0
C NLS =       Index of the last non-constraint node
C PASS1 =     Logical variable used to flag the first pass
C               through the constraint nodes
C R =         Aspect ratio DX/DY
C SFX,SFY =   Scale factors for mapping world coordinates
C               (window coordinates in [WX1,WX2] X [WY1,WY2])
C               to viewport coordinates in [IPX1,IPX2] X
C               [IPY1,IPY2]
C T =         Temporary variable
C TX,TY =     Translation vector for mapping world coordi-
C               nates to viewport coordinates
C X0,Y0 =     X(N0),Y(N0) or label location
C
C
C Test for error 1, and set NLS to the last non-constraint
C   node.
C
      IF (LUN .LT. 0  .OR.  LUN .GT. 99  .OR.
     .    PLTSIZ .LT. 1.0  .OR.  PLTSIZ .GT. 8.5  .OR.
     .    NCC .LT. 0  .OR.  N .LT. 3) GO TO 11
      NLS = N
      IF (NCC .GT. 0) NLS = LCC(1)-1
C
C Compute the aspect ratio of the window.
C
      DX = WX2 - WX1
      DY = WY2 - WY1
      IF (DX .LE. 0.0  .OR.  DY .LE. 0.0) GO TO 12
      R = DX/DY
C
C Compute the lower left (IPX1,IPY1) and upper right
C   (IPX2,IPY2) corner coordinates of the bounding box.
C   The coordinates, specified in default user space units
C   (points, at 72 points/inch with origin at the lower
C   left corner of the page), are chosen to preserve the
C   aspect ratio R, and to center the plot on the 8.5 by 11
C   inch page.  The center of the page is (306,396), and
C   T = PLTSIZ/2 in points.
C
      T = 36.0*PLTSIZ
      IF (R .GE. 1.0) THEN
        IPX1 = 306 - NINT(T)
        IPX2 = 306 + NINT(T)
        IPY1 = 396 - NINT(T/R)
        IPY2 = 396 + NINT(T/R)
      ELSE
        IPX1 = 306 - NINT(T*R)
        IPX2 = 306 + NINT(T*R)
        IPY1 = 396 - NINT(T)
        IPY2 = 396 + NINT(T)
      ENDIF
C
C Output header comments.
C
      WRITE (LUN,100,ERR=13) IPX1, IPY1, IPX2, IPY2
  100 FORMAT ('%!PS-Adobe-3.0 EPSF-3.0'/
     .        '%%BoundingBox:',4I4/
     .        '%%Title:  Triangulation'/
     .        '%%Creator:  TRIPACK'/
     .        '%%EndComments')
C
C Set (IPX1,IPY1) and (IPX2,IPY2) to the corner coordinates
C   of a viewport obtained by shrinking the bounding box by
C   12% in each dimension.
C
      IW = NINT(0.88*DBLE(IPX2-IPX1))
      IH = NINT(0.88*DBLE(IPY2-IPY1))
      IPX1 = 306 - IW/2
      IPX2 = 306 + IW/2
      IPY1 = 396 - IH/2
      IPY2 = 396 + IH/2
C
C Set the line thickness to 2 points, and draw the
C   viewport boundary.
C
      T = 2.0
      WRITE (LUN,110,ERR=13) T
      WRITE (LUN,120,ERR=13) IPX1, IPY1
      WRITE (LUN,130,ERR=13) IPX1, IPY2
      WRITE (LUN,130,ERR=13) IPX2, IPY2
      WRITE (LUN,130,ERR=13) IPX2, IPY1
      WRITE (LUN,140,ERR=13)
      WRITE (LUN,150,ERR=13)
  110 FORMAT (F12.6,' setlinewidth')
  120 FORMAT (2I4,' moveto')
  130 FORMAT (2I4,' lineto')
  140 FORMAT ('closepath')
  150 FORMAT ('stroke')
C
C Set up a mapping from the window to the viewport.
C
      SFX = DBLE(IW)/DX
      SFY = DBLE(IH)/DY
      TX = IPX1 - SFX*WX1
      TY = IPY1 - SFY*WY1
      WRITE (LUN,160,ERR=13) TX, TY, SFX, SFY
  160 FORMAT (2F12.6,' translate'/
     .        2F12.6,' scale')
C
C The line thickness (believe it or fucking not) must be
C   changed to reflect the new scaling which is applied to
C   all subsequent output.  Set it to 1.0 point.
C
      T = 2.0/(SFX+SFY)
      WRITE (LUN,110,ERR=13) T
C
C Save the current graphics state, and set the clip path to
C   the boundary of the window.
C
      WRITE (LUN,170,ERR=13)
      WRITE (LUN,180,ERR=13) WX1, WY1
      WRITE (LUN,190,ERR=13) WX2, WY1
      WRITE (LUN,190,ERR=13) WX2, WY2
      WRITE (LUN,190,ERR=13) WX1, WY2
      WRITE (LUN,200,ERR=13)
  170 FORMAT ('gsave')
  180 FORMAT (2F12.6,' moveto')
  190 FORMAT (2F12.6,' lineto')
  200 FORMAT ('closepath clip newpath')
C
C Draw the edges N0->N1, where N1 > N0, beginning with a
C   loop on non-constraint nodes N0.  LPL points to the
C   last neighbor of N0.
C
      DO 3 N0 = 1,NLS
        X0 = X(N0)
        Y0 = Y(N0)
        LPL = LEND(N0)
        LP = LPL
C
C   Loop on neighbors N1 of N0.
C
    2   LP = LPTR(LP)
          N1 = ABS(LIST(LP))
          IF (N1 .GT. N0) THEN
C
C   Add the edge to the path.
C
            WRITE (LUN,210,ERR=13) X0, Y0, X(N1), Y(N1)
  210       FORMAT (2F12.6,' moveto',2F12.6,' lineto')
          ENDIF
          IF (LP .NE. LPL) GO TO 2
    3   CONTINUE
C
C Loop through the constraint nodes twice.  The non-
C   constraint arcs incident on constraint nodes are
C   drawn (with solid lines) on the first pass, and the
C   constraint arcs (both boundary and interior, if any)
C   are drawn (with dashed lines) on the second pass.
C
      PASS1 = .TRUE.
C
C Loop on constraint nodes N0 with (N0BAK,N0,N0FOR) a sub-
C   sequence of constraint I.  The outer loop is on
C   constraints I with first and last nodes IFRST and ILAST.
C
    4 IFRST = N+1
      DO 8 I = NCC,1,-1
        ILAST = IFRST - 1
        IFRST = LCC(I)
        N0BAK = ILAST
        DO 7 N0 = IFRST,ILAST
          N0FOR = N0 + 1
          IF (N0 .EQ. ILAST) N0FOR = IFRST
          LPL = LEND(N0)
          X0 = X(N0)
          Y0 = Y(N0)
          LP = LPL
C
C   Loop on neighbors N1 of N0.  CNSTR = TRUE iff N0-N1 is a
C     constraint arc.
C
C   Initialize CNSTR to TRUE iff the first neighbor of N0
C     strictly follows N0FOR and precedes or coincides with
C     N0BAK (in counterclockwise order).
C
    5     LP = LPTR(LP)
            N1 = ABS(LIST(LP))
            IF (N1 .NE. N0FOR  .AND.  N1 .NE. N0BAK) GO TO 5
          CNSTR = N1 .EQ. N0BAK
          LP = LPL
C
C   Loop on neighbors N1 of N0.  Update CNSTR and test for
C     N1 > N0.
C
    6     LP = LPTR(LP)
            N1 = ABS(LIST(LP))
            IF (N1 .EQ. N0FOR) CNSTR = .TRUE.
            IF (N1 .GT. N0) THEN
C
C   Draw the edge iff (PASS1=TRUE and CNSTR=FALSE) or
C     (PASS1=FALSE and CNSTR=TRUE); i.e., CNSTR and PASS1
C     have opposite values.
C
              IF (CNSTR .NEQV. PASS1)
     .          WRITE (LUN,210,ERR=13) X0, Y0, X(N1), Y(N1)
            ENDIF
            IF (N1 .EQ. N0BAK) CNSTR = .FALSE.
C
C   Bottom of loops.
C
            IF (LP .NE. LPL) GO TO 6
          N0BAK = N0
    7     CONTINUE
    8   CONTINUE
      IF (PASS1) THEN
C
C End of first pass:  paint the path and change to dashed
C   lines for subsequent drawing.  Since the scale factors
C   are applied to everything, the dash length must be
C   specified in world coordinates.
C
        PASS1 = .FALSE.
        WRITE (LUN,150,ERR=13)
        T = DASHL*2.0/(SFX+SFY)
        WRITE (LUN,220,ERR=13) T
  220   FORMAT ('[',F12.6,'] 0 setdash')
        GO TO 4
      ENDIF
C
C Paint the path and restore the saved graphics state (with
C   no clip path).
C
      WRITE (LUN,150,ERR=13)
      WRITE (LUN,230,ERR=13)
  230 FORMAT ('grestore')
      IF (NUMBR) THEN
C
C Nodes in the window are to be labeled with their indexes.
C   Convert FSIZN from points to world coordinates, and
C   output the commands to select a font and scale it.
C
        T = FSIZN*2.0/(SFX+SFY)
        WRITE (LUN,240,ERR=13) T
  240   FORMAT ('/Helvetica findfont'/
     .          F12.6,' scalefont setfont')
C
C   Loop on nodes N0 with coordinates (X0,Y0).
C
        DO 9 N0 = 1,N
          X0 = X(N0)
          Y0 = Y(N0)
          IF (X0 .LT. WX1  .OR.  X0 .GT. WX2  .OR.
     .        Y0 .LT. WY1  .OR.  Y0 .GT. WY2) GO TO 9
C
C   Move to (X0,Y0), and draw the label N0.  The first char-
C     acter will have its lower left corner about one
C     character width to the right of the nodal position.
C
          WRITE (LUN,180,ERR=13) X0, Y0
          WRITE (LUN,250,ERR=13) N0
  250     FORMAT ('(',I3,') show')
    9     CONTINUE
      ENDIF
C
C Convert FSIZT from points to world coordinates, and output
C   the commands to select a font and scale it.
C
      T = FSIZT*2.0/(SFX+SFY)
      WRITE (LUN,240,ERR=13) T
C
C Display TITLE centered above the plot:
C
      Y0 = WY2 + 3.0*T
      WRITE (LUN,260,ERR=13) TITLE, (WX1+WX2)/2.0, Y0
  260 FORMAT (A80/'  stringwidth pop 2 div neg ',F12.6,
     .        ' add ',F12.6,' moveto')
      WRITE (LUN,270,ERR=13) TITLE
  270 FORMAT (A80/'  show')
      IF (ANNOT) THEN
C
C Display the window extrema below the plot.
C
        X0 = WX1
        Y0 = WY1 - 100.0/(SFX+SFY)
        WRITE (LUN,180,ERR=13) X0, Y0
        WRITE (LUN,280,ERR=13) WX1, WX2
        Y0 = Y0 - 2.0*T
        WRITE (LUN,290,ERR=13) X0, Y0, WY1, WY2
  280   FORMAT ('(Window:   WX1 = ',E9.3,',   WX2 = ',E9.3,
     .          ') show')
  290   FORMAT ('(Window:  ) stringwidth pop ',F12.6,' add',
     .          F12.6,' moveto'/
     .          '( WY1 = ',E9.3,',   WY2 = ',E9.3,') show')
      ENDIF
C
C Paint the path and output the showpage command and
C   end-of-file indicator.
C
      WRITE (LUN,300,ERR=13)
  300 FORMAT ('stroke'/
     .        'showpage'/
     .        '%%EOF')
C
C HP's interpreters require a one-byte End-of-PostScript-Job
C   indicator (to eliminate a timeout error message):
C   ASCII 4.
C
      WRITE (LUN,310,ERR=13) CHAR(4)
  310 FORMAT (A1)
C
C No error encountered.
C
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   11 IER = 1
      RETURN
C
C DX or DY is not positive.
C
   12 IER = 2
      RETURN
C
C Error writing to unit LUN.
C
   13 IER = 3
      RETURN
      END
      SUBROUTINE TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,
     .                   PRNTX)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
     .        LOUT
      LOGICAL PRNTX
      DOUBLE PRECISION X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/30/98
C
C   Given a triangulation of a set of points in the plane,
C this subroutine prints the adjacency lists and, option-
C ally, the nodal coordinates on logical unit LOUT.  The
C list of neighbors of a boundary node is followed by index
C 0.  The numbers of boundary nodes, triangles, and arcs,
C and the constraint curve starting indexes, if any, are
C also printed.
C
C
C On input:
C
C       NCC = Number of constraints.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes in the triangulation -- not used
C             unless PRNTX = TRUE.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       LOUT = Logical unit number for output.  0 .LE. LOUT
C              .LE. 99.  Output is printed on unit 6 if LOUT
C              is outside its valid range on input.
C
C       PRNTX = Logical variable with value TRUE if and only
C               if X and Y are to be printed (to 6 decimal
C               places).
C
C None of the parameters are altered by this routine.
C
C Modules required by TRPRNT:  None
C
C***********************************************************
C
      INTEGER I, INC, K, LP, LPL, LUN, NA, NABOR(100), NB,
     .        ND, NL, NLMAX, NMAX, NODE, NN, NT
      DATA  NMAX/9999/,  NLMAX/60/
C
      NN = N
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test the range of N.
C
      WRITE (LUN,100) NN
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) THEN
C
C N is outside its valid range.
C
        WRITE (LUN,110)
        GO TO 5
      ENDIF
C
C Initialize NL (the number of lines printed on the current
C   page) and NB (the number of boundary nodes encountered).
C
      NL = 6
      NB = 0
      IF (.NOT. PRNTX) THEN
C
C Print LIST only.  K is the number of neighbors of NODE
C   which are stored in NABOR.
C
        WRITE (LUN,101)
        DO 2 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    1     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 1
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.  Correct the sign of the last
C     neighbor, add 0 to the end of the list, and increment
C     NB.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print the list of neighbors.
C
          INC = (K-1)/14 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,106)
            NL = INC
          ENDIF
          WRITE (LUN,103) NODE, (NABOR(I), I = 1,K)
          IF (K .NE. 14) WRITE (LUN,105)
    2     CONTINUE
      ELSE
C
C Print X, Y, and LIST.
C
        WRITE (LUN,102)
        DO 4 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
    3     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 3
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, and NABOR.
C
          INC = (K-1)/8 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,106)
            NL = INC
          ENDIF
          WRITE (LUN,104) NODE, X(NODE), Y(NODE),
     .                    (NABOR(I), I = 1,K)
          IF (K .NE. 8) WRITE (LUN,105)
    4     CONTINUE
      ENDIF
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NT = 2*NN - NB - 2
      NA = NT + NN - 1
      IF (NL .GT. NLMAX-6) WRITE (LUN,106)
      WRITE (LUN,107) NB, NA, NT
C
C Print NCC and LCC.
C
    5 WRITE (LUN,108) NCC
      IF (NCC .GT. 0) WRITE (LUN,109) (LCC(I), I = 1,NCC)
      RETURN
C
C Print formats:
C
  100 FORMAT (///,26X,'Adjacency Sets,    N = ',I5//)
  101 FORMAT (1X,'Node',32X,'Neighbors of Node'//)
  102 FORMAT (1X,'Node',5X,'X(Node)',8X,'Y(Node)',
     .        20X,'Neighbors of Node'//)
  103 FORMAT (1X,I4,5X,14I5/(1X,9X,14I5))
  104 FORMAT (1X,I4,2E15.6,5X,8I5/(1X,39X,8I5))
  105 FORMAT (1X)
  106 FORMAT (///)
  107 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  108 FORMAT (/1X,'NCC =',I3,' Constraint Curves')
  109 FORMAT (1X,9X,14I5)
  110 FORMAT (1X,10X,'*** N is outside its valid',
     .        ' range ***')
      END
      PROGRAM TTIDBS
C     PROGRAM  TTIDBS(OUTPUT,TAPE6=OUTPUT)                              ID000070
C THIS PROGRAM IS A TEST PROGRAM FOR THE IDBVIP/IDSFFT SUBPRO-          ID000080
C GRAM PACKAGE.  ALL ELEMENTS OF RESULTING DZI1 AND DZI2 ARRAYS         ID000090
C ARE EXPECTED TO BE ZERO.                                              ID000100
C THE LUN CONSTANT IN THE LAST DATA INITIALIZATION STATEMENT IS         ID000110
C THE LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,           ID000120
C THEREFORE, SYSTEM DEPENDENT.                                          ID000130
C DECLARATION STATEMENTS                                                ID000140
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   XD(30),YD(30),ZD(30),                                 ID000150
     1            XI(6),YI(5),ZI(6,5),MISSI(6,5),                       ID000160
     2            ZI1(6,5),ZI2(6,5),DZI1(6,5),DZI2(6,5),                ID000170
     3            IWK(1030),WK(240)                                     ID000180
      LOGICAL MISSI
      DATA  NCP/4/                                                      ID000190
      DATA  NDP/30/                                                     ID000200
      DATA  XD(1), XD(2), XD(3), XD(4), XD(5), XD(6),                   ID000210
     1      XD(7), XD(8), XD(9), XD(10),XD(11),XD(12),                  ID000220
     2      XD(13),XD(14),XD(15),XD(16),XD(17),XD(18),                  ID000230
     3      XD(19),XD(20),XD(21),XD(22),XD(23),XD(24),                  ID000240
     4      XD(25),XD(26),XD(27),XD(28),XD(29),XD(30)/                  ID000250
     5      11.16, 24.20, 19.85, 10.35, 19.72,  0.00,                   ID000260
     6      20.87, 19.99, 10.28,  4.51,  0.00, 16.70,                   ID000270
     7       6.08, 25.00, 14.90,  0.00,  9.66,  5.22,                   ID000280
     8      11.77, 15.10, 25.00, 25.00, 14.59, 15.20,                   ID000290
     9       5.23,  2.14,  0.51, 25.00, 21.67,  3.31/                   ID000300
      DATA  YD(1), YD(2), YD(3), YD(4), YD(5), YD(6),                   ID000310
     1      YD(7), YD(8), YD(9), YD(10),YD(11),YD(12),                  ID000320
     2      YD(13),YD(14),YD(15),YD(16),YD(17),YD(18),                  ID000330
     3      YD(19),YD(20),YD(21),YD(22),YD(23),YD(24),                  ID000340
     4      YD(25),YD(26),YD(27),YD(28),YD(29),YD(30)/                  ID000350
     5       1.24, 16.23, 10.72,  4.11,  1.39, 20.00,                   ID000360
     6      20.00,  4.62, 15.16, 20.00,  4.48, 19.65,                   ID000370
     7       4.58, 11.87,  3.12,  0.00, 20.00, 14.66,                   ID000380
     8      10.47, 17.19,  3.87,  0.00,  8.71,  0.00,                   ID000390
     9      10.72, 15.03,  8.37, 20.00, 14.36,  0.13/                   ID000400
      DATA  ZD(1), ZD(2), ZD(3), ZD(4), ZD(5), ZD(6),                   ID000410
     1      ZD(7), ZD(8), ZD(9), ZD(10),ZD(11),ZD(12),                  ID000420
     2      ZD(13),ZD(14),ZD(15),ZD(16),ZD(17),ZD(18),                  ID000430
     3      ZD(19),ZD(20),ZD(21),ZD(22),ZD(23),ZD(24),                  ID000440
     4      ZD(25),ZD(26),ZD(27),ZD(28),ZD(29),ZD(30)/                  ID000450
     5      22.15,  2.83,  7.97, 22.33, 16.83, 34.60,                   ID000460
     6       5.74, 14.72, 21.59, 15.61, 61.77,  6.31,                   ID000470
     7      35.74,  4.40, 21.70, 58.20,  4.73, 40.36,                   ID000480
     8      13.62, 12.57,  8.74, 12.00, 14.81, 21.60,                   ID000490
     9      26.50, 53.10, 49.43,  0.60,  5.52, 44.08/                   ID000500
      DATA  NXI/6/, NYI/5/                                              ID000510
      DATA  XI(1), XI(2), XI(3), XI(4), XI(5), XI(6)/                   ID000520
     1       0.00,  5.00, 10.00, 15.00, 20.00, 25.00/                   ID000530
      DATA  YI(1), YI(2), YI(3), YI(4), YI(5)/                          ID000540
     1       0.00,  5.00, 10.00, 15.00, 20.00/                          ID000550
      DATA  ZI(1,1),ZI(2,1),ZI(3,1),ZI(4,1),ZI(5,1),ZI(6,1),            ID000560
     1      ZI(1,2),ZI(2,2),ZI(3,2),ZI(4,2),ZI(5,2),ZI(6,2),            ID000570
     2      ZI(1,3),ZI(2,3),ZI(3,3),ZI(4,3),ZI(5,3),ZI(6,3),            ID000580
     3      ZI(1,4),ZI(2,4),ZI(3,4),ZI(4,4),ZI(5,4),ZI(6,4),            ID000590
     4      ZI(1,5),ZI(2,5),ZI(3,5),ZI(4,5),ZI(5,5),ZI(6,5)/            ID000600
     5      58.20, 39.55, 26.90, 21.71, 17.68, 12.00,                   ID000610
     6      61.58, 39.39, 22.04, 21.29, 14.36,  8.04,                   ID000620
     7      59.18, 27.39, 16.78, 13.25,  8.59,  5.36,                   ID000630
     8      52.82, 40.27, 22.76, 16.61,  7.40,  2.88,                   ID000640
     9      34.60, 14.05,  4.12,  3.17,  6.31,  0.60/                   ID000650
      DATA  LUN/6/                                                      ID000660
C CALCULATION                                                           ID000670
   10 MD=1                                                              ID000680
      DO 12  IYI=1,NYI                                                  ID000690
        DO 11  IXI=1,NXI                                                ID000700
          IF(IXI.NE.1.OR.IYI.NE.1)  MD=2                                ID000710
          MISSI(IXI,IYI)=.FALSE.
          CALL IDBVIP(MD,NCP,NDP,XD,YD,ZD,1,XI(IXI),YI(IYI),            ID000720
     1                ZI1(IXI,IYI),IWK,WK,MISSI)                        ID000730
   11   CONTINUE                                                        ID000740
   12 CONTINUE                                                          ID000750
   15 CALL IDSFFT(1,NCP,NDP,XD,YD,ZD,NXI,NYI,XI,YI,ZI2,IWK,WK,MISSI)    ID000760
      DO 17  IYI=1,NYI                                                  ID000770
        DO 16  IXI=1,NXI                                                ID000780
          DZI1(IXI,IYI)=ABS(ZI1(IXI,IYI)-ZI(IXI,IYI))                   ID000790
          DZI2(IXI,IYI)=ABS(ZI2(IXI,IYI)-ZI(IXI,IYI))                   ID000800
   16   CONTINUE                                                        ID000810
   17 CONTINUE                                                          ID000820
C PRINTING OF INPUT DATA                                                ID000830
   20 WRITE (LUN,2020)  NDP                                             ID000840
      DO 23  IDP=1,NDP                                                  ID000850
        IF(MOD(IDP,5).EQ.1)    WRITE (LUN,2021)                         ID000860
        WRITE (LUN,2022)  IDP,XD(IDP),YD(IDP),ZD(IDP)                   ID000870
   23 CONTINUE                                                          ID000880
C PRINTING OF OUTPUT RESULTS                                            ID000890
   30 WRITE (LUN,2030)                                                  ID000900
      WRITE (LUN,2031)  YI                                              ID000910
      DO 33  IXI=1,NXI                                                  ID000920
        WRITE (LUN,2032)  XI(IXI),(ZI1(IXI,IYI),IYI=1,NYI)              ID000930
   33 CONTINUE                                                          ID000940
   40 WRITE (LUN,2040)                                                  ID000950
      WRITE (LUN,2031)  YI                                              ID000960
      DO 43  IXI=1,NXI                                                  ID000970
        WRITE (LUN,2032)  XI(IXI),(DZI1(IXI,IYI),IYI=1,NYI)             ID000980
   43 CONTINUE                                                          ID000990
   50 WRITE (LUN,2050)                                                  ID001000
      WRITE (LUN,2031)  YI                                              ID001010
      DO 53  IXI=1,NXI                                                  ID001020
        WRITE (LUN,2032)  XI(IXI),(ZI2(IXI,IYI),IYI=1,NYI)              ID001030
   53 CONTINUE                                                          ID001040
   60 WRITE (LUN,2060)                                                  ID001050
      WRITE (LUN,2031)  YI                                              ID001060
      DO 63  IXI=1,NXI                                                  ID001070
        WRITE (LUN,2032)  XI(IXI),(DZI2(IXI,IYI),IYI=1,NYI)             ID001080
   63 CONTINUE                                                          ID001090
      STOP                                                              ID001100
C FORMAT STATEMENTS                                                     ID001110
 2020 FORMAT(1H1,6HTTIDBS/////3X,10HINPUT DATA,8X,5HNDP =,I3///         ID001120
     1   30H      I      XD     YD     ZD /)                            ID001130
 2021 FORMAT(1X)                                                        ID001140
 2022 FORMAT(5X,I2,2X,3F7.2)                                            ID001150
 2030 FORMAT(1H1,6HTTIDBS/////3X,17HIDBVIP SUBROUTINE///                ID001160
     1   26X,10HZI1(XI,YI))                                             ID001170
 2031 FORMAT(7X,2HXI,4X,3HYI=/12X,5F7.2/)                               ID001180
 2032 FORMAT(1X/1X,F9.2,2X,5F7.2)                                       ID001190
 2040 FORMAT(1X/////3X,10HDIFFERENCE///                                 ID001200
     1   25X,11HDZI1(XI,YI))                                            ID001210
 2050 FORMAT(1H1,6HTTIDBS/////3X,17HIDSFFT SUBROUTINE///                ID001220
     1   26X,10HZI2(XI,YI))                                             ID001230
 2060 FORMAT(1X/////3X,10HDIFFERENCE///                                 ID001240
     1   25X,11HDZI2(XI,YI))                                            ID001250
      END                                                               ID001260
