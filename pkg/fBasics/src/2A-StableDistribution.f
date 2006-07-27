
CONTENT:

C Numerical approximation to the Symmetric Stable Distribution
 
C###############################################################################


C
C SYMSTB
C  Fortran program to compute a fast numerical approximation to the Symmetric 
C  Stable distribution and density functions.  
C  (Hu McCulloch, mcculloch.2@osu.edu) 
C 
C Submission:
C  SYMSTB  (VS FORTRAN)
C  Numerical Approximation to Symmetric Stable Distribution and Density.
C  Posted by: J. Huston McCulloch
C            Economics Dept.
C            Ohio State Univ.
C            1945 N. High St.
C            Columbus, OH 43210
C            (614) 292-0382
C            mcculloch.2@osu.edu            
C The code may be freely used for non-commercial purposes and freely copied 
C  and distributed. The computation is described in 
C  J. Huston McCulloch, "Numerical Approximation of the Symmetric Stable 
C  Distributions and Densities," Ohio State Univ. Economics Dept., Oct. 1994, 
C  which is available from the author on request. Any research use 
C  of this code should cite this working paper. Contact the author for   
C  assistance.
C 
C Accuracy:
C  The expected relative density precision is 1.0e-6 for alpha in the 
C  range [.84, 2.00].  The programs have considerably reduced precision for
C  alpha < .84, although no error message is given for lower alpha 
C  values.  The absolute precision of the density is 6.6e-5 
C  for alpha in the range [..92, 2.00], while that of the distribution is
C  2.2e-5 in the same range.  See paper for details. 
C 
C Speed: 
C  Changing alpha induces set-up calculations, so submit as many x values
C  as you can before changing alpha.  Only the commands from 20 down are
C  are executed if alpha is unchanged from the previous call.  The GAUSS 
C  version of this program takes as little as 33 microseconds to compute 
C  the density (only) on a P5/100.
C 
C Compatibility:
C  DERFC is a built-in VS FORTRAN function that computes the complemented
C  error function, which is related to the cumulative normal distribution.  
C  If your FORTRAN does not have this, but instead has a complemented 
C  cumulative normal distribution function named (eg) NCDFC, command #1
C  may be replaced by 
C  1     GFUN(X) = NCDFC(X/SQRT(2))
C 
C  A GAUSS version of this routine is archived at 
C    gopher.american.edu/academic.depts/cas/econ/software/gauss
C 
C FORTRAN code follows:
C 
C
C ******************************************************************************
C
      SUBROUTINE SYMSTB(XX,YY,ZZ,NN,ALPHA)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION XX(NN),YY(NN),ZZ(NN)
      DIMENSION EI(3),U(3),S(4,19),R(19),Q(0:5),P(0:5,0:19)
      DIMENSION PD(0:4,0:19),ALF2I(4)
      DIMENSION ZNOT(19),ZN4(19),ZN5(19),COMBO(0:5),ZJI(19,0:5)
      DATA COMBO/1.0D0,5.0D0,10.0D0,10.0D0,5.0D0,1.0D0/
C
      DATA ((S(I, J), I = 1, 4), J = 1, 7) /                   
     1    1.85141 90959 D2,   -4.67693 32663 D2,               
     1    4.84247 20302 D2,   -1.76391 53404 D2,               
     1   -3.02365 52164 D2,    7.63519 31975 D2,               
     1   -7.85603 42101 D2,    2.84263 13374 D2,               
     1    4.40789 23600 D2,   -1.11811 38121 D3,               
     1    1.15483 11335 D3,   -4.19696 66223 D2,               
     1   -5.24481 42165 D2,    1.32244 87717 D3,               
     1   -1.35556 48053 D3,    4.88340 79950 D2,               
     1    5.35304 35018 D2,   -1.33745 70340 D3,               
     1    1.36601 40118 D3,   -4.92860 99583 D2,               
     1   -4.89889 57866 D2,    1.20914 18165 D3,               
     1   -1.22858 72257 D3,    4.40631 74114 D2,               
     1    3.29055 28742 D2,   -7.32117 67697 D2,               
     1    6.81836 41829 D2,   -2.28242 91084 D2 /              
      DATA ((S(I, J), I = 1, 4), J = 8,14) /                   
     1   -2.14954 02244 D2,    3.96949 06604 D2,               
     1   -3.36957 10692 D2,    1.09058 55709 D2,               
     1    2.11125 81866 D2,   -2.79211 07017 D2,               
     1    1.17179 66020 D2,    3.43946 64342 D0,               
     1   -2.64867 98043 D2,    1.19990 93707 D2,               
     1    2.10448 41328 D2,   -1.51108 81541 D2,               
     1    9.41057 84123 D2,   -1.72219 88478 D3,               
     1    1.40875 44698 D3,   -4.24725 11892 D2,               
     1   -2.19904 75933 D3,    4.26377 20422 D3,               
     1   -3.47239 81786 D3,    1.01743 73627 D3,               
     1    3.10474 90290 D3,   -5.42042 10990 D3,               
     1    4.22210 52925 D3,   -1.23459 71177 D3,               
     1   -5.14082 60668 D3,    1.10902 64364 D4,               
     1   -1.02703 37246 D4,    3.42434 49595 D3 /              
      DATA ((S(I, J), I = 1, 4), J = 15, 19) /                 
     1    1.12151 57876 D4,   -2.42435 29825 D4,               
     1    2.15360 57267 D4,   -6.84909 96103 D3,               
     1   -1.81206 31586 D4,    3.14301 32257 D4,               
     1   -2.41642 85641 D4,    6.91268 62826 D3,               
     1    1.73884 13126 D4,   -2.21083 97686 D4,               
     1    1.33979 99271 D4,   -3.12466 11987 D3,               
     1   -7.24357 75303 D3,    4.35453 99418 D3,               
     1    2.36161 55949 D2,   -7.65716 53073 D2,               
     1   -8.73767 25439 D3,    1.55108 52129 D4,               
     1   -1.37897 64138 D4,    4.63874 17712 D3  /             
C
      PI=3.141592653589793D0
      CA=DGAMMA(ALPHA)*DSIN(PI*ALPHA/2.0D0)/PI
      SQPI=DSQRT(PI)
      A2=DSQRT(2.0D0)-1.0D0
      CPXP0=1.0D0/PI
      GPXP0=1.0D0/(4.0D0*A2*SQPI)
      CPXPP0=CPXP0*2.0D0
      GPXPP0=GPXP0*1.5D0
      CPPP=CPXPP0*3.0D0-2.0D0/PI
      GPPP=GPXPP0*2.5D0-1.0D0/(32.0D0*SQPI*A2**3)
      DO J=1,19
         ZNOT(J)=J*0.05D0
         ZN4(J)=(1-ZNOT(J))**4
         ZN5(J)=(1-ZNOT(J))*ZN4(J)
         DO I=0,5
            ZJI(J,I)=COMBO(I)*(-ZNOT(J))**(5-I)
         ENDDO     
      ENDDO       
      DO I=1,3
         EI(I)=I
         U(I)=1
      ENDDO     
      Q(0)=0.0D0
      A=2.0D0**(1.0D0/ALPHA)-1.0D0
      ALA=ALPHA*A
      ALF2=2.0D0-ALPHA
      ALF1=ALPHA-1.0D0
      PIALF=PI*ALPHA
      SP0=DGAMMA(1.0D0/ALPHA)/PIALF
      SPPP0=-DGAMMA(3.0D0/ALPHA)/PIALF
      XP0=1.0D0/(ALA)
      XPP0=XP0*(1.0D0+ALPHA)/ALPHA
      XPPP0=XPP0*(1.0D0+2.0D0*ALPHA)/ALPHA
      SPZP1=(A**ALPHA)*DGAMMA(ALPHA)*DSIN(PIALF/2.0D0)/PI
      RP0=-SP0*XP0+ALF2*CPXP0+ALF1*GPXP0
      RPP0=-SP0*XPP0+ALF2*CPXPP0+ALF1*GPXPP0
      RPPP0=-SP0*XPPP0-SPPP0*XP0**3+ALF2*CPPP+ALF1*GPPP
      RP1=-SPZP1+ALF2/PI
      DO I=1,4
         ALF2I(I)=ALF2**I-1.0D0
      ENDDO    
      DO J=1,19
         R(J)=ALF2*(ALF2I(1)*S(1,J)+ALF2I(2)*S(2,J)
     &             +ALF2I(3)*S(3,J)+ALF2I(4)*S(4,J))
      ENDDO     
      Q(1)=RP0
      Q(2)=RPP0/2.0D0
      Q(3)=RPPP0/6.0D0
      B=-(U(1)+U(2)+U(3))*Q(1)
      DO IP=1,19
         B=B-R(IP)*ZN5(IP)
      ENDDO
      C=RP1-(EI(1)+EI(2)+EI(3))*Q(1)
      DO IP=1,19
         C=C-5.0D0*R(IP)*ZN4(IP)
      ENDDO
      Q(4)=5.0D0*B-C
      Q(5)=B-Q(4)
      DO I=0,5
         P(I,0)=Q(I)
         DO J=1,19
            PRD=0.0D0
            DO IP=1,J
               PRD=PRD+R(IP)
            ENDDO
            P(I,J)=Q(I)+PRD*ZJI(1,I)
         ENDDO     
      ENDDO    
      DO I=1,5
         DO J=0,19
            PD(I-1,J)=I*P(I,J)
         ENDDO     
      ENDDO    
C LOOP OVER ALL DATAPOINTS:
      DO II=1,NN
         X=XX(II)
         XA1=1.0D0+A*DABS(X)
         XA1A=XA1**(-ALPHA)
         Z=1.0D0-XA1A
         ZP=ALA*XA1A/XA1
         X1=((1.0D0-Z)**(-1.0D0)-1.0D0)
         X2=((1.0D0-Z)**(-0.5D0)-1.0D0)/A2
         X1P=1.0D0/((1.0D0+X1)**(-2.0D0))
         X2P=1.0D0/(2.0D0*A2*(1.0D0+A2*X2)**(-3.0D0))
         J=20*Z
         IF (J.GT.19) J=19
         RZ=P(0,J)
         DO IP=4,0,-1
            RZ=RZ*Z+P(0,J)
         ENDDO
         RPZ=PD(0,J)
         DO IP=3,0,-1
            RPZ=RPZ*Z+PD(0,J)
         ENDDO
C
C *** CUMULATED PROBABILITY FUNCTION:
         CFUN=0.5D0-DATAN(X1)/PI
         GFUN=0.5D0*DERFC2(X2/2.0D0)
         PROBFUN=ALF2*CFUN+ALF1*GFUN+RZ
           IF(X.LT.0.0D0) PROBFUN=1.0D0-PROBFUN 
           YY(II)=1.0D0-PROBFUN
         IF (YY(II).LT.10*2.2D-5) THEN
           YY(II)=CA*DABS(X)**(-ALPHA)
         ENDIF
C
C *** PROBABILITY DENSITY FUNCTION:
         CDEN=1.0D0/(PI*(1.0D0+X1*X1))
         GDEN=DEXP(-x2*X2/4.0D0)/(2.0D0*SQPI)
         PROBDEN=(ALF2*CDEN*X1P+ALF1*GDEN*X2P-RPZ)*ZP
           ZZ(II)=PROBDEN
         IF (ZZ(II).LT.10*6.6D-5) THEN
           ZZ(II)=ALPHA*CA*DABS(X)**(-ALPHA-1.0D0)
         ENDIF
C
C END OF X(NN) LOOP:
      ENDDO
      RETURN
      END
C
C ******************************************************************************
C
      DOUBLE PRECISION FUNCTION DERFC2(X)
      INTEGER JINT
      DOUBLE PRECISION X, RESULT
      JINT = 1
      CALL CALERF(X,RESULT,JINT)
      DERFC2 = RESULT
      RETURN
      END
C
      SUBROUTINE CALERF(ARG,RESULT,JINT)
      INTEGER I,JINT
      DOUBLE PRECISION
     1     A,ARG,B,C,D,DEL,FOUR,HALF,P,ONE,Q,RESULT,SIXTEN,SQRPI,
     2     TWO,THRESH,X,XBIG,XDEN,XHUGE,XINF,XMAX,XNEG,XNUM,XSMALL,
     3     Y,YSQ,ZERO
      DIMENSION A(5),B(4),C(9),D(8),P(6),Q(5)
      DATA FOUR,ONE,HALF,TWO,ZERO/4.0D0,1.0D0,0.5D0,2.0D0,0.0D0/,
     1     SQRPI/5.6418958354775628695D-1/,THRESH/0.46875D0/,
     2     SIXTEN/16.0D0/
      DATA XINF,XNEG,XSMALL/1.79D308,-26.628D0,1.11D-16/,
     1     XBIG,XHUGE,XMAX/26.543D0,6.71D7,2.53D307/
      DATA A/3.16112374387056560D00,1.13864154151050156D02,
     1       3.77485237685302021D02,3.20937758913846947D03,
     2       1.85777706184603153D-1/
      DATA B/2.36012909523441209D01,2.44024637934444173D02,
     1       1.28261652607737228D03,2.84423683343917062D03/
      DATA C/5.64188496988670089D-1,8.88314979438837594D0,
     1       6.61191906371416295D01,2.98635138197400131D02,
     2       8.81952221241769090D02,1.71204761263407058D03,
     3       2.05107837782607147D03,1.23033935479799725D03,
     4       2.15311535474403846D-8/
      DATA D/1.57449261107098347D01,1.17693950891312499D02,
     1       5.37181101862009858D02,1.62138957456669019D03,
     2       3.29079923573345963D03,4.36261909014324716D03,
     3       3.43936767414372164D03,1.23033935480374942D03/
      DATA P/3.05326634961232344D-1,3.60344899949804439D-1,
     1       1.25781726111229246D-1,1.60837851487422766D-2,
     2       6.58749161529837803D-4,1.63153871373020978D-2/
      DATA Q/2.56852019228982242D00,1.87295284992346047D00,
     1       5.27905102951428412D-1,6.05183413124413191D-2,
     2       2.33520497626869185D-3/
      X = ARG
      Y = DABS(X)
      IF (Y .LE. THRESH) THEN
            YSQ = ZERO
            IF (Y .GT. XSMALL) YSQ = Y * Y
            XNUM = A(5)*YSQ
            XDEN = YSQ
            DO 20 I = 1, 3
               XNUM = (XNUM + A(I)) * YSQ
               XDEN = (XDEN + B(I)) * YSQ
   20       CONTINUE
            RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
            IF (JINT .NE. 0) RESULT = ONE - RESULT
            IF (JINT .EQ. 2) RESULT = DEXP(YSQ) * RESULT
            GO TO 800
         ELSE IF (Y .LE. FOUR) THEN
            XNUM = C(9)*Y
            XDEN = Y
            DO 120 I = 1, 7
               XNUM = (XNUM + C(I)) * Y
               XDEN = (XDEN + D(I)) * Y
  120       CONTINUE
            RESULT = (XNUM + C(8)) / (XDEN + D(8))
            IF (JINT .NE. 2) THEN
               YSQ = DINT(Y*SIXTEN)/SIXTEN
               DEL = (Y-YSQ)*(Y+YSQ)
               RESULT = DEXP(-YSQ*YSQ) * DEXP(-DEL) * RESULT
            END IF
         ELSE
            RESULT = ZERO
            IF (Y .GE. XBIG) THEN
               IF ((JINT .NE. 2) .OR. (Y .GE. XMAX)) GO TO 300
               IF (Y .GE. XHUGE) THEN
                  RESULT = SQRPI / Y
                  GO TO 300
               END IF
            END IF
            YSQ = ONE / (Y * Y)
            XNUM = P(6)*YSQ
            XDEN = YSQ
            DO 240 I = 1, 4
               XNUM = (XNUM + P(I)) * YSQ
               XDEN = (XDEN + Q(I)) * YSQ
  240       CONTINUE
            RESULT = YSQ *(XNUM + P(5)) / (XDEN + Q(5))
            RESULT = (SQRPI -  RESULT) / Y
            IF (JINT .NE. 2) THEN
               YSQ = DINT(Y*SIXTEN)/SIXTEN
               DEL = (Y-YSQ)*(Y+YSQ)
               RESULT = DEXP(-YSQ*YSQ) * DEXP(-DEL) * RESULT
            END IF
      END IF
  300 IF (JINT .EQ. 0) THEN
            RESULT = (HALF - RESULT) + HALF
            IF (X .LT. ZERO) RESULT = -RESULT
         ELSE IF (JINT .EQ. 1) THEN
            IF (X .LT. ZERO) RESULT = TWO - RESULT
         ELSE
            IF (X .LT. ZERO) THEN
               IF (X .LT. XNEG) THEN
                     RESULT = XINF
                  ELSE
                     YSQ = DINT(X*SIXTEN)/SIXTEN
                     DEL = (X-YSQ)*(X+YSQ)
                     Y = DEXP(YSQ*YSQ) * DEXP(DEL)
                     RESULT = (Y+Y) - RESULT
               END IF
            END IF
      END IF
  800 RETURN
      END
C
C*******************************************************************************
C
      DOUBLE PRECISION FUNCTION DGAMMA(X)
      INTEGER I,N
      LOGICAL PARITY
      DOUBLE PRECISION 
     1    C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE,
     2    TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
     1     SQRTPI/0.9189385332046727417803297D0/,
     2     PI/3.1415926535897932384626434D0/
      DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,
     1     XINF/1.79D308/
      DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
     1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
     2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
     3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
      DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
     1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
     2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
     3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     1     -5.952379913043012D-04,7.93650793500350248D-04,
     2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     3      5.7083835261D-03/
      CONV(I) = DBLE(I)
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y .LE. ZERO) THEN
            Y = -X
            Y1 = DINT(Y)
            RES = Y - Y1
            IF (RES .NE. ZERO) THEN
                  IF (Y1 .NE. DINT(Y1*HALF)*TWO) PARITY = .TRUE.
                  FACT = -PI / DSIN(PI*RES)
                  Y = Y + ONE
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
      IF (Y .LT. EPS) THEN
            IF (Y .GE. XMININ) THEN
                  RES = ONE / Y
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
         ELSE IF (Y .LT. TWELVE) THEN
            Y1 = Y
            IF (Y .LT. ONE) THEN
                  Z = Y
                  Y = Y + ONE
               ELSE
                  N = INT(Y) - 1
                  Y = Y - CONV(N)
                  Z = Y - ONE
            END IF
            XNUM = ZERO
            XDEN = ONE
            DO 260 I = 1, 8
               XNUM = (XNUM + P(I)) * Z
               XDEN = XDEN * Z + Q(I)
  260       CONTINUE
            RES = XNUM / XDEN + ONE
            IF (Y1 .LT. Y) THEN
                  RES = RES / Y1
               ELSE IF (Y1 .GT. Y) THEN
                  DO 290 I = 1, N
                     RES = RES * Y
                     Y = Y + ONE
  290             CONTINUE
            END IF
         ELSE
            IF (Y .LE. XBIG) THEN
                  YSQ = Y * Y
                  SUM = C(7)
                  DO 350 I = 1, 6
                     SUM = SUM / YSQ + C(I)
  350             CONTINUE
                  SUM = SUM/Y - Y + SQRTPI
                  SUM = SUM + (Y-HALF)*LOG(Y)
                  RES = DEXP(SUM)
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
      IF (PARITY) RES = -RES
      IF (FACT .NE. ONE) RES = FACT / RES
  900 DGAMMA = RES
      RETURN
      END
      
     
C ##############################################################################

