
CONTENT:

C PART I:
C GRASS programs for distributional testing.

C PART II:
C Kolmogorov Smirnow Finite Sample probabilities


C ##############################################################################
C PART I:


C  This program is free software; you can redistribute it and/or
C  modify it under the terms of the GNU General Public License
C  as published by the Free Software Foundation; either version 2
C  of the License, or (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public License
C  along with this program; if not, write to the Free Software
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


C  Copyright (C) 1994-1995  James Darrell McCauley
C  Copyright (C) 1994       Paul Johnson


C ******************************************************************************


C  s.normal - GRASS program for distributional testing.
C  Copyright (C) 1994-1995. James Darrell McCauley.
C
C  Author: James Darrell McCauley darrell@mccauley-usa.com
C  http://mccauley-usa.com/
C
C  "s.normal v 0.4B <21 Jun 1995>; (C) 1994-1995. J.D. McCauley" */


C ******************************************************************************


C  The following is a slightly modified version of the file cdshc.f from
C  the grass-5.0.3 distribution downloaded from http://grass.itc.it/


C ******************************************************************************


C     Description:
C       Goodness of fit test suite for 13 tests of the hypothesis that
C       the data are normally distributed
C     Original Fortran Source: 
C       Paul Johnson
C       1420 Lake Blvd #29
C       Davis,California 95616
C       EZ006244@ALCOR.UCDAVIS.EDU

 
C *****************************************************************************


C TESTS OF COMPOSITE DISTRIBUTIONAL HYPOTHESES FOR
C THE ANALYSIS OF BIOLOGICAL & ENVIRONMENTAL DATA
C         
C This package contains FORTRAN subroutines for testing the hypothesis
C of normality. Lognormality can be tested by carrying out the tests of 
C normality on the log transformed data. 
C
C Some tests are general goodness-of-fit tests that allow any
C distribution to be tested simply by basing the distributional
C components of the test statistic on the hypothesized distribution.
C
C From the original set of 24 separate tests we have selected 13.
C These include:
C 
C     #     TEST NAME 
C     1     Omnibus Moments Test for Normality
C     2     Geary's Test of Normality
C     3     Studentized Range for Testing Normality
C     4     D'Agostino's D-Statistic Test of Normality
C     5     Kuiper V-Statistic Modified to Test Normality
C     6     Watson U^2-Statistic Modified to Test Normality
C     7     Durbin's Exact Test (Normal Distribution)
C     8     Anderson-Darling Statistic Modified to Test Normality
C     9     Cramer-Von Mises W^2-Statistic to Test Normality *
C    10     Kolmogorov-Smirnov D-Statistic to Test Normality *
C    11     Kolmogorov-Smirnov D-Statistic ( Lilliefors Critical Values)
C    12     Chi-Square Test of Normality (Equal Probability Classes)
C    14     Shapiro-Francia W' Test of Normality for Large Samples
C
C *  indicates the test statistic is modified
C E.P.C. --- Equal Probability Classes

 
C ******************************************************************************
C
C  TESTS OF COMPOSITE DISTRIBUTIONAL HYPOTHESES
C  --------------------------------------------
C
C  INPUT:  X [The vector of observed values]
C          N [The number of input observations]
C
C  OUTPUT: Y [The Composite Distributional Test Statistics]
C
C  SUBROUTINES:
C
C      ID  TEST NAME
C
C   TEST1  Omnibus Moments Test for Normality
C   TEST2  Geary's Test of Normality
C   TEST3  Studentized Range for Testing Normality
C   TEST4  D'Agostino's D-Statistic Test of Normality
C   TEST5  Kuiper V-Statistic Modified to Test Normality
C   TEST6  Watson U^2-Statistic Modified to Test Normality
C   TEST7  Durbin's Exact Test (Normal Distribution,Simple Hypothesis)
C   TEST8  Anderson-Darling Statistic Modified to Test Normality
C   TEST9  Cramer-Von Mises W^2-Statistic Modified to Test Normality
C  TEST10  Kolmogorov-Smirnov D-Statistic Modified to Test Normality
C  TEST11  Kolmogorov-Smirnov D-Statistic with Lilliefors Critical Values
C  TEST12  Chi-Square Test of Normality (with Equal Probability Classes)
C  TEST14  Shapiro-Francia W' Test of Normality for Large Samples
C
C  USAGE:    CALL TEST#(X,Y,N)   with # = 1,2,3...........,24
C
C  EXAMPLE:  CALL TEST20(X,Y,10) for an input vector X consisting of
C            10 observations results in the output vector Y where
C            Y(1) = AD(E).[The Shapiro-Wilk W Test of Exponentiality].
C
C  REFERENCES:
C
C    Anderson, T.W. and D.A. Darling.1954.A Test of Goodness of Fit.
C      JASA 49:765-69.
C    D'Agostino, R.B. and E.S. Pearson.1973.Tests for Departure from Normality.
C      Biometrika 60(3):613-22.
C    D'Agostino, R.B. and B. Rosman.1974.The Power of Geary's Test of 
C      Normality.Biometrika 61(1):181-84.
C    Durbin, J.1961.Some Methods of Constructing Exact Tests.
C      Biometrika 48(1&2):41-55. 
C    Durbin, J.1973.Distribution Theory Based on the Sample Distribution
C      Function.SIAM.Philadelphia.
C    Geary, R.C.1947.Testing for Normality.Biometrika 36:68-97.
C    Kotz, S. 1973. Normality vs. Lognormality with Applications.
C      Communications in Statistics 1(2):113-32.
C    Lehmann,E.L.1986.Testing Statistical Hypotheses.John Wiley & Sons.
C      New York.
C    Linnet, K.1988. Testing Normality of Transformed Data.
C      Applied Statistics 32(2):180-186. 
C    SAS [Statistical Analysis System] User's Guide:Basics.Version 5.1985.
C    SAS User's Guide:Statistics.Version 6.Volumes 1 and 2.1993.
C    Shapiro, S.S. and R.S.Francia.1972.An Approximate Analysis of Variance 
C      Test for Normality.JASA 67(337):215-216.
C    Shapiro, S.S., M.B.Wilk and H.J.Chen.1968.A Comparative Study of Various
C      Tests for Normality.JASA 63:1343-72.
C    Weiss, M.S. 1978.Modification of the Kolmogorov-Smirnov Statistic for Use 
C      with Correlated Data.JASA 73(364):872-75.
C
C
C****************************************************************************
C
CF
CF FORTRAN CALL ROUTINE:
CF
      subroutine gofs(x,n,y1,y2,z1,z2,z3,z4,z5,z6,z7)

CF    parameter (n=5000)
          
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y1(13),Y2(13),Y(2)
      DIMENSION Z1(N+1),Z2(N+1),Z3(N+1),Z4(N+1),Z5(N+1),Z6(N+1),Z7(N+1)

CF    do i=1,n
CF       read(*,*) x(i)
CF    enddo

      i=0
      CALL TEST1 (X,Y,N,Z1)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST2 (X,Y,N,Z1)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST3 (X,Y,N,Z1)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST4 (X,Y,N,Z1)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST5 (X,Y,N,Z1,Z2,Z3,Z4,Z5,Z6,Z7)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST6 (X,Y,N,Z1,Z2,Z3,Z4)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST7 (X,Y,N,Z1,Z2,Z3,Z4,Z5)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST8 (X,Y,N,Z1,Z2,Z3,Z4)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST9 (X,Y,N,Z1,Z2,Z3,Z4)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST10(X,Y,N,Z1,Z2,Z3,Z4,Z5,Z6,Z7)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST11(X,Y,N,Z1,Z2,Z3,Z4,Z5,Z6,Z7)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
      CALL TEST12(X,Y,N,Z1,Z2,Z3,Z4,Z5)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
C     CALL TEST13 - not used
      CALL TEST14(X,Y,N,Z1,Z2,Z3)
      i=i+1
      y1(i)=y(1)
      y2(i)=y(2)
CF
CF    write (*,*)
CF    write (*,*) y1
CF    write (*,*)
CF    write (*,*) y2
CF
      return
      end
C
C*******************************************************************************
C
      SUBROUTINE TEST1(X,Y,N,Z)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),Z(N)
      REAL*8 MEAN
      ntest=1
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)-MEAN)**3
      DO 30 I = 1,N
   30 Z(I)=(X(I)-MEAN)**2
      DO 40 I = 1,N
   40 SUM3=SUM3+Z(I)
      SUM5=SUM3
      SUM3=SUM3**1.5
      TSSM=(N**0.5d0)*SUM2/SUM3
      DO 50 I = 1,N
   50 SUM4=SUM4+(X(I)-MEAN)**4
      FSSM=(N*SUM4)/(SUM5*SUM5)
      Y(1) = TSSM
      Y(2) = FSSM
C     WRITE(6,300)
C 300 FORMAT(1X,' ')
C     WRITE(6,301)
C 301 FORMAT(10X,'TESTS OF COMPOSITE DISTRIBUTIONAL HYPOTHESES')
C     WRITE(6,302)
C 302 FORMAT(1X,' ')
C     WRITE(6,100) Y(1),Y(2)
C 100 FORMAT(/,2X,'TEST1  TSM    =',F10.4,'   FSM    =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST2(X,Y,N,Z)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),Z(N)
      REAL*8 MEAN
      ntest=2
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      DO 10 I = 1,N
   10 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      DO 20 I = 1,N
   20 Z(I)=DABS(X(I)-MEAN)
      DO 30 I = 1,N
   30 SUM2=SUM2+Z(I)
      DO 40 I = 1,N
   40 SUM3=SUM3+(X(I)-MEAN)**2
      S=N*SUM3
      S1=DSQRT(S)
      Y(1)=SUM2/S1
      Y(2)=(Y(1)-0.7979d0)*DSQRT(N*1.0d0)/0.2123d0
C     WRITE(6,100) Y(1),Y(2)
C 100 FORMAT(/,2X,'TEST2  GTN    =',F10.4,'   Z(GTN) =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST3(X,Y,N,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),X1(N)
      ntest=3
      y(2)=0.0d0
      DO 10 I=1,N
   10 X1(I)=X(I)
      CALL SORT(N,X)
      XS=X(1)
      XM=X(N)
      DO 15 I = 1,N
   15 IF (XS .GE. X(I)) XS=X(I)
      DO 20 I = 1,N
   20 IF (XM .LE. X(I)) XM=X(I)
      SUM1=0.0d0
      SUM2=0.0d0
      DO 30 I = 1,N
      SUM1=SUM1+X(I)
      SUM2=SUM2+(X(I)*X(I))
   30 CONTINUE
      XBAR=SUM1/N
      S1 =SUM2-((SUM1*SUM1)/N)
      S2=S1/(N-1)
      S3=DSQRT(S2)
      Y(1)=(XM-XS)/S3
      DO 40 I=1,N
   40 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST3  U      =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST4(X,Y,K,A1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(K),Y(2),A1(K)
      REAL M2
      REAL MN
      ntest=4
      y(2)=0.0d0
      S1=0.0d0
      T=0.0d0
      MN=0.0d0
      E=1
      DO 10 I=1,K
   10 A1(I) = X(I)
      CALL SORT(K,A1)
      DO 20 I =1,K
      T = T + (I - 0.5d0*(K+1))*A1(I)
   20 CONTINUE
      DO 30 I = 1,K
      MN=MN+A1(I)
   30 CONTINUE
      M2=MN/K
      DO 40 I=1,K
      S1=S1+(A1(I)-M2)**2
   40 CONTINUE
      S2=S1/K
      S =DSQRT(S2)
      D = T/(K**2*S)
      Y(1)=(D-1.0d0/(2.0d0*DSQRT(3.141592654d0)))*
     &  DSQRT(K*1.0d0)/0.02998598d0
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST4  DAGN   =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST5(X,Y,N,FX,FN1,FN2,T,Z,FN3,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION T(N),Z(N),D(2),FN3(N),X1(N)
      REAL*8 MEAN
      ntest=5
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FN3(I)=DFLOAT(I)/R
      FX(I)=.5+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0) FX(I) = 0.00001d0
   30 IF (FX(I) .GE. 1.0) FX(I) = 0.99999d0
      DO 40 J=1,N
   40 Z(J) = (FN3(J) - FX(J))
      CALL SORT(N,Z)
      D1=Z(N)
      DO 50 J=1,N
   50 T(J) = FX(J)-((J-1)/R)
      CALL SORT(N,T)
      D2=T(N)
      D(1)=D1
      D(2)=D2
      CALL SORT(2,D)
      V=D(1)+D(2)
      V=V*(DSQRT(R)+(0.82d0/DSQRT(R))+0.05d0)
      Y(1)=V
      DO 60 I=1,N
   60 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST5  KV(N)  =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST6(X,Y,N,FX,FN1,FN2,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION X1(N)
      REAL*8 MEAN
      ntest=6
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FX(I)=.5+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0d0) FX(I) = 0.00001d0
   30 IF (FX(I) .GE. 1.0d0) FX(I) = 0.99999d0
      DO 40 I=1,N
      A=((2.0d0*I)-1.0d0)*DLOG(FX(I))
      B=((2.0d0*I)-1.0d0)*DLOG(1.0d0-FX(N+1-I))
      SUM3=SUM3+A+B
      SUM4=SUM4+((-FN2(I)+FX(I))**2)
      FN1(I)=DABS(FN1(I)-FX(I))
   40 CONTINUE
      CVM=1./DFLOAT(12*N)+SUM4
      DO 50 I=1,N
      SUM5=SUM5+FX(I)
   50 CONTINUE
      ZBAR=SUM5/R
      W=CVM-R*(ZBAR-0.5)*(ZBAR-0.5)
      W=W*(1.0d0+0.5d0/R)
      Y(1)=W
      DO 60 I=1,N
   60 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST6  WU2(N) =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST7(X,Y,N,C,X1,G,Z,B)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),C(N+1),X1(N),G(N),Z(N),B(N),Y(2)
      ntest=7
      y(2)=0.0d0
      R=DFLOAT(N)
      SUMX=0.0d0
      SUMX2=0.0d0
      DO 10 I=1,N
      SUMX=SUMX+X(I)
      SUMX2=SUMX2+X(I)**2
   10 X1(I)=X(I)
      S2=(SUMX2-SUMX**2/N)/(N-1)
      DO 15 I=1,N
      X(I)=(X(I)-SUMX/N)/DSQRT(S2)
      B(I)=0.5d0+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
   15 CONTINUE
      CALL SORT(N,B)
      DO 20 I=2,N
      C(I)=B(I)-B(I-1)
   20 CONTINUE
      C(1)=B(1)
      C(N+1)=1.0d0-B(N)
      CALL SORT(N+1,C)
      DO 30 J=2,N
      G(J)=(N+2-J)*(C(J)-C(J-1))
   30 CONTINUE
      G(1)=(N+1)*C(1)
      G(N+1)=C(N+1)-C(N)
      DO 60 I=1,N
      SUM1=0.0d0
      DO 50 J=1,I
      SUM1=SUM1+G(J)
   50 CONTINUE
      Z(I)=(I/R)-SUM1
   60 CONTINUE
      CALL SORT(N,Z)
      R=Z(N)
      Y(1)=R
      DO 70 I=1,N
   70 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST7  DRB(N) =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST8(X,Y,N,FX,FN1,FN2,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION X1(N)
      EXTERNAL enormp
      REAL*8 MEAN
      ntest=8
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FX(I)=.5+(enormp(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0d0) FX(I) = 0.00001d0
   30 IF (FX(I) .GE. 1.0d0) FX(I) = 0.99999d0
      DO 40 I=1,N
      A=((2.0d0*I)-1)*DLOG(FX(I))
      B=((2.0d0*I)-1)*DLOG(1.0d0-FX(N+1-I))
      SUM3=SUM3+A+B
      SUM4=SUM4+((-FN2(I)+FX(I))**2)
      FN1(I)=DABS(FN1(I)-FX(I))
   40 CONTINUE
      ADB=SUM3/R
      ADSTAT=-R-ADB
      ADMOD=ADSTAT*(1.0+(.75/R)+(2.25/DFLOAT(N**2)))
      Y(1)=ADMOD
      DO 50 I=1,N
   50 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST8  AD(N)  =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST9(X,Y,N,FX,FN1,FN2,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION X1(N)
      REAL*8 MEAN
      ntest=9
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FX(I)=0.5d0+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0d0) FX(I) = 0.00001d0
   30 IF (FX(I) .GE. 1.0d0) FX(I) = 0.99999d0
      DO 40 I=1,N
      A=((2.0d0*I)-1.0d0)*DLOG(FX(I))
      B=((2.0d0*I)-1.0d0)*DLOG(1.0d0-FX(N+1-I))
      SUM3=SUM3+A+B
      SUM4=SUM4+((-FN2(I)+FX(I))**2)
      FN1(I)=DABS(FN1(I)-FX(I))
   40 CONTINUE
      CVM=1.0d0/DFLOAT(12*N)+SUM4
      CVMOD=CVM*(1.0d0+(0.5d0/R))
      Y(1)=CVMOD
      DO 50 I=1,N
   50 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST9  CVM(N) =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST10(X,Y,N,FX,FN1,FN2,T,Z,FN3,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION T(N),Z(N),D(2),FN3(N),X1(N)
      REAL*8 MEAN,KS
      ntest=10
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      SUM4=0.0d0
      SUM5=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FN3(I)=DFLOAT(I)/R
      FX(I)=0.5d0+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0d0) FX(I) = 0.00001d0
   30 IF (FX(I) .GE. 1.0d0) FX(I) = 0.99999d0
      DO 40 J=1,N
   40 Z(J)=(FN3(J)-FX(J))
      CALL SORT(N,Z)
      D1=Z(N)
      DO 50 J=1,N
   50 T(J)=FX(J)-((J-1)/R)
      CALL SORT(N,T)
      D2=T(N)
      D(1)=D1
      D(2)=D2
      CALL SORT(2,D)
      DMAX=D(2)
      KS=DMAX*(DSQRT(R)+(0.85d0/DSQRT(R))-0.01d0)
      Y(1)=KS
      DO 60 I=1,N
   60 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST10 KSD(N) =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST11(X,Y,N,FX,FN1,FN2,T,Z,FN3,X1)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),FX(N),FN1(N),FN2(N)
      DIMENSION T(N),Z(N),D(2),FN3(N),X1(N)
      REAL*8 MEAN
      ntest=11
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      DO 10 I = 1,N
   10 X1(I)=X(I)
      DO 15 I =1,N
   15 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      XBAR=MEAN
      DO 20 I = 1,N
   20 SUM2=SUM2+(X(I)*X(I))
      S1=((N*SUM2)-(SUM1*SUM1))/(N*(N-1))
      SDX=DSQRT(S1)
      R=DFLOAT(N)
      CALL SORT(N,X)
      DO 30 I=1,N
      X(I)=(X(I)-XBAR)/SDX
      FN1(I)=DFLOAT(I)/R
      FN2(I)=DFLOAT((2*I)-1)/DFLOAT(2*N)
      FN3(I)=DFLOAT(I)/R
      FX(I)=0.5d0+(ENORMP(X(I)/DSQRT(2.0d0))/2.0d0)
      IF (FX(I) .LE. 0.0) FX(I) = 0.00001
   30 IF (FX(I) .GE. 1.0) FX(I) = 0.99999
      DO 40 J=1,N
   40 Z(J)=(FN3(J)-FX(J))
      CALL SORT(N,Z)
      D1=Z(N)
      DO 50 J=1,N
   50 T(J)=FX(J)-((J-1)/R)
      CALL SORT(N,T)
      D2=T(N)
      D(1)=D1
      D(2)=D2
      CALL SORT(2,D)
      Y(1)=D(2)
      DO 60 I=1,N
   60 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST11 KSD    =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST12(X,Y,N,V,V2,F,P,Z)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),Y(2),V(N),V2(N),F(N)
      DIMENSION P(N),Z(N)
      EXTERNAL xinormal
      REAL*8 MEAN
      ntest=12
      y(2)=0.0d0
      SUM1=0.0d0
      SUM2=0.0d0
      SUM3=0.0d0
      A=4*((0.75d0*(N-1)*(N-1))**0.2d0)
      K1=A
      C=A-K1
      IF(C .GT. 0.5d0) K1=K1+1
   10 R=N/K1
      IF(R .LT. 5.0d0) K1=K1-1
      IF(R .LT. 5.0d0) GOTO 10
      K2=K1-1
      DO 15 I=1,K1
   15 F(I)=0.0d0
      DO 20 I=1,N
   20 SUM1=SUM1+X(I)
      MEAN=SUM1/N
      DO 25 I = 1,N
   25 SUM2=SUM2+(X(I)-MEAN)*(X(I)-MEAN)
      S1=DSQRT(SUM2/(N-1))
      DO 30 I=1,K2
   30 P(I)=DFLOAT(I)/K1
      DO 40 I=1,K2
   40 Z(I)=XINORMAL(P(I))
      DO 50 I=1,K2
   50 V(I)=MEAN+(Z(I)*S1)
      DO 51 I=1,K2
   51 V2(I+1)=V(I)+0.0001d0
      DO 55 I=1,N
      DO 45 J=2,K2
      IF(X(I) .GE. V2(J) .AND. X(I) .LE. V(J)) F(J)=1.0d0+F(J)
   45 CONTINUE
      IF(X(I) .GE. V2(K2+1)) F(K2+1)=F(K2+1)+1.0d0
      IF(X(I) .LE. V(1)) F(1)=F(1)+1.0d0
   55 CONTINUE
      DO 65 I=1,K1
   65 SUM3=SUM3+F(I)*F(I)
      Y(1)=SUM3*K1/N-N
      Y(2)=DFLOAT(K1)-3
C     WRITE(6,100) Y(1),Y(2)
C 100 FORMAT(/,2X,'TEST12 CS(N)  =',F10.4,'   DOF  =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE TEST14(X,Y,N,X1,Z,P)
      implicit real*8 (a-h,o-z)
      DIMENSION X(N),X1(N),Y(2),Z(N),P(N)
      EXTERNAL xinormal
      ntest=14
      y(2)=0.0d0
      SUMA=0.0d0
      SUMB=0.0d0
      SUMC=0.0d0
      SUMD=0.0d0
      DO 10 I =1,N
   10 X1(I)=X(I)
      CALL SORT(N,X)
      DO 20 I = 1,N
   20 P(I)=(DFLOAT(I)-.375)/(0.25+N)
      DO 25 I=1,N
   25 Z(I)=XINORMAL(P(I))
      DO 30 I=1,N
      SUMA=SUMA+(Z(I)*X(I))
      SUMB=SUMB+(Z(I)**2)
      SUMC=SUMC+X(I)
   30 SUMD=SUMD+X(I)**2
      Y(1)=(SUMA**2/SUMB)/(SUMD-SUMC**2/N)
      DO 40 I=1,N
   40 X(I)=X1(I)
C     WRITE(6,100) Y(1)
C 100 FORMAT(/,2X,'TEST14 SF(N)  =',F10.4)
CW    write (*,*) ntest,y
      RETURN
      END
C
      SUBROUTINE SORT(NN,X)
      implicit real*8(a-h,o-z)
      INTEGER I,J,K,L,IJ,NN,M,IU(16),IL(16)
      REAL*8 Y,YY,X(NN)
      M=1
      I=1
      J=NN
  150 IF (I .GE. J) GO TO 220
  160 K=I
      IJ=(J+I)/2
      Y=X(IJ)
      IF (X(I) .LE. Y) GO TO 170
      X(IJ)=X(I)
      X(I)=Y
      Y=X(IJ)
  170 L=J
      IF (X(J) .GE. Y) GO TO 190
      X(IJ)=X(J)
      X(J)=Y
      Y=X(IJ)
      IF (X(I) .LE. Y) GO TO 190
      X(IJ)=X(I)
      X(I)=Y
      Y=X(IJ)
      GO TO 190
  180 X(L)=X(K)
      X(K)=YY
  190 L=L-1
      IF (X(L) .GT. Y) GO TO 190
      YY=X(L)
  200 K=K+1
      IF (X(K) .LT. Y) GO TO 200
      IF (K .LE. L) GO TO 180
      IF (L-I .LE. J-K) GO TO 210
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 230
  210 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 230
  220 M=M-1
      IF (M .EQ. 0) GO TO 260
      I=IL(M)
      J=IU(M)
  230 IF (J-I .GE. 1) GO TO 160
      I=I-1
  240 I=I+1
      IF (I .EQ. J) GO TO 220
      Y=X(I+1)
      IF (X(I) .LE. Y) GO TO 240
      K=I
  250 X(K+I)=X(K)
      K=K-1
      IF (Y .LT. X(K)) GO TO 250
      X(K+1)=Y
      GO TO 240
  260 CONTINUE
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION enormp(x)
      implicit real*8 (a-h,o-z)
      x4 =                  0.0D0
      xp1 = 0.771058495001320D-04
      xp2 =-0.00133733772997339D0
      xp3 =  0.0323076579225834D0
      xp4 =  0.0479137145607681D0
      xp5 =   0.128379167095513D0
      xq1 = 0.00301048631703895D0
      xq2 =  0.0538971687740286D0
      xq3 =   0.375795757275549D0
      xr1 = -1.36864857382717D-07
      xr2 =   0.564195517478974D0
      xr3 =    7.21175825088309D0
      xr4 =    43.1622272220567D0
      xr5 =    152.989285046940D0
      xr6 =    339.320816734344D0
      xr7 =    451.918953711873D0
      xr8 =    300.459261020162D0
      xs1 =                 1.0D0
      xs2 =    12.7827273196294D0
      xs3 =    77.0001529352295D0
      xs4 =    277.585444743988D0
      xs5 =    638.980264465631D0
      xs6 =    931.354094850610D0
      xs7 =    790.950925327898D0
      xs8 =    300.459260956983D0
      xt1 =    2.10144126479064D0
      xt2 =    26.2370141675169D0
      xt3 =    21.3688200555087D0
      xt4 =    4.65807828718470D0
      xt5 =   0.282094791773523D0
      xu1 =    94.1537750555460D0
      xu2 =    187.114811799590D0
      xu3 =    99.0191814623914D0
      xu4 =    18.0124575948747D0
      x3  =   0.564189583547756D0
      x1  = dabs(x)
      IF (x1.GT.0.5D0) GO TO 10
      x4 = x*x
      yy1 = ((((xp1*x4+xp2)*x4+xp3)*x4+xp4)*x4+xp5) +1.0D0
      yy2 = (((xq1*x4+xq2)*x4+xq3)*x4) + 1.0D0
      enormp = x* (yy1/yy2)
      RETURN
   10 IF (x1.GT.4.0D0) GO TO 20
      yy1 = ((((((xr1*x1+xr2)*x1+xr3)*x1+xr4)*x1+xr5)*x1
     +    + xr6)*x1+xr7)*x1+xr8
      yy2 = ((((((xs1*x1+xs2)*x1+xs3)*x1+xs4)*x1+xs5)*x1
     +    + xs6)*x1+xs7)*x1+xs8
      enormp =  1.0D0-exp(-x*x)*yy1/yy2
      IF (x .LT. 0.0D0) enormp = -enormp
      RETURN
   20 x2 = x*x
      x4=1.0D0*x4
      yy1 = ((((xt1*x4+xt2)*x4+xt3)*x4+xt4)*x4) + xt5
      yy2 = ((((xu1*x4+xu2)*x4+xu3)*x4+xu4)*x4) + 1.0D0
      enormp = (x3/x1)-(yy1*x1)/(x2*yy2)
      enormp = 1.0D0-dexp(-x2)*enormp
      IF (x .LT. 0.0D0) enormp = -enormp
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION xinormal(p)
      implicit real*8 (a-h,o-z)
      p0=-0.322232431088D0
      p1=-1.0D0
      p2=-0.342242088547D0
      p3=-0.0204231210245D0
      p4=-0.0000453642210148D0
      q0=0.099348462606D0
      q1=0.588581570495D0
      q2=0.531103462366D0
      q3=0.10353775285D0
      q4=0.0038560700634D0
      pind=p
      IF (p .LT. 1.0D-10) THEN
         xinormal = -10
         RETURN
      ENDIF
      IF (p .GE. 1.0) THEN
         xinormal = 10
         RETURN
      ENDIF
      IF (p .EQ. 0.5D0) THEN
         xinormal = 0.5
         RETURN
      ENDIF
      IF (p .GT. 0.5D0) p=p-1
      pw=DSQRT(DLOG(1.0d0/(p*p)))
      f0=(((pw*q4+q3)*pw+q2)*pw+q1)*pw+q0
      px=pw+((((pw*p4+p3)*pw+p2)*pw+p1)*pw+p0)/f0
      if (pind .LT. 0.5D0) px=-px
      xinormal = px
      RETURN
      END

      


C ##############################################################################
C PART II:
C Kolmogorov Smirnow Finite Sample probabilities


C     ALGORITHM 487 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 17, NO. 12,
C     P. 703.


C ------------------------------------------------------------------------------
C DW: I have added the following lines:


      SUBROUTINE PKS2(D, L, N, ANS)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
      DOUBLE PRECISION D(L), ANS(L)
      DO 10 I = 1, L
      	ANS(I) = FPKS2(N, D(I))
   10 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE PKS(EPS, L, N, ANS)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
      DOUBLE PRECISION EPS(L), ANS(L)
      DO 10 I = 1, L
      	ANS(I) = FPKS(N, EPS(I))
   10 CONTINUE
      RETURN
      END      
     
      
C ------------------------------------------------------------------------------
C DW: Converted to Double Precision     
      

      FUNCTION FPKS2(N, D)                                   
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
      INTEGER N
      
C N IS THE SAMPLE SIZE USED.

      
C D IS THE MAXIMUM MAGNITUDE (OF THE DISCREPANCY
C BETWEEN THE EMPIRICAL AND PROPOSED DISTRIBUTIONS)
C IN EITHER THE POSITIVE OR NEGATIVE DIRECTION.
C FPKS2 IS THE EXACT PROBABILITY OF OBTAINING A
C DEVIATION NO LARGER THAN D.
C THESE FORMULAS APPEAR AS (23) AND (24) IN
C J. DURBIN.  THE PROBABILITY THAT THE SAMPLE
C DISTRIBUTION FUNCTION LIES BETWEEN TWO PARALLEL
C STRAIGHT LINES. ANNALS OF MATHEMATICAL STATISTICS
C 39, 2(APRIL 1968),398-411.

      DOUBLE PRECISION Q(141), FACT(141), SUM, CI,
     * FT, FU, FV
      IF (N.EQ.1) GO TO 90
      FN = DFLOAT(N)
      FND = FN*D
      NDT = IFIX(REAL(2.0D0*FND))
      IF (NDT.LT.1) GO TO 100
      ND = IFIX(REAL(FND))
      NDD = MIN0(2*ND, N)
      NDP = ND + 1
      NDDP = NDD + 1
      FACT(1) = 1.0D0
      CI = 1.0D0
      DO 10 I = 1 ,N
        FACT(I+1) = FACT(I)*CI
        CI = CI + 1.0D0
   10 CONTINUE
      Q(1) = 1.
      IF (NDD.EQ.0) GO TO 50
      CI = 1.
      DO 20 I=1,NDD
        Q(I+1) = CI**I/FACT(I+1)
        CI = CI + 1.0D0
   20 CONTINUE
      IF (NDP.GT.N) GO TO 80
      FV = DFLOAT(NDP) - FND
      JMAX = IDINT(FV) + 1
      DO 40 I=NDP,NDD
        SUM = 0.0D0
        FT = FND
        K = I
        FU = FV
        DO 30 J = 1, JMAX
          SUM = SUM + FT**(J-2)/FACT(J)*FU**K/
     *     FACT(K+1)
          FT = FT + 1.
          FU = FU - 1.
          K = K - 1
   30   CONTINUE
        Q(I+1) = Q(I+1) - 2.0D0*FND*SUM
        JMAX = JMAX + 1
        FV = FV + 1.0D0
   40 CONTINUE
      IF (NDD.EQ.N) GO TO 80
   50 DO 70 I = NDDP, N
        SUM = 0.
        DSIGN = 1.0D0
        FT = 2.0D0*FND
        DO 60 J = 1, NDT
          FT = FT - 1.0D0
          K = I - J + 1
          SUM = SUM + DSIGN*FT**J/FACT(J+1)*Q(K)
          DSIGN = -DSIGN
   60   CONTINUE
        Q(I+1) = SUM
   70 CONTINUE
   80 FPKS2 = Q(N+1)*FACT(N+1)/FN**N
      RETURN
   90 FPKS2 = 2.0D0*D - 1.
      RETURN
  100 FPKS2 = 0.
      RETURN
      END
      
      
      
      SUBROUTINE PRFAC 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)  
                                     
      DOUBLE PRECISION PF(4,40)
      DIMENSION DXA(4)
      COMMON DX, DXA, PF, J
      DATA I /1/
      DO 10 J = 1, 4
        IF (DXA(J).EQ.DX) RETURN
   10 CONTINUE
      J = I
      I = I + 1
      IF (I.EQ.5) I = 1
      DXA(J) = DX
      PF(J,1) = 1.
      DO 20 K=2,38
        PF(J,K) = (PF(J, K-1)*DX)/DFLOAT(K-1)
   20 CONTINUE
      RETURN
      END
      
      
      DOUBLE PRECISION FUNCTION CEIL(X)  
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)  
                                                      
      IF (X.GE.0.0D0) GO TO 10
      I = -X
      CEIL = -I
      RETURN
   10 I = X + 0.99999999D0
      CEIL = I
      RETURN
      END
      
      
      FUNCTION FPKS(N, EPS)                                       
      
      
C CALCULATE THE CUMULATIVE DISTRIBUTION OF THE
C KOLMOGOROV-SMIRNOV STATISTIC USING THE FORMULAS OF
C JOHN POMERANZ. EXACT VALUES OF THE TWO-SIDED
C KOLMOGOROV- SMIRNOV CUMULATIVE DISTRIBUTION FOR
C FINITE SAMPLE SIZE. TECHNICAL REPORT NUMBER 88,
C COMPUTER SCIENCES DEPARTMENT, PURDUE UNIVERSITY,
C FEBRUARY 1973.


      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION PF(4,40), U(40), V(40)
      
      DIMENSION DXA(4)
      COMMON DX, DXA, PF, L
      DATA MNP /40/
      FN = N
      RN = 1.0D0/FN
      K = EPS*FN + 0.00000001D0
      FK = K
      IF (DABS(FK-EPS*FN).GT.0.00000001D0) GO TO 10
      K = K - 1
      FK = K
   10 CONTINUE
      DEL = EPS - FK*RN
      XUP = RN - DEL
      XLO = DEL
      IF (DABS(XUP-XLO).LT.0.00000001D0) XUP = XLO
      XPREV = 0.
      DO 20 I=1,MNP
        U(I) = 0.0D0
   20 CONTINUE
      U(K+1) = 1.0D0
      IMIN = -K
   30 X = AMIN1(REAL(XUP), REAL(XLO))
      IF (X.GT.0.999999D0) X = 1.0D0
      DX = X - XPREV
      JMIN = CEIL((X-EPS)*FN-.00000001)
      IF (DABS(DFLOAT(JMIN)-(X-EPS)*FN).LT..00000001)
     * JMIN = JMIN + 1
      JMAX = (X+EPS)*FN + 0.00000001D0
      IF (DABS(DFLOAT(JMAX)-(X+EPS)*FN).LT..00000001)
     * JMAX = JMAX - 1
      JMAX = JMAX - JMIN + 1
      CALL PRFAC
      DO 60 J=1,MNP
        SUM = 0.
        IF (J.GT.JMAX) GO TO 50
        I = 1
   40   IP = J - I + 1 + JMIN - IMIN
        SUM = SUM + U(I)*PF(L,IP)
        I = I + 1
        IF ((IMIN+I).LE.(JMIN+J)) GO TO 40
   50   V(J) = SUM
   60 CONTINUE
      DO 70 I=1,MNP
        U(I) = V(I)
   70 CONTINUE
      IMIN = JMIN
      XPREV = X
      IF (X.EQ.XUP) XUP = XUP + RN
      IF (X.EQ.XLO) XLO = XLO + RN
      IF (X.LT.1.) GO TO 30
      DO 80 I = 1, N
        U(K+1) = U(K+1)*DFLOAT(I)
   80 CONTINUE
      FPKS = U(K+1)
      RETURN
      END

      
C ##############################################################################

