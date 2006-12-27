
CONTENT:

C Kolmogorov Smirnow Finite Sample Probabilities

C###############################################################################

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

