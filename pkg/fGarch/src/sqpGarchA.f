
C SQP

C PART 0:    GARCHFIT


C ------------------------------------------------------------------------------     
C PART 0: GARCH PARAMETER ESTIMATION:


      SUBROUTINE GARCHFIT(NN, YY, ZZ, HH, NF, X, XL, XU, DPARM,  
     >  MDIST, IPAR, RPAR, IPRNT, MYPAR, F)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION F, CMAX, GMAX
      DOUBLE PRECISION YY(NN), ZZ(NN), HH(NN)
      DOUBLE PRECISION Y(99999), Z(99999), H(99999)
      DOUBLE PRECISION CF(200), CL(200), CU(200), RA(4000), RPAR(7)
      DOUBLE PRECISION X(NF), XL(NF), XU(NF), DPARM(3) 
      DOUBLE PRECISION XDELTA, XSKEW, XSHAPE
      EXTERNAL PSQPN
      INTEGER IA(200),IC(200),IX(NF),IPAR(7), MYPAR(11)
      INTEGER I, IPRNT, IERR, IEXT, ITERM, ITIME, NB, NC, NF 
      INTEGER NADD, NDEC, NFG, NFH, NFV, NIT, NREM, NRES
      COMMON /STAT/ NRES, NDEC, NREM, NADD, NIT, NFV, NFG, NFH      
      COMMON /DATA1/ Y, Z, H, N     
      COMMON /DATA2/ INCMEAN, NR, NS, NP, NQ, INITREC, NORM
      COMMON /DATA3/ INCDELTA, LEVERAGE, NDIST, INCSKEW, INCSHAPE  
      COMMON /DATA4/ XDELTA, XSKEW, XSHAPE  
      SAVE /STAT/, /DATA1/, /DATA2/, /DATA3/, /DATA4/
      
C     SET COMMON BLOCK:
      DO I = 1, NN
         Y(I) = YY(I)
         Z(I) = ZZ(I)
         H(I) = HH(I)
      END DO       
      N = NN
   
C     MY PARAMETERS: 
      NDIST    = MDIST
      INITREC  = MYPAR(1)
      LEVERAGE = MYPAR(2)
      INCMEAN  = MYPAR(3)
      INCDELTA = MYPAR(4)
      INCSKEW  = MYPAR(5)
      INCSHAPE = MYPAR(6)
      NR = MYPAR(7)
      NS = MYPAR(8)
      NP = MYPAR(9)
      NQ = MYPAR(10)
      NORM = MYPAR(11)
      
C     WHICH TYPE OF BOUNDS?   
      DO I = 1, NF
         IX(I) = 3
      END DO   
                 
      XDELTA = DPARM(1)
      XSKEW  = DPARM(2)
      XSHAPE = DPARM(3)
                 
C     FIND SOLUTION:
      CALL PSQPN(NF, 1, 0, X, IX, XL, XU, CF, IC, CL, CU,
     >     IPAR, RPAR, F, GMAX, CMAX, IPRNT, ITERM) 

      CALL DBLEPR("LLH final Value:", -1, F, 1)
      call DBLEPR("With X:", -1, X, NF)


      RETURN
      END   
      
      
C ------------------------------------------------------------------------------
C GARCH LOG-LIKELIHOOD FUNCTION:


      SUBROUTINE GARCHLLH(NF, X, F) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION Y(99999), H(99999), Z(99999)
      DOUBLE PRECISION X(*), DIST, LLH, F, MEAN, DD
      DOUBLE PRECISION XDELTA 
      COMMON /DATA1/ Y, Z, H, N     
      COMMON /DATA2/ INCMEAN, NR, NS, NP, NQ, INITREC, NORM
      COMMON /DATA3/ INCDELTA, LEVERAGE, NDIST, INCSKEW, INCSHAPE  
      COMMON /DATA4/ XDELTA, XSKEW, XSHAPE 
     
C     VECTOR START POSITIONS: 
      IAR    = INCMEAN + 1
      IMA    = INCMEAN+NR + 1
      IOMEGA = INCMEAN+NR+NS + 1
      IALPHA = INCMEAN+NR+NS+1 + 1
      IBETA  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE) + 1
      IDELTA = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ + 1
      ISHAPE = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA + 1
      ISKEW  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA+INCSHAPE + 1
  
C     INCLUDE MEAN?    
      IF (INCMEAN.EQ.1) THEN     
          XMEAN = X(1)
      ELSE 
          XMEAN = 0.0D0
      END IF
      
C     INCLUDE DELTA ?    
      IF (INCDELTA.EQ.1) THEN     
          XDELTA = X(IDELTA)
      END IF
      XINVD = 1.0D0/XDELTA
      
C     INCLUDE SKEW ?    
      IF (INCSKEW.EQ.1) THEN     
          XSKEW = X(ISKEW)
      END IF
      
C     INCLUDE DELTA ?    
      IF (INCSHAPE.EQ.1) THEN     
          XSHAPE = X(ISHAPE)
      END IF
      
C     POSTION OMEGA:
      XOMEGA = X(IOMEGA)
    
C     ARMA RECURSION:
      DO I = 1, MAX(NR,NS)
         Z(I) = 0.0D0
      END DO      
      DO I = MAX(NR,NS)+1, N
         Z(I) = Y(I) - XMEAN
         NEXT = IAR
         DO IR = 1, NR, 1
            Z(I) = Z(I) - X(NEXT)*Y(I-IR)
            NEXT = NEXT + 1
         END DO
         NEXT = IMA
         DO IR = 1, NS, 1
            Z(I) = Z(I) - X(NEXT)*Z(I-IR)
            NEXT = NEXT + 1
         END DO
      END DO
      
C     COMPUTE (UNLEVERAGED) PERSISTENCE:
      SUMALPHA = 0.0D0
      NEXT = IALPHA
      DO IP = 1, NP, 1
         SUMALPHA = SUMALPHA + X(NEXT)
         NEXT = NEXT + 1
      END DO
      NEXT = IBETA
      SUMBETA = 0.0D0
      DO IP = 1, NQ, 1
         SUMBETA = SUMBETA + X(NEXT)
         NEXT = NEXT + 1
      END DO
      PERSISTENCE = SUMALPHA + SUMBETA
 
C     INITIALZE RECURSION - 1 (FCP) | 2 (TSP) LIKE:
      IF (INITREC.EQ.1) THEN
         VAR = 0.0D0
         DO I = 1, N
            VAR = VAR + Z(I)**2
         END DO
         VAR = VAR/N
      END IF
      IF (INITREC.EQ.2) THEN
         VAR = XOMEGA/(1.0D0-PERSISTENCE)
      END IF

C     ITERATE H:      
      DO I = 1, MAX(NP,NQ)
         H(I) = XOMEGA + PERSISTENCE*VAR  
      END DO  
      IF (LEVERAGE.EQ.1) THEN
          DO I = MAX(NP,NQ)+1, N
             H(I) = XOMEGA 
             NEXT = IALPHA
             DO IP = 1, NP, 1
                ZI = DABS(Z(I-IP))-X(NEXT+NP)*Z(I-IP)
                H(I) = H(I) + X(NEXT)*DABS(ZI)**XDELTA
                NEXT = NEXT + 1
             END DO
             NEXT = IBETA
             DO IQ = 1, NQ, 1
                H(I) = H(I) + X(NEXT)*H(I-IQ)
                NEXT = NEXT + 1
             END DO
          END DO          
      ELSE
          DO I = MAX(NP,NQ)+1, N
             H(I) = XOMEGA 
             NEXT = IALPHA
             DO IP = 1, NP, 1
                H(I) = H(I) + X(NEXT)*DABS(Z(I-IP))**XDELTA
                NEXT = NEXT + 1
             END DO
             NEXT = IBETA
             DO IQ = 1, NQ, 1
                H(I) = H(I) + X(NEXT)*H(I-IQ)
                NEXT = NEXT + 1
             END DO
          END DO    
      END IF
           
C     COMPUTE LIKELIHOOD:    
      LLH = 0.0D0
      DO I = 1, N
         ZZ = Z(I)
         HH = DABS(H(I))**XINVD
         DD = DLOG( DIST(ZZ, HH, XSKEW, XSHAPE, NDIST) )
         LLH = LLH - DD
      END DO 
      F = LLH/NORM
 
      RETURN
      END  

      
C ------------------------------------------------------------------------------
C NORMAL:


      DOUBLE PRECISION FUNCTION DNORM(X)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PI = 3.141592653589793D0  
      TWO = 2.0D0
      DNORM = DEXP(-X**2/TWO) / DSQRT(TWO*PI)
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C SKEW NORMAL:


      DOUBLE PRECISION FUNCTION DSNORM(X, XI)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION M1, MU
      ONE = 1.0D0
      TWO = 2.0D0
      PI = 3.141592653589793D0 
      M1 = TWO/DSQRT(TWO*PI)
      MU = M1*(XI-ONE/XI)
      SIGMA = DSQRT((ONE-M1**2)*(XI**2+ONE/XI**2)+TWO*M1**2-ONE)
      Z = X*SIGMA+MU 
      IF (Z.LT.0.0D0) XI = 1/XI 
      G = TWO/(XI+ONE/XI)
      DSNORM = G*DNORM(Z/XI)*SIGMA
      RETURN
      END

            
C ------------------------------------------------------------------------------
C GED:


      DOUBLE PRECISION FUNCTION DGED(X, NU) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NU, LAMBDA 
      HALF = 0.50D0
      ONE = 1.0D0
      TWO = 2.0D0
      THREE = 3.0D0
      LAMBDA = DSQRT(TWO**(-TWO/NU)*DGAM(ONE/NU)/DGAM(THREE/NU))
      G = NU/(LAMBDA*(TWO**(ONE+ONE/NU))*DGAM(ONE/NU))
      DGED = G*DEXP(-HALF*(DABS(X/LAMBDA))**NU)
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C SKEW GED:


      DOUBLE PRECISION FUNCTION DSGED(X, NU, XI) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NU, LAMBDA, M1, MU
      HALF = 0.50D0
      ONE = 1.0D0
      TWO = 2.0D0
      THREE = 3.0D0   
      LAMBDA = DSQRT(TWO**(-TWO/NU)*DGAM(ONE/NU)/DGAM(THREE/NU))
      G = NU/(LAMBDA*(TWO**(ONE+ONE/NU))*DGAM(ONE/NU))
      M1 = (TWO**(ONE/NU))*LAMBDA*DGAM(TWO/NU)/DGAM(ONE/NU)
      MU = M1*(XI-ONE/XI)
      SIGMA = (ONE-M1**2)*(XI**2+ONE/(XI**2))+TWO*(M1**2)-ONE
      SIGMA = DSQRT(SIGMA)
      Z = X*SIGMA+MU
      IF (Z.LT.0.0D0) XI = 1/XI 
      DSGED = (TWO/(XI+ONE/XI))*DGED(Z/XI, NU)*SIGMA
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C STUDENT T:


      DOUBLE PRECISION FUNCTION DT(X, NU) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NU
      ONE = 1.0D0
      TWO = 2.0D0
      PI = 3.141592653589793D0  
      A = DGAM((NU+ONE)/TWO)/DSQRT(PI*NU)
      B = DGAM(NU/TWO)*(ONE+(X*X)/NU)**((NU+ONE)/TWO)
      DT = A/B
      RETURN
      END
      

C ------------------------------------------------------------------------------
C STANDARDIZED STUDENT T:

    
      DOUBLE PRECISION FUNCTION DSTD(X, NU) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NU
      TWO = 2.0D0
      S = DSQRT(NU/(NU-TWO))
      DSTD = S*DT(X*S,NU)
      RETURN
      END

            
C ------------------------------------------------------------------------------
C STANDARDIZED SKEW STUDENT T:

      
      DOUBLE PRECISION FUNCTION DSSTD(X, NU, XI) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NU, M1, MU
      ONE = 1.0D0
      TWO = 2.0D0
      A = ONE/TWO
      B = NU/TWO
      BETA = (DGAM(A)/DGAM(A+B))*DGAM(B)
      M1 = TWO*DSQRT(NU-TWO)/(NU-ONE)/BETA
      MU = M1*(XI-ONE/XI)
      SIGMA = DSQRT((ONE-M1**2)*(XI**2+ONE/XI**2)+TWO*M1**2-ONE)
      Z = X*SIGMA+MU
      IF (Z.LT.0.0D0) XI = 1/XI 
      G = TWO/(XI+ONE/XI)
      DSSTD = G*DSTD(Z/XI,NU)*SIGMA   
      RETURN
      END
 
      
C ------------------------------------------------------------------------------
C CONDITIONAL DISTRIBUTON:     
    
  
      DOUBLE PRECISION FUNCTION DIST(Z, HH, SKEW, SHAPE, NDIST)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IF (NDIST.EQ.10) THEN
C        NORMAL:
         DIST = DNORM(-Z/HH)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.11) THEN
C        SKEW NORMAL:
         DIST = DSNORM(-Z/HH, SKEW)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.20) THEN
C        STUDENT-T:
         DIST = DSTD(-Z/HH, SHAPE)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.21) THEN
C        SKEW STUDENT-T:
         DIST = DSSTD(-Z/HH, SHAPE, SKEW)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.30) THEN
C        GED:
         DIST = DGED(-Z/HH, SHAPE)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.31) THEN
C        SKEW GED:
         DIST = DSGED(-Z/HH, SHAPE, SKEW)/HH 
         RETURN
      END IF 
      RETURN
      END
    
      
C ------------------------------------------------------------------------------  
C OBJECTIVE FUNCTION:
          
      
      SUBROUTINE OBJ(NF, X, F) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION X(NF)
      CALL GARCHLLH(NF, X, F)
      RETURN
      END       
      
      
C ------------------------------------------------------------------------------  
C DERIVATIVE OF OBJECTIVE FUNCTION:


      SUBROUTINE DOBJ(NF, X, G) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION X(NF), G(NF), XX(99), EPS0, EPS1, EPS
      DOUBLE PRECISION FM1, FM2, FP1, FP2
      EPS0 = 1.0D-3
      DO I = 1, NF
         DO J = 1, NF
            XX(J)= X(J)
         END DO
         EPS = EPS0 * DABS(X(I))
         IF (EPS.EQ.0.0D0) EPS = EPS0    
         XX(I) = X(I) + 2.0D0*EPS
         CALL OBJ(NF, XX, FP2)
         XX(I) = X(I) + EPS
         CALL OBJ(NF, XX, FP1)
         XX(I) = X(I) - EPS
         CALL OBJ(NF, XX, FM1)  
         XX(I) = X(I) - 2.0D0*EPS
         CALL OBJ(NF, XX, FM2)       
         G(I) = (-FP2 + 8.0D0*FP1 - 8.0D0*FM1 + FM2) / (12.0D0*EPS)
      END DO      
      RETURN
      END

      
C ------------------------------------------------------------------------------
C EMPTY FUNCTIONS:


      SUBROUTINE FUN(N, KA, X, FA)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      RETURN
      END
      
      SUBROUTINE DFUN(N, KA, X, GA)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      RETURN
      END     
      
      SUBROUTINE CON(NF, KC, X, FC)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      RETURN
      END
          
      SUBROUTINE DCON(NF, KC, X, GC)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C GAMMA FUNCTION:
             

      DOUBLE PRECISION FUNCTION DGAM(X)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION G(26)
      PI = 3.141592653589793D0
      IF (X.EQ.INT(X)) THEN
         IF (X.GT.0.0D0) THEN
            DGAM = 1.0D0
            M1 = X-1
            DO K = 2, M1
               DGAM = DGAM*K
            END DO
          ELSE
            DGAM = 1.0D+300
          END IF
      ELSE
          IF (DABS(X).GT.1.0D0) THEN
             Z = DABS(X)
             M = INT(Z)
             R = 1.0D0
             DO K = 1, M
                R = R*(Z-K)
             END DO
             Z = Z-M
          ELSE
             Z = X
          END IF
          DATA G/1.0D0,0.5772156649015329D0,
     +          -0.6558780715202538D0, -0.420026350340952D-1,
     +          0.1665386113822915D0,-.421977345555443D-1,
     +          -.96219715278770D-2, .72189432466630D-2,
     +          -.11651675918591D-2, -.2152416741149D-3,
     +          .1280502823882D-3, -.201348547807D-4,
     +          -.12504934821D-5, .11330272320D-5,
     +          -.2056338417D-6, .61160950D-8,
     +          .50020075D-8, -.11812746D-8,
     +          .1043427D-9, .77823D-11,
     +          -.36968D-11, .51D-12,
     +          -.206D-13, -.54D-14, .14D-14, .1D-15/
          GR = G(26)
          DO K = 25, 1, -1
             GR = GR*Z+G(K)
          END DO
          DGAM = 1.0D0/(GR*Z)
          IF (DABS(X).GT.1.0D0) THEN
             DGAM = DGAM*R
             IF (X.LT.0.0D0) DGAM = -PI/(X*GA*DSIN(PI*X))
          END IF
      END IF
      RETURN
      END

      
C ------------------------------------------------------------------------------

