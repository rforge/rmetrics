      
C ------------------------------------------------------------------------------
C CONDITIONAL DISTRIBUTON:     
    
  
      DOUBLE PRECISION FUNCTION DIST(Z, HH, SKEW, SHAPE, NDIST)
      IMPLICIT NONE
      INTEGER NDIST
      DOUBLE PRECISION Z, HH, SKEW, SHAPE
      DOUBLE PRECISION DNORM, DSNORM, DSTD, DSSTD, DGED, DSGED
      IF (NDIST.EQ.10) THEN
C        NORMAL:
         DIST = DNORM(Z/HH)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.11) THEN
C        SKEW NORMAL:
         DIST = DSNORM(Z/HH, SKEW)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.20) THEN
C        STUDENT-T:
         DIST = DSTD(Z/HH, SHAPE)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.21) THEN
C        SKEW STUDENT-T:
         DIST = DSSTD(Z/HH, SHAPE, SKEW)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.30) THEN
C        GED:
         DIST = DGED(Z/HH, SHAPE)/HH 
         RETURN
      END IF 
      IF (NDIST.EQ.31) THEN
C        SKEW GED:
         DIST = DSGED(Z/HH, SHAPE, SKEW)/HH 
         RETURN
      END IF 
      RETURN
      END
      
C ------------------------------------------------------------------------------
C NORMAL:


      DOUBLE PRECISION FUNCTION DNORM(X)
      IMPLICIT NONE
      DOUBLE PRECISION X
      DOUBLE PRECISION PI, TWO
      PI = 3.141592653589793D0  
      TWO = 2.0D0
      DNORM = DEXP(-X**2/TWO) / DSQRT(TWO*PI)
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C SKEW NORMAL:


      DOUBLE PRECISION FUNCTION DSNORM(X, XI)
      IMPLICIT NONE
      DOUBLE PRECISION X, XI
      DOUBLE PRECISION ONE, TWO, PI, M1, MU, SIGMA
      DOUBLE PRECISION Z, XXI, G
      DOUBLE PRECISION DNORM
      ONE = 1.0D0
      TWO = 2.0D0
      PI = 3.141592653589793D0 
      M1 = TWO/DSQRT(TWO*PI)
      MU = M1*(XI-ONE/XI)
      SIGMA = DSQRT((ONE-M1**2)*(XI**2+ONE/XI**2)+TWO*M1**2-ONE)
      Z = X*SIGMA+MU 
      XXI = XI**SIGN(ONE, Z)
      IF (Z.EQ.0.0D0) THEN
        XXI = XI**0.0D0
      END IF       
      G = TWO/(XI+ONE/XI)
      DSNORM = G*DNORM(Z/XXI)*SIGMA
      RETURN
      END

            
C ------------------------------------------------------------------------------
C GED:


      DOUBLE PRECISION FUNCTION DGED(X, NU) 
      IMPLICIT NONE
      DOUBLE PRECISION X, NU
      DOUBLE PRECISION HALF, ONE, TWO, THREE, LAMBDA, G
      DOUBLE PRECISION DGAM
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
      IMPLICIT NONE
      DOUBLE PRECISION X, NU, XI
      DOUBLE PRECISION HALF, ONE, TWO, THREE, LAMBDA, G, M1, MU
      DOUBLE PRECISION SIGMA, Z, XXI
      DOUBLE PRECISION DGAM, DGED
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
      XXI = XI**SIGN(ONE, Z)
      IF (Z.EQ.0.0D0) THEN
        XXI = XI**0.0D0
      END IF       
      DSGED = (TWO/(XI+ONE/XI))*DGED(Z/XXI, NU)*SIGMA
      RETURN
      END
      
      
C ------------------------------------------------------------------------------
C STUDENT T:


      DOUBLE PRECISION FUNCTION DT(X, NU) 
      IMPLICIT NONE
      DOUBLE PRECISION X, NU
      DOUBLE PRECISION ONE, TWO, PI, A, B
      DOUBLE PRECISION DGAM
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
      IMPLICIT NONE
      DOUBLE PRECISION X, NU
      DOUBLE PRECISION TWO, S
      DOUBLE PRECISION DT
      TWO = 2.0D0
      S = DSQRT(NU/(NU-TWO))
      DSTD = S*DT(X*S,NU)
      RETURN
      END

            
C ------------------------------------------------------------------------------
C STANDARDIZED SKEW STUDENT T:

      
      DOUBLE PRECISION FUNCTION DSSTD(X, NU, XI) 
      IMPLICIT NONE
      DOUBLE PRECISION X, NU, XI
      DOUBLE PRECISION ONE, TWO, A, B, BETA, M1, MU
      DOUBLE PRECISION SIGMA, Z, G, XXI
      DOUBLE PRECISION DSTD, DGAM
      ONE = 1.0D0
      TWO = 2.0D0
      A = ONE/TWO
      B = NU/TWO
      BETA = (DGAM(A)/DGAM(A+B))*DGAM(B)
      M1 = TWO*DSQRT(NU-TWO)/(NU-ONE)/BETA
      MU = M1*(XI-ONE/XI)
      SIGMA = DSQRT((ONE-M1**2)*(XI**2+ONE/XI**2)+TWO*M1**2-ONE)
      Z = X*SIGMA+MU
      XXI = XI**SIGN(ONE, Z)
      IF (Z.EQ.0.0D0) THEN
        XXI = XI**0.0D0
      END IF
      G = TWO/(XI+ONE/XI)
      DSSTD = G*DSTD(Z/XXI,NU)*SIGMA 
      RETURN
      END
 
