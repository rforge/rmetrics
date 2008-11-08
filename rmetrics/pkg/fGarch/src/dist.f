      
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
 9999    DIST = DSTD(-Z/HH, SHAPE)/HH 
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
 
