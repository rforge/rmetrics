
C ------------------------------------------------------------------------------     
C NLMINB: GARCH PARAMETER ESTIMATION:


      SUBROUTINE GARCHFIT2(NN, YY, ZZ, HH, NF, X, XL, XU, DPARM,  
     >  MDIST, IPAR, RPAR, MYPAR, F)
     
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION F, CMAX, GMAX
      
      DOUBLE PRECISION YY(NN), ZZ(NN), HH(NN)
      DOUBLE PRECISION Y(99999), Z(99999), H(99999)
      DOUBLE PRECISION CF(200), CL(200), CU(200), RA(4000), RPAR(7)
      DOUBLE PRECISION X(NF), XL(NF), XU(NF), DPARM(3) 
      DOUBLE PRECISION XDELTA, XSKEW, XSHAPE, FXLLH
      
      DOUBLE PRECISION B(2, NF)
      
      EXTERNAL GARCHLLH2
      
      INTEGER IA(200),IC(200),IX(NF),IPAR(7), MYPAR(11)
      INTEGER I, IERR, IEXT, ITERM, ITIME, NB, NC, NF 
      INTEGER NADD, NDEC, NFG, NFH, NFV, NIT, NREM, NRES
      
      INTEGER MXFCAL, P
      
C COMMON BLOCKS:      
      
      COMMON /STATSQP/ NRES, NDEC, NREM, NADD, NIT, NFV, NFG, NFH      
      COMMON /DATA1/ Y, Z, H, N     
      COMMON /DATA2/ INCMEAN, NR, NS, NP, NQ, INITREC, NORM
      COMMON /DATA3/ INCDELTA, LEVERAGE, NDIST, INCSKEW, INCSHAPE  
      COMMON /DATA4/ XDELTA, XSKEW, XSHAPE
      COMMON /DATA5/ FXLLH  
      
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

C     SQP:
C     CALL PSQPN(NF, 1, 0, X, IX, XL, XU, CF, IC, CL, CU, IA, RA,                  
C     >     IPAR, RPAR, F, CMAX, GMAX, ITERM) 
    
C     CALL DSMNFB(P, X, B, CALCF, MXFCAL, ACC) 
C       ** SIMPLIED VERSION OF DMNF
C       INPUT PARAMETERS
C           P      NUMBER OF UNKNOWNS
C           X      APPROXIMATE SOLUTION
C           B      FIRST ROW OF B GIVES LOWER BOUNDS ON X AND SECOND 
C                  GIVES UPPER BOUNDS
C           CALCF  SUBROUTINE TO EVALUATE FUNCTION
C           MXFCAL MAXIMUM NUMBER OF PERMITTED FUNCTION EVALUATIONS
C           ACC    ACCURACY IN X
C       OUTPUT PARAMETERS
C           X      SOLUTION      

C SET UP THE BOUND ARRAY
C R1MACH(2) CONTAINS THE LARGEST NUMBER IN THE MACHINE
      DO I=1, NF
        B(1,I) = XL(I)
        B(2,I) = XU(I)
      ENDDO
        
      MXFCAL = 2000
      ACC = 1.0D-10
      FXLLH = 1d99
      
      CALL DSMNFB(NF, X, B, GARCHLLH2, MXFCAL, ACC)
      F = FXLLH
      
      CALL DBLEPR("LLH done ...", -1, FXLLH, 1)
      CALL DBLEPR("With X:", -1, X, NF)   

      DO I = 1, NN
        YY(I) = Y(I)
        ZZ(I) = Z(I)
        HH(I) = H(I)
      END DO 
       
      RETURN
      END   
      
      
C ------------------------------------------------------------------------------
C GARCH LOG-LIKELIHOOD FUNCTION:

C     SQP        CALCF == GARCHLLH(NF, X, F) 
C     NLMINB     CALCF(P, X, NF, F, UI, UR, UF)
      
      SUBROUTINE GARCHLLH2(P, X, NF, F) 
      
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
      INTEGER P, NF
      
      DOUBLE PRECISION Y(99999), H(99999), Z(99999)
      DOUBLE PRECISION X(N), DIST, LLH, F, MEAN, DD
      DOUBLE PRECISION XDELTA, FXLLH
      
      COMMON /DATA1/ Y, Z, H, N     
      COMMON /DATA2/ INCMEAN, NR, NS, NP, NQ, INITREC, NORM
      COMMON /DATA3/ INCDELTA, LEVERAGE, NDIST, INCSKEW, INCSHAPE  
      COMMON /DATA4/ XDELTA, XSKEW, XSHAPE 
      COMMON /DATA5/ FXLLH 
      
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
      
      IF (F.LT.FXLLH) THEN
        FXLLH = F
        CALL DBLEPR("LLH Final Value:", -1, F, 1)
        CALL DBLEPR("With X:", -1, X, P)   
      END IF
      
      RETURN
      END  

      
C ------------------------------------------------------------------------------
C DENSITIES:
C   THEY ARE DEFINED IN GARCHModellingA.f ...

C     DOUBLE PRECISION FUNCTION DNORM(X)
C     DOUBLE PRECISION FUNCTION DSNORM(X, XI)
C     DOUBLE PRECISION FUNCTION DGED(X, NU) 
C     DOUBLE PRECISION FUNCTION DSGED(X, NU, XI) 
C     DOUBLE PRECISION FUNCTION DT(X, NU) 
C     DOUBLE PRECISION FUNCTION DSTD(X, NU) 
C     DOUBLE PRECISION FUNCTION DSSTD(X, NU, XI) 

C     DOUBLE PRECISION FUNCTION DIST(Z, HH, SKEW, SHAPE, NDIST) 


C ==============================================================================


C$TEST NTLE
C TO RUN AS A MAIN PROGRAM REMOVE NEXT LINE
      SUBROUTINE NTLE (Z)
      
        REAL*8 Z
        REAL*8 ZZ
        COMMON /DATA/ ZZ

        INTEGER N
        EXTERNAL ROSN
        REAL X(2), B(2,2)
        N = 2
        
        ZZ = Z
        
C INITIALIZE X
        X(1) = -1.2
        X(2) = 1.0
        
C SET UP THE BOUND ARRAY
C R1MACH(2) CONTAINS THE LARGEST NUMBER IN THE MACHINE
        B(1,1) = -R1MACH(2)
        B(2,1) = 0.5
        B(1,2) = 0.0
        B(2,2) = 1.0
C
C SOLVE THE PROBLEM
C
        CALL DSMNFB(P, X, B, CALCF, MXFCAL, ACC )
        CALL SMNFB (N, X, B, ROSN,  100,    1.E-4)
        
C       PRINT RESULTS ON STANDARD OUTPUT UNIT
        IWRITE=I1MACH(2)
        
        WRITE(IWRITE,10)(X(I),I=1,N)
 10     FORMAT(10H SOLUTION-,5E15.5)
 
        F = 0
        call dblepr("LLH final Value:", -1, F, 0)
        call dblepr("With X:", -1, X, N)
 
        Z = 2*Z
        
        RETURN
        END
        
        
        SUBROUTINE ROSN(N, X, NF, F)
C THIS SUBROUTINE COMPUTES THE  FUNCTION
        INTEGER N, NF
        REAL X(N), F
        REAL*8 ZZ
        COMMON /DATA/ ZZ
        
        call dblepr("Got ZZ:", -1, ZZ, 1)
        
        F=100.0*(X(2)-X(1)*X(1))**2 + (1.0 - X(1))**2
        
        RETURN
        END
        
    
C ##############################################################################

