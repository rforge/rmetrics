

C ------------------------------------------------------------------------------
C Central Difference Hessian:
C
C Central difference approximations are usually more precise than forward
C   difference approaximations, but they consume more computer time!
C
C
      SUBROUTINE GARCHHESS(NN, YY, ZZ, HH, NF, X, DPARM,
     +  MDIST, MYPAR, EPS, HESS)
C
C  CHOOSE EPS=1.0D-4
C     
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION YY(NN), ZZ(NN), HH(NN)
      DOUBLE PRECISION Y(99999), Z(99999), H(99999)
      DOUBLE PRECISION X(NF), HESS(NF,NF), DPARM(3)
      DOUBLE PRECISION X1(99), X2(99), X3(99), X4(99), DEPS(99)
      
      INTEGER MYPAR(10) 
     
      COMMON /HESS1/ Y, Z, H, N     
      COMMON /HESS2/ INCMEAN, NR, NS, NP, NQ, INITREC
      COMMON /HESS3/ INCDELTA, LEVERAGE
      COMMON /HESS4/ XDELTA, XSKEW, XSHAPE
      COMMON /HESS5/ NDIST, INCSKEW, INCSHAPE
C            
C     SET COMMON BLOCK:
      N = NN
      DO I = 1, NN
         Y(I) = YY(I)
         Z(I) = ZZ(I)
         H(I) = HH(I)
      END DO       
C   
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
C           
      XDELTA = DPARM(1)
      XSKEW  = DPARM(2)
      XSHAPE = DPARM(3)             
C      
      DO I = 1, NF
         DEPS(I) = EPS * X(I)
      END DO
C 
      DO I = 1, NF  
         DO J = 1, NF  
            DO K = 1, NF
               X1(K) = X(K)
               X2(K) = X(K)
               X3(K) = X(K)
               X4(K) = X(K)
            END DO
            X1(I) = X1(I) + DEPS(I)
            X1(J) = X1(J) + DEPS(J)
            X2(I) = X2(I) + DEPS(I)
            X2(J) = X2(J) - DEPS(J)
            X3(I) = X3(I) - DEPS(I)
            X3(J) = X3(J) + DEPS(J)
            X4(I) = X4(I) - DEPS(I)
            X4(J) = X4(J) - DEPS(J)           
            CALL LLH4HESS(NF, X1, F1)            
            CALL LLH4HESS(NF, X2, F2)         
            CALL LLH4HESS(NF, X3, F3)     
            CALL LLH4HESS(NF, X4, F4)           
            HESS(I,J) = (F1-F2-F3+F4)/(4.0D0*DEPS(I)*DEPS(J))
         END DO
      END DO
C     
      RETURN
      END 
C
C      
C ------------------------------------------------------------------------------
C
C     
      SUBROUTINE LLH4HESS(NF, X, F) 
C      
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION Y(99999), H(99999), Z(99999)
      DOUBLE PRECISION X(*), DIST, LLH, F, MEAN, DD
      COMMON /HESS1/ Y, Z, H, N     
      COMMON /HESS2/ INCMEAN, NR, NS, NP, NQ, INITREC
      COMMON /HESS3/ INCDELTA, LEVERAGE
      COMMON /HESS4/ XDELTA, XSKEW, XSHAPE
      COMMON /HESS5/ NDIST, INCSKEW, INCSHAPE
C     
C     VECTOR START POSITIONS: 
      IAR    = INCMEAN + 1
      IMA    = INCMEAN+NR + 1
      IOMEGA = INCMEAN+NR+NS + 1
      IALPHA = INCMEAN+NR+NS+1 + 1
      IBETA  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE) + 1
      IDELTA = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ + 1
      ISKEW  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA + 1
      ISHAPE = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA+INCSKEW + 1
C 
C     INCLUDE MEAN ?    
      IF (INCMEAN.EQ.1) THEN     
          XMEAN = X(1)
      ELSE 
          XMEAN = 0.0D0
      END IF
C      
C     INCLUDE DELTA ?    
      IF (INCDELTA.EQ.1) THEN     
          XDELTA = X(IDELTA)
      END IF
      XINVD = 1.0D0/XDELTA
C      
C     INCLUDE SKEW ?    
      IF (INCSKEW.EQ.1) THEN     
          XSKEW = X(ISKEW)
      END IF
C      
C     INCLUDE SHAPE ?    
      IF (INCSHAPE.EQ.1) THEN     
          XSHAPE = X(ISHAPE)
      END IF
C      
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
C      
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
C
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
C
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
C          
C     COMPUTE LIKELIHOOD:    
      LLH = 0.0D0
      DO I = 1, N
         ZZ = Z(I)
         HH = DABS(H(I))**XINVD
         DD = DLOG( DIST(ZZ, HH, XSKEW, XSHAPE, NDIST) )
         LLH = LLH - DD
      END DO 
      F = LLH
C 
      RETURN
      END    
C
C ------------------------------------------------------------------------------
C Forward DifferenCe Hessian:
C
C
      SUBROUTINE GFDHESS(NN, YY, ZZ, HH, NF, X, DPARM,
     +  MDIST, MYPAR, EPS, HESS, SCL, STPSZ, FNBR)
C 
C     SUBROUTINE HESS(FN, X, N, SCL, STPSZ, FNBR, H)
C       CalCulate SIngle PreCIsIon HESSIan ApproXImatIon usIng Only FunCtIon 
C       Values.
C     DESCRIPTION:
C       CalCulates a Forward dIFFerenCe approXImatIon to the HESSIan usIng  
C       only the values oF a user-supplIed FunCtIon. AlgorIthm taken From  
C       AppendIX A oF "NumerICal Methods For UnConstraIned OptImIzatIon and  
C       NonlInear EquatIons" by J.E. DennIs, Jr. and Robert B. SChnabel, 1983, 
C       p.321-322. 
C     ARGUMENTS:
c       FN -    name of the user-supplied Function. Be sure to declare  
c               external In the calling program!
c       X -     real array dimensioned X(N) which contains the current  
c               parameters of the function at which the Hessian is to be 
c               calculated.
c       N -     Number of parameters. (Integer)
c       ScL -   real array dimensioned SCL(N) which contains the scaling  
c               factors for each parameter. Here 1/SCL(I) is the typical 
c               size of abs(X(I)).
c       STPSZ - real array dimensioned STPSZ(N) which contains the stepsize  
c               For each parameter. (Passed only For storage considerations.)
c       FNBR -  real array dimensioned FNBR(N) which contaIns the function 
c               value of the i'th parameter plus the i'th stepsize. Only For 
c               storage considerations.)
c       H <--   real array dImensioned H(N, N) which contains the calculated 
c               HESSIan values.  
c       EPS     not used here
     
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION YY(NN), ZZ(NN), HH(NN)
      DOUBLE PRECISION Y(99999), Z(99999), H(99999)
      DOUBLE PRECISION X(NF), HESS(NF,NF), DPARM(3)
      DOUBLE PRECISION X1(99), X2(99), X3(99), X4(99), DEPS(99)
      
      INTEGER MYPAR(10) 
     
      COMMON /HESS1/ Y, Z, H, N     
      COMMON /HESS2/ INCMEAN, NR, NS, NP, NQ, INITREC
      COMMON /HESS3/ INCDELTA, LEVERAGE
      COMMON /HESS4/ XDELTA, XSKEW, XSHAPE
      COMMON /HESS5/ NDIST, INCSKEW, INCSHAPE
      
C     Declarations - Changed to DOUBLE PRECISION declarations
      DOUBLE PRECISION SCL(NF), STPSZ(NF), FNBR(NF) 
      DOUBLE PRECISION MACHEP
      DOUBLE PRECISION FC, FNVAL
C            
C     SET COMMON BLOCK:
      N = NN
      DO I = 1, NN
         Y(I) = YY(I)
         Z(I) = ZZ(I)
         H(I) = HH(I)
      END DO       
C   
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
           
      XDELTA = DPARM(1)
      XSKEW  = DPARM(2)
      XSHAPE = DPARM(3)             
      
      MACHEP = R1MACH(4)
C      C = MACHEP**(1.0D0/3.0D0)
      
      C = 1.0D-4
     
      CALL LLH4HESS(NF, X, FNVAL)
      FC = FNVAL
           
C     CALCULATE STEPSIZE AND UPDATED FUNCTION VALUE
      DO I = 1, NF
          STPSZ(I) = MAX(DABS(X(I)), 1.0D0/SCL(I))
          STPSZ(I) = STPSZ(I)*C*DSIGN(1.0D0, X(I))
          TEMPI = X(I)
          X(I) = X(I) + STPSZ(I)
          STPSZ(I) = X(I) - TEMPI
          CALL LLH4HESS(NF, X, FNVAL)
          FNBR(I) = FNVAL
          X(I) = TEMPI
      END DO
      DO I = 1, NF
C         CALCULATE HESSIAN - DIAGONAL ELEMENTS
          TEMPI = X(I)
          X(I) = X(I) + 2.0D0*STPSZ(I)
          CALL LLH4HESS(NF, X, FNVAL)
          FII = FNVAL
          HESS(I,I) = ((FC-FNBR(I))+ (FII-FNBR(I)))/ (STPSZ(I)*STPSZ(I))
          X(I) = TEMPI + STPSZ(I)
          DO J = I + 1, NF
C             CALCULATE OFF-DIAGONAL ELEMENTS
              TEMPJ = X(J)
              X(J) = X(J) + STPSZ(J)
              CALL LLH4HESS(NF, X, FNVAL)
              FIJ = FNVAL
              HESS(I,J)=((FC-FNBR(I))+(FIJ-FNBR(J)))/(STPSZ(I)*STPSZ(J))
              HESS(J,I)=HESS(I,J)
              X(J) = TEMPJ
          END DO
          X(I) = TEMPI
      END DO
   
      RETURN
      END

      
C ------------------------------------------------------------------------------