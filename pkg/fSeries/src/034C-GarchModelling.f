

C ------------------------------------------------------------------------------
C Hessian:
C CHOOSE E0=1.0D-4


      SUBROUTINE GARCHHESS(NN, YY, ZZ, HH, NF, X, DPARM,
     +  MDIST, MYPAR, E0, HESS)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION YY(NN), ZZ(NN), HH(NN)
      DOUBLE PRECISION Y(99999), Z(99999), H(99999)
      DOUBLE PRECISION X(NF), HESS(NF,NF), DPARM(3)
      DOUBLE PRECISION X1(99), X2(99), X3(99), X4(99), EPS(99)
      INTEGER MYPAR(10)    
      COMMON /HESS1/ Y, Z, H, N     
      COMMON /HESS2/ INCMEAN, NR, NS, NP, NQ, INITREC
      COMMON /HESS3/ INCDELTA, LEVERAGE
      COMMON /HESS4/ XDELTA, XSKEW, XSHAPE
      COMMON /HESS5/ NDIST, INCSKEW, INCSHAPE
            
C     SET COMMON BLOCK:
      N = NN
      DO I = 1, NN
         Y(I) = YY(I)
         Z(I) = ZZ(I)
         H(I) = HH(I)
      END DO       
   
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
           
      XDELTA   = DPARM(1)
      XSKEW    = DPARM(2)
      XSHAPE   = DPARM(3)             
      
      DO I = 1, NF
	     EPS(I) = E0 * X(I)
      END DO
 
      DO I = 1, NF  
         DO J = 1, NF  
            DO K = 1, NF
               X1(K) = X(K)
               X2(K) = X(K)
               X3(K) = X(K)
               X4(K) = X(K)
            END DO
            X1(I) = X1(I) + EPS(I)
            X1(J) = X1(J) + EPS(J)
            X2(I) = X2(I) + EPS(I)
            X2(J) = X2(J) - EPS(J)
            X3(I) = X3(I) - EPS(I)
            X3(J) = X3(J) + EPS(J)
            X4(I) = X4(I) - EPS(I)
            X4(J) = X4(J) - EPS(J)           
            CALL LLH4HESS(NF, X1, F1)            
            CALL LLH4HESS(NF, X2, F2)         
            CALL LLH4HESS(NF, X3, F3)     
            CALL LLH4HESS(NF, X4, F4)           
            HESS(I,J) = (F1-F2-F3+F4)/(4.0D0*EPS(I)*EPS(J))
         END DO
      END DO
     
      RETURN
      END 

            
C ------------------------------------------------------------------------------

     
      SUBROUTINE LLH4HESS(NF, X, F) 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION Y(99999), H(99999), Z(99999)
      DOUBLE PRECISION X(*), DIST, LLH, F, MEAN, DD
      COMMON /HESS1/ Y, Z, H, N     
      COMMON /HESS2/ INCMEAN, NR, NS, NP, NQ, INITREC
      COMMON /HESS3/ INCDELTA, LEVERAGE
      COMMON /HESS4/ XDELTA, XSKEW, XSHAPE
      COMMON /HESS5/ NDIST, INCSKEW, INCSHAPE
     
C     VECTOR START POSITIONS: 
      IAR    = INCMEAN + 1
      IMA    = INCMEAN+NR + 1
      IOMEGA = INCMEAN+NR+NS + 1
      IALPHA = INCMEAN+NR+NS+1 + 1
      IBETA  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE) + 1
      IDELTA = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ + 1
      ISKEW  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA + 1
      ISHAPE = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA+INCSKEW + 1
  
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
      
C     INCLUDE SHAPE ?    
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
      F = LLH
 
      RETURN
      END    

C ------------------------------------------------------------------------------

