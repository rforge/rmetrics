      SUBROUTINE GARCHLLH(N, Y, Z, H, NF, X, DPARM, MDIST, MYPAR, F)

      IMPLICIT NONE

C     DECLARATION OF ARGUMENTS
      INTEGER N
      DOUBLE PRECISION Y(N), Z(N), H(N)
      INTEGER NF
      DOUBLE PRECISION X(NF)
      DOUBLE PRECISION DPARM(3)
      INTEGER MDIST
      INTEGER MYPAR(8)
      DOUBLE PRECISION F

C     LOCAL VARIABLE
      INTEGER INITREC, LEVERAGE, INCMEAN, INCDELTA ,INCSKEW, INCSHAPE
      INTEGER NR, NS, NP, NQ, NORM
      DOUBLE PRECISION XMEAN, XOMEGA, XDELTA, XSKEW, XSHAPE
      INTEGER IAR, IMA, IOMEGA, IALPHA, IBETA, IDELTA, ISKEW, ISHAPE 
      
      INTEGER I, NEXT, IP, IQ, IR
      DOUBLE PRECISION SUMALPHA, SUMBETA, PERSISTENCE
      DOUBLE PRECISION VAR, ZI, ZZ, HH, LLH, DD 
      DOUBLE PRECISION XINVD

C     EXTRENAL FUNC
      DOUBLE PRECISION DLOG, DIST

C     MYPAR
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

C     DPARM
      XDELTA = DPARM(1)
      XSKEW  = DPARM(2)
      XSHAPE = DPARM(3)
      
C     VECTOR START POSITIONS: 
      IAR    = INCMEAN + 1
      IMA    = INCMEAN+NR + 1
      IOMEGA = INCMEAN+NR+NS + 1
      IALPHA = INCMEAN+NR+NS+1 + 1
      IBETA  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE) + 1
      IDELTA = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ + 1
      ISKEW  = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA + 1
      ISHAPE = INCMEAN+NR+NS+1+NP*(1+LEVERAGE)+NQ+INCDELTA+INCSKEW+1

C     TODO 
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
         DD = DLOG( DIST(ZZ, HH, XSKEW, XSHAPE, MDIST) )
         LLH = LLH - DD
      END DO
      F = LLH

      RETURN
      END  
