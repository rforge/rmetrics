
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
