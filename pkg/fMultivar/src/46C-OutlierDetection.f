

C fquad.f ######################################################################


      SUBROUTINE FQUAD(D,NR,MULT,FD,EPS1,EPS2,DELTA,XKRIT)
 
C       This subroutine comes from the chapter 9 of:
C         'ON THE THEORY AND APPLICATION OF THE GENERAL LINEAR MODEL' DE
C         J. KOERTS ET A.P.J. ABRAHAMSE, ROTTERDAM UNIVERSITY PRESS, 1969.
 
C       PURPOSE : FIND THE DISTRIBUTION FUNCTION F(X) = P(Q<=X) OF 
C                 QUADRATIC FORMS IN NORMAL VARIABLES

C	Note: We suggest EPS1 = EPS2 = .0001
 
C     DESCRIPTION OF PARAMETERS
 
C       D     = A VECTOR OF LENGTH NR CONTAINING THE NON-ZERO LATENT ROOTS
C       NR    = NUMBER OF NON-ZERO LATENT ROOTS
C       MULT  = A VECTOR OF LENGTH NR CONTAINING THE ORDERS OF MULTIPLICITY
C               OF THE CORRESPONDING ROOTS
C       FD    = VALUE OF THE DISTRIBUTION FUNCTION
C       EPS1  = DESIRED TRUNCATION ERROR TU
C       EPS2  = DESIRED ACCURACY OF NUMERICAL INTEGRATION
C       DELTA = A VECTOR OF LENGTH NR CONTAINING THE NON-CENTRALITY PARAMETERS
C       XKRIT = VALUE OF X
 
C     METHOD : THE INTEGRAL DERIVED BY IMHOF IS USED AND NUMERICALLY 
C              INTEGRATED BY SIMPSON'S RULE
 
C     REFERENCE : 'COMPUTING THE DISTRIBUTION OF QUADRATIC FORMS IN
C                  NORMAL VARIABLES' BY J.P. IMHOF
C                  BIOMETRIKA (1961), 48, 3 AND 4, P. 419
 
      DIMENSION D(NR),DELTA(NR),MULT(NR)
 
      XK=0.
      SLAM=0.
      SDELT=0.
      DO 6 I=1,NR
        XK=XK+0.5*MULT(I)
        SLAM=SLAM+0.5*MULT(I)*ALOG(ABS(D(I)))
        SDELT=SDELT+0.5*DELTA(I)
6     CONTINUE
C         COMPUTATION OF THE UPPER-BOUND OF THE INTEGRAL GIVEN THE 
C         TRUNCATION ERROR
      UB=EXP(-(ALOG(EPS1*XK)+1.14472989+(SLAM+SDELT))/XK)
7     SUM=0.
      DO 8 I=1,NR
        DD=(D(I)*UB)**2
        SUM=SUM+DELTA(I)*DD/(1.+DD)
8     CONTINUE
      SUM=0.5*SUM
      TU=EXP(1.14472989+ALOG(XK)+(SLAM+SUM)+XK*ALOG(UB))
      IF(1./TU-EPS1) 20,9,9
9     UB=UB+5./XK
      GOTO 7
C         COMPUTATION OF THE INTEGRAND GIVEN THE VALUE OF U
10    IF(U) 15,15,11
11    TETA=0.
      SUM=0.
      RHO=1.
      DO 14 I=1,NR
        C1=D(I)*U
        C2=1.+C1**2
        TETA=TETA+MULT(I)*ATAN(C1)+DELTA(I)*C1/C2
        SUM=SUM+DELTA(I)*C1**2/C2
        RHO=RHO*(C2**(0.25*MULT(I)))
14    CONTINUE
      TETA=0.5*(TETA-XKRIT*U)
      RHO=RHO*EXP(0.5*SUM)
      FBL=SIN(TETA)/(RHO*U)
      GOTO 18
15    FBL=0.
      DO 16 I=1,NR
16    FBL=FBL+D(I)*(MULT(I)+DELTA(I))
      FBL=0.5*(FBL-XKRIT)
18    GOTO (21,22,23,24,25), KSKIP
C         EVALUATION OF THE INTEGRAL BY SIMPSON'S RULE
20    RANGE=UB
      MH=1
      U=RANGE*0.5
      KSKIP=1
      GOTO 10
21    SUMK=FBL*RANGE*2./3.
      U=0.
      KSKIP=2
      GOTO 10
22    VINT2=SUMK+FBL*RANGE/6.
      U=UB
      KSKIP=3
      GOTO 10
23    VINT2=VINT2+FBL*RANGE/6.
      FD=0.5-.318309886*VINT2
      DO 28 NIT=1,14
        FD1=FD
        VINT2=(VINT2-SUMK*0.5)*0.5
        MH=2*MH
        STEP=RANGE/MH
        U=STEP*0.5
        KSKIP=4
        GOTO 10
24      SUMK=FBL
c		Modification of the original source for best results on
c		many platforms...
c        DO 25 K=2,MH
	k=1
2525	k=k+1
          U=U+STEP
          KSKIP=5
          GOTO 10
25      SUMK=SUMK+FBL
c               Modification of the original source for best results on
c               many platforms...
	if(k.lt.mh) goto 2525
        SUMK=SUMK*STEP*2./3.
        VINT2=VINT2+SUMK
        FD=0.5-.318309886*VINT2
        IF(NIT-3) 28,28,27
27    IF(ABS(FD1-FD)-EPS2) 29,28,28
28    CONTINUE
29    RETURN
      END

      
C robstS.f #####################################################################







      SUBROUTINE ROBSTS(X,NOBS,NVAR,SZ,NCOV,A,B,AINV,COV,SC1,SC2,
     @			istop,messag)
 
      PARAMETER (NFIRST=10)
C
C  ROBUST AFFINE INVARIANT COVARIANCES
C
      DIMENSION X(NOBS,NVAR),SZ(NVAR),A(NCOV),B(NVAR),AINV(NCOV)
     @            ,COV(NCOV),SC1(NVAR),SC2(NCOV),SC(NFIRST)
	dimension istop(2)
	character*6 messag,messg
c **      EXTERNAL WGTF0
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BETA,CW
	common /berr1/istp(2)
	common /berr2/messg
 
      DATA ILOC,MAXIT,NITMON,EPS,TOL/ 1 , 100 , 5 , 0.1 , 0.0001 /
C
C  SET CONSTANTS
C
      MDX=NOBS
      IUCV=1
	istp(1)=-1
	istp(2)=-1
C
C  COMPUTE CONSTANTS FOR WEIGHT FUNCTIONS
C
      CALL CICLOC(EPS,TOL,CW)
      CALL CIA2B2(EPS,NVAR,TOL,MAXIT,A2,B2)
      CALL CIBEAT(A2,B2,NVAR,BETA)
      T2=1.
C
C  COMPUTE ROBUST COVARIANCES USING CYWALG
C
      CALL CIMEDV(X,NOBS,NVAR,NCOV,MDX,NFIRST,ILOC,A,B,SC)
c **      CALL CYWALG(X,A,B,WGTF0,NOBS,NVAR,NCOV,MDX,MAXIT,NITMON,
      CALL CYWALG(X,A,B,NOBS,NVAR,NCOV,MDX,MAXIT,NITMON,
     1            TOL,ILOC,NIT,SZ,SC1,SC2)
      CALL CFRCOV(A,NVAR,NCOV,T2,AINV,COV)
	istop(1)=istp(1)
        if(istp(1).ne.-1) then
		istop(2)=istp(2)
		messag=messg
	endif
      END
      SUBROUTINE CHISQ(KODE,FN,X,PR)
      LOGICAL EVEN,BIGX,ODD,SMLX
      DATA XLSPI,YLSPI/0.572364942925,0.564189583548/
C
      S=1.
      IF (X.LE.0..OR.FN.LT.1.) GOTO 99
      NU=IFIX(FN+.5)
      CALL MACH(3,EXMIN)
      A=0.5*X
      BIGX=.FALSE.
      IF (-A.LE.EXMIN) BIGX=.TRUE.
      SMLX=.NOT.BIGX
      EVEN=(2*(NU/2).EQ.NU)
      ODD=.NOT.EVEN
      IF ((EVEN.OR.NU.GT.2).AND.SMLX) S=EXP(-A)
      IF (BIGX) S=0.
      Y=S
      IF (EVEN) GOTO 10
      SX=-SQRT(X)
      CALL NORM(1,SX,ANS)
      S=2.0*ANS
C
C  NU.LE.2
C
   10 IF (NU.LE.2) GOTO 99
C
C  NU.GT.2
C
      X1=0.5*(FN-1.0)
      IF (EVEN) Z=1.0
      IF (ODD ) Z=0.5
      IF (SMLX) GOTO 30
      IF (EVEN) E=0.0
      IF (ODD ) E=XLSPI
      C=ALOG(A)
   20 E=ALOG(Z)+E
      IF (C*Z-A-E.GT.EXMIN) S=EXP(C*Z-A-E)+S
      Z=Z+1.0
      IF (Z.LE.X1) GOTO 20
      GOTO 99
   30 IF (EVEN) E=1.0
      IF (ODD ) E=YLSPI/SQRT(A)
      C=0.0
   40 E=E*A/Z
      C=C+E
      Z=Z+1.0
      IF (Z.LE.X1) GOTO 40
      S=C*Y+S
   99 PR=S
      IF (KODE.EQ.1) PR=1.0-PR
      RETURN
      END
      SUBROUTINE CIA2B2(EPS,NVAR,TOL,MAXIT,A2,B2)
C
      COMMON/EPSCPR/IP,TL
      EXTERNAL EPSC
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=EPS.GT.0..AND.EPS.LT.1..AND.TOL.GT.0..AND.NVAR.GT.0
     1       .AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'CIA2B2',1)
	return
      endif
C
      IP=NVAR
      XP=FLOAT(IP)
      EP=1./(1.-EPS)
      TL=TOL
      A=1.
      B=1.
   20 FB=EPSC(B)-EP
      IF (FB.LT.0.) GOTO 30
      A=B
      B=B+1.
      GOTO 20
   30 FA=EPSC(A)-EP
      IF (FA.GT.0.) GOTO 40
      B=A
      A=A/2.
      GOTO 30
   40 CALL RGFL(EPSC,EP,A,B,ROOT,TOL,MAXIT,ITERM)
      IF (ITERM.EQ.1) GOTO 50
      CALL MESSGE(101,'CIA2B2',0)
   50 CAP=ROOT
      A2=AMAX1(XP-CAP,0.)
      B2=XP+CAP
      RETURN
      END
      SUBROUTINE CIBEAT(A2,B2,NVAR,BT)
C
      COMMON/EXPUPR/XP,A21,B21
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=A2.GE.0..AND.B2.GT.0..AND.NVAR.GT.0
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'CIBEAT',1)
	return
      endif
C
      A21=A2
      B21=B2
      XP=FLOAT(NVAR)
      TAU2=1.
      BT=EXPU(TAU2)/XP
      RETURN
      END
      SUBROUTINE CICLOC(EPS,TOL,C)
C
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=EPS.GT.0..AND.EPS.LT.1..AND.TOL.GT.0.
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'CICLOC',1)
        return
      endif
C
      CONST=(EPS-2.)/(1.-EPS)/2.
      C=0.
   20 CALL XERF(1,C,EX)
      CALL NORM(1,C,PH)
      F=EX+C*(PH+CONST)
      IF (ABS(F).LT.TOL) GOTO 30
      FP=PH+CONST
      C=C-F/FP
      GOTO 20
   30 RETURN
      END
      SUBROUTINE CIMEDV(X,NOBS,NVAR,NCOV,MDX,NFIRST,ILOC,A,B,SC)
C
C  INITIAL VALUES FOR A AND B
C
      DIMENSION X(MDX,NVAR),A(NCOV),B(NVAR),SC(NFIRST)
      LOGICAL NPRCHK
      DATA TL/1.E-7/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NN.EQ.NCOV.AND.NFIRST.GT.0
     1       .AND.MDX.GE.NOBS.AND.(ILOC.EQ.0.OR.ILOC.EQ.1)
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'CIMEDV',1)
        return
      endif
C
C  COMPUTE INITIAL VALUES FOR A AND B
C
      N0=MIN0(NFIRST,NOBS)
      DO 20 I=1,NCOV
   20 A(I)=0.
      DO 50 J=1,NVAR
      CALL LMDD(X(1,J),SC,N0,1,XME,XMD,XSD)
      IF (ILOC.EQ.1) B(J)=XME
      SQDEV2=SQRT(XSD**2+(XME-B(J))**2)
      JJ=(J*J+J)/2
      IF (SQDEV2.GT.TL) GOTO 45
      CALL MESSGE(301,'CIMEDV',0)
      A(JJ)=9999.
      GOTO 50
   45 A(JJ)=1./SQDEV2
   50 CONTINUE
      RETURN
      END
      SUBROUTINE CFRCOV(A,NVAR,NCOV,TAU2,AINV,COV)
C
      DIMENSION A(NCOV),AINV(NCOV),COV(NCOV)
      LOGICAL NPRCHK
      DATA TL/1.E-7/
C
C  PARAMETER CHECK
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NCOV.EQ.NN.AND.TAU2.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CFRCOV',0)
C
C  COMPUTE INVERSE OF A
C
      DO 20 I=1,NCOV
   20 AINV(I)=A(I)
      CALL MINV(AINV,NVAR,NCOV,TL,ISING)
      IF (ISING.NE.1) GOTO 30
      CALL MESSGE(401,'CFRCOV',0)
      RETURN
   30 CONTINUE
C
C  COMPUTE COVARIANCE MATRIX
C
      CALL MTT2(AINV,COV,NVAR,NCOV)
      CALL SCAL(COV,TAU2,NCOV,1,NCOV)
      RETURN
      END
      SUBROUTINE CYWALG (X,A,B,NOBS,NVAR,NCOV,MDX,
c **  SUBROUTINE CYWALG (X,A,B,EXWGHT,NOBS,NVAR,NCOV,MDX,
     1                   MAXIT,NITMON,TOL,ILOC,NIT,SZ,SC1,SC2)
C
C  REWEIGHTING ALGORITHM FOR ROBUST COVARIANCES
C
      DIMENSION X(MDX,NVAR),SZ(NVAR),A(NCOV),B(NVAR),SC1(NVAR),SC2(NCOV)
      LOGICAL NPRCHK
      DOUBLE PRECISION U,V,W
c **      EXTERNAL EXWGHT
      DATA TL,BLIJ,BUIJ,BLII,BUII/1.E-7,-0.9,0.9,-0.9,0.9/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0
      NPRCHK=NVAR.GT.0.AND.NN.EQ.NCOV.AND.MDX.GE.NOBS
     1      .AND.NOBS.GE.NVAR.AND.TOL.GT.0..AND.MAXIT.GT.0
     2      .AND.(ILOC.EQ.0.OR.ILOC.EQ.1)
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'CYMALG',1)
        return
      endif
      NIT=0
C
C  ITERATIONS
C
   20 NIT=NIT+1
C
C  COMPUTE AVERAGES
C
      IF (ILOC.EQ.0) GOTO 40
      DO 30 I=1,NVAR
   30 SC1(I)=0.
   40 DO 50 I=1,NCOV
   50 SC2(I)=0.
      SW=0.
      SV=0.
      DO 100 L=1,NOBS
      DO 60 J=1,NVAR
   60 SZ(J)=X(L,J)-B(J)
      CALL MLY(A,SZ,NVAR,NCOV,NVAR,1)
      CALL NRM2(SZ,NVAR,1,NVAR,ZNR)
      CALL EXWGHT(ZNR,U,V,W)
      IF (ILOC.EQ.1)SW=SW+W
      SV=SV+V
      IJ=0
      DO 70 I=1,NVAR
      IF (ILOC.EQ.1) SC1(I)=SC1(I)+(X(L,I)-B(I))*W
      DO 70 J=1,I
      IJ=IJ+1
   70 SC2(IJ)=SC2(IJ)+(SZ(I)*U)*SZ(J)
  100 CONTINUE
c **      WRITE(3,*) SW,TL,ILOC,SV
      NPRCHK=(ABS(SW).GT.TL.OR.ILOC.EQ.0)
     1       .AND.ABS(SV).GT.TL
      IF (NPRCHK) GOTO 101
      CALL MESSGE(401,'CYMALG',0)
      RETURN
  101 CONTINUE
C
C  FIND IMPROVEMENT MATRIX (SC2) FOR A AND IMPROVEMENT
C  VECTOR (SC1) FOR B    TRUNCATE IF NECESSARY; FIND MAXIMUM IMPROVEMENT
C
      BMAX=0.
      SMAX=0.
      IJ=0
      DO 130 I=1,NVAR
      IF (ILOC.EQ.0) GOTO 105
      SC1(I)=SC1(I)/SW
      BMAX=AMAX1(BMAX,ABS(SC1(I)))
  105 IF (I.EQ.1) GOTO 120
      I1=I-1
      DO 110 J=1,I1
      IJ=IJ+1
      SC2(IJ)=-AMIN1(AMAX1(SC2(IJ)/SV,BLIJ),BUIJ)
  110 SMAX=AMAX1(SMAX,ABS(SC2(IJ)))
  120 IJ=IJ+1
      C=-AMIN1(AMAX1((SC2(IJ)/SV-1.)/2.,BLII),BUII)
      SMAX=AMAX1(SMAX,ABS(C))
  130 SC2(IJ)=C+1.
C
C  FIND NEW TRANSFORMATION MATRIX A AND NEW LOCATION VECTOR B
C
      IF (ILOC.EQ.0) GOTO 145
      DO 140 I=1,NVAR
  140 B(I)=B(I)+SC1(I)
  145 CALL MTT3(A,SC2,SC2,NVAR,NCOV)
      DO 150 I=1,NCOV
  150 A(I)=SC2(I)
C
C  ITERATION MONITORING
C
      IF (NITMON.LE.0) GOTO 135
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITC(NIT,NVAR,NCOV,B,A,BMAX,SMAX)
  135 CONTINUE
C
C  STOP ITERATIONS IF GIVEN TOLERANCE OR MAXIMUM
C  NUMBER OF ITERATIONS IS REACHED
C
      IF ((SMAX.GE.TOL.OR.BMAX.GE.TOL).AND.NIT.LT.MAXIT) GOTO 20
      RETURN
      END
      FUNCTION DERFC(Y)                                                 
      DOUBLE PRECISION   DERFC,Y                                        
      DIMENSION          P(5),Q(4),P1(9),Q1(8),P2(6),Q2(5)              
      DOUBLE PRECISION   P,Q,P1,Q1,P2,Q2,XMIN,XLARGE,SQRPI,X,           
     *                   RES,XSQ,XNUM,XDEN,XI,XBIG                      
      INTEGER            ISW,I                                          
      DATA               P(1)/113.8641541510502D0/,                     
     *                   P(2)/377.4852376853020D0/,                     
     *                   P(3)/3209.377589138469D0/,                     
     *                   P(4)/.1857777061846032D0/,                     
     *                   P(5)/3.161123743870566D0/                      
      DATA               Q(1)/244.0246379344442D0/,                     
     *                   Q(2)/1282.616526077372D0/,                     
     *                   Q(3)/2844.236833439171D0/,                     
     *                   Q(4)/23.60129095234412D0/                      
      DATA               P1(1)/8.883149794388376D0/,                    
     *                   P1(2)/66.11919063714163D0/,                    
     *                   P1(3)/298.6351381974001D0/,                    
     *                   P1(4)/881.9522212417691D0/,                    
     *                   P1(5)/1712.047612634071D0/,                    
     *                   P1(6)/2051.078377826071D0/,                    
     *                   P1(7)/1230.339354797997D0/,                    
     *                   P1(8)/2.153115354744038D-8/,                   
     *                   P1(9)/.5641884969886701D0/                     
      DATA               Q1(1)/117.6939508913125D0/,                    
     *                   Q1(2)/537.1811018620099D0/,                    
     *                   Q1(3)/1621.389574566690D0/,                    
     *                   Q1(4)/3290.799235733460D0/,                    
     *                   Q1(5)/4362.619090143247D0/,                    
     *                   Q1(6)/3439.367674143722D0/,                    
     *                   Q1(7)/1230.339354803749D0/,                    
     *                   Q1(8)/15.74492611070983D0/                     
      DATA               P2(1)/-3.603448999498044D-01/,                 
     *                   P2(2)/-1.257817261112292D-01/,                 
     *                   P2(3)/-1.608378514874228D-02/,                 
     *                   P2(4)/-6.587491615298378D-04/,                 
     *                   P2(5)/-1.631538713730210D-02/,                 
     *                   P2(6)/-3.053266349612323D-01/                  
      DATA               Q2(1)/1.872952849923460D0/,                    
     *                   Q2(2)/5.279051029514284D-01/,                  
     *                   Q2(3)/6.051834131244132D-02/,                  
     *                   Q2(4)/2.335204976268692D-03/,                  
     *                   Q2(5)/2.568520192289822D0/                     
      DATA               XMIN/1.0D-7/,XLARGE/6.375D0/                   
      DATA               XBIG/13.3D0/                                   
      DATA               SQRPI/.5641895835477563D0/                     
      X = Y                                                             
      ISW = 1                                                           
      IF (X.GE.0.0D0) GO TO 5                                           
      ISW = -1                                                          
      X = -X                                                            
    5 IF (X.LT..477D0) GO TO 10                                         
      IF (X.LE.4.0D0) GO TO 30                                          
      IF (ISW .GT. 0) GO TO 40                                          
      IF (X.LT.XLARGE) GO TO 45                                         
      RES = 2.0D0                                                       
      GO TO 70                                                          
   10 IF (X.LT.XMIN) GO TO 20                                           
      XSQ = X*X                                                         
      XNUM = P(4)*XSQ+P(5)                                              
      XDEN = XSQ+Q(4)                                                   
      DO 15 I = 1,3                                                     
         XNUM = XNUM*XSQ+P(I)                                           
         XDEN = XDEN*XSQ+Q(I)                                           
   15 CONTINUE                                                          
      RES = X*XNUM/XDEN                                                 
      GO TO 25                                                          
   20 RES = X*P(3)/Q(3)                                                 
   25 IF (ISW.EQ.-1) RES = -RES                                         
      RES = 1.0D0-RES                                                   
      GO TO 70                                                          
   30 XSQ = X*X                                                         
      XNUM = P1(8)*X+P1(9)                                              
      XDEN = X+Q1(8)                                                    
      DO 35 I=1,7                                                       
         XNUM = XNUM*X+P1(I)                                            
         XDEN = XDEN*X+Q1(I)                                            
   35 CONTINUE                                                          
      RES = XNUM/XDEN                                                   
      GO TO 60                                                          
   40 IF (X.GT.XBIG) GO TO 65                                           
   45 XSQ = X*X                                                         
      XI = 1.0D0/XSQ                                                    
      XNUM= P2(5)*XI+P2(6)                                              
      XDEN = XI+Q2(5)                                                   
      DO 50 I = 1,4                                                     
         XNUM = XNUM*XI+P2(I)                                           
         XDEN = XDEN*XI+Q2(I)                                           
   50 CONTINUE                                                          
      RES = (SQRPI+XI*XNUM/XDEN)/X                                      
   60 RES = RES*DEXP(-XSQ)                                              
      IF (ISW.EQ.-1) RES = 2.0D0-RES                                    
      GO TO 70                                                          
   65 RES = 0.0D0                                                       
   70 DERFC = RES                                                       
      RETURN                                                            
      END                                                               
      FUNCTION EPSC(CAP)
C
C  PURPOSE
C  -------
C  AUXILIARY SUBROUTINE FOR CIA2B2.
C
      COMMON/EPSCPR/IP,TL
C
C  IF CAP=0 SET EPSC EQUAL A POSITIVE NUMBER
C
      IF (CAP.GT.0.) GOTO 10
      EPSC=1000.
      RETURN
   10 CONTINUE
C
C  COMPUTE AUXILIARY QUANTITIES
C
      XP=FLOAT(IP)
      A2=AMAX1(XP-CAP,0.)
      B2=XP+CAP
      A=SQRT(A2)
      B=SQRT(B2)
      CALL CHISQ(1,XP,A2,PA)
      CALL CHISQ(1,XP,B2,PB)
      CALL NLGM(IP,XLGM)
      XLCP=(1.-XP/2.)*ALOG(2.)-XLGM
C
C  COMPUTE INTEGRAL PARTS AND EPSC
C
      XI1=0.
      XI3=0.
      XI2=PB-PA
      IF (A.GT.0.) XI1=EXP(-A2/2.+XP*ALOG(A)-ALOG(XP-A2)+XLCP)
      IF (XI2.LT.1.-TL)
     1   XI3=EXP(-B2/2.+XP*ALOG(B)-ALOG(B2-XP)+XLCP)
      EPSC=XI1+XI2+XI3
      RETURN
      END
      FUNCTION EXPU(TAU2)
C
C  PURPOSE
C  -------
C  COMPUTATION OF THE EXPECTED VALUE OF U(TAU*NORM(X))*(NORM(TAU*X)**2)
C  WHERE X IS A STANDARD P-VARIATE NORMAL VECTOR AND U IS THE
C  HUBER WEIGHT FUNCTION COMPUTED BY THE FUNCTION HBWT.
C
      COMMON/EXPUPR/XP,A21,B21
      A2=A21
      B2=B21
      IF (TAU2.GT.0.) GOTO 10
      EXPU=A2
      RETURN
   10 AT=A2/TAU2
      BT=B2/TAU2
      CALL CHISQ(1,XP,AT,P1)
      CALL CHISQ(1,XP,BT,P2)
      CALL CHISQ(1,XP+2.,BT,P3)
      CALL CHISQ(1,XP+2.,AT,P4)
      EXPU=A2*P1+B2*(1.-P2)+TAU2*XP*(P3-P4)
      RETURN
      END
      SUBROUTINE LMDD(X,Y,N,ISORT,XME,XMD,XSD)
C
      DIMENSION X(N),Y(N)
C
      KM=(N+1)/2
      DO 20 I=1,N
   20 Y(I)=X(I)
      IF (ISORT.NE.0) CALL SRT1(Y,N,1,N)
      XME=Y(KM)
      IF (KM*2.EQ.N) XME=(XME+Y(KM+1))/2.
      K=0
      K1=KM
      K2=KM
      X1=0.
      X2=0.
   30 IF (K.GE.KM) GOTO 50
      K=K+1
      IF (X1.GT.X2) GOTO 40
      K1=K1-1
      IF (K1.EQ.0) GOTO 50
      X1=XME-Y(K1)
      GOTO 30
   40 K2=K2+1
      IF (K2.GT.N) GOTO 50
      X2=Y(K2)-XME
      GOTO 30
   50 XMD=AMIN1(X1,X2)
      XSD=XMD/.6745
      RETURN
      END
      SUBROUTINE MACH(I,X)
C
C  MACHINE PARAMETERS
C
      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
     1     /2.,5.960465E-8,-88.4,0.294E-38,-88.72241,1.7E38,1.0E-7/
C
      IF (I.EQ.1) X=RADIX
      IF (I.EQ.2) X=PREC
      IF (I.EQ.3) X=EXMIN
      IF (I.EQ.4) X=XLGMN
      IF (I.EQ.5) X=YLGMN
      IF (I.EQ.6) X=XBIG
      IF (I.EQ.7) X=EPMACH
      RETURN
      END
      SUBROUTINE MDNORD (Y,P)                                            
      DOUBLE PRECISION   P,Y                                             
      DOUBLE PRECISION   DERFC,SQR1D2                                    
      DATA               SQR1D2/.7071067811865475D0/                     
      P = .5D0 * DERFC(-Y*SQR1D2)                                        
      RETURN                                                             
      END                                                                
      SUBROUTINE MESSGE(NUMBER,ITEXT,ISTOP)

c	Cette sous-routine a ete adaptee pour etre appelee dans S

      CHARACTER *6 ITEXT,messg

	common /berr1/istp(2)
	common /berr2/messg

c **      WRITE(*,9000)NUMBER,ITEXT
 9000 FORMAT(10H *********,10HMESSAGE N.,I4,4H IN ,A6)
c **      IF (ISTOP.EQ.1) STOP
	istp(1)=number
	istp(2)=istop
	messg=itext
      RETURN
      END
      SUBROUTINE MINV(R,N,NN,TAU,ISING)
C
      DIMENSION R(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2).AND.TAU.GE.0.
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'MINV  ',1)
        return
      endif
C
      DZERO=0.D0
      ISING=0
      I1=0
      DO 10 I=1,N
      I1=I1+I
c **      WRITE(3,*) ISING,I1,R(I1),TAU
      IF (ABS(R(I1)).LE.TAU) GOTO 900
   10 R(I1)=1./R(I1)
      IF (N.EQ.1) RETURN
      I1=0
      NM1=N-1
      DO 40 I=1,NM1
      I1=I1+I
      J1=I1+I
      IP1=I+1
      DO 30 J=IP1,N
      SM=DZERO
      IL=I1
      LJ=J1
      JM1=J-1
      DO 20 L=I,JM1
      SM=SM+R(IL)*DBLE(R(LJ))
      LJ=LJ+1
   20 IL=IL+L
      R(J1)=-R(LJ)*SM
   30 J1=J1+J
   40 CONTINUE
      RETURN
  900 ISING=1
      RETURN
      END
      SUBROUTINE MLY(A,Y,N,NN,NY,IYE)
C
      DIMENSION A(NN),Y(NY)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM,DZERO
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
     1       .AND.IYE.GE.0.AND.(NY.GT.IYE.OR.NY.EQ.1)
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'MLY   ',1)
        return
      endif
C
      DZERO=0.D0
      IA=NN
      IY1=N*IYE+1
      DO 20 J1=1,N
      J=N-J1+1
      IY1=IY1-IYE
      IY=IY1
      SM=DZERO
      DO 10 I=1,J
      SM=SM+A(IA)*DBLE(Y(IY))
      IA=IA-1
   10 IY=IY-IYE
   20 Y(IY1)=SM
      RETURN
      END
      SUBROUTINE MONITC(NIT,NVAR,NCOV,B,A,TOLB,TOLA)
      DIMENSION B(NVAR),A(NCOV)
      DATA NEXT,INIT/0,0/
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
c **      IF (NEXT.EQ.0) WRITE(3,1000)
      NEXT=NIT+INIT
c **      WRITE(3,1001) NIT,B(1),A(1)
      J2=1
      DO 100 I=2,NVAR
        J1=J2+1
        J2=J2+I
c **        WRITE(3,1002) B(I),(A(N),N=J1,J2)
  100 CONTINUE
c **      WRITE(3,1003) TOLB,TOLA
      RETURN
 1000 FORMAT(//,14X,46H* * *  I T E R A T I O N   M O N I T O R I N G,
     +7H  * * *,//,4H NIT,8X,1HB,13X,1HA,/)
 1001 FORMAT(I4,2(3X,E11.5))
 1002 FORMAT(7X,E11.5,(T22,E11.5,1X,E11.5,1X,E11.5,1X,E11.5,1X,E11.5))
 1003 FORMAT
     +(/,4X,2(3X,E11.5),1X,38H( CONVERGENCE CRITERIONS FOR B AND A ),/)
      END
      SUBROUTINE MTT2(A,B,N,NN)
C
      DIMENSION A(NN),B(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'MTT2  ',1)
        return
      endif
C
      DZERO=0.D0
      JJ=NN+N+1
      DO 30 J=N,1,-1
      JJ=JJ-(J+1)
      IAT=JJ+1
      DO 20 I=1,J
      IB=JJ+1-I
      IA=IB+1
      SM=DZERO
      DO 10 L=1,(J-I+1)
      IA=IA-1
      IAT=IAT-1
   10 SM=SM+A(IA)*DBLE(A(IAT))
   20 B(IB)=SM
   30 CONTINUE
      RETURN
      END
      SUBROUTINE MTT3(A,B,C,N,NN)
C
      DIMENSION A(NN),B(NN),C(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'MTT3  ',1)
        return
      endif
C
      DZERO=0.D0
      IC=0
      JJ=0
      DO 30 J=1,N
      II=0
      DO 20 I=1,J
      II=II+I
      IL=II
      IC=IC+1
      SM=DZERO
      DO 10 L=I,J
      JL=JJ+L
      SM=SM+A(IL)*DBLE(B(JL))
   10 IL=IL+L
      C(IC)=SM
   20 CONTINUE
      JJ=JJ+J
   30 CONTINUE
      RETURN
      END
      SUBROUTINE NLGM(N,XL)
C
      DATA PI/3.1415926535898/
C
      XL2=ALOG(2.)
      XL=0.
      K=N-2
   20 IF (K.LE.1) GOTO 30
      XL=XL+ALOG(FLOAT(K))-XL2
      K=K-2
      GOTO 20
   30 IF (K.EQ.1) XL=XL+ALOG(SQRT(PI))-XL2
      IF (N.EQ.1) XL=ALOG(SQRT(PI))
      RETURN
      END
      SUBROUTINE NORM(KODE,X,ANS)
C
      DATA SPI/2.506628275/
C
C  PREC IS A MACHINE DEPENDENT PARAMETER SPECIFYING THE SMALLEST POSITIVE
C  REAL NUMBER SUCH THAT (1.0+PREC).GT.1.0
C
      CALL MACH(2,PREC)
      IF (ABS(X).LE.6.) GOTO 30
      IF (X.GT.6.) GOTO 10
      IF (X.LT.-6.) GOTO 20
   10 ANS=1.
      GOTO 900
   20 ANS=0.
      GOTO 900
   30 CONTINUE
      X2=X*X
      A=X*EXP(-X2/2.)/SPI
      Q=1.
      SH=A
   40 S=SH
      Q=Q+2.
      A=A*X2/Q
      IF (ABS(A).LT.PREC) GOTO 50
      SH=S+A
      GOTO 40
   50 ANS=.5+S
  900 IF (KODE.EQ.2) ANS=1.-ANS
      RETURN
      END
      SUBROUTINE NRM2(X,N,INCX,MDX,XNRM)
C
      DIMENSION X(MDX)
      DATA ZERO,ONE/0.0E0,1.0E0/
      DATA CUTLO,CUTHI/4.441E-16,1.304E19/
C
      IF (N.GT.0) GOTO 10
      XNRM=ZERO
      GOTO 300
C
   10 ASSIGN 30 TO NEXT
      SUM=ZERO
      NN=N*INCX
C
C  BEGIN MAIN LOOP
C
      I=1
   20 GOTO NEXT,(30,50,70,110)
   30 IF (ABS(X(I)).GT.CUTLO) GOTO 85
      ASSIGN 50 TO NEXT
      XMAX=ZERO
C
C  PHASE1.  SUM IS ZERO
C
   50 IF (X(I).EQ.ZERO) GOTO 200
      IF (ABS(X(I)).GT.CUTLO) GOTO 85
C
C  PREPARE FOR PHASE 2.
C
      ASSIGN 70 TO NEXT
      GOTO 105
C
C  PREPARE FOR PHASE 4.
C
  100 I=J
      ASSIGN 110 TO NEXT
      SUM=(SUM/X(I))/X(I)
  105 XMAX=ABS(X(I))
      GOTO 115
C
C  PHASE 2.  SUM IS SMALL. SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF (ABS(X(I)).GT.CUTLO) GOTO 75
C
C  COMMON CODE FOR PHASE 2 AND 4.
C  IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF (ABS(X(I)).LE.XMAX) GOTO 115
      SUM=ONE+SUM*(XMAX/X(I))**2
      XMAX=ABS(X(I))
      GOTO 200
C
  115 SUM=SUM+(X(I)/XMAX)**2
      GOTO 200
C
C  PREPARE FOR PHASE 3.
C
   75 SUM=(SUM*XMAX)*XMAX
C
C  SET HITEST=CUTHI/N
C
   85 HITEST=CUTHI/FLOAT(N)
C
C  PHASE3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J=I,NN,INCX
      IF (ABS(X(J)).GE.HITEST) GOTO 100
   95 SUM=SUM+X(J)**2
      XNRM=SQRT(SUM)
      GOTO 300
C
  200 CONTINUE
      I=I+INCX
      IF (I.LE.NN) GOTO 20
C
C  END MAIN LOOP
C
      XNRM=XMAX*SQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE RGFL(F,Y,A,B,ROOT,TOL,M,ITERM)
C
      EXTERNAL F
      LOGICAL NPRCHK
      DATA TL/1.E-7/
C
C  PARAMETER CHECK
C
      NPRCHK=A.LE.B.AND.TOL.GT.0..AND.M.GT.1
      IF (.NOT.NPRCHK) then 
	CALL MESSGE(500,'RGFL  ',1)
        return
      endif
C
C  INITIALIZE
C
      ITR=1
      FA=F(A)-Y
      FB=F(B)-Y
C
C  REGULA FALSI ITERATION
C
   20 IF (ABS(FA-FB).GT.TL) GOTO 30
      CALL MESSGE(401,'RGFL  ',0)
      RETURN
   30 XN=(A*FB-B*FA)/(FB-FA)
      FN=F(XN)-Y
C
C  TEST TO SEE IF MAXIMUM NUMBER OF ITERATIONS HAS BEEN EXECUTED
C
      IF (ITR.GE.M) GOTO 60
C
C  TEST TO SEE IF ROOT HAS BEEN FOUND
C
      IF (ABS(FN).LT.TOL) GOTO 70
      IF (FA*FN.LE.0.) GOTO 40
      A=XN
      FA=FN
      GOTO 50
   40 B=XN
      FB=FN
C
C  INCREMENT ITERATION COUNTER
C
   50 ITR=ITR+1
      GOTO 20
C
   60 ITERM=2
      ROOT=XN
      RETURN
   70 ITERM=1
      ROOT=XN
      RETURN
      END
      SUBROUTINE SCAL(X,SA,N,INCX,MDX)
C
      DIMENSION X(MDX)
C
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1) GOTO 20
C
C  CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX=N*INCX
      DO 10 I=1,NINCX,INCX
      X(I)=SA*X(I)
   10 CONTINUE
      RETURN
C
C  CODE FOR INCREMENT EQUAL TO 1
C
   20 M=MOD(N,5)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      X(I)=SA*X(I)
   30 CONTINUE
      IF (N.LT.5) RETURN
   40 MP1=M+1
      DO 50 I=MP1,N,5
      X(I)=SA*X(I)
      X(I+1)=SA*X(I+1)
      X(I+2)=SA*X(I+2)
      X(I+3)=SA*X(I+3)
      X(I+4)=SA*X(I+4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SRT1(A,N,K1,K2)
C
      DIMENSION A(N)
      LOGICAL NPRCHK
C
      NPRCHK=N.GT.0.AND.K1.GE.1.AND.K2.GE.K1.AND.K2.LE.N
      IF (.NOT.NPRCHK) then
	CALL MESSGE(500,'SRT1  ',1)
        return
      endif
      N1=K2-K1+1
      I=1
   10 I=I+I
      IF (I.LE.N1) GOTO 10
      M=I-1
   20 M=M/2
      IF (M.EQ.0) RETURN
      K=N1-M
      DO 40 J=1,K
      L=J
   50 IF (L.LT.1) GOTO 40
      LPM=L+M
      LPM1=LPM+K1-1
      L1=L+K1-1
      IF (A(LPM1).GE.A(L1)) GOTO 40
      X=A(LPM1)
      A(LPM1)=A(L1)
      A(L1)=X
      L=L-M
      GOTO 50
   40 CONTINUE
      GOTO 20
      END
      SUBROUTINE EXWGHT(X,U,V,W)
c **      SUBROUTINE WGTF0(X,U,V,W)
C
C  PURPOSE
C  -------
C  WEIGHT FUNCTIONS U,V,W
C  FOR AFFINE INVARIANT COVARIANCES
C
      DOUBLE PRECISION U,V,W,X2,Q,Q2,PD,PC,DEXP,DSPI
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      DATA GAM,DSPI/1.E-3,2.50662827463100/
      IUC=IABS(IUCV)
      U=1.D0
      V=DBLE(BT)
      W=1.D0
      IF (IUC.EQ.0) RETURN
      IF (IUC.EQ.1) GOTO 100
      IF (IUC.EQ.2) GOTO 200
      IF (IUC.EQ.3) GOTO 300
      IF (IUC.EQ.4) GOTO 400
  100 Y=X
      IF (X.GT.GAM.OR.A2.EQ.0.) GOTO 110
      CALL MESSGE(201,'WGTF1 ',0)
      X=GAM
  110 X2=DBLE(X)*X
      IF (X2.GT.B2) U=B2/X2
      IF (X2.LT.A2) U=A2/X2
      IF (Y.GT.GAM.OR.Y.LT.CW) GOTO 120
      CALL MESSGE(202,'WGTF1 ',0)
      Y=GAM
      X=GAM
  120 IF (Y.GT.CW) W=CW/DBLE(Y)
      RETURN
  200 IF (X.LE.0.) RETURN
      Q=CHK/DBLE(X)
      CALL MDNORD(Q,PC)
      U=2.D0*PC-1.D0
      RETURN
  300 IF (X.LE.0.) RETURN
      Q=CKW/DBLE(X)
      Q2=Q*Q
      CALL MDNORD(Q,PC)
      PD=DEXP(-Q2/2.D0)/DSPI
      U=Q2+(1.D0-Q2)*(2.D0*PC-1.D0)-2.D0*Q*PD
      RETURN
  400 IF (X.GT.GAM.OR.X.LT.BB) GOTO 410
      CALL MESSGE(203,'WGTF0 ',0)
      X=GAM
  410 IF (X.GT.BB) U=BB/DBLE(X)
      RETURN
      END
      SUBROUTINE XERF(KODE,X,ANS)
C
      DATA SPI/2.506628274631/
C
C  EXMIN IS A MACHINE DEPENDENT PARAMETER SPECIFYING THE LARGEST NEGATIVE
C  REAL VALUE SUCH THAT EXP(EXMIN) CAN BE SUCCESSFULLY EVALUATED WITHOUT
C  UNDERFLOW.
C
      CALL MACH(3,EXMIN)
      X2=-X*X/2.
      ANS=0.
      IF (X2.GE.EXMIN)ANS=EXP(X2)
      IF (KODE.EQ.1)ANS=ANS/SPI
      RETURN
      END
      
      
C ##############################################################################

