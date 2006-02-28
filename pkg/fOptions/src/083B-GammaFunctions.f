
C FROM: http://iris-lee3.ece.uiuc.edu/~jjin/routines/routines.html

C All the programs and subroutines contained in this archive are 
C copyrighted. However, we give permission to the user who downloads 
C these routines to incorporate any of these routines into his or 
C her programs provided that the copyright is acknowledged. 

C Contact Information 
C Email: j-jin1@uiuc.edu 
C Phone: (217) 244-0756 
C Fax: (217) 333-5962 
C Professor Jianming Jin 
C Department of Electrical and Computer Engineering 
C University of Illinois at Urbana-Champaign 
C 461 William L Everitt Laboratory 
C 1406 West Green Street 
C Urbana, IL 61801-2991 


C ******************************************************************************


        SUBROUTINE CGAMA(X,Y,KF,GR,GI)
C
C       =========================================================
C       Purpose: Compute the gamma function â(z) or ln[â(z)]
C                for a complex argument
C       Input :  x  --- Real part of z
C                y  --- Imaginary part of z
C                KF --- Function code
C                       KF=0 for ln[â(z)]
C                       KF=1 for â(z)
C       Output:  GR --- Real part of ln[â(z)] or â(z)
C                GI --- Imaginary part of ln[â(z)] or â(z)
C       ========================================================
C


        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION A(10)
        PI=3.141592653589793D0
        DATA A/8.333333333333333D-02,-2.777777777777778D-03,
     &         7.936507936507937D-04,-5.952380952380952D-04,
     &         8.417508417508418D-04,-1.917526917526918D-03,
     &         6.410256410256410D-03,-2.955065359477124D-02,
     &         1.796443723688307D-01,-1.39243221690590D+00/
     
CC DW
		Y1 = 0.0D0
		X1 = 0.0D0
		NA = 0
CC DW
        IF (Y.EQ.0.0D0.AND.X.EQ.INT(X).AND.X.LE.0.0D0) THEN
           GR=1.0D+300
           GI=0.0D0
           RETURN
        ELSE IF (X.LT.0.0D0) THEN
           X1=X
           Y1=Y
           X=-X
           Y=-Y
        ENDIF
        X0=X
        IF (X.LE.7.0) THEN
           NA=INT(7-X)
           X0=X+NA
        ENDIF
        Z1=DSQRT(X0*X0+Y*Y)
        TH=DATAN(Y/X0)
        GR=(X0-.5D0)*DLOG(Z1)-TH*Y-X0+0.5D0*DLOG(2.0D0*PI)
        GI=TH*(X0-0.5D0)+Y*DLOG(Z1)-Y
        DO 10 K=1,10
           T=Z1**(1-2*K)
           GR=GR+A(K)*T*DCOS((2.0D0*K-1.0D0)*TH)
10         GI=GI-A(K)*T*DSIN((2.0D0*K-1.0D0)*TH)
        IF (X.LE.7.0) THEN
           GR1=0.0D0
           GI1=0.0D0
           DO 15 J=0,NA-1
              GR1=GR1+.5D0*DLOG((X+J)**2+Y*Y)
15            GI1=GI1+DATAN(Y/(X+J))
           GR=GR-GR1
           GI=GI-GI1
        ENDIF
        IF (X1.LT.0.0D0) THEN
           Z1=DSQRT(X*X+Y*Y)
           TH1=DATAN(Y/X)
           SR=-DSIN(PI*X)*DCOSH(PI*Y)
           SI=-DCOS(PI*X)*DSINH(PI*Y)
           Z2=DSQRT(SR*SR+SI*SI)
           TH2=DATAN(SI/SR)
           IF (SR.LT.0.0D0) TH2=PI+TH2
           GR=DLOG(PI/(Z1*Z2))-GR
           GI=-TH1-TH2-GI
           X=X1
           Y=Y1
        ENDIF
        IF (KF.EQ.1) THEN
           G0=DEXP(GR)
           GR=G0*DCOS(GI)
           GI=G0*DSIN(GI)
        ENDIF
        RETURN
        END

      
C ******************************************************************************


        SUBROUTINE CPSI(X,Y,PSR,PSI)
C
C       =============================================
C       Purpose: Compute the psi function for a
C                complex argument
C       Input :  x   --- Real part of z
C                y   --- Imaginary part of z
C       Output:  PSR --- Real part of psi(z)
C                PSI --- Imaginary part of psi(z)
C       =============================================
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION A(8)
        DATA A/-.8333333333333D-01,.83333333333333333D-02,
     &       -.39682539682539683D-02,.41666666666666667D-02,
     &       -.75757575757575758D-02,.21092796092796093D-01,
     &       -.83333333333333333D-01,.4432598039215686D0/
        PI=3.141592653589793D0
        
CC DW
		Y1 = 0.0D0
		X1 = 0.0D0
		N = 0
CC DW

        IF (Y.EQ.0.0D0.AND.X.EQ.INT(X).AND.X.LE.0.0D0) THEN
           PSR=1.0D+300
           PSI=0.0D0
        ELSE
           IF (X.LT.0.0D0) THEN
              X1=X
              Y1=Y
              X=-X
              Y=-Y
           ENDIF
           X0=X
           IF (X.LT.8.0D0) THEN
              N=8-INT(X)
              X0=X+N
           ENDIF
           IF (X0.EQ.0.0D0.AND.Y.NE.0.0D0) TH=0.5D0*PI
           IF (X0.NE.0.0D0) TH=DATAN(Y/X0)
           Z2=X0*X0+Y*Y
           Z0=DSQRT(Z2)
           PSR=DLOG(Z0)-0.5D0*X0/Z2
           PSI=TH+0.5D0*Y/Z2
           DO 10 K=1,8
              PSR=PSR+A(K)*Z2**(-K)*DCOS(2.0D0*K*TH)
10            PSI=PSI-A(K)*Z2**(-K)*DSIN(2.0D0*K*TH)
           IF (X.LT.8.0D0) THEN
              RR=0.0D0
              RI=0.0D0
              DO 20 K=1,N
                 RR=RR+(X0-K)/((X0-K)**2.0D0+Y*Y)
20               RI=RI+Y/((X0-K)**2.0D0+Y*Y)
              PSR=PSR-RR
              PSI=PSI+RI
           ENDIF
           IF (X1.LT.0.0D0) THEN
              TN=DTAN(PI*X)
              TM=DTANH(PI*Y)
              CT2=TN*TN+TM*TM
              PSR=PSR+X/(X*X+Y*Y)+PI*(TN-TN*TM*TM)/CT2
              PSI=PSI-Y/(X*X+Y*Y)-PI*TM*(1.0D0+TN*TN)/CT2
              X=X1
              Y=Y1
           ENDIF
        ENDIF
        RETURN
        END


C ------------------------------------------------------------------------------

