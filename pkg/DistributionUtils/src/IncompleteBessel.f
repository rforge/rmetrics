C     Last change:  DJS    30 August 2010
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC    Programmer: Richard M. Slevinsky and Hassan Safouhi           CCC
CCC                                                                  CCC
CCC   Function: IBFs                                                 CCC
CCC                                                                  CCC
CCC   Evaluation of the nu'th order Incomplete Bessel		     CCC
CCC   function at the point x,y with a relative error of 10^(-10)    CCC
CCC                                                                  CCC
CCC   Input :                                                        CCC
CCC      - nu : order of the Incomplete Bessel function              CCC
CCC      - x : evaluation point					     CCC
CCC      - y : other evaluation point				     CCC
CCC                                                                  CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                                                                  CCC
CCC                                                                  CCC
CCC   Modified to be a subroutine for use in R                       CCC
CCC                                                                  CCC
CCC   David Scott, 30/08/2010                                        CCC
CCC                                                                  CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine incompleteBesselK(x,y,nu,eps,nmax,KNu,IBF,result)
C     implicit double precision(a-h,o-z)
C     implicit integer (i-n)
      integer nmax
      integer result
      double precision x,y,nu,eps,KNu,IBF
      parameter(numax=100)
      double precision G(1:nmax)
      double precision GM(1:nmax),GN(0:nmax)
      double precision Am(0:nmax,0:nmax)
      double precision An(0:nmax,0:nmax)
      double precision BI(0:numax),BK(0:numax)
      double precision Cnp(0:(nmax+1)*(nmax+2)/2)

      call combinatorial(nmax, Cnp)

      if(x.ge.y) then
      	call SSFcoef(nmax,nu-1D0,Am)
      	call SSFcoef(nmax,-nu-1D0,An)
      	call GDENOM(0,x,y,nu,An,nmax,Cnp,GN)
      	call GDENOM(1,x,y,nu,An,nmax,Cnp,GN)
      	call GNUM(1,x,y,nu,Am,An,nmax,Cnp,GM,GN)
      	G(1) = x**nu*GM(1)/GN(1)
      	do n=2,nmax
           call GDENOM(n,x,y,nu,An,nmax,Cnp,GN)
           call GNUM(n,x,y,nu,Am,An,nmax,Cnp,GM,GN)
           G(n) = x**nu*GM(n)/GN(n)
c          write(*,*) G(n),n
           if(dabs(G(n)-G(n-1)).lt.eps) then
              goto 100
           end if
      	end do
      else if(y.gt.x) then
c      	call IKV(nu,2D0*dsqrt(x*y),pnu,BI,BK)
      	call SSFcoef(nmax,-nu-1D0,Am)
      	call SSFcoef(nmax,nu-1D0,An)
      	call GDENOM(0,y,x,-nu,An,nmax,Cnp,GN)
      	call GDENOM(1,y,x,-nu,An,nmax,Cnp,GN)
      	call GNUM(1,y,x,-nu,Am,An,nmax,Cnp,GM,GN)
      	G(1) = y**(-nu)*GM(1)/GN(1)
      	do n=2,nmax
           call GDENOM(n,y,x,-nu,An,nmax,Cnp,GN)
           call GNUM(n,y,x,-nu,Am,An,nmax,Cnp,GM,GN)
           G(n) = y**(-nu)*GM(n)/GN(n)
c          write(*,*) G(n),n
           if(dabs(G(n)-G(n-1)).lt.eps) then
              G(n) = 2D0*(x/y)**(nu/2D0)*KNu-G(n)
c             write(*,*) 'BK',BK(int(nu))
              goto 100
           end if
      	end do
      end if

100   if(n.gt.nmax) then
         result = 1
      end if

      IBF = G(n)
      return
      end


      subroutine SSFcoef(nmax,nu,A)
      implicit double precision(a-h,o-z)
      implicit integer (i-n)
      integer l,i,nmax
      double precision nu,A(0:nmax,0:nmax)
      A(0,0) = 1D0
      do l=1,nmax
      	do i=1,l-1
      		A(l,i) = (-nu+i+l-1D0)*A(l-1,i)+A(l-1,i-1)
      	end do
      	A(l,0) = (-nu+l-1D0)*A(l-1,0)
      	A(l,l) = 1D0
      end do
      return
      end

      subroutine combinatorial(nu, Cnp)
      implicit double precision (a-h, o-z)
      implicit integer (i-n)

      dimension Cnp(0:*)

      do n=0, nu
         Cnp(n*(n+1)/2 + 0) = 1.0d0
         Cnp(n*(n+1)/2 + n) = 1.0d0
         do np=1, n-1
            Cnp(n*(n+1)/2+np) = Cnp(n*(n-1)/2+np-1)+Cnp(n*(n-1)/2+np)
         end do
      end do
      return
      end

      subroutine GNUM(n,x,y,nu,Am,An,nmax,Cnp,GM,GN)
      implicit double precision(a-h,o-z)
      implicit integer (i-n)
      integer n,nmax
      double precision x,y,nu
      double precision Am(0:nmax,0:nmax)
      double precision An(0:nmax,0:nmax)
      double precision Cnp(0:*),GM(1:nmax),GN(0:nmax)
      GM(n) = 0D0
      do ir=1,n
      	terme=0D0
      	do is=0,ir-1
      			termepr = 0D0
      		do i=0,is
      	termepr = termepr+Am(is,i)*(-x)**i
      		end do
      terme = terme + termepr*Cnp(ir*(ir-1)/2+is)*(1D0/y)**is
      	end do
      	GM(n) = GM(n) + Cnp(n*(n+1)/2+ir)*(x*y)**ir
     $        * GN(n-ir)*terme
      end do
      GM(n) = GM(n)*dexp(-x-y)/x**nu/y
      return
      end

      subroutine GDENOM(n,x,y,nu,An,nmax,Cnp,GN)
      implicit double precision(a-h,o-z)
      implicit integer (i-n)
      integer n,nmax
      double precision x,y,nu
      double precision An(0:nmax,0:nmax)
      double precision Cnp(0:*),GN(0:nmax)
      GN(n) = 0D0
      do ir=0,n
      	terme=0D0
      	do i=0,ir
      		terme = terme+An(ir,i)*x**i
      	end do
      	GN(n) = GN(n) + Cnp(n*(n+1)/2+ir)*(-1D0/y)**ir*terme
      end do
      GN(n) = GN(n)*(-x*y)**n*x**(nu+1)*dexp(x+y)
      return
      end

