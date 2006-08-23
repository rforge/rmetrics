
CC SOURCE:
CC -----------------------------------------------------------------------------
CC Package: mexp
CC Version: 0.1
CC Title: Matrix exponential
CC Date: 2004-01-22
CC Author: David Firth
CC Maintainer: David Firth <d.firth@warwick.ac.uk>
CC Description: Exponentiate a matrix by Pade or Taylor approximation
CC License: GPL Version 2 or later

 

c   This program computes exp(A) for a given matrix A.
c   2 algorithms are employed: 1. The Taylor expansion of order
c       "ntaylor," denoted by T(ntaylor). 2. When "ntaylor"
c       is set to zero, the Pade diagonal approximation of order
c       "npade," denoted by P(npade), is used instead.
c       The algorithm is applied twice to calculate
c       T(ntaylor) and T(ntaylor+10) [or, when ntaylor=0,
c       to calculate P(npade) and P(npade+10)].
c   An upper bound for the L2 norm of the Cauchy error
c       T(ntaylor+10)-T(ntaylor) [or, when ntaylor=0,
c       P(npade+10)-P(npade)] is computed.
c       The result exp(A) is returned via the first argument.
c
c
    subroutine matrexp(a, ndim, ntaylor, npade, accuracy)
    implicit double precision (a-h,o-z)
c
c   "ndim" is the order of the given matrix A
c
    dimension a(ndim,ndim)
    dimension sum(ndim,ndim)
    dimension solution(ndim,ndim)
    dimension error(ndim,ndim)
    dimension dkeep(ndim,ndim)
c
c   use the algorithm to compute T(ntaylor) or P(npade)
c
    npower=log2(dsqrt(dl1norm(ndim,a)*dlinfnorm(ndim,a)))+4
    if(ntaylor.gt.0)then
        call taylor(ndim,ntaylor,npower,a,sum)
    else
        call pade(ndim,npade,npower,a,sum)
    endif
    call powermatrix(ndim,sum,npower,dkeep)
    call id(ndim,dkeep,sum)
c
c   computing the "solution" T(ntaylor+10) or P(npade+10)
c
    if(ntaylor.gt.0)then
      call taylor(ndim,ntaylor+10,npower,a,solution)
    else
      call pade(ndim,npade+10,npower,a,solution)
    endif
    call powermatrix(ndim,solution,npower,dkeep)
    call id(ndim,dkeep,solution)
c
c       copy the result back into a
c
        do 10 i=1,ndim
           do 10 j=1,ndim
 10       a(i,j) = sum(i,j)
c
c   compute the Cauchy error T(ntaylor+10)-T(ntaylor)
c                             or P(npade+10)-P(npade)
c
    call subtract(ndim,sum,solution,error)
    accuracy = dsqrt(dl1norm(ndim,error)*dlinfnorm(ndim,error))
    return
    end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine taylor(m,ntaylor,npower,a,sum)

c   Taylor series for exp(a/2**npower)

    implicit double precision (a-h,o-z)
    dimension a(m,m),sum(m,m),dkeep(m,m)
    nscale=2**npower
c   print*,'A is scaled by 2**',npower,'  =',nscale
    call initialize(m,sum,0.d0)
    call addtodiag(m,sum,1.d0)
    do 10 n=ntaylor,1,-1
      call multiplymatrix(m,sum,a,dkeep)
      call multiplyscalar(m,dkeep,1.d0/dble(n*nscale),sum)
10    call addtodiag(m,sum,1.d0)
    return
    end

    subroutine pade(m,npade,npower,a,approx)

c   Pade approximation for exp(a/2**npower)

    implicit double precision (a-h,o-z)
    dimension a(m,m),aminus(m,m),approx(m,m),dkeep(m,m)
    dimension padenom(m,m),padedenom(m,m)
    nscale=2**npower
c   print*,'A is scaled by 2**',npower,'  =',nscale
    call initialize(m,padenom,0.d0)
    call addtodiag(m,padenom,1.d0)
    do 10 n=npade,1,-1
      call multiplymatrix(m,padenom,a,dkeep)
      call multiplyscalar(m,dkeep,
     $      dble(npade-n+1)/dble(n*(2*npade-n+1)*nscale),padenom)
10    call addtodiag(m,padenom,1.d0)
    call minus(m,a,aminus)
    call initialize(m,padedenom,0.d0)
    call addtodiag(m,padedenom,1.d0)
    do 20 n=npade,1,-1
      call multiplymatrix(m,padedenom,aminus,dkeep)
      call multiplyscalar(m,dkeep,
     $      dble(npade-n+1)/dble(n*(2*npade-n+1)*nscale),padedenom)
20    call addtodiag(m,padedenom,1.d0)
    do 30 i=1,m
30    call solve(m,padedenom,padenom(1,i),approx(1,i))
    return
    end

    subroutine initialize(m,x,s)

c   initializing a matrix to a scalar s

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    do 10 i=1,m
      do 10 j=1,m
10      x(i,j)=s
    return
    end

    subroutine subtract(m,x,y,z)

c   subtracting a matrix y from a matrix x

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m),z(m,m)
    do 10 i=1,m
      do 10 j=1,m
10      z(i,j)=x(i,j)-y(i,j)
    return
    end

    subroutine multiplyscalar(m,x,s,y)

c   multiplying a matrix x by a scalar s

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m)
    do 10 i=1,m
      do 10 j=1,m
10      y(i,j)=x(i,j)*s
    return
    end

    subroutine multiplymatrix(m,x,y,z)

c   multiplying 2 matrices

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m),z(m,m)
    do 10 i=1,m
      do 10 j=1,m
        z(i,j)=0.d0
        do 10 k=1,m
10        z(i,j)=z(i,j)+x(i,k)*y(k,j)
    return
    end

    subroutine addtodiag(m,x,s)

c   add a scalar s to the main diagonal elements of a matrix x

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    do 10 i=1,m
10    x(i,i)=x(i,i)+s
    return
    end

    subroutine minus(m,x,y)

c   the minus of a matrix

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m)
    do 10 i=1,m
      do 10 j=1,m
10      y(i,j)=-x(i,j)
    return
    end

    subroutine id(m,x,y)

c   assign a matrix x to y

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m)
    do 10 i=1,m
      do 10 j=1,m
10      y(i,j)=x(i,j)
    return
    end

    subroutine readmatrix(m,x)

c   read a matrix x from a file A

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    dimension v(m)
        open(2,file='A')
    do 10 i=1,m
          read(2,*)v
      do 10 j=1,m
10    x(i,j)=v(j)
    return
    end

    subroutine printmatrix(m,x)

c   print a matrix x to a file expA

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    dimension v(m)
    open(3,file='expA')
    do 10 i=1,m
      do 20 j=1,m
20      v(j)=x(i,j)
c                          10   write(3,*)v
10  write(3,1000)v
1000    format(3e20.13)
    return
    end

    double precision function dl1norm(m,x)

c   l-1 norm of a matrix

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    dl1norm=0.d0
    do 10 j=1,m
      sum=0.d0
      do 20 i=1,m
20      sum=sum+dabs(x(i,j))
          if(sum.gt.dl1norm)dl1norm=sum
10  continue
    return
    end

    double precision function dlinfnorm(m,x)

c   l-infinity norm of a matrix

    implicit double precision (a-h,o-z)
    dimension x(m,m)
    dlinfnorm=0.d0
    do 10 i=1,m
      sum=0.d0
      do 20 j=1,m
20      sum=sum+dabs(x(i,j))
          if(sum.gt.dlinfnorm)dlinfnorm=sum
10  continue
    return
    end

    subroutine powermatrix(m,x,ipower,y)

c   computing the ith power of a matrix x

    implicit double precision (a-h,o-z)
    dimension x(m,m),y(m,m),dkeep(m,m)
    call id(m,x,y)
    call id(m,x,dkeep)
    do 10 i=1,ipower
      call multiplymatrix(m,dkeep,dkeep,y)
10    call id(m,y,dkeep)
    return
    end

    function log2(x)

c       the leaset integer larger than log_2(x)

    implicit double precision (a-h,o-z)
    log2=0
8   log2=log2+1
    if(dble(2**log2).lt.x)goto 8
    return
    end

    subroutine zero(m,x)

c   zeroing a vector

    implicit double precision (a-h,o-z)
    dimension x(m)
    do 10 i=1,m
10      x(i)=0.d0
    return
    end

    subroutine iden(m,x,y)

c   assign a vector x to y

    implicit double precision (a-h,o-z)
    dimension x(m),y(m)
    do 10 i=1,m
10      y(i)=x(i)
    return
    end

    subroutine comb(m,x,a,y,z)

c   linear combination of 2 vectors

    implicit double precision (a-h,o-z)
    dimension x(m),y(m),z(m)
    do 10 i=1,m
10      z(i)=x(i)+a*y(i)
    return
    end

    subroutine add(m,x,y,z)

c   adding 2 vectors

    implicit double precision (a-h,o-z)
    dimension x(m),y(m),z(m)
    do 10 i=1,m
10      z(i)=x(i)+y(i)
    return
    end

    subroutine sub(m,x,y,z)

c   subtracting a vector y from a vector x

    implicit double precision (a-h,o-z)
    dimension x(m),y(m),z(m)
    do 10 i=1,m
10      z(i)=x(i)-y(i)
    return
    end

    double precision function dip(m,u,v)

c   inner product of 2 vectors

    implicit double precision (a-h,o-z)
    dimension u(m),v(m)
    dip=0.d0
    do 10 i=1,m
10    dip = dip+u(i)*v(i)
    return
    end

    function nfact(n)

c   factorial function

    implicit double precision (a-h,o-z)
    nfact=1
    do 10 i=1,n
10    nfact=nfact*i
    return
    end

    double precision function c(n,k)

c   kth coefficient in the nth Pade polynom

    implicit double precision (a-h,o-z)
    padenom=dble(nfact(2*n-k)*nfact(n))
    padedenom=dble(nfact(2*n)*nfact(k)*nfact(n-k))
    c=padenom/padedenom
    return
    end

    double precision function dl2norm(m,u)

c   l2 norm of a vector

    implicit double precision (a-h,o-z)
    dimension u(m)
    dl2norm=dsqrt(dip(m,u,u))
    return
    end

    subroutine multiplyvector(m,a,x,y)

c   multiplying matrix times vector

    implicit double precision (a-h,o-z)
    dimension a(m,m),x(m),y(m)
    call zero(m,y)
    do 10 i=1,m
      do 10 j=1,m
10      y(i)=y(i)+a(i,j)*x(j)
    return
    end

    subroutine solve(m,A,f,x)

c   CGS iteration

    implicit double precision(a-h,o-z)     
    dimension A(m,m),f(m),x(m)
    dimension save(m),rcgs(m),r(m)
    dimension p(m),u(m)
    dimension rbar(m),v(m),q(m)
    thresh=1.d-100
    eps=1.d-30
    call zero(m,x)
    call iden(m,f,r)
    call iden(m,r,rcgs)
    call iden(m,r,p)
    call iden(m,r,u)
    omega0=dl2norm(m,rcgs)
    omegainit=omega0
c   print*,'res0=',dabs(omegainit)
    tau=omega0
    vv=0.d0
    eta=0.d0
    call iden(m,r,rbar)
    rho0=dip(m,rbar,r)
    if(dabs(rho0).le.thresh)then
c     print*,'rho0=',rho0,'   MG iteration number=1'
      return
    endif
    do 10 l=1,m
      call multiplyvector(m,A,p,v)
      sigma=dip(m,rbar,v)
      if(dabs(sigma).le.thresh)then
c       print*,'sigma=',sigma,'  iteration number=',2*l+1
        return
      endif
      alpha=rho0/sigma
      if(dabs(alpha).le.thresh)then
c       print*,'alpha=',alpha,'  iteration number=',2*l+1
        return
      endif
      scalar=-alpha
      call comb(m,u,scalar,v,q)
      call add(m,u,q,v)
      call multiplyvector(m,A,v,save)
      call comb(m,rcgs,scalar,save,rcgs)
      omega1=dl2norm(m,rcgs)
      call comb(m,x,alpha,v,x)
c     print*,'residual=',dabs(omega1),'  iteration number=',2*l+1
      if(dabs(omega1/omegainit).le.eps)then
c       print*,'iteration number=',2*l+1
        return
      endif
      omega0=omega1

      rho1=dip(m,rbar,rcgs)
      if(dabs(rho1).le.thresh)then
c       print*,'rho1=',rho1,'   iteration number=',2*l+1
        return
      endif
      beta=rho1/rho0
      rho0=rho1
      call comb(m,rcgs,beta,q,u)
      call comb(m,q,beta,p,save)
10    call comb(m,u,beta,save,p)
c   print*,'iteration number=',2*l+1
    return
    end
