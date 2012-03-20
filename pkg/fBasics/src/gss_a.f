C Output from Public domain Ratfor, version 1.01
      subroutine dnewton (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs,
     * nqd, nt, bwt, qdwt, prec, maxiter, mchpr, jpvt, wk, info)
      integer nxis, nxi, nobs, cntsum, cnt(*), nqd, nt, maxiter, jpvt(*)
     *, info
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,*), bwt(*),
     * qdwt(nt,*), prec, mchpr, wk(*)
      integer imrs, iwt, iwtsum, ifit, imu, imuwk, iv, ivwk, icdnew, iwt
     *new, iwtsumnew, ifitnew, iwk
      imrs = 1
      iwt = imrs + max0 (nxis, 3)
      iwtsum = iwt + nqd*nt
      ifit = iwtsum + nt
      imu = ifit + nobs
      imuwk = imu + nxis
      iv = imuwk + nxis
      ivwk = iv + nxis*nxis
      icdnew = ivwk + nxis*nxis
      iwtnew = icdnew + nxis
      iwtsumnew = iwtnew + nqd*nt
      ifitnew = iwtsumnew + nt
      iwk = ifitnew + nobs
      call dnewton1 (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs, nqd,
     * nt, bwt, qdwt, prec, maxiter, mchpr, wk(imrs), wk(iwt), wk(iwtsum
     *), wk(ifit), wk(imu), wk(imuwk), wk(iv), wk(ivwk), jpvt, wk(icdnew
     *), wk(iwtnew), wk(iwtsumnew), wk(ifitnew), wk(iwk), info)
      return
      end
      subroutine dnewton1 (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs
     *, nqd, nt, bwt, qdwt, prec, maxiter, mchpr, mrs, wt, wtsum, fit, m
     *u, muwk, v, vwk, jpvt, cdnew, wtnew, wtsumnew, fitnew, wk, info)
      integer nxis, nxi, nobs, cntsum, cnt(*), nqd, nt, maxiter, jpvt(*)
     *, info
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,*), bwt(*),
     * qdwt(nt,*), prec, mchpr, mrs(*), wt(nt,*), wtsum(*), fit(*), mu(*
     *), muwk(*), v(nxis,*), vwk(nxis,*), cdnew(*), wtnew(nt,*), wtsumne
     *w(*), fitnew(*), wk(*)
      integer i, j, k, m, iter, flag, rkv, idamax, infowk
      double precision norm, tmp, ddot, fitmean, lkhd, mumax, lkhdnew, d
     *isc, disc0, trc
      info = 0
      i=1
23000 if(.not.(i.le.nxis))goto 23002
      mrs(i) = 0.d0
      if(cntsum.eq.0)then
      j=1
23005 if(.not.(j.le.nobs))goto 23007
      mrs(i) = mrs(i) + rs(i,j)
23006 j=j+1
      goto 23005
23007 continue
      mrs(i) = mrs(i) / dfloat (nobs)
      else
      j=1
23008 if(.not.(j.le.nobs))goto 23010
      mrs(i) = mrs(i) + rs(i,j) * dfloat (cnt(j))
23009 j=j+1
      goto 23008
23010 continue
      mrs(i) = mrs(i) / dfloat (cntsum)
      endif
23001 i=i+1
      goto 23000
23002 continue
      m=1
23011 if(.not.(m.le.nt))goto 23013
      wtsum(m) = 0.d0
23012 m=m+1
      goto 23011
23013 continue
      i=1
23014 if(.not.(i.le.nqd))goto 23016
      tmp = dexp (ddot (nxis, qdrs(i,1), nqd, cd, 1))
      m=1
23017 if(.not.(m.le.nt))goto 23019
      wt(m,i) = qdwt(m,i) * tmp
      wtsum(m) = wtsum(m) + wt(m,i)
23018 m=m+1
      goto 23017
23019 continue
23015 i=i+1
      goto 23014
23016 continue
      norm = 0.d0
      m=1
23020 if(.not.(m.le.nt))goto 23022
      norm = norm + bwt(m) * dlog (wtsum(m))
23021 m=m+1
      goto 23020
23022 continue
      fitmean = 0.d0
      i=1
23023 if(.not.(i.le.nobs))goto 23025
      tmp = ddot (nxis, rs(1,i), 1, cd, 1)
      fit(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23024 i=i+1
      goto 23023
23025 continue
      if(cntsum.eq.0)then
      fitmean = fitmean / dfloat (nobs)
      else
      fitmean = fitmean / dfloat (cntsum)
      endif
      call dsymv ('u', nxi, 1.d0, q, nxi, cd, 1, 0.d0, wk, 1)
      lkhd = ddot (nxi, cd, 1, wk, 1) / 2.d0 - fitmean + norm
      iter = 0
      flag = 0
23030 continue
      iter = iter + 1
      call dset(nxis, 0.d0, mu, 1)
      call dset(nxis*nxis, 0.d0, v, 1)
      m=1
23033 if(.not.(m.le.nt))goto 23035
      i=1
23036 if(.not.(i.le.nxis))goto 23038
      muwk(i) = - ddot (nqd, wt(m,1), nt, qdrs(1,i), 1) / wtsum(m)
23037 i=i+1
      goto 23036
23038 continue
      i=1
23039 if(.not.(i.le.nxis))goto 23041
      j=i
23042 if(.not.(j.le.nxis))goto 23044
      vwk(i,j) = 0.d0
      k=1
23045 if(.not.(k.le.nqd))goto 23047
      vwk(i,j) = vwk(i,j) + wt(m,k) * qdrs(k,i) * qdrs(k,j)
23046 k=k+1
      goto 23045
23047 continue
      vwk(i,j) = vwk(i,j) / wtsum(m) - muwk(i) * muwk(j)
23043 j=j+1
      goto 23042
23044 continue
23040 i=i+1
      goto 23039
23041 continue
      call daxpy (nxis, bwt(m), muwk, 1, mu, 1)
      call daxpy (nxis*nxis, bwt(m), vwk, 1, v, 1)
23034 m=m+1
      goto 23033
23035 continue
      i=1
23048 if(.not.(i.le.nxi))goto 23050
      j=i
23051 if(.not.(j.le.nxi))goto 23053
      v(i,j) = v(i,j) + q(i,j)
23052 j=j+1
      goto 23051
23053 continue
23049 i=i+1
      goto 23048
23050 continue
      call daxpy (nxis, 1.d0, mrs, 1, mu, 1)
      call dsymv ('u', nxi, -1.d0, q, nxi, cd, 1, 1.d0, mu, 1)
      mumax = dabs(mu(idamax(nxis, mu, 1)))
      i=1
23054 if(.not.(i.le.nxis))goto 23056
      jpvt(i) = 0
23055 i=i+1
      goto 23054
23056 continue
      call dchdc (v, nxis, nxis, wk, jpvt, 1, rkv)
23057 if(v(rkv,rkv).lt.v(1,1)*dsqrt(mchpr))then
      rkv = rkv - 1
      goto 23057
      endif
23058 continue
      i=rkv+1
23059 if(.not.(i.le.nxis))goto 23061
      v(i,i) = v(1,1)
      call dset (i-rkv-1, 0.d0, v(rkv+1,i), 1)
23060 i=i+1
      goto 23059
23061 continue
23062 continue
      call dcopy (nxis, mu, 1, cdnew, 1)
      call dprmut (cdnew, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, cdnew, 11, infowk)
      call dset (nxis-rkv, 0.d0, cdnew(rkv+1), 1)
      call dtrsl (v, nxis, nxis, cdnew, 01, infowk)
      call dprmut (cdnew, nxis, jpvt, 1)
      call daxpy (nxis, 1.d0, cd, 1, cdnew, 1)
      m=1
23065 if(.not.(m.le.nt))goto 23067
      wtsumnew(m) = 0.d0
23066 m=m+1
      goto 23065
23067 continue
      i=1
23068 if(.not.(i.le.nqd))goto 23070
      tmp = ddot (nxis, qdrs(i,1), nqd, cdnew, 1)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23070
      endif
      tmp = dexp (tmp)
      m=1
23073 if(.not.(m.le.nt))goto 23075
      wtnew(m,i) = qdwt(m,i) * tmp
      wtsumnew(m) = wtsumnew(m) + wtnew(m,i)
23074 m=m+1
      goto 23073
23075 continue
23069 i=i+1
      goto 23068
23070 continue
      norm = 0.d0
      m=1
23076 if(.not.(m.le.nt))goto 23078
      norm = norm + bwt(m) * dlog (wtsumnew(m))
23077 m=m+1
      goto 23076
23078 continue
      if((flag.eq.0).or.(flag.eq.2))then
      fitmean = 0.d0
      i=1
23081 if(.not.(i.le.nobs))goto 23083
      tmp = ddot (nxis, rs(1,i), 1, cdnew, 1)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23083
      endif
      fitnew(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23082 i=i+1
      goto 23081
23083 continue
      if(cntsum.eq.0)then
      fitmean = fitmean / dfloat (nobs)
      else
      fitmean = fitmean / dfloat (cntsum)
      endif
      call dsymv ('u', nxi, 1.d0, q, nxi, cdnew, 1, 0.d0, wk, 1)
      lkhdnew = ddot (nxi, cdnew, 1, wk, 1) / 2.d0 - fitmean + norm
      endif
      if(flag.eq.1)then
      call dset (nxis, 0.d0, cd, 1)
      call dcopy (nt*nqd, qdwt, 1, wt, 1)
      lkhd = 0.d0
      m=1
23092 if(.not.(m.le.nt))goto 23094
      wtsum(m) = 0.d0
      i=1
23095 if(.not.(i.le.nqd))goto 23097
      wtsum(m) = wtsum(m) + wt(m,i)
23096 i=i+1
      goto 23095
23097 continue
      lkhd = lkhd + bwt(m) * dlog (wtsum(m))
23093 m=m+1
      goto 23092
23094 continue
      call dset (nobs, 1.d0, fit, 1)
      iter = 0
      goto 23064
      endif
      if(flag.eq.3)then
      goto 23064
      endif
      if(lkhdnew-lkhd.lt.1.d1*(1.d0+dabs(lkhd))*mchpr)then
      goto 23064
      endif
      call dscal (nxis, .5d0, mu, 1)
      if(dabs(mu(idamax(nxis, mu, 1))/mumax).lt.1.d1*mchpr)then
      goto 23064
      endif
23063 goto 23062
23064 continue
      if(flag.eq.1)then
      flag = 2
      goto 23031
      endif
      if(flag.eq.3)then
      info = 1
      return
      endif
      disc = 0.d0
      i=1
23108 if(.not.(i.le.nqd))goto 23110
      m=1
23111 if(.not.(m.le.nt))goto 23113
      disc = dmax1 (disc, dabs(wt(m,i)-wtnew(m,i))/(1.d0+dabs(wt(m,i))))
23112 m=m+1
      goto 23111
23113 continue
23109 i=i+1
      goto 23108
23110 continue
      i=1
23114 if(.not.(i.le.nobs))goto 23116
      disc = dmax1 (disc, dabs(fit(i)-fitnew(i))/(1.d0+dabs(fit(i))))
23115 i=i+1
      goto 23114
23116 continue
      disc = dmax1 (disc, (mumax/(1.d0+dabs(lkhd)))**2)
      disc0 = dmax1 ((mumax/(1.d0+lkhd))**2, dabs(lkhd-lkhdnew)/(1.d0+da
     *bs(lkhd)))
      call dcopy (nxis, cdnew, 1, cd, 1)
      call dcopy (nqd*nt, wtnew, 1, wt, 1)
      call dcopy (nt, wtsumnew, 1, wtsum, 1)
      call dcopy (nobs, fitnew, 1, fit, 1)
      lkhd = lkhdnew
      if(disc0.lt.prec)then
      goto 23032
      endif
      if(disc.lt.prec)then
      goto 23032
      endif
      if(iter.lt.maxiter)then
      goto 23031
      endif
      if(flag.eq.0)then
      call dset (nxis, 0.d0, cd, 1)
      call dcopy (nt*nqd, qdwt, 1, wt, 1)
      lkhd = 0.d0
      m=1
23125 if(.not.(m.le.nt))goto 23127
      wtsum(m) = 0.d0
      i=1
23128 if(.not.(i.le.nqd))goto 23130
      wtsum(m) = wtsum(m) + wt(m,i)
23129 i=i+1
      goto 23128
23130 continue
      lkhd = lkhd + bwt(m) * dlog (wtsum(m))
23126 m=m+1
      goto 23125
23127 continue
      call dset (nobs, 1.d0, fit, 1)
      iter = 0
      flag = 2
      else
      info = 2
      goto 23032
      endif
23031 goto 23030
23032 continue
      i=1
23131 if(.not.(i.le.nobs))goto 23133
      call daxpy (nxis, -1.d0, mrs, 1, rs(1,i), 1)
      call dprmut (rs(1,i), nxis, jpvt, 0)
      if(cntsum.ne.0)then
      call dscal (nxis, dsqrt(dfloat(cnt(i))), rs(1,i), 1)
      endif
      call dtrsl (v, nxis, nxis, rs(1,i), 11, infowk)
      call dset (nxis-rkv, 0.d0, rs(rkv+1,i), 1)
23132 i=i+1
      goto 23131
23133 continue
      trc = ddot (nobs*nxis, rs, 1, rs, 1)
      if(cntsum.eq.0)then
      trc = trc / dfloat(nobs) / (dfloat(nobs)-1.d0)
      lkhd = 0.d0
      i=1
23138 if(.not.(i.le.nobs))goto 23140
      lkhd = lkhd + dlog (fit(i))
23139 i=i+1
      goto 23138
23140 continue
      lkhd = lkhd / dfloat (nobs)
      else
      trc = trc / dfloat(cntsum) / (dfloat(cntsum)-1.d0)
      lkhd = 0.d0
      i=1
23141 if(.not.(i.le.nobs))goto 23143
      lkhd = lkhd + dfloat (cnt(i)) * dlog (fit(i))
23142 i=i+1
      goto 23141
23143 continue
      lkhd = lkhd / dfloat (cntsum)
      endif
      m=1
23144 if(.not.(m.le.nt))goto 23146
      lkhd = lkhd - bwt(m) * dlog (wtsum(m))
23145 m=m+1
      goto 23144
23146 continue
      mrs(1) = lkhd
      mrs(2) = trc
      return
      end
      subroutine dprmut (x,npar,jpvt,job)
      integer npar,jpvt(npar),job
      double precision x(npar)
c
c Purpose: permute the elements of the array x according to the index 
c	vector jpvt (either forward or backward permutation).
c
c On Entry:
c   x(npar)		array to be permuted
c   npar		size of x (and jpvt)
c   jpvt		indices of the permutation
c   job			indicator of forward or backward permutation
c			if job = 0 forward permutation  
c				x(jpvt(i)) moved to x(i)
c			if job is nonzero backward permutation 
c				x(i) moved to x(jpvt(i))
c On Exit:
c   x(npar)		array with permuted entries
c
c   Written:	Yin Ling	U. of Maryland, August,1978
c
c $Header: dprmut.f,v 2.1 86/04/08 14:05:53 lindstrom Exp $
c
      integer i,j,k
      double precision t
c
      if (npar .le. 1) then
         return
      endif
      do 10 j = 1,npar
         jpvt(j) = -jpvt(j)
   10 continue
      if (job .eq. 0) then
c		forward permutation
         do 30 i = 1,npar 
            if (jpvt(i) .gt. 0) then
               goto 30
            endif
            j = i
            jpvt(j) = -jpvt(j)
            k = jpvt(j)
c           while
   20       if (jpvt(k) .lt. 0) then
               t = x(j)
               x(j) = x(k)
               x(k) = t
               jpvt(k) = -jpvt(k)
               j = k
               k = jpvt(k)
               goto 20
c           endwhile
            endif
   30    continue
      endif
      if (job .ne. 0 ) then
c			backward permutation
         do 50 i = 1,npar 
            if (jpvt(i) .gt. 0) then
               goto 50
            endif
            jpvt(i) = -jpvt(i)
            j = jpvt(i)
c           while
   40       if (j .ne. i) then
               t = x(i)
               x(i) = x(j)
               x(j) = t
               jpvt(j) = -jpvt(j)
               j = jpvt(j)
               goto 40
c           endwhile
            endif
   50    continue
      endif
      return
      end
      subroutine  dset(n,da,dx,incx)
      integer n,incx
      double precision da,dx(*)
c
c Purpose : set vector dx to constant da. Unrolled loops are used for 
c	increment equal to one.
c
c On Entry:
c   n			length of dx
c   da			any constant
c   incx		increment for dx
c
c On Exit:
c   dx(n)		vector with all n entries set to da
c
c $Header: dset.f,v 2.1 86/04/08 14:06:25 lindstrom Exp $
c
      integer i,m,mp1,nincx
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da
        dx(i + 1) = da
        dx(i + 2) = da
        dx(i + 3) = da
        dx(i + 4) = da
   50 continue
      return
      end
c  To get dgamma,  "send dgamma from fnlib".
c  To get d1mach, mail netlib
c       send d1mach from core
c
      subroutine gaussq(kind, n, alpha, beta, kpts, endpts, b, t, w)
c
c           this set of routines computes the nodes t(j) and weights
c        w(j) for gaussian-type quadrature rules with pre-assigned
c        nodes.  these are used when one wishes to approximate
c
c                 integral (from a to b)  f(x) w(x) dx
c
c                              n
c        by                   sum w  f(t )
c                             j=1  j    j
c
c        (note w(x) and w(j) have no connection with each other.)
c        here w(x) is one of six possible non-negative weight
c        functions (listed below), and f(x) is the
c        function to be integrated.  gaussian quadrature is particularly
c        useful on infinite intervals (with appropriate weight
c        functions), since then other techniques often fail.
c
c           associated with each weight function w(x) is a set of
c        orthogonal polynomials.  the nodes t(j) are just the zeroes
c        of the proper n-th degree polynomial.
c
c     input parameters (all real numbers are in double precision)
c
c        kind     an integer between 1 and 6 giving the type of
c                 quadrature rule:
c
c        kind = 1:  legendre quadrature, w(x) = 1 on (-1, 1)
c        kind = 2:  chebyshev quadrature of the first kind
c                   w(x) = 1/sqrt(1 - x*x) on (-1, +1)
c        kind = 3:  chebyshev quadrature of the second kind
c                   w(x) = sqrt(1 - x*x) on (-1, 1)
c        kind = 4:  hermite quadrature, w(x) = exp(-x*x) on
c                   (-infinity, +infinity)
c        kind = 5:  jacobi quadrature, w(x) = (1-x)**alpha * (1+x)**
c                   beta on (-1, 1), alpha, beta .gt. -1.
c                   note: kind=2 and 3 are a special case of this.
c        kind = 6:  generalized laguerre quadrature, w(x) = exp(-x)*
c                   x**alpha on (0, +infinity), alpha .gt. -1
c
c        n        the number of points used for the quadrature rule
c        alpha    real parameter used only for gauss-jacobi and gauss-
c                 laguerre quadrature (otherwise use 0.d0).
c        beta     real parameter used only for gauss-jacobi quadrature--
c                 (otherwise use 0.d0)
c        kpts     (integer) normally 0, unless the left or right end-
c                 point (or both) of the interval is required to be a
c                 node (this is called gauss-radau or gauss-lobatto
c                 quadrature).  then kpts is the number of fixed
c                 endpoints (1 or 2).
c        endpts   real array of length 2.  contains the values of
c                 any fixed endpoints, if kpts = 1 or 2.
c        b        real scratch array of length n
c
c     output parameters (both double precision arrays of length n)
c
c        t        will contain the desired nodes.
c        w        will contain the desired weights w(j).
c
c     underflow may sometimes occur, but is harmless.
c
c     references
c        1.  golub, g. h., and welsch, j. h., "calculation of gaussian
c            quadrature rules," mathematics of computation 23 (april,
c            1969), pp. 221-230.
c        2.  golub, g. h., "some modified matrix eigenvalue problems,"
c            siam review 15 (april, 1973), pp. 318-334 (section 7).
c        3.  stroud and secrest, gaussian quadrature formulas, prentice-
c            hall, englewood cliffs, n.j., 1966.
c
c        original version 20 jan 1975 from stanford
c        modified 21 dec 1983 by eric grosse
c          imtql2 => gausq2
c          hex constant => d1mach (from core library)
c          compute pi using datan
c          removed accuracy claims, description of method
c          added single precision version
c
      double precision b(n), t(n), w(n), endpts(2), muzero, t1,
     x gam, solve, dsqrt, alpha, beta
c
      call class (kind, n, alpha, beta, b, t, muzero)
c
c           the matrix of coefficients is assumed to be symmetric.
c           the array t contains the diagonal elements, the array
c           b the off-diagonal elements.
c           make appropriate changes in the lower right 2 by 2
c           submatrix.
c
      if (kpts.eq.0)  go to 100
      if (kpts.eq.2)  go to  50
c
c           if kpts=1, only t(n) must be changed
c
      t(n) = solve(endpts(1), n, t, b)*b(n-1)**2 + endpts(1)
      go to 100
c
c           if kpts=2, t(n) and b(n-1) must be recomputed
c
   50 gam = solve(endpts(1), n, t, b)
      t1 = ((endpts(1) - endpts(2))/(solve(endpts(2), n, t, b) - gam))
      b(n-1) = dsqrt(t1)
      t(n) = endpts(1) + gam*t1
c
c           note that the indices of the elements of b run from 1 to n-1
c           and thus the value of b(n) is arbitrary.
c           now compute the eigenvalues of the symmetric tridiagonal
c           matrix, which has been modified as necessary.
c           the method used is a ql-type method with origin shifting
c
  100 w(1) = 1.0d0
      do 105 i = 2, n
  105    w(i) = 0.0d0
c
      call gausq2 (n, t, b, w, ierr)
      do 110 i = 1, n
  110    w(i) = muzero * w(i) * w(i)
c
      return
      end
c
c
c
      double precision function solve(shift, n, a, b)
c
c       this procedure performs elimination to solve for the
c       n-th component of the solution delta to the equation
c
c             (jn - shift*identity) * delta  = en,
c
c       where en is the vector of all zeroes except for 1 in
c       the n-th position.
c
c       the matrix jn is symmetric tridiagonal, with diagonal
c       elements a(i), off-diagonal elements b(i).  this equation
c       must be solved to obtain the appropriate changes in the lower
c       2 by 2 submatrix of coefficients for orthogonal polynomials.
c
c
      double precision shift, a(n), b(n), alpha
c
      alpha = a(1) - shift
      nm1 = n - 1
      do 10 i = 2, nm1
   10    alpha = a(i) - shift - b(i-1)**2/alpha
      solve = 1.0d0/alpha
      return
      end
c
c
c
      subroutine class(kind, n, alpha, beta, b, a, muzero)
c
c           this procedure supplies the coefficients a(j), b(j) of the
c        recurrence relation
c
c             b p (x) = (x - a ) p   (x) - b   p   (x)
c              j j            j   j-1       j-1 j-2
c
c        for the various classical (normalized) orthogonal polynomials,
c        and the zero-th moment
c
c             muzero = integral w(x) dx
c
c        of the given polynomial's weight function w(x).  since the
c        polynomials are orthonormalized, the tridiagonal matrix is
c        guaranteed to be symmetric.
c
c           the input parameter alpha is used only for laguerre and
c        jacobi polynomials, and the parameter beta is used only for
c        jacobi polynomials.  the laguerre and jacobi polynomials
c        require the gamma function.
c
      double precision a(n), b(n), muzero, alpha, beta
      double precision abi, a2b2, dgamma, pi, dsqrt, ab
c
      pi = 4.0d0 * datan(1.0d0)
      nm1 = n - 1
      go to (10, 20, 30, 40, 50, 60), kind
c
c              kind = 1:  legendre polynomials p(x)
c              on (-1, +1), w(x) = 1.
c
   10 muzero = 2.0d0
      do 11 i = 1, nm1
         a(i) = 0.0d0
         abi = i
   11    b(i) = abi/dsqrt(4*abi*abi - 1.0d0)
      a(n) = 0.0d0
      return
c
c              kind = 2:  chebyshev polynomials of the first kind t(x)
c              on (-1, +1), w(x) = 1 / sqrt(1 - x*x)
c
   20 muzero = pi
      do 21 i = 1, nm1
         a(i) = 0.0d0
   21    b(i) = 0.5d0
      b(1) = dsqrt(0.5d0)
      a(n) = 0.0d0
      return
c
c              kind = 3:  chebyshev polynomials of the second kind u(x)
c              on (-1, +1), w(x) = sqrt(1 - x*x)
c
   30 muzero = pi/2.0d0
      do 31 i = 1, nm1
         a(i) = 0.0d0
   31    b(i) = 0.5d0
      a(n) = 0.0d0
      return
c
c              kind = 4:  hermite polynomials h(x) on (-infinity,
c              +infinity), w(x) = exp(-x**2)
c
   40 muzero = dsqrt(pi)
      do 41 i = 1, nm1
         a(i) = 0.0d0
   41    b(i) = dsqrt(i/2.0d0)
      a(n) = 0.0d0
      return
c
c              kind = 5:  jacobi polynomials p(alpha, beta)(x) on
c              (-1, +1), w(x) = (1-x)**alpha + (1+x)**beta, alpha and
c              beta greater than -1
c
   50 ab = alpha + beta
      abi = 2.0d0 + ab
      muzero = 2.0d0 ** (ab + 1.0d0) * dgamma(alpha + 1.0d0) * dgamma(
     x beta + 1.0d0) / dgamma(abi)
      a(1) = (beta - alpha)/abi
      b(1) = dsqrt(4.0d0*(1.0d0 + alpha)*(1.0d0 + beta)/((abi + 1.0d0)*
     1  abi*abi))
      a2b2 = beta*beta - alpha*alpha
      do 51 i = 2, nm1
         abi = 2.0d0*i + ab
         a(i) = a2b2/((abi - 2.0d0)*abi)
   51    b(i) = dsqrt (4.0d0*i*(i + alpha)*(i + beta)*(i + ab)/
     1   ((abi*abi - 1)*abi*abi))
      abi = 2.0d0*n + ab
      a(n) = a2b2/((abi - 2.0d0)*abi)
      return
c
c              kind = 6:  laguerre polynomials l(alpha)(x) on
c              (0, +infinity), w(x) = exp(-x) * x**alpha, alpha greater
c              than -1.
c
   60 muzero = dgamma(alpha + 1.0d0)
      do 61 i = 1, nm1
         a(i) = 2.0d0*i - 1.0d0 + alpha
   61    b(i) = dsqrt(i*(i + alpha))
      a(n) = 2.0d0*n - 1 + alpha
      return
      end
c
c
      subroutine gausq2(n, d, e, z, ierr)
c
c     this subroutine is a translation of an algol procedure,
c     num. math. 12, 377-383(1968) by martin and wilkinson,
c     as modified in num. math. 15, 450(1970) by dubrulle.
c     handbook for auto. comp., vol.ii-linear algebra, 241-248(1971).
c     this is a modified version of the 'eispack' routine imtql2.
c
c     this subroutine finds the eigenvalues and first components of the
c     eigenvectors of a symmetric tridiagonal matrix by the implicit ql
c     method.
c
c     on input:
c
c        n is the order of the matrix;
c
c        d contains the diagonal elements of the input matrix;
c
c        e contains the subdiagonal elements of the input matrix
c          in its first n-1 positions.  e(n) is arbitrary;
c
c        z contains the first row of the identity matrix.
c
c      on output:
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1, 2, ..., ierr-1;
c
c        e has been destroyed;
c
c        z contains the first components of the orthonormal eigenvectors
c          of the symmetric tridiagonal matrix.  if an error exit is
c          made, z contains the eigenvectors associated with the stored
c          eigenvalues;
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c     ------------------------------------------------------------------
c
      integer i, j, k, l, m, n, ii, mml, ierr
      real*8 d(n), e(n), z(n), b, c, f, g, p, r, s, machep
      real*8 dsqrt, dabs, dsign, d1mach
c
      machep=d1mach(4)
c
      ierr = 0
      if (n .eq. 1) go to 1001
c
      e(n) = 0.0d0
      do 240 l = 1, n
         j = 0
c     :::::::::: look for small sub-diagonal element ::::::::::
  105    do 110 m = l, n
            if (m .eq. n) go to 120
            if (dabs(e(m)) .le. machep * (dabs(d(m)) + dabs(d(m+1))))
     x         go to 120
  110    continue
c
  120    p = d(l)
         if (m .eq. l) go to 240
         if (j .eq. 30) go to 1000
         j = j + 1
c     :::::::::: form shift ::::::::::
         g = (d(l+1) - p) / (2.0d0 * e(l))
         r = dsqrt(g*g+1.0d0)
         g = d(m) - p + e(l) / (g + dsign(r, g))
         s = 1.0d0
         c = 1.0d0
         p = 0.0d0
         mml = m - l
c
c     :::::::::: for i=m-1 step -1 until l do -- ::::::::::
         do 200 ii = 1, mml
            i = m - ii
            f = s * e(i)
            b = c * e(i)
            if (dabs(f) .lt. dabs(g)) go to 150
            c = g / f
            r = dsqrt(c*c+1.0d0)
            e(i+1) = f * r
            s = 1.0d0 / r
            c = c * s
            go to 160
  150       s = f / g
            r = dsqrt(s*s+1.0d0)
            e(i+1) = g * r
            c = 1.0d0 / r
            s = s * c
  160       g = d(i+1) - p
            r = (d(i) - g) * s + 2.0d0 * c * b
            p = s * r
            d(i+1) = g + p
            g = c * r - b
c     :::::::::: form first component of vector ::::::::::
            f = z(i+1)
            z(i+1) = s * z(i) + c * f
  200       z(i) = c * z(i) - s * f
c
         d(l) = d(l) - p
         e(l) = g
         e(m) = 0.0d0
         go to 105
  240 continue
c
c     :::::::::: order eigenvalues and eigenvectors ::::::::::
      do 300 ii = 2, n
         i = ii - 1
         k = i
         p = d(i)
c
         do 260 j = ii, n
            if (d(j) .ge. p) go to 260
            k = j
            p = d(j)
  260    continue
c
         if (k .eq. i) go to 300
         d(k) = d(i)
         d(i) = p
         p = z(i)
         z(i) = z(k)
         z(k) = p
  300 continue
c
      go to 1001
c     :::::::::: set error -- no convergence to an
c                eigenvalue after 30 iterations ::::::::::
 1000 ierr = l
 1001 return
c     :::::::::: last card of gausq2 ::::::::::
      end
c
c
c
      double precision function dgamma(x)
      double precision x
      dgamma = 1.0d0
      return
      end
