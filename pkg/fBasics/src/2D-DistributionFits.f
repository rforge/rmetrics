
CONTENT:

C Gaussian-Type Quadrature

C###############################################################################

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
cc      double precision function dgamma(x)
cc      double precision x
cc      dgamma = 1.0d0
cc      return
cc      end


C ==============================================================================


C Output from Public domain Ratfor, version 1.0
      subroutine dnewton (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs,
     * nqd, qdwt, prec, maxiter, mchpr, wk, info)
      integer nxis, nxi, nobs, cntsum, cnt(*), nqd, maxiter, info
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,*), qdwt(*)
     *, prec, mchpr, wk(*)
      integer imrs, iwt, ifit, imu, iv, ijpvt, icdnew, iwtnew, ifitnew, 
     *iwk
      imrs = 1
      iwt = imrs + max0 (nxis, 3)
      ifit = iwt + nqd
      imu = ifit + nobs
      iv = imu + nxis
      ijpvt = iv + nxis*nxis
      icdnew = ijpvt + nxis
      iwtnew = icdnew + nxis
      ifitnew = iwtnew + nqd
      iwk = ifitnew + nobs
      call dnewton1 (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs, nqd,
     * qdwt, prec, maxiter, mchpr, wk(imrs), wk(iwt), wk(ifit), wk(imu),
     * wk(iv), wk(ijpvt), wk(icdnew), wk(iwtnew), wk(ifitnew), wk(iwk), 
     * info)
      return
      end
      
C Output from Public domain Ratfor, version 1.0
      subroutine dnewton1 (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, 
     * qdrs, nqd, qdwt, prec, maxiter, mchpr, mrs, wt, fit, mu, v, jpvt, 
     * cdnew, wtnew, fitnew, wk, info)
      integer nxis, nxi, nobs, cntsum, cnt(*), nqd, maxiter, jpvt(*), in
     *fo
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,*), 
     * qdwt(*), prec, mchpr, mrs(*), wt(*), fit(*), mu(*), v(nxis,*),  
     * cdnew(*), wtnew(*), fitnew(*), wk(*)
      integer i, j, k, iter, flag, rkv, idamax, infowk
      double precision wtsum, tmp, ddot, fitmean, lkhd, mumax, wtsumnew,
     * lkhdnew, disc, disc0, trc
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
      wtsum = 0.d0
      i=1
23011 if(.not.(i.le.nqd))goto 23013
      wt(i) = qdwt(i) * dexp (ddot (nxis, qdrs(i,1), nqd, cd, 1))
      wtsum = wtsum + wt(i)
23012 i=i+1
      goto 23011
23013 continue
      fitmean = 0.d0
      i=1
23014 if(.not.(i.le.nobs))goto 23016
      tmp = ddot (nxis, rs(1,i), 1, cd, 1) - dlog (wtsum)
      fit(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23015 i=i+1
      goto 23014
23016 continue
      if(cntsum.eq.0)then
      fitmean = fitmean / dfloat (nobs)
      else
      fitmean = fitmean / dfloat (cntsum)
      endif
      call dsymv ('u', nxi, 1.d0, q, nxi, cd, 1, 0.d0, wk, 1)
      lkhd = ddot (nxi, cd, 1, wk, 1) / 2.d0 - fitmean
      iter = 0
      flag = 0
23021 continue
      iter = iter + 1
      i=1
23024 if(.not.(i.le.nxis))goto 23026
      mu(i) = - ddot (nqd, wt, 1, qdrs(1,i), 1) / wtsum
23025 i=i+1
      goto 23024
23026 continue
      i=1
23027 if(.not.(i.le.nxis))goto 23029
      j=i
23030 if(.not.(j.le.nxis))goto 23032
      v(i,j) = 0.d0
      k=1
23033 if(.not.(k.le.nqd))goto 23035
      v(i,j) = v(i,j) + wt(k) * qdrs(k,i) * qdrs(k,j)
23034 k=k+1
      goto 23033
23035 continue
      v(i,j) = v(i,j) / wtsum - mu(i) * mu(j)
      if(j.le.nxi)then
      v(i,j) = v(i,j) + q(i,j)
      endif
23031 j=j+1
      goto 23030
23032 continue
23028 i=i+1
      goto 23027
23029 continue
      call daxpy (nxis, 1.d0, mrs, 1, mu, 1)
      call dsymv ('u', nxi, -1.d0, q, nxi, cd, 1, 1.d0, mu, 1)
      mumax = dabs(mu(idamax(nxis, mu, 1)))
      i=1
23038 if(.not.(i.le.nxis))goto 23040
      jpvt(i) = 0
23039 i=i+1
      goto 23038
23040 continue
      call dchdc (v, nxis, nxis, wk, jpvt, 1, rkv)
23041 if(v(rkv,rkv).lt.v(1,1)*dsqrt(mchpr))then
      rkv = rkv - 1
      goto 23041
      endif
23042 continue
      i=rkv+1
23043 if(.not.(i.le.nxis))goto 23045
      v(i,i) = v(1,1)
      call dset (i-rkv-1, 0.d0, v(rkv+1,i), 1)
23044 i=i+1
      goto 23043
23045 continue
23046 continue
      call dcopy (nxis, mu, 1, cdnew, 1)
      call dprmut (cdnew, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, cdnew, 11, infowk)
      call dset (nxis-rkv, 0.d0, cdnew(rkv+1), 1)
      call dtrsl (v, nxis, nxis, cdnew, 01, infowk)
      call dprmut (cdnew, nxis, jpvt, 1)
      call daxpy (nxis, 1.d0, cd, 1, cdnew, 1)
      wtsumnew = 0.d0
      i=1
23049 if(.not.(i.le.nqd))goto 23051
      wtnew(i) = qdwt(i) * dexp (ddot (nxis, qdrs(i,1), nqd, cdnew, 1))
      wtsumnew = wtsumnew + wtnew(i)
23050 i=i+1
      goto 23049
23051 continue
      fitmean = 0.d0
      i=1
23052 if(.not.(i.le.nobs))goto 23054
      tmp = ddot (nxis, rs(1,i), 1, cdnew, 1) - dlog (wtsumnew)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23054
      endif
      fitnew(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23053 i=i+1
      goto 23052
23054 continue
      if(cntsum.eq.0)then
      fitmean = fitmean / dfloat (nobs)
      else
      fitmean = fitmean / dfloat (cntsum)
      endif
      call dsymv ('u', nxi, 1.d0, q, nxi, cdnew, 1, 0.d0, wk, 1)
      lkhdnew = ddot (nxi, cdnew, 1, wk, 1) / 2.d0 - fitmean
      if(flag.eq.1)then
      call dset (nxis, 0.d0, cd, 1)
      wtsum = 0.d0
      i=1
23063 if(.not.(i.le.nqd))goto 23065
      wt(i) = qdwt(i)
      wtsum = wtsum + wt(i)
23064 i=i+1
      goto 23063
23065 continue
      call dset (nobs, 1.d0/wtsum, fit, 1)
      fitmean = - dlog (wtsum)
      lkhd = - fitmean
      iter = 0
      goto 23048
      endif
      if(flag.eq.3)then
      goto 23048
      endif
      if(lkhdnew-lkhd.lt.1.d1*(1.d0+dabs(lkhd))*mchpr)then
      goto 23048
      endif
      call dscal (nxis, .5d0, mu, 1)
      if(dabs(mu(idamax(nxis, mu, 1))/mumax).lt.1.d1*mchpr)then
      goto 23048
      endif
23047 goto 23046
23048 continue
      if(flag.eq.1)then
      flag = 2
      goto 23022
      endif
      if(flag.eq.3)then
      info = 1
      return
      endif
      disc = 0.d0
      i=1
23076 if(.not.(i.le.nqd))goto 23078
      disc = dmax1 (disc, dabs(wt(i)-wtnew(i))/(1.d0+dabs(wt(i))))
23077 i=i+1
      goto 23076
23078 continue
      i=1
23079 if(.not.(i.le.nobs))goto 23081
      disc = dmax1 (disc, dabs(fit(i)-fitnew(i))/(1.d0+dabs(fit(i))))
23080 i=i+1
      goto 23079
23081 continue
      disc = dmax1 (disc, (mumax/(1.d0+dabs(lkhd)))**2)
      disc0 = dmax1 ((mumax/(1.d0+lkhd))**2, dabs(lkhd-lkhdnew)/(1+dabs(
     *lkhd)))
      call dcopy (nxis, cdnew, 1, cd, 1)
      call dcopy (nqd, wtnew, 1, wt, 1)
      wtsum = wtsumnew
      call dcopy (nobs, fitnew, 1, fit, 1)
      lkhd = lkhdnew
      if(disc0.lt.prec)then
      goto 23023
      endif
      if(disc.lt.prec)then
      goto 23023
      endif
      if(iter.lt.maxiter)then
      goto 23022
      endif
      if(flag.eq.0)then
      call dset (nxis, 0.d0, cd, 1)
      wtsum = 0.d0
      i=1
23090 if(.not.(i.le.nqd))goto 23092
      wt(i) = qdwt(i)
      wtsum = wtsum + wt(i)
23091 i=i+1
      goto 23090
23092 continue
      call dset (nobs, 1.d0/wtsum, fit, 1)
      fitmean = - dlog (wtsum)
      lkhd = - fitmean
      iter = 0
      flag = 2
      else
      info = 2
      goto 23023
      endif
23022 goto 23021
23023 continue
      i=1
23093 if(.not.(i.le.nobs))goto 23095
      call daxpy (nxis, -1.d0, mrs, 1, rs(1,i), 1)
      call dprmut (rs(1,i), nxis, jpvt, 0)
      if(cntsum.ne.0)then
      call dscal (nxis, dsqrt(dfloat(cnt(i))), rs(1,i), 1)
      endif
      call dtrsl (v, nxis, nxis, rs(1,i), 11, infowk)
23094 i=i+1
      goto 23093
23095 continue
      trc = ddot (nobs*nxis, rs, 1, rs, 1)
      if(cntsum.eq.0)then
      trc = trc / dfloat(nobs) / (dfloat(nobs)-1.d0)
      lkhd = 0.d0
      i=1
23100 if(.not.(i.le.nobs))goto 23102
      lkhd = lkhd + dlog (fit(i))
23101 i=i+1
      goto 23100
23102 continue
      lkhd = lkhd / dfloat (nobs)
      else
      trc = trc / dfloat(cntsum) / (dfloat(cntsum)-1.d0)
      lkhd = 0.d0
      i=1
23103 if(.not.(i.le.nobs))goto 23105
      lkhd = lkhd + dfloat (cnt(i)) * dlog (fit(i))
23104 i=i+1
      goto 23103
23105 continue
      lkhd = lkhd / dfloat (cntsum)
      endif
      mrs(1) = lkhd
      mrs(2) = trc
      mrs(3) = wtsum
      return
      end


C ==============================================================================

      subroutine dprmut (x,npar,jpvt,job)
      integer npar,jpvt(npar),job
      double precision x(npar)
c
c Purpose: permute the elements of the array x according to the index 
c   vector jpvt (either forward or backward permutation).
c
c On Entry:
c   x(npar)     array to be permuted
c   npar        size of x (and jpvt)
c   jpvt        indices of the permutation
c   job         indicator of forward or backward permutation
c           if job = 0 forward permutation  
c               x(jpvt(i)) moved to x(i)
c           if job is nonzero backward permutation 
c               x(i) moved to x(jpvt(i))
c On Exit:
c   x(npar)     array with permuted entries
c
c   Written:    Yin Ling    U. of Maryland, August,1978
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
c       forward permutation
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
c           backward permutation
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


C ==============================================================================
      
 
      subroutine  dset(n,da,dx,incx)
      integer n,incx
      double precision da,dx(*)
c
c Purpose : set vector dx to constant da. Unrolled loops are used for 
c   increment equal to one.
c
c On Entry:
c   n           length of dx
c   da          any constant
c   incx        increment for dx
c
c On Exit:
c   dx(n)       vector with all n entries set to da
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
     
      
C ==============================================================================

