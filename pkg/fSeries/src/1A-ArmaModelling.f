
C FRACDIFF Fortran Code


C ##############################################################################
C FRACDIFF-fdcore


      subroutine fracdf( x, n, M, nar, nma, dtol, drange,
     *                   hood, d, ar, ma, w, lenw, inform,
     *                   flmin, flmax, epmin, epmax)

      integer            n, M, nar, nma, lenw, inform
c     real               x(n)
      double precision   x(n)
      double precision   d, dtol, hood
      double precision   ar(nar), ma(nma), drange(2)
c     double precision   ar(*), ma(*), drange(2)
      double precision   w(lenw)
c     double precision   w(*)
      double precision   flmin, flmax, epmin, epmax

c------------------------------------------------------------------------------
c
c   Input :
c
c  x       double   time series for the ARIMA model
c  n       integer  length of the time series
c  M       integer  number of terms in the likelihood approximation
c                   suggested value 100 (see Haslett and Raftery 1989)
c  nar     integer  number of autoregressive parameters
c  nma     integer  number of moving average parameters
c  dtol    double   desired length of final interval of uncertainty for d
c                   suggested value : 4th root of machine precision
c                   if dtol < 0 it is automatically set to this value
c                   dtol will be altered if necessary by the program
c  drange  double   array of length 2 giving minimum and maximum values f
c                   for the fractional differencing parameter
c  d       double   initial guess for optimal fractional differencing parameter
c  w       double   work array
c  lenw    integer  length of double precision workspace w, must be at least
c  max( p+q+2*(n+M), 3*n+(n+6.5)*(p+q)+1,(3+2*(p+q+1))*(p+q+1)+1)
c
c  Output :
c
c  dtol    double   value of dtol ultimately used by the algorithm
c  d       double   final value optimal fractional differencing parameter
c  hood    double   logarithm of the maximum likelihood
c  ar      double   optimal autoregressive parameters
c  ma      double   optimal moving average parameters
c
c------------------------------------------------------------------------------

      integer ilim, lfree, minpq
      double precision   dopt, delta

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      integer            nn, MM, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    nn, MM, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer            maxopt,maxfun,nopt,nfun,ngrd,ifun,igrd,info
      common /CNTRFD/    maxopt,maxfun,nopt,nfun,ngrd,ifun,igrd,info
      save   /CNTRFD/

      double precision   told, tolf, tolx, tolg, anorm, deltax, gnorm
      common /TOLSFD/    told, tolf, tolx, tolg, anorm, deltax, gnorm
      save   /TOLSFD/

      integer            lenthw, lwfree
      common /WORKFD/    lenthw, lwfree
      save   /WORKFD/

      integer            ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer            lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

      integer            ILIMIT, JLIMIT
      common /LIMSFD/    ILIMIT, JLIMIT
      save   /LIMSFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

      integer            IMINPK, JMINPK
      common /MNPKFD/    IMINPK, JMINPK
      save   /MNPKFD/

      integer            KSVD, KCOV, KCOR
      common /HESSFD/    KSVD, KCOV, KCOR
      save   /HESSFD/

      double precision   zero, one
      parameter         (zero=0.d0, one=1.d0)

c  copyright 1991 Department of Statistics, University of Washington
c  written by Chris Fraley

c-----------------------------------------------------------------------------

c machine constants

      FLTMIN = flmin
      FLTMAX = flmax
      EPSMIN = epmin
      EPSMAX = epmax
      EPSPT5 = sqrt(EPSMIN)
      EPSP25 = sqrt(EPSPT5)
      EPSPT3 = EPSMIN**(.3)
      EPSP75 = EPSMIN**(.75)
      BIGNUM = one / EPSMIN

c set error and warning flags

      inform = 0

      IGAMMA = 0
      IMINPK = 0
      ILIMIT = 0

      JGAMMA = 0
      JMINPK = 0
      JLIMIT = 0

c useful quantities

      if (M .le. 0) M = 100

      nn    = n
      MM    = M
      np    = nar
      nq    = nma

      npq    = np + nq
      npq1   = npq + 1
      maxpq  = max(np,nq)
      minpq  = min(np,nq)
      maxpq1 = maxpq + 1

      maxopt = 100
      maxfun = 100

      if (dtol .gt. .1d0)  dtol = .1d0

      if ( dtol .le. zero) then
        told  =  EPSP25
        tolf  =  EPSPT3
      else
        told  =  max( dtol, EPSPT5)
        tolf  =  max( dtol/10d0, EPSP75)
      end if

      tolg   = tolf
      tolx   = told
      dtol   = told

      nm     = n - maxpq

c workspace allocation

      lqp    = 1
      ly     = lqp    +  npq
      lamk   = ly
      lak    = lamk   +  n
      lphi   = lak    +  n
      lvk    = lphi   +  M
      lpi    = lphi
      la     = ly     +  n
      lajac  = la     +  n - minpq
      ipvt   = lajac  +  max( (n-np)*np, (n-nq)*nq, (n-maxpq)*npq)
      ldiag  = ipvt   +  npq/2 + 1
      lqtf   = ldiag  +  npq
      lwa1   = lqtf   +  npq
      lwa2   = lwa1   +  npq
      lwa3   = lwa2   +  npq
      lwa4   = lwa3   +  npq
      lfree  = lwa4   +  n - minpq

      lwfree = max( (lvk+M), (lwa4+n-minpq), (12*31))
      lenthw = lenw

      if (lwfree  .gt. (lenw+1)) then
        ILIMIT = lwfree - lenw
        ilim   = ILIMIT
c       write( 6, *) 'insufficient storage : ',
c    *               'increase length of w by at least', incw
        inform = 1
        return
      endif

c     if (npq .ne. 0) call dcopy( npq, zero, 0, w(lqp), 1)

      if (npq .ne. 0) then
        call dcopy( np, ar, 1, w(lqp+nq), 1)
        call dcopy( nq, ma, 1, w(lqp)   , 1)
      end if

      nopt = 0
      nfun = 0
      ngrd = 0

      d = dopt( x, d, drange, hood, delta, w)

      if (nopt .ge. maxopt) JLIMIT = 1
c       write( 6, *)
c       write( 6, *) 'WARNING : optimization limit reached'
c     end if

      if (IGAMMA .ne. 0 .or. IMINPK .ne. 0) then
        d    = FLTMAX
        hood = FLTMAX
        call dcopy( np, FLTMAX, 0, ar, 1)
        call dcopy( nq, FLTMAX, 0, ma, 1)
        if (IGAMMA .ne. 0) inform = 2
        if (IMINPK .ne. 0) inform = 3
        return
      end if

      call dcopy( np, w(lqp+nq), 1, ar, 1)
      call dcopy( nq, w(lqp   ), 1, ma, 1)

      if (JGAMMA .ne. 0) inform = 4
      if (JMINPK .ne. 0) inform = 5
      if (JLIMIT .ne. 0) inform = 6

      return
c 900  format( 4h itr, 14h     d          ,   14h    est mean  ,
c     *                16h     white noise,  17h     log likelihd,
c     *                 4h  nf, 3h ng)
      end
c     fracdf() {main}

*******************************************************************************
*******************************************************************************
c
c optimization with respect to d based on Brent's fmin algorithm
c
      double precision function dopt( x, dinit, drange, hood, delta, w)

c     real              x(n)
      double precision  x(*)
      double precision  dinit, drange(2), hood, delta
      double precision  w(*)

      double precision  pqopt
      double precision  d, dd, ee, hh, rr, ss, tt
      double precision  uu, vv, ww, fu, fv, fw
      double precision  eps, tol, tol1, tol2, tol3

      intrinsic         abs, sqrt

      double precision  cc

      double precision  zero, half, one, two, three
      parameter        (zero=0.d0, half=.5d0, one=1.d0,
     *                   two=2.d0, three=3.d0)

      integer           maxopt, maxfun, nopt, nfun, ngrd,
     *                  ifun, igrd, info
      common /CNTRFD/   maxopt, maxfun, nopt, nfun, ngrd,
     *                  ifun, igrd, info
      save   /CNTRFD/

      integer            n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      double precision   aa, xx, bb, fa, fx, fb
      save               aa, xx, bb, fa, fx, fb

      integer            lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

      double precision   hatmu, wnv, cllf
      common /FILTFD/    hatmu, wnv, cllf
      save   /FILTFD/

      double precision  dtol, ftol, xtol, gtol, anorm, deltax, gnorm
      common /TOLSFD/   dtol, ftol, xtol, gtol, anorm, deltax, gnorm
      save   /TOLSFD/

      double precision  FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

      integer            IMINPK, JMINPK
      common /MNPKFD/    IMINPK, JMINPK
      save   /MNPKFD/

c  copyright 1991 Department of Statistics, University of Washington
c  written by Chris Fraley

c------------------------------------------------------------------------------
c
c  cc is the squared inverse of the golden ratio (see data statement)
c
c     cc = half*(three-sqrt(5.0d0))
      data cc /.38196601125011d0/
c
c  eps is approximately the square root of the relative machine
c  precision.
c
      eps  =  EPSMAX
      tol1 =  one + eps
      eps  =  sqrt(eps)
c -Wall:
      dopt = -1d0
      dd = 0d0
c
      aa   =  drange(1)
      bb   =  drange(2)
      if (dinit .gt. (aa + dtol) .and. dinit .lt. (bb - dtol)) then
        vv = dinit
      else
        vv = aa + cc*(bb-aa)
      end if
      ww   =  vv
      xx   =  vv
      uu   =  xx
      ee   =  zero

      nopt = 1

      fx   =  pqopt( x, xx, w)

      fv   =  fx
      fw   =  fx

      tol  = max(dtol,zero)
      tol3 = tol/three
c
c  main loop starts here
c
   10 continue

      if (IGAMMA .ne. 0 .or. IMINPK .ne. 0) then
        d    = uu
        hood = FLTMAX
        return
      end if

      hh   =  half*(aa+bb)
      tol1 =  eps*(one+abs(xx)) + tol3
      tol2 =  two*tol1
c
c  check stopping criterion
c
      delta = abs(xx-hh) + half*(bb-aa)
c     if (abs(xx-hh) .le. (tol2-half*(bb-aa))) go to 100
      if (delta .le. tol2) go to 100

      if (nopt .ge. maxopt) go to 100

c     if (delpq .le. EPSMAX*(one+pqnorm)) go to 100

      rr   =  zero
      ss   =  zero
      tt   =  zero

      if (abs(ee) .gt. tol1) then
c
c  fit parabola
c
        rr   = (xx-ww)*(fx-fv)
        ss   = (xx-vv)*(fx-fw)
        tt   = (xx-vv)*ss-(xx-ww)*rr
        ss   =  two*(ss-rr)
        if (ss .le. zero) then
          ss = -ss
        else
          tt = -tt
        end if
        rr   =  ee
        ee   =  dd
      end if

      if ((abs(tt) .ge. abs(half*ss*rr)) .or.
     *    (tt .le. ss*(aa-xx)) .or. (tt .ge. ss*(bb-xx))) then
c
c  a golden-section step
c
        if (xx .ge. hh) then
          ee = aa - xx
        else
          ee = bb - xx
        end if
        dd   =  cc*ee

      else
c
c  a parabolic-interpolation step
c
        dd   =  tt / ss
        uu   =  xx + dd
c
c  f must not be evaluated too close to aa or bb
c
        if (((uu-aa) .lt. tol2) .or. ((bb-uu) .lt. tol2)) then
          dd  =  tol1
          if (xx .ge. hh) dd = -dd
        end if
      end if
c
c  f must not be evaluated too close to xx
c
      if (abs(dd) .ge. tol1)  then
        uu = xx + dd
      else
        if (dd .le. zero) then
          uu = xx - tol1
        else
          uu = xx + tol1
        end if
      end if

      nopt = nopt + 1

      fu   =  pqopt( x, uu, w)
c
c  update  aa, bb, vv, ww, and xx
c
      if (fx .ge. fu) then
        if (uu .ge. xx) then
          aa = xx
          fa = fx
        else
          bb = xx
          fb = fx
        end if
        vv   =  ww
        fv   =  fw
        ww   =  xx
        fw   =  fx
        xx   =  uu
        fx   =  fu
      else
        if (uu .ge. xx) then
          bb = uu
          fb = fu
        else
          aa = uu
          fa = fu
        end if
        if ((fu .gt. fw) .and. (ww .ne. xx)) then
          if ((fu .le. fv) .or. (vv .eq. xx) .or. (vv .eq. ww)) then
             vv   =  uu
             fv   =  fu
          end if
        else
          vv   =  ww
          fv   =  fw
          ww   =  uu
          fw   =  fu
        end if
      end if

      go to 10
c
c  end of main loop
c
  100 dopt =  xx
      hood = -fx
      cllf =  hood

      return
c 900  format( i4, 2(1pe14.6), 1pe16.7, 1pe17.8, 1x, 2(i3))
c 901  format( i4, 3(1pe10.2), 1pe11.2, 2(i3), 3(1pe8.1), i2)
      end
c     dopt()

***************************************************************************
***************************************************************************

      double precision function pqopt( x, d, w)

c     real              x(n)
      double precision  x(*)

      double precision  d
      double precision  w(*)

      double precision bic, slogvk
      double precision t, u

      intrinsic        log

      double precision  ddot
c     These are passed to LMDER1() to be optimized:
      external          ajp, ajq, ajqp

      double precision zero, one
      parameter       (zero=0.d0, one=1.d0)

      double precision  hatmu, wnv, hood
      common /FILTFD/   hatmu, wnv, hood
      save   /FILTFD/

      double precision dtol, ftol, xtol, gtol, anorm, deltax, gnorm
      common /TOLSFD/  dtol, ftol, xtol, gtol, anorm, deltax, gnorm
      save   /TOLSFD/

      integer          n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/  n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer          maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      common /CNTRFD/  maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      save   /CNTRFD/

      integer           ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/   ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer           lqp, la, lajac, ipvt, ldiag, lqtf,
     *                  lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/   lqp, la, lajac, ipvt, ldiag, lqtf,
     *                  lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

      integer           modelm
      double precision  factlm

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

      integer            IMINPK, JMINPK
      common /MNPKFD/    IMINPK, JMINPK
      save   /MNPKFD/

      data              modelm/1/, factlm /100.d0/

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c----------------------------------------------------------------------------

        call fdfilt( x, d, w(ly), slogvk,
     *               w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

        if (IGAMMA .ne. 0) then
          pqopt  =  FLTMAX
          wnv    =  FLTMAX
          hood   = -FLTMAX
          return
        end if

        t = dble(n)

        if (npq .eq. 0) then
          wnv   = ddot( n, w(ly), 1, w(ly), 1) / t
          ifun  =  0
          igrd  =  0
          info  = -1
          goto 100
        endif
c
c optimize as an unconstrained optimization problem
c
         if (modelm .eq. 2) call dcopy( npq,  one, 0, w(ldiag), 1)

         if (nopt .lt. 0) then
           if (np .ne. 0) then
             call LMDER1( ajp, n-np, np, w(lqp+nq),w(la),w(lajac), n-np,
     *                    ftol, xtol, gtol, maxfun, w(ldiag), modelm,
     *                    factlm, info, ifun, igrd, w(ipvt), w(lqtf),
     *                    w(lwa1), w(lwa2), w(lwa3), w(lwa4), w(ly))
           end if
           if (nq .ne. 0) then
             call LMDER1( ajq, n-nq, nq, w(lqp),w(la),w(lajac), n-nq,
     *                    ftol, xtol, gtol, maxfun, w(ldiag), modelm,
     *                    factlm, info, ifun, igrd, w(ipvt), w(lqtf),
     *                    w(lwa1), w(lwa2), w(lwa3), w(lwa4), w(ly))
           end if
         end if

         call LMDER1( ajqp, nm, npq, w(lqp), w(la), w(lajac), nm,
     *                ftol, xtol, gtol, maxfun, w(ldiag), modelm,
     *                factlm, info, ifun, igrd, w(ipvt), w(lqtf),
     *                w(lwa1), w(lwa2), w(lwa3), w(lwa4), w(ly))

        if (info .eq. 0) then
c         write( 6, *) 'MINPACK : improper input parameters
          IMINPK = 10
          pqopt  =  FLTMAX
          wnv    =  FLTMAX
          hood   = -FLTMAX
          return
        end if

        if (info .eq. 5) then
c         write( 6, *) 'MINPACK : function evaluation limit reached'
          JMINPK = 5
        end if

        if (info .eq. 6 ) then
c         write( 6, *) 'MINPACK : ftol is too small'
          JMINPK = 6
        end if

        if (info .eq. 7) then
c         write( 6, *) 'MINPACK : xtol is too small'
          JMINPK = 7
        end if

        if (info .eq. 8) then
c         write( 6, *) 'MINPACK : gtol is too small'
          JMINPK = 8
        end if

c        call daxpy( npq, (-one), w(lpq), 1, w(lqp), 1
c        delpq  = sqrt(ddot( npq, w(lqp), 1, w(lqp), 1))
c        pqnorm = sqrt(ddot( npq, w(lpq), 1, w(lpq), 1))

        wnv   =  (anorm*anorm) / dble(nm-1)
 100    u     = (t*(2.8378d0+log(wnv))+slogvk)
        pqopt =  u / 2.d0
        bic   =  u + dble(np+nq+1)*log(t)
        hood  = -pqopt

      return
      end
c     pqopt()

***************************************************************************
***************************************************************************

      subroutine fdfilt( x, d, y, slogvk, amk, ak, vk, phi, pi)

c     real              x(n)
      double precision  x(*)
      double precision  d, slogvk
c     double precision  y(n), amk(n), ak(n)
      double precision  y(*), amk(*), ak(*)
c     double precision  vk(M), phi(M), pi(M)
      double precision  vk(*), phi(*), pi(*)

c**************************************************************************
c input  :
c          x       real    original time series
c          d       double  estimated value of d
c output :
c          y       double  filtered series
c          slogvk  double  the sum of the logarithms of the vk
c notes  :
c          y can use the same storage as either ak or amk
c          phi and pi can use the same storage
c          can be arranged so that phi, pi and vk share the same storage
c**************************************************************************

      integer           j, k, km, mcap, mcap1
      double precision  g0, r, s, t, u, v, z, sumlog

      double precision  zero, one, two
      parameter        (zero=0.d0, one=1.d0, two=2.d0)

      double precision  dgamma, dgamr

      intrinsic         log, sqrt

      double precision  hatmu, wnv, cllf
      common /FILTFD/   hatmu, wnv, cllf
      save   /FILTFD/

      integer           n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/   n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c-----------------------------------------------------------------------

        mcap  = min(M,n)
        mcap1 = mcap + 1
c
c calculate amk(k), vk(k), and ak(k) for k=1,n (see W522-4 for notation).
c
c
c  k = 1
c
        amk(1) = zero
        ak(1)  = one
c
c  k = 2 ;  initialize phi(1)
c
        z      = d/(one-d)
        amk(2) = z*dble(x(1))
        ak(2)  = one - z
        phi(1) = z

        t  = dgamr(one-d)
        if (IGAMMA .ne. 0) return

        g0 = dgamma(one-(two*d))*(t*t)
        if (IGAMMA .ne. 0) return

        vk(1)  = g0
        vk(2)  = g0*(one-(z*z))
c
c  k = 3, mcap
c
        do k = 3, mcap
          km = k - 1
          t  = dble(km)
          u  = t - d
c
c  calculate phi() and vk() using the recursion formula on W498
c
        do j = 1, km-1
            s      = t-dble(j)
        phi(j) = phi(j)*(t*(s-d)/(u*s))
          end do

          v       = d / u
          phi(km) = v
          vk(k)   = vk(km)*(one-(v*v))
c
c  form amk(k) and ak(k)
c
      u = zero
      v =  one
      do j = 1, km
            t  = phi(j)
        u  = u + (t*dble(x(k-j)))
        v  = v - t
          end do
          amk(k) = u
          ak(k)  = v
        end do

        if (mcap .eq. n) go to 200
c
c  k = mcap+1, n
c
c calculate pi(j), j = 1,mcap
c
        pi(1) = d
        s     = d
        do j = 2, mcap
          u     = dble(j)
          t     = pi(j-1)*((u-one-d)/u)
        s     = s + t
          pi(j) = t
        end do

        s =  one - s
        r = zero
        u = dble(mcap)
        t = u*pi(mcap)
c
      do  k = mcap1, n
      km = k - mcap
          z  = zero
      do  j = 1, mcap
         z = z + (pi(j)*dble(x(k-j)))
          end do
          if (r .eq. zero) then
            amk(k) = z
            ak(k)  = s
          else
            v      = (t*(one - (u/dble(k))**d))/d
        amk(k) = z + ((v*r)/(dble(km)-one))
            ak(k)  = s - v
          end if
          r = r + dble(x(km))
        end do

 200    continue
c
c  form muhat - see formula on W523.
c
        r = zero
        s = zero
        do  k = 1, n
           t = ak(k)
       u = (dble(x(k))-amk(k))*t
       v = t*t
           if (k .le. mcap) then
             z = vk(k)
             u = u / z
             v = v / z
           end if
           r = r + u
           s = s + v
        end do

        hatmu = r / s
c
c  form filtered version
c
        s = zero
        do k= 1, mcap
      s = s + log(vk(k))
        end do

        slogvk = s
        sumlog = s

        s = zero
        do k= 1, n
          t    = (dble(x(k))-amk(k)-hatmu*ak(k))
          if (k .le. mcap) t = t / sqrt(vk(k))
      s    = s + t
          y(k) = t
        end do

        if (npq .eq. 0) return

        t = dble(n)

        u = z / t
        do k= 1, n
          y(k) = y(k) - u
        end do

        return
        end

*****************************************************************************

      subroutine ajqp( qp, a, ajac, lajac, iflag, y)

      integer          lajac, iflag
c     double precision qp(npq), a(nm), ajac(nm,npq), y(n)
      double precision qp(*), a(*), ajac(lajac,*), y(*)

      integer          i,k,km,l

      integer          maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      common /CNTRFD/  maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      save   /CNTRFD/

      integer          n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/  n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      double precision s, t

      double precision   zero, one
      parameter         (zero=0.d0, one=1.d0)

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c--------------------------------------------------------------------------

        if (iflag .eq. 2) goto 200

        if (iflag .ne. 1) return
c
c  objective calculation
c
        do k = maxpq1, n
          km = k - maxpq
          t  = zero
          if (np .ne. 0) then
            do l = 1, np
              t  = t - qp(nq+l)*y(k-l)
            end do
          end if
          s = zero
          if (nq .ne. 0) then
            do l = 1, nq
              if (km .le. l) goto 101
              s  = s + qp(l)*a(km-l)
            end do
          end if
 101      s = y(k) + (t + s)
          if (abs(s) .le. BIGNUM) then
            a(km) = s
          else
            a(km) = sign(one,s)*BIGNUM
          end if
        end do

        nfun = nfun + 1

        return

 200    continue
c
c  jacobian calculation
c
        do i = 1, npq
          do k = maxpq1, n
            km  =  k - maxpq
            t   = zero
            if (nq .ne. 0) then
              do l = 1, nq
                if (km .le. l) goto 201
                t  = t +  qp(l)*ajac(km-l,i)
              end do
            end if
 201        continue
            if (i .le. nq) then
              if (km .gt. i) then
                s = a(km-i) + t
              else
                s =           t
              end if
            else
              s = -y(k-(i-nq)) + t
            end if
            if (abs(s) .le. BIGNUM) then
              ajac(km,i) = s
            else
              ajac(km,i) = sign(one,s)*BIGNUM
            end if
          end do
        end do

        ngrd = ngrd + 1

      return
      end

*****************************************************************************
*****************************************************************************

      subroutine  ajp( p, a, ajac, lajac, iflag, y)

      integer          lajac, iflag
c     double precision p(np), a(nm), ajac(nm,npq), y(n)
      double precision p(*), a(*), ajac(lajac,*), y(*)

      integer          n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/  n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer i,k,l
      double precision  t

      double precision zero
      parameter       (zero=0.d0)

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c--------------------------------------------------------------------------

        if (iflag .eq. 2) goto 200

        if (iflag .ne. 1) return

        if (np .eq. 0) return
c
c  objective calculation
c
        do k = np+1, n
          t  = zero
          do l = 1, np
            t  = t - p(l)*y(k-l)
          end do
 101      a(k-np) = y(k) + t
        end do

        return

 200    continue
c
c  jacobian calculation
c
          do i = 1, np
            do k = np+1, n
              ajac(k-np,i) = -y(k-i)
            end do
          end do

      return
      end

*****************************************************************************
*****************************************************************************

      subroutine  ajq( qp, a, ajac, lajac, iflag, y)

      integer          lajac, iflag
c     double precision qp(npq), a(nm), ajac(nm,npq), y(n)
      double precision qp(*), a(*), ajac(lajac,*), y(*)

      integer  i,k,km,l

      integer          n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/  n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer          maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      common /CNTRFD/  maxopt, maxfun, nopt, nfun, ngrd,
     *                 ifun, igrd, info
      save   /CNTRFD/

      double precision s, t

      double precision zero
      parameter       (zero=0.d0)

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c--------------------------------------------------------------------------

        if (iflag .eq. 2) goto 200

        if (iflag .ne. 1) return

        if (nq. eq. 0) return
c
c  objective calculation
c
        do k = maxpq1, n
          km = k - maxpq
          t  = zero
          if (np .ne. 0) then
            do l = 1, np
              t  = t - qp(nq+l)*y(k-l)
            end do
          end if
          s = zero
          if (nq .ne. 0) then
            do l = 1, nq
              if (km .le. l) goto 101
              s  = s + qp(l)*a(km-l)
            end do
          end if
 101      a(km) = y(k) + (t + s)
        end do

        nfun = nfun + 1

        return

 200    continue
c
c  jacobian calculation
c
        do i = 1, npq
          do k = maxpq1, n
            km  =  k - maxpq
            t   = zero
            if (nq .ne. 0) then
              do l = 1, nq
                if (km .le. l) goto 201
                t  = t +  qp(l)*ajac(km-l,i)
              end do
            end if
 201        continue
            if (i .le. nq) then
              if (km .gt. i) then
                ajac(km,i) = a(km-i)    + t
              else
                ajac(km,i) =              t
              end if
            else
              ajac(km,i) = -y(k-(i-nq)) + t
            end if
          end do
        end do

        ngrd = ngrd + 1

      return
      end

C ##############################################################################
C FRACDIFF-fdgam


      double precision function dgamma (x)
c     jan 1984 edition.  w. fullerton, c3, los alamos scientific lab.
C     double precision x, gamcs(42), dxrel, pi, sinpiy, sq2pil, xmax,
C     1  xmin, y, d9lgmc, dcsevl, d1mach, dexp, dint, dlog,
C     2  dsin, dsqrt

      double precision x
      double precision gamcs(42), dxrel, pi, sinpiy, sq2pil, xmax,
     1     xmin, xsml, y, temp
      integer ngam, n, i

      double precision d9lgmc, dcsevl
      integer initds
C     external d1mach, d9lgmc, dcsevl, dexp, dint, dlog, dsin, dsqrt,
C     1  initds
      external d9lgmc, dcsevl, initds

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
c     series for gam        on the interval  0.          to  1.00000e+00
c     with weighted error   5.79e-32
c     log weighted error  31.24
c     significant figures required  30.00
c     decimal places required  32.05
c
      data gamcs(  1) / +.8571195590 9893314219 2006239994 2 d-2      /
      data gamcs(  2) / +.4415381324 8410067571 9131577165 2 d-2      /
      data gamcs(  3) / +.5685043681 5993633786 3266458878 9 d-1      /
      data gamcs(  4) / -.4219835396 4185605010 1250018662 4 d-2      /
      data gamcs(  5) / +.1326808181 2124602205 8400679635 2 d-2      /
      data gamcs(  6) / -.1893024529 7988804325 2394702388 6 d-3      /
      data gamcs(  7) / +.3606925327 4412452565 7808221722 5 d-4      /
      data gamcs(  8) / -.6056761904 4608642184 8554829036 5 d-5      /
      data gamcs(  9) / +.1055829546 3022833447 3182350909 3 d-5      /
      data gamcs( 10) / -.1811967365 5423840482 9185589116 6 d-6      /
      data gamcs( 11) / +.3117724964 7153222777 9025459316 9 d-7      /
      data gamcs( 12) / -.5354219639 0196871408 7408102434 7 d-8      /
      data gamcs( 13) / +.9193275519 8595889468 8778682594 0 d-9      /
      data gamcs( 14) / -.1577941280 2883397617 6742327395 3 d-9      /
      data gamcs( 15) / +.2707980622 9349545432 6654043308 9 d-10     /
      data gamcs( 16) / -.4646818653 8257301440 8166105893 3 d-11     /
      data gamcs( 17) / +.7973350192 0074196564 6076717535 9 d-12     /
      data gamcs( 18) / -.1368078209 8309160257 9949917230 9 d-12     /
      data gamcs( 19) / +.2347319486 5638006572 3347177168 8 d-13     /
      data gamcs( 20) / -.4027432614 9490669327 6657053469 9 d-14     /
      data gamcs( 21) / +.6910051747 3721009121 3833697525 7 d-15     /
      data gamcs( 22) / -.1185584500 2219929070 5238712619 2 d-15     /
      data gamcs( 23) / +.2034148542 4963739552 0102605193 2 d-16     /
      data gamcs( 24) / -.3490054341 7174058492 7401294910 8 d-17     /
      data gamcs( 25) / +.5987993856 4853055671 3505106602 6 d-18     /
      data gamcs( 26) / -.1027378057 8722280744 9006977843 1 d-18     /
      data gamcs( 27) / +.1762702816 0605298249 4275966074 8 d-19     /
      data gamcs( 28) / -.3024320653 7353062609 5877211204 2 d-20     /
      data gamcs( 29) / +.5188914660 2183978397 1783355050 6 d-21     /
      data gamcs( 30) / -.8902770842 4565766924 4925160106 6 d-22     /
      data gamcs( 31) / +.1527474068 4933426022 7459689130 6 d-22     /
      data gamcs( 32) / -.2620731256 1873629002 5732833279 9 d-23     /
      data gamcs( 33) / +.4496464047 8305386703 3104657066 6 d-24     /
      data gamcs( 34) / -.7714712731 3368779117 0390152533 3 d-25     /
      data gamcs( 35) / +.1323635453 1260440364 8657271466 6 d-25     /
      data gamcs( 36) / -.2270999412 9429288167 0231381333 3 d-26     /
      data gamcs( 37) / +.3896418998 0039914493 2081663999 9 d-27     /
      data gamcs( 38) / -.6685198115 1259533277 9212799999 9 d-28     /
      data gamcs( 39) / +.1146998663 1400243843 4761386666 6 d-28     /
      data gamcs( 40) / -.1967938586 3451346772 9510399999 9 d-29     /
      data gamcs( 41) / +.3376448816 5853380903 3489066666 6 d-30     /
      data gamcs( 42) / -.5793070335 7821357846 2549333333 3 d-31     /
c
      data pi / 3.1415926535 8979323846 2643383279 50 d0 /
c     sq2pil is 0.5*alog(2*pi) = alog(sqrt(2*pi))
      data sq2pil / 0.9189385332 0467274178 0329736405 62 d0 /
      data ngam, xmin, xmax, xsml, dxrel / 0, 4*0.d0 /
      dgamma = -999d0
c
      if (ngam.eq.0) then
C        ngam = initds (gamcs, 42, 0.1*sngl(  d1mach) )
         ngam = initds (gamcs, 42, 0.1*sngl(  EPSMIN ) )
c
         call d9gaml (xmin, xmax)
         if (IGAMMA .ne. 0) return
C        xsml = dexp (dmax1 (dlog(d1mach(1)), -dlog(d1mach(2)))+0.01d0)
         xsml =  exp ( max  ( log( FLTMIN  ), - log( FLTMAX  ))+0.01d0)
C        dxrel = dsqrt (d1mach(4))
         dxrel =  sqrt (  EPSMAX )
c
      endif
C     y = dabs(x)
      y =  abs(x)
      if (y .gt. 10.d0) go to 50
c
c     compute gamma(x) for -xbnd .le. x .le. xbnd.  reduce interval and find
c     gamma(1+y) for 0.0 .le. y .lt. 1.0 first of all.
c
      n = int(x)
      if (x.lt.0.d0) n = n - 1
      y = x - dble(float(n))
      n = n - 1
C     dgamma = 0.9375d0 + dcsevl (2.d0*y-1.d0, gamcs, ngam)
      temp = dcsevl (2.d0*y-1.d0, gamcs, ngam)
      if (IGAMMA .ne. 0) return
      dgamma = 0.9375d0 + temp
      if (n.eq.0) return
c
      if (n.gt.0) go to 30
c
c     compute gamma(x) for x .lt. 1.0
c
      n = -n

C     if (x.eq.0.d0) call seteru (14hdgamma  x is 0, 14, 4, 2)
C     if (x.lt.0d0 .and. x+dble(float(n-2)).eq.0.d0) call seteru (
C     1  31hdgamma  x is a negative integer, 31, 4, 2)
C     if (x.lt.(-0.5d0) .and. dabs((x-dint(x-0.5d0))/x).lt.dxrel) call
C     1  seteru (68hdgamma  answer lt half precision because x too near n
C     2egative integer, 68, 1, 1)
C     if (y.lt.xsml) call seteru (
C     1  54hdgamma  x is so close to 0.0 that the result overflows,
C     2  54, 5, 2)

      if (x.eq.0.d0) then
C     write(6,*) 'dgamma : x is 0'
         IGAMMA = 11
         return
      end if

      if (x.lt.0d0 .and. x+dble(float(n-2)).eq.0.d0) then
C     write( 6, *) 'dgamma : x is a negative integer'
         IGAMMA = 12
         return
      end if

      if (x.lt.(-0.5d0) .and. abs((x-dble(int(x-0.5d0)))/x).lt.dxrel)
C     1  write(6,*) 'dgamma : answer lt half precision because
C     2                       x too near a negative integer'
     *     JGAMMA = 11

      if (y.lt.xsml) then
c     write(6,*)  'dgamma :,
c     1               x is so close to 0.0 that the result overflows'
         IGAMMA = 13
         return
      end if
c
      do 20 i=1,n
         dgamma = dgamma/(x+dble(float(i-1)) )
 20   continue
      return
c
c     gamma(x) for x .ge. 2.0 and x .le. 10.0
c
 30   do 40 i=1,n
         dgamma = (y+dble(float(i))) * dgamma
 40   continue
      return
c
c     gamma(x) for dabs(x) .gt. 10.0.  recall y = dabs(x).
c
C50   if (x.gt.xmax) call seteru (32hdgamma  x so big gamma overflows,
C    1  32, 3, 2)

 50   if (x.gt.xmax) then
c     write(6,*) 'dgamma : x so big gamma overflows'
         IGAMMA = 14
         return
      end if
c
      dgamma = 0.d0
C     if (x.lt.xmin) call seteru (35hdgamma  x so small gamma underflows
C     1  , 35, 2, 0)
C     if (x.lt.xmin) return

      if (x.lt.xmin) then
c     write(6,*) 'dgamma : x so small gamma underflows'
         JGAMMA = 12
         return
      end if
c
C     dgamma = dexp ((y-0.5d0)*dlog(y) - y + sq2pil + d9lgmc(y) )
      temp = d9lgmc(y)
      if (IGAMMA .ne. 0) return
      dgamma =  exp ((y-0.5d0)* log(y) - y + sq2pil + temp)
      if (x.gt.0.d0) return
c
C     if (dabs((x-dint(x-0.5d0))/x).lt.dxrel) call seteru (
C     1  61hdgamma  answer lt half precision, x too near negative integer
C     2  , 61, 1, 1)

      if (abs((x-dble(int(x-0.5d0)))/x).lt.dxrel) JGAMMA = 11
c
C     sinpiy = dsin (pi*y)
      sinpiy =  sin (pi*y)
C     if (sinpiy.eq.0.d0) call seteru (
C     1  31hdgamma  x is a negative integer, 31, 4, 2)

      if (sinpiy.eq.0.d0) then
C     write(6,*) 'dgamma : x is a negative integer'
         IGAMMA = 12
         return
      end if
c
      dgamma = -pi/(y*sinpiy*dgamma)
c
      return
      end

      double precision function dgamr (x)
c july 1977 edition.  w. fullerton, c3, los alamos scientific lab.
c this routine, not dgamma(x), should be the fundamental one.
c
C     double precision x, alngx, sgngx, dgamma, dint, dexp, d1mach
      double precision x, alngx, sgngx, temp,  dgamma

C     external dexp, dgamma, dint, d1mach
      external dgamma

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
      dgamr = 0d0
C     if (x.le.0d0 .and. dint(x).eq.x) return
      if (x.le.0d0 .and. dble(int(x)).eq.x) return
c
C     call entsrc (irold, 1)
      if (dabs(x).gt.10d0) go to 10
C     dgamr = 1.0d0/dgamma(x)
C     call erroff
C     call entsrc (ir, irold)
      temp = dgamma(x)
      if (IGAMMA .ne. 0) then
C       dgamr = d1mach(2)
        dgamr = FLTMAX
        return
      end if
      dgamr = 1.0d0/temp
      return
c
 10   call dlgams (x, alngx, sgngx)
      if (IGAMMA .ne. 0) return
C     call erroff
C     call entsrc (ir, irold)
C     dgamr = sgngx * dexp(-alngx)
      dgamr = sgngx *  exp(-alngx)
      return
c
      end

      subroutine dlgams (x, dlgam, sgngam)
c july 1977 edition.  w. fullerton, c3, los alamos scientific lab.
c
c evaluate log abs (gamma(x)) and return the sign of gamma(x) in sgngam.
c sgngam is either +1.0 or -1.0.
c
C     double precision x, dlgam, sgngam, dint, dlngam
      double precision x, dlgam, sgngam, dlngam
      integer intx
C     external dint, dlngam
      external dlngam

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
      dlgam = dlngam(x)
      if (IGAMMA .ne. 0) return
      sgngam = 1.0d0
      if (x.gt.0.d0) return
c
C     int = dmod (-dint(x), 2.0d0) + 0.1d0
C     if (int.eq.0) sgngam = -1.0d0
      intx =  mod (-dble(int(x)), 2.0d0) + 0.1d0
      if (intx.eq.0) sgngam = -1.0d0
c
      return
      end

      integer function initds (dos, nos, eta)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
c
c initialize the double precision orthogonal series dos so that initds
c is the number of terms needed to insure the error is no larger than
c eta.  ordinarily eta will be chosen to be one-tenth machine precision.
c
c             input arguments --
c dos    dble prec array of nos coefficients in an orthogonal series.
c nos    number of coefficients in dos.
c eta    requested accuracy of series.
c
      integer nos
      double precision dos(nos)
      real eta

      integer ii, i
      double precision err

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
C     if (nos.lt.1) call seteru (
C    1  35hinitds  number of coefficients lt 1, 35, 2, 2)
      if (nos.lt.1) JGAMMA = 31
c
      i = -1
      err = 0.
      do 10 ii=1,nos
        i = nos + 1 - ii
        err = err + abs(sngl(dos(i)))
        if (err.gt.eta) go to 20
 10   continue
c
C20   if (i.eq.nos) call seteru (28hinitds  eta may be too small, 28,
C    1  1, 2)
 20   continue
C     if (i.eq.nos) write(6,*) 'initds : eta may be too small'
      if (i.eq.nos) JGAMMA = 32
      initds = i
c
      return
      end

      subroutine d9gaml (xmin, xmax)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
c
c calculate the minimum and maximum legal bounds for x in gamma(x).
c xmin and xmax are not the only bounds, but they are the only non-
c trivial ones to calculate.
c
c             output arguments --
c xmin   dble prec minimum legal value of x in gamma(x).  any smaller
c        value of x might result in underflow.
c xmax   dble prec maximum legal value of x in gamma(x).  any larger
c        value of x might cause overflow.
c
C     double precision xmin, xmax, alnbig, alnsml, xln, xold, d1mach,
C    1  dlog
      double precision xmin, xmax

      double precision alnbig, alnsml, xln, xold
      integer i
C     external d1mach, dlog

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
C     alnsml = dlog(d1mach(1))
      alnsml =  log( FLTMIN  )
      xmin = -alnsml
      do 10 i=1,10
        xold = xmin
C       xln = dlog(xmin)
        xln =  log(xmin)
        xmin = xmin - xmin*((xmin+0.5d0)*xln - xmin - 0.2258d0 + alnsml)
     1    / (xmin*xln+0.5d0)
C       if (dabs(xmin-xold).lt.0.005d0) go to 20
        if ( abs(xmin-xold).lt.0.005d0) go to 20
 10   continue
C     call seteru (27hd9gaml  unable to find xmin, 27, 1, 2)
C     write(6,*) 'd9gaml : unable to find xmin'
      IGAMMA = 21
      return

c
 20   xmin = -xmin + 0.01d0
c
C     alnbig = dlog (d1mach(2))
      alnbig =  log ( FLTMAX  )
      xmax = alnbig
      do 30 i=1,10
        xold = xmax
C       xln = dlog(xmax)
        xln =  log(xmax)
        xmax = xmax - xmax*((xmax-0.5d0)*xln - xmax + 0.9189d0 - alnbig)
     1    / (xmax*xln-0.5d0)
C       if (dabs(xmax-xold).lt.0.005d0) go to 40
        if ( abs(xmax-xold).lt.0.005d0) go to 40
 30   continue
C     call seteru (27hd9gaml  unable to find xmax, 27, 2, 2)
C     write(6,*) 'd9gaml : unable to find xmax'
      IGAMMA = 22
      return
c
 40   xmax = xmax - 0.01d0
      xmin = dmax1 (xmin, -xmax+1.d0)
c
      return
      end

      double precision function d9lgmc (x)
c august 1977 edition.  w. fullerton, c3, los alamos scientific lab.
c
c compute the log gamma correction factor for x .ge. 10. so that
c dlog (dgamma(x)) = dlog(dsqrt(2*pi)) + (x-.5)*dlog(x) - x + d9lgmc(x)
c
C     double precision x, algmcs(15), xbig, xmax, dcsevl, d1mach,
C    1  dexp, dlog, dsqrt
      double precision x

      double precision algmcs(15), xbig, xmax, temp
      integer nalgm

      double precision dcsevl
      integer initds
C     external d1mach, dcsevl, dexp, dlog, dsqrt, initds
      external dcsevl, initds

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
c series for algm       on the interval  0.          to  1.00000e-02
c                                        with weighted error   1.28e-31
c                                         log weighted error  30.89
c                               significant figures required  29.81
c                                    decimal places required  31.48
c
      data algmcs(  1) / +.1666389480 4518632472 0572965082 2 d+0      /
      data algmcs(  2) / -.1384948176 0675638407 3298605913 5 d-4      /
      data algmcs(  3) / +.9810825646 9247294261 5717154748 7 d-8      /
      data algmcs(  4) / -.1809129475 5724941942 6330626671 9 d-10     /
      data algmcs(  5) / +.6221098041 8926052271 2601554341 6 d-13     /
      data algmcs(  6) / -.3399615005 4177219443 0333059966 6 d-15     /
      data algmcs(  7) / +.2683181998 4826987489 5753884666 6 d-17     /
      data algmcs(  8) / -.2868042435 3346432841 4462239999 9 d-19     /
      data algmcs(  9) / +.3962837061 0464348036 7930666666 6 d-21     /
      data algmcs( 10) / -.6831888753 9857668701 1199999999 9 d-23     /
      data algmcs( 11) / +.1429227355 9424981475 7333333333 3 d-24     /
      data algmcs( 12) / -.3547598158 1010705471 9999999999 9 d-26     /
      data algmcs( 13) / +.1025680058 0104709120 0000000000 0 d-27     /
      data algmcs( 14) / -.3401102254 3167487999 9999999999 9 d-29     /
      data algmcs( 15) / +.1276642195 6300629333 3333333333 3 d-30     /
c
      data nalgm, xbig, xmax / 0, 2*0.d0 /
c
      if (nalgm.ne.0) go to 10
C     nalgm = initds (algmcs, 15, sngl(d1mach(3)) )
      nalgm = initds (algmcs, 15, sngl(  EPSMIN ) )
C     xbig = 1.0d0/dsqrt(d1mach(3))
      xbig = 1.0d0/ sqrt(  EPSMIN )
C     xmax = dexp (dmin1(dlog(d1mach(2)/12.d0), -dlog(12.d0*d1mach(1))))
      xmax =  exp ( min ( log(FLTMAX   /12.d0), - log(12.d0*FLTMIN   )))
c
C10   if (x.lt.10.d0) call seteru (23hd9lgmc  x must be ge 10, 23, 1, 2)
c
 10   if (x.lt.10.d0) then
c       write(6,*) 'd9lgmc : x must be ge 10'
        IGAMMA = 51
C       d9lgmc = d1mach(2)
        d9lgmc = FLTMAX
        return
      end if

      if (x.ge.xmax) go to 20
c
      d9lgmc = 1.d0/(12.d0*x)
C     if (x.lt.xbig) d9lgmc = dcsevl (2.0d0*(10.d0/x)**2-1.d0, algmcs,
C    1  nalgm) / x

      if (x.lt.xbig) then
        temp   = dcsevl(2.0d0*(10.d0/x)**2-1.d0, algmcs, nalgm)
        if (IGAMMA .ne. 0) then
C         d9lgmc = d1mach(2)
          d9lgmc = FLTMAX
        else
          d9lgmc = temp / x
        end if
      end if
      return
c
 20   d9lgmc = 0.d0
C     call seteru (34hd9lgmc  x so big d9lgmc underflows, 34, 2, 0)
c     write(6,*) 'd9lgmc : x so big d9lgmc underflows'
      JGAMMA = 51
      return
c
      end

      double precision function dcsevl (x, a, n)
c
c evaluate the n-term chebyshev series a at x.  adapted from
c r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973).
c
c             input arguments --
c x      dble prec value at which the series is to be evaluated.
c a      dble prec array of n terms of a chebyshev series.  in eval-
c        uating a, only half the first coef is summed.
c n      number of terms in array a.
c
      integer n
      double precision a(n), x
C     double precision d1mach
C     external         d1mach
      double precision twox, b0, b1, b2
      integer i, ni

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

c
      b2 = 0.
C     if (n.lt.1) call seteru (28hdcsevl  number of terms le 0, 28, 2,2)
C     if (n.gt.1000) call seteru (31hdcsevl  number of terms gt 1000,
C    1  31, 3, 2)
C     if (x.lt.(-1.1d0) .or. x.gt.1.1d0) call seteru (
C    1  25hdcsevl  x outside (-1,+1), 25, 1, 1)
c
      if (n.lt.1) then
C       write(6,*) 'dcsevl : number of terms le 0'
        IGAMMA = 41
C       dcsevl = d1mach(2)
        dcsevl = FLTMAX
        return
      end if

      if (n.gt.1000) then
C       write(6,*) 'dcsevl : number of terms gt 1000'
        IGAMMA = 42
C       dcsevl = d1mach(2)
        dcsevl = FLTMAX
        return
      end if

      if (x.lt.(-1.1d0) .or. x.gt.1.1d0) then
C       write(6,*) 'dcsevl : x outside (-1,+1)'
        IGAMMA = 43
C       dcsevl = d1mach(2)
        dcsevl = FLTMAX
        return
      end if
c
      twox = 2.0d0*x
      b1 = 0.d0
      b0 = 0.d0
      do 10 i=1,n
        b2 = b1
        b1 = b0
        ni = n - i + 1
        b0 = twox*b1 - b2 + a(ni)
 10   continue
c
      dcsevl = 0.5d0 * (b0-b2)
c
      return
      end

      double precision function dlngam (x)
c     august 1980 edition.   w. fullerton, c3, los alamos scientific lab.
C     double precision x, dxrel, pi, sinpiy, sqpi2l, sq2pil,
C     1  y, xmax, dint, dgamma, d9lgmc, d1mach, dlog, dsin, dsqrt
      double precision x, dxrel, pi, sinpiy, sqpi2l, sq2pil,
     1     y, xmax, dgamma, d9lgmc
      double precision   temp
C     external d1mach, d9lgmc, dgamma, dint, dlog, dsin, dsqrt
      external d9lgmc, dgamma

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/
c
      data sq2pil / 0.9189385332 0467274178 0329736405 62 d0 /
c     sq2pil = alog (sqrt(2*pi)),  sqpi2l = alog(sqrt(pi/2))
      data sqpi2l / +.2257913526 4472743236 3097614947 441 d0 /
      data pi / 3.1415926535 8979323846 2643383279 50 d0 /
c
      data xmax, dxrel / 2*0.d0 /
c
      dlngam = 0d0
      if (xmax .eq. 0) then
C        xmax = d1mach(2)/dlog(d1mach(2))
         xmax =  FLTMAX  / log( FLTMAX  )
C        dxrel = dsqrt (d1mach(4))
         dxrel =  sqrt ( FLTMAX  )
       endif
C10   y = dabs (x)
 10   y =  abs (x)
      if (y.gt.10.d0) go to 20
c
c     dlog (dabs (dgamma(x)) ) for dabs(x) .le. 10.0
c
C     dlngam = dlog (dabs (dgamma(x)) )
      temp   = dgamma(x)
      if (IGAMMA .ne. 0) then
C     dlngam = d1mach(2)
         dlngam = FLTMAX
         return
      end if
      dlngam = log (abs (temp) )
      return
c
c     dlog ( dabs (dgamma(x)) ) for dabs(x) .gt. 10.0
c
C     20   if (y.gt.xmax) call seteru (
C     1  39hdlngam  dabs(x) so big dlngam overflows, 39, 2, 2)

 20   if (y.gt.xmax) then
c     write(6,*) 'dlngam : abs(x) so big dlngam overflows'
         IGAMMA = 61
C     dlngam = d1mach(2)
         dlngam = FLTMAX
         return
      end if
c
C     if (x.gt.0.d0) dlngam = sq2pil + (x-0.5d0)*dlog(x) - x + d9lgmc(y)

      temp = d9lgmc(y)
      if (IGAMMA .ne. 0) then
C     dlngam = d1mach(2)
         dlngam = FLTMAX
         return
      end if

      if (x.gt.0.d0) dlngam = sq2pil + (x-0.5d0)*log(x) - x + temp
      if (x.gt.0.d0) return
c
C     sinpiy = dabs (dsin(pi*y))
      sinpiy =  abs ( sin(pi*y))
C     if (sinpiy.eq.0.d0) call seteru (
C     1  31hdlngam  x is a negative integer, 31, 3, 2)

      if (sinpiy.eq.0.d0) then
c     write(6,*) 'dlngam : x is a negative integer'
         IGAMMA = 62
C     dlngam = d1mach(2)
         dlngam = FLTMAX
         return
      end if
c
C     dlngam = sqpi2l + (x-0.5d0)*dlog(y) - x - dlog(sinpiy) - d9lgmc(y)

      temp = d9lgmc(y)
      if (IGAMMA .ne. 0) then
C     dlngam = d1mach(2)
         dlngam = FLTMAX
         return
      end if

      dlngam = sqpi2l + (x-0.5d0)*log(y) - x - log(sinpiy) - temp
c
C     if (dabs((x-dint(x-0.5d0))*dlngam/x).lt.dxrel) call seteru (
C     1  68hdlngam  answer lt half precision because x too near negative
C     2integer, 68, 1, 1)
      if ( abs((x-dble(int(x-0.5d0)))*dlngam/x).lt.dxrel) JGAMMA = 61

      return
c
      end

 
C ##############################################################################
C FRACDIFF-fdhess


c Fill "parameter"s into global variables (Common blocks) called later:

      subroutine fdcom( n, M, nar, nma, hood, flmin,flmax, epmin,epmax)

      integer            n, M, nar, nma
      double precision   hood, flmin, flmax, epmin, epmax

      integer minpq

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      integer            nn, MM, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    nn, MM, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      double precision   hatmu, wnv, cllf
      common /FILTFD/    hatmu, wnv, cllf
      save   /FILTFD/

      integer            ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer            lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

c  copyright 1991 Department of Statistics, University of Washington
c  written by Chris Fraley

c-----------------------------------------------------------------------------

      cllf = hood

      FLTMIN = flmin
      FLTMAX = flmax
      EPSMAX = epmax
      EPSMIN = epmin
      EPSPT5 = sqrt(EPSMIN)
      EPSP25 = sqrt(EPSPT5)
      EPSPT3 = EPSMIN**(.3)
      EPSP75 = EPSMIN**(.75)
      BIGNUM = 1d0 / EPSMIN

      nn    = n
      MM    = M
      np    = nar
      nq    = nma

      npq    = np + nq
      npq1   = npq + 1
      maxpq  = max(np,nq)
      minpq  = min(np,nq)
      maxpq1 = maxpq + 1
      maxpq1 = maxpq + 1
      nm     = n - maxpq

      lqp    = 1
      ly     = lqp    +  npq
      lamk   = ly
      lak    = lamk   +  n
      lphi   = lak    +  n
      lvk    = lphi   +  M
      lpi    = lphi
      la     = ly     +  n
      lajac  = la     +  n - minpq
      ipvt   = lajac  +  max( (n-np)*np, (n-nq)*nq, (n-maxpq)*npq)
      ldiag  = ipvt   +  npq/2 + 1
      lqtf   = ldiag  +  npq
      lwa1   = lqtf   +  npq
      lwa2   = lwa1   +  npq
      lwa3   = lwa2   +  npq
      lwa4   = lwa3   +  npq
c      lfree  = lwa4   +  n - minpq

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine fdhpq( x, H, lH, w)

      integer            lH
c     real               x(n)
      double precision   x(*)
c     double precision   H(lH, npq1)
      double precision   H(lH, *)
      double precision   w(*)

      double precision   zero
      parameter         (zero=0.d0)

      integer            n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer            ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer            lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *                   lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

c  copyright 1991 Department of Statistics, University of Washington
c  written by Chris Fraley

c-----------------------------------------------------------------------------

      call hesspq( w(lqp), w(la), w(lajac), nm, H, lH,
     *             w(lwa4), w(lwa1))

c     call dcopy( npq1, zero, 0, H(1,1), lH)
c     call dcopy( npq , zero, 0, H(2,1), 1)

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine fdcov( x, d, hh, hd, cov, lcov, cor, lcor, se, w, info)

      integer            lcov, lcor, info
c     real               x(n)
      double precision   x(*), d, hh, hd(*), cov(lcov,*), cor(lcor,*),
     +      se(*), w(*)
c     double precision   d, hh, hd(npq1), cov(lcov,npq1),
c    *                   cor(lcor,npq1), se(npq1)

      integer            i,j,k, le, ls,lu,lv, lwork
      double precision   temp

      double precision   zero, one, two
      parameter         (zero=0.d0, one=1.d0, two=2.d0)

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      integer            n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer            ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

      integer            KSVD, KCOV, KCOR
      common /HESSFD/    KSVD, KCOV, KCOR
      save   /HESSFD/

c  copyright 1991 Department of Statistics, University of Washington
c  written by Chris Fraley

c-----------------------------------------------------------------------------

      call hesdpq( x, d, hh, hd, w)

      call dcopy( npq1, hd, 1, cov, lcov)

      IGAMMA = 0
      JGAMMA = 0

      KSVD = 0
      KCOV = 0
      KCOR = 0

      info = 0

      temp = one
      do i = 1, npq1
        do j = i+1, npq1
          cov(j,i) = cov(i,j)
        end do
      end do

      ls    = ly
      lu    = ls    + npq1 + 1
      lv    = lu    + npq1*npq1
      le    = lv    + npq1*npq1
      lwork = le    + npq1
c      lfree = lwork + npq1

      call dsvdc( cov, lcov, npq1, npq1, w(ls), w(le),
     *            w(lu), npq1, w(lv), npq1, w(lwork), 11, info)

      if (info .ne. 0) then
        call dcopy( npq1, zero, 0, se, 1)
        do j = 1, npq1
           call dcopy( npq1, zero, 0, cov(1,j), 1)
        end do
        KSVD = 1
        info = 3
        return
      end if

      call invsvd( w(ls), w(lu), npq1, w(lv), npq1, cov, lcov)

      do i = 1, npq1
        do j = i+1, npq1
          cov(j,i) = cov(i,j)
        end do
      end do

      temp = one
      do j = 1, npq1
        if (cov(j,j) .gt. zero) then
          se(j) = sqrt(cov(j,j))
        else
          temp  = min(temp,cov(j,j))
          se(j) = zero
        end if
      end do

      if (temp .eq. one) then
        do k = 1, npq1
          call dcopy( k, cov( 1, k), 1, cor( 1, k), 1)
        end do
        do i = 1, npq1
          call dscal( (npq1-i+1), (one/se(i)), cor(i,i), lcor)
        end do
        do j = 1, npq1
          call dscal( j, (one/se(j)), cor(1,j),    1)
        end do
      else
        KCOR = 1
        do j = 1, npq1
          call dcopy( npq1, zero, 0, cor(1,j), 1)
        end do
      end if

      do i = 1, npq1
        do j = i+1, npq1
          cor(j,i) = cor(i,j)
        end do
      end do

      if (IGAMMA .ne. 0) info = 4
      if (JGAMMA .ne. 0) info = 1

      if (KSVD   .ne. 0) info = 3
      if (KCOV   .ne. 0) info = 2
      if (KCOR   .ne. 0) info = 3

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine invsvd ( s, u, lu, v, lv, cov, lcov)

      integer            lu, lv, lcov
c     double precision   s(npq1), u(lu,npq1), v(lv,npq1), cov(lcov,npq1)
      double precision   s(*), u(lu,*), v(lv,*), cov(lcov,*)

      integer            i,j,k, krank
      double precision   ss

      double precision   zero, one
      parameter         (zero=0.d0, one=1.d0)

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      integer            n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer            KSVD, KCOV, KCOR
      common /HESSFD/    KSVD, KCOV, KCOR
      save   /HESSFD/

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c-----------------------------------------------------------------------------

      krank = npq1

      do i = 1, npq1
        ss = s(i)
        do j = 1, npq1
          if (ss .lt. one) then
            if (abs(u(i,j)) .gt. ss*FLTMAX) then
              krank = i - 1
              KCOV  = 1
              goto 100
            end if
          end if
        end do
      end do

 100  continue

      do k = 1, npq1
        call dcopy( k, zero, 0, cov( 1, k), 1)
      end do

      if (krank .eq. 0) return

c      do k = 1, npq1
c        do i = 1, npq1
c          do j = i, npq1
c            H(i,j) =  H(i,j) + s(k)*u(i,k)*v(j,k)
c          end do
c        end do
c      end do

c      do k = 1, npq1
c        ss = s(k)
c        do j = 1, npq1
c          call daxpy( j, ss*v(j,k), u(1,k), 1, H(1,j), 1)
c        end do
c      end do

      do k = 1, krank
        ss = (-one/s(k))
        do j = 1, npq1
          call daxpy( j, (ss*u(j,k)), v(1,k), 1, cov(1,j), 1)
        end do
      end do

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine hesspq( qp, a, ajac, lajac, H, lH, aij, g)

      integer       lajac, lH
c     double precision  qp(npq), a(nm), ajac(nm,npq)
      double precision  qp(*), a(*), ajac(lajac,*)
c     double precision  H(lH,npq1), aij(nm), g(npq)
      double precision  H(lH,*), aij(*), g(*)

c analytic Hessian with respect to p and q variables

      integer       i,j,k,km,l
      double precision  ddot

      integer       n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/   n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      double precision   hatmu, wnv, cllf
      common /FILTFD/    hatmu, wnv, cllf
      save   /FILTFD/

      double precision  fac, s, t, u

      double precision  zero, one, two
      parameter        (zero=0.d0, one=1.d0, two=2.d0)

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c-----------------------------------------------------------------------------

      fac = one / (wnv * dble(nm-1))

      if (nq .ne. 0 .and. np .ne. 0) then
        do k = 1, npq
          g(k) = ddot( nm, a, 1, ajac( 1, k), 1)
        end do
        do i = 1, np
          u = g(nq+i)
          do j = 1, nq
            u = g(j)*u
            do k = maxpq1, n
              km = k - maxpq
              t  = zero
              do l = 1, nq
                if (km .le. l) goto 301
                t  = t + qp(l)*aij(km-l)
              end do
 301          continue
              if (km .gt. j) then
                aij(km) = ajac(km-j,nq+i) + t
              else
                aij(km) =                   t
              end if
            end do
            s = ddot( nm, ajac( 1, nq+i), 1, ajac( 1, j), 1)
            t = ddot( nm, a             , 1, aij        , 1)
            H(i+1,np+j+1) = -dble(n)*((s + t) - two*fac*u)*fac
          end do
        end do
      end if

      if (nq .ne. 0) then
        do i = 1, nq
          u = g(i)
          do j = i, nq
            u = g(j)*u
            do k = maxpq1, n
              km = k - maxpq
              t  = zero
              do l = 1, nq
                if (km .le. l) goto 302
                t  = t + qp(l)*aij(km-l)
              end do
 302          continue
              s  = zero
              if (km .gt. i) s = s + ajac(km-i,j)
              if (km .gt. j) s = s + ajac(km-j,i)
              aij(km) = s + t
            end do
            s = ddot( nm, ajac( 1, i), 1, ajac( 1, j), 1)
            t = ddot( nm, a          , 1, aij        , 1)
            H(np+i+1,np+j+1) = -dble(n)*((s + t) - two*fac*u)*fac
          end do
        end do
      end if

      if (np .ne. 0) then
        do i = 1, np
          u = g(nq+i)
          do j = i, np
            u = g(nq+j)*u
c            do k = maxpq1, n
c              km  =  k - maxpq
c              t  = zero
c              if (nq .ne. 0) then
c               do l = 1, nq
c                  if (km .le. l) goto 303
c                  t  = t + qp(l)*aij(km-l)
c               end do
c              end if
c 303          continue
c              aij(km) = t
c            end do
            s = ddot( nm, ajac( 1, nq+i), 1, ajac( 1, nq+j), 1)
c            t = ddot( nm, a             , 1, aij           , 1)
c            H(i+1,j+1) = -dble(n)*((s + t) - two*fac*u)*fac
            H(i+1,j+1) = -dble(n)*(s - two*fac*u)*fac
          end do
        end do
      end if

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine hesdpq( x, d, hh, hd, w)

      double precision   x(*)
c     real       x(n)
c     double precision   d, hh, hd(npq1), w(*)
      double precision   d, hh, hd(*), w(*)

      double precision   slogvk, fa,fb

      intrinsic      log
      double precision   ddot

      double precision   hatmu, wnv, cllf
      common /FILTFD/    hatmu, wnv, cllf
      save   /FILTFD/

      double precision   zero, half, one, two
      parameter     (zero=0.d0, half=.5d0, one=1.d0, two=2.d0)

      integer        n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer        ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer        lqp, la, lajac, ipvt, ldiag, lqtf,
     *           lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *           lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c-----------------------------------------------------------------------------

      if (hh .le. zero) hh = (one+abs(cllf))*EPSPT5

      hh = min( hh, .1d0)

      if ((d-hh) .gt. zero) then

        call fdfilt( x, (d-hh), w(ly), slogvk,
     *               w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

        if (npq .ne. 0) then
          call ajqp( w(lqp), w(la), w(lajac), nm, 1, w(ly))
          call ajqp( w(lqp), w(la), w(lajac), nm, 2, w(ly))

          call gradpq( w(lwa1), w(la), w(lajac), nm)

          wnv = ddot( nm, w(la), 1, w(la), 1)

          call dscal( npq, (one/wnv), w(lwa1), 1)

          wnv = wnv / dble(nm - 1)
        else
          wnv = ddot( nm, w(ly), 1, w(ly), 1) / dble(nm-1)
        end if

        fa  = -(dble(n)*(2.8378d0+log(wnv))+slogvk) / two

        if ((d+hh) .lt. half) then

          call fdfilt( x, (d+hh), w(ly), slogvk,
     *                 w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

          if (npq .ne. 0) then
            call ajqp( w(lqp), w(la), w(lajac), nm, 1, w(ly))
            call ajqp( w(lqp), w(la), w(lajac), nm, 2, w(ly))

            call gradpq( w(lwa2), w(la), w(lajac), nm)

            wnv = ddot( nm, w(la), 1, w(la), 1)

            call dscal( npq, (one/wnv), w(lwa2), 1)

            wnv = wnv / dble(nm - 1)
          else
            wnv = ddot( nm, w(ly), 1, w(ly), 1) / dble(nm-1)
          end if

          fb  = -(dble(n)*(2.8378d0+log(wnv))+slogvk) / two

          hd(1) = ((fa + fb) - two*cllf) / (hh*hh)

        else

          call fdfilt( x, (d-two*hh), w(ly), slogvk,
     *                 w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

          if (npq .ne. 0) then
            call ajqp( w(lqp), w(la), w(lajac), nm, 1, w(ly))
            call ajqp( w(lqp), w(la), w(lajac), nm, 2, w(ly))

            call gradpq( w(lwa2), w(la), w(lajac), nm)

            wnv = ddot( nm, w(la), 1, w(la), 1)

            call dscal( npq, (one/wnv), w(lwa2), 1)

            wnv = wnv / dble(nm - 1)
          else
            wnv = ddot( nm, w(ly), 1, w(ly), 1) / dble(nm-1)
          end if

          fb  = -(dble(n)*(2.8378d0+log(wnv))+slogvk) / two

          hd(1) = ((cllf + fb) -two*fa) / (two*hh*hh)

        endif

      else

        call fdfilt( x, (d+hh), w(ly), slogvk,
     *               w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

        if (npq .ne. 0) then
          call ajqp( w(lqp), w(la), w(lajac), nm, 1, w(ly))
          call ajqp( w(lqp), w(la), w(lajac), nm, 2, w(ly))

          call gradpq( w(lwa1), w(la), w(lajac), nm)

          wnv = ddot( nm, w(la), 1, w(la), 1)

          call dscal( npq, (one/wnv), w(lwa1), 1)

          wnv = wnv / dble(nm - 1)
        else
          wnv = ddot( nm, w(ly), 1, w(ly), 1) / dble(nm-1)
        end if

        fa  = -(dble(n)*(2.8378d0+log(wnv))+slogvk) / two

        call fdfilt( x, (d+two*hh), w(ly), slogvk,
     *               w(lamk), w(lak), w(lvk), w(lphi), w(lpi))

        if (npq .ne. 0) then
          call ajqp( w(lqp), w(la), w(lajac), nm, 1, w(ly))
          call ajqp( w(lqp), w(la), w(lajac), nm, 2, w(ly))

          call gradpq( w(lwa1), w(la), w(lajac), nm)

          wnv = ddot( nm, w(la), 1, w(la), 1)

          call dscal( npq, (one/wnv), w(lwa1), 1)

          wnv = wnv / dble(nm - 1)
        else
          wnv = ddot( nm, w(ly), 1, w(ly), 1) / dble(nm-1)

        end if

        fb  = -(dble(n)*(2.8378d0+log(wnv))+slogvk) / two

        hd(1) = ((cllf + fb) - two*fa) / (two*hh*hh)

      end if

      if (npq .eq. 0) return

      call daxpy( npq, (-one), w(lwa2), 1, w(lwa1), 1)
      call dscal( npq, (dble(n)/(two*hh)), w(lwa1), 1)

      call dcopy( npq, w(lwa1), 1, hd(2), 1)

      return
      end

*******************************************************************************
*******************************************************************************

      subroutine gradpq( g, a, ajac, ljac)

      integer        ljac
c     double precision   g(npq), a(nm), ajac(nm,npq)
      double precision   g(*), a(*), ajac(ljac,*)

      integer        i,j
      double precision   ddot

      integer        n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      common /DIMSFD/    n, M, np, nq, npq, npq1, maxpq, maxpq1, nm
      save   /DIMSFD/

      integer        ly, lamk, lak, lvk, lphi, lpi
      common /WFILFD/    ly, lamk, lak, lvk, lphi, lpi
      save   /WFILFD/

      integer        lqp, la, lajac, ipvt, ldiag, lqtf,
     *           lwa1, lwa2, lwa3, lwa4
      common /WOPTFD/    lqp, la, lajac, ipvt, ldiag, lqtf,
     *           lwa1, lwa2, lwa3, lwa4
      save   /WOPTFD/

c copyright 1991 Department of Statistics, University of Washington
c written by Chris Fraley

c------------------------------------------------------------------------------

      if (np .ne. 0) then
        do i = 1, np
          g(i)    = ddot( nm, a, 1, ajac( 1, nq+i), 1)
        end do
      end if

      if ( nq .ne. 0) then
        do j = 1, nq
          g(np+j) = ddot( nm, a, 1, ajac( 1,    j), 1)
        end do
      end if

      return
      end

      
C ##############################################################################
C FRACDIFF-fdmin


      subroutine lmder1(fcn,m,n,x,fvec,fjac,ldfjac,ftol,xtol,gtol,
     *                  maxfev,diag,mode,factor,info,nfev,njev,
     *                  ipvt,qtf,wa1,wa2,wa3,wa4,Y)

      integer m,n,ldfjac,maxfev,mode,nprint,info,nfev,njev
      integer ipvt(n)
      double precision ftol,xtol,gtol,factor
      double precision x(n),fvec(m),fjac(ldfjac,n),diag(n),qtf(n),
     *                 wa1(n),wa2(n),wa3(n),wa4(m),Y(*)
      external         fcn
c     **********
c
c     subroutine lmder
c
c     the purpose of lmder is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of
c     the levenberg-marquardt algorithm. the user must provide a
c     subroutine which calculates the functions and the jacobian.
c
c     the subroutine statement is
c       subroutine lmder(fcn,m,n,x,fvec,fjac,ldfjac,ftol,xtol,gtol,
c                        maxfev,diag,mode,factor,nprint,info,nfev,
c                        njev,ipvt,qtf,wa1,wa2,wa3,wa4)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions and the jacobian. fcn must
c         be declared in an external statement in the user
c         calling program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,fjac,ldfjac,iflag)
c         integer m,n,ldfjac,iflag
c         double precision x(n),fvec(m),fjac(ldfjac,n)
c         ----------
c         if iflag = 1 calculate the functions at x and
c         return this vector in fvec. do not alter fjac.
c         if iflag = 2 calculate the jacobian at x and
c         return this matrix in fjac. do not alter fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmder.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       fjac is an output m by n array. the upper n by n submatrix
c         of fjac contains an upper triangular matrix r with
c         diagonal elements of nonincreasing magnitude such that
c
c                t     t           t
c               p *(jac *jac)*p = r *r,
c
c         where p is a permutation matrix and jac is the final
c         calculated jacobian. column j of p is column ipvt(j)
c         (see below) of the identity matrix. the lower trapezoidal
c         part of fjac contains information generated during
c         the computation of r.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       ftol is a nonnegative input variable. termination
c         occurs when both the actual and predicted relative
c         reductions in the sum of squares are at most ftol.
c         therefore, ftol measures the relative error desired
c         in the sum of squares.
c
c       xtol is a nonnegative input variable. termination
c         occurs when the relative error between two consecutive
c         iterates is at most xtol. therefore, xtol measures the
c         relative error desired in the approximate solution.
c
c       gtol is a nonnegative input variable. termination
c         occurs when the cosine of the angle between fvec and
c         any column of the jacobian is at most gtol in absolute
c         value. therefore, gtol measures the orthogonality
c         desired between the function vector and the columns
c         of the jacobian.
c
c       maxfev is a positive integer input variable. termination
c         occurs when the number of calls to fcn with iflag = 1
c         has reached maxfev.
c
c       diag is an array of length n. if mode = 1 (see
c         below), diag is internally set. if mode = 2, diag
c         must contain positive entries that serve as
c         multiplicative scale factors for the variables.
c
c       mode is an integer input variable. if mode = 1, the
c         variables will be scaled internally. if mode = 2,
c         the scaling is specified by the input diag. other
c         values of mode are equivalent to mode = 1.
c
c       factor is a positive input variable used in determining the
c         initial step bound. this bound is set to the product of
c         factor and the euclidean norm of diag*x if nonzero, or else
c         to factor itself. in most cases factor should lie in the
c         interval (.1,100.).100. is a generally recommended value.
c
c       nprint is an integer input variable that enables controlled
c         printing of iterates if it is positive. in this case,
c         fcn is called with iflag = 0 at the beginning of the first
c         iteration and every nprint iterations thereafter and
c         immediately prior to return, with x, fvec, and fjac
c         available for printing. fvec and fjac should not be
c         altered. if nprint is not positive, no special calls
c         of fcn with iflag = 0 are made.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  both actual and predicted relative reductions
c                   in the sum of squares are at most ftol.
c
c         info = 2  relative error between two consecutive iterates
c                   is at most xtol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  the cosine of the angle between fvec and any
c                   column of the jacobian is at most gtol in
c                   absolute value.
c
c         info = 5  number of calls to fcn with iflag = 1 has
c                   reached maxfev.
c
c         info = 6  ftol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  xtol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c         info = 8  gtol is too small. fvec is orthogonal to the
c                   columns of the jacobian to machine precision.
c
c       nfev is an integer output variable set to the number of
c         calls to fcn with iflag = 1.
c
c       njev is an integer output variable set to the number of
c         calls to fcn with iflag = 2.
c
c       ipvt is an integer output array of length n. ipvt
c         defines a permutation matrix p such that jac*p = q*r,
c         where jac is the final calculated jacobian, q is
c         orthogonal (not stored), and r is upper triangular
c         with diagonal elements of nonincreasing magnitude.
c         column j of p is column ipvt(j) of the identity matrix.
c
c       qtf is an output array of length n which contains
c         the first n elements of the vector (q transpose)*fvec.
c
c       wa1, wa2, and wa3 are work arrays of length n.
c
c       wa4 is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... dpmpar,enorm,lmpar,qrfac
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iflag,iter,j,l
      double precision actred,dirder,fnorm1,
     *                 one,par,pnorm,prered,p1,p5,p25,p75,p0001,ratio,
     *                 sum,temp,temp1,temp2,xnorm,zero
c     double precision dpmpar,enorm
      double precision enorm

      double precision   FLTMIN, FLTMAX, EPSMIN, epsmch
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, epsmch
      save   /MACHFD/

      double precision   EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      common /MAUXFD/    EPSP25, EPSPT3, EPSPT5, EPSP75, BIGNUM
      save   /MAUXFD/

      double precision   told, tolf, tolx, tolg, fnorm, delta, gnorm
      common /TOLSFD/    told, tolf, tolx, tolg, fnorm, delta, gnorm
      save   /TOLSFD/
c
c     epsmch is the machine precision.
c
c     epsmch = dpmpar(1)
c
      data one,p1,p5,p25,p75,p0001,zero
     *     /1.0d0,1.0d-1,5.0d-1,2.5d-1,7.5d-1,1.0d-4,0.0d0/

      temp = 0d0
      nprint = 0
      info = 0
      iflag = 0
      nfev = 0
      njev = 0

c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. ldfjac .lt. m
     *    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero
     *    .or. maxfev .le. 0 .or. factor .le. zero) go to 300
      if (mode .ne. 2) go to 20
      do 10 j = 1, n
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue
c
c     evaluate the function at the starting point
c     and calculate its norm.
c
      iflag = 1
      call fcn(x,fvec,fjac,ldfjac,iflag,Y)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = min(enorm(m,fvec),BIGNUM)
c
c     initialize levenberg-marquardt parameter and iteration counter.
c
      par = zero
      iter = 1
c
c     beginning of the outer loop.
c
   30 continue

c
c        calculate the jacobian matrix.
c
         iflag = 2
         call fcn(x,fvec,fjac,ldfjac,iflag,Y)
            njev = njev + 1
         if (iflag .lt. 0) go to 300
c
c        if requested, call fcn to enable printing of iterates.
c
         if (nprint .le. 0) go to 40
         iflag = 0
         if (mod(iter-1,nprint) .eq. 0)
     *      call fcn(x,fvec,fjac,ldfjac,iflag,Y)
         if (iflag .lt. 0) go to 300
   40    continue
c
c        compute the qr factorization of the jacobian.
c
         call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3)
c
c        on the first iteration and if mode is 1, scale according
c        to the norms of the columns of the initial jacobian.
c
         if (iter .ne. 1) go to 80
         if (mode .eq. 2) go to 60
         do 50 j = 1, n
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   50       continue
   60    continue
c
c        on the first iteration, calculate the norm of the scaled x
c        and initialize the step bound delta.
c
         do 70 j = 1, n
            wa3(j) = diag(j)*x(j)
   70       continue
         xnorm = enorm(n,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   80    continue
c
c        form (q transpose)*fvec and store the first n components in
c        qtf.
c
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90       continue
         do 130 j = 1, n
            if (fjac(j,j) .eq. zero) go to 120
            sum = zero
            do 100 i = j, m
               sum = sum + fjac(i,j)*wa4(i)
  100          continue
            temp = -sum/fjac(j,j)
            do 110 i = j, m
               wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
  120       continue
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130       continue
c
c        compute the norm of the scaled gradient.
c
         gnorm = zero
         if (fnorm .eq. zero) go to 170
         do 160 j = 1, n
            l = ipvt(j)
            if (wa2(l) .eq. zero) go to 150
            sum = zero
            do 140 i = 1, j
               sum = sum + fjac(i,j)*(qtf(i)/fnorm)
  140          continue
            gnorm = dmax1(gnorm,dabs(sum/wa2(l)))
  150       continue
  160       continue
  170    continue
c
c        test for convergence of the gradient norm.
c
         if (gnorm .le. gtol)  info = 4
         if (info .ne. 0) go to 300
c
c        rescale if necessary.
c
         if (mode .eq. 2) go to 190
         do 180 j = 1, n
            diag(j) = dmax1(diag(j),wa2(j))
  180       continue
  190    continue
c
c        beginning of the inner loop.
c
  200    continue


c           determine the levenberg-marquardt parameter.
c
            call lmpar(n,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,
     *                 wa3,wa4)
c
c           store the direction p and x + p. calculate the norm of p.
c
            do 210 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210          continue
            pnorm = enorm(n,wa3)
c
c           on the first iteration, adjust the initial step bound.
c
            if (iter .eq. 1) delta = dmin1(delta,pnorm)
c
c           evaluate the function at x + p and calculate its norm.
c
            iflag = 1
            call fcn(wa2,wa4,fjac,ldfjac,iflag,Y)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = min(enorm(m,wa4),BIGNUM)
c
c           compute the scaled actual reduction.
c
           actred = -one
           if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
C          actred = (fnorm*fnorm - fnorm1*fnorm1)
c
c           compute the scaled predicted reduction and
c           the scaled directional derivative.
c
            do 230 j = 1, n
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220             continue
  230          continue
            temp1 = enorm(n,wa3)/fnorm
            temp2 = (dsqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
C           temp1  = enorm(n,wa3)
C           temp2  = (dsqrt(par)*pnorm)
C           prered = (temp1**2 + 2.d0*temp2**2)
            dirder = -(temp1**2 + temp2**2)
c
c           compute the ratio of the actual to the predicted
c           reduction.
c
            ratio = zero
            if (prered .ne. zero) ratio = actred/prered
c
c           update the step bound.
c
            if (ratio .gt. p25) go to 240
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero)
     *            temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*dmin1(delta,pnorm/p1)
               par = par/temp
               go to 260
  240       continue
               if (par .ne. zero .and. ratio .lt. p75) go to 250
               delta = pnorm/p5
               par = p5*par
  250          continue
  260       continue
c
c           test for successful iteration.
c
            if (ratio .lt. p0001) go to 290
c
c           successful iteration. update x, fvec, and their norms.
c
c
            do 270 j = 1, n
               x(j) = wa2(j)
               wa2(j) = diag(j)*x(j)
  270          continue
            do 280 i = 1, m
               fvec(i) = wa4(i)
  280          continue
            xnorm = enorm(n,wa2)
            fnorm = fnorm1
            iter = iter + 1
  290       continue
c
c           tests for convergence.
c
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one) info = 1
            if (fnorm  .le. ftol) info = 1
            if (delta  .le. xtol) info = 2
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (info .ne. 0) go to 300
c
c           tests for termination and stringent tolerances.
c
            if (nfev .ge. maxfev) info = 5
            if (dabs(actred) .le. epsmch .and. prered .le. epsmch
     *          .and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300
c
c           end of the inner loop. repeat if iteration unsuccessful.
c
            if (ratio .lt. p0001) go to 200
c
c        end of the outer loop.
c
         go to 30
  300 continue
c
c     termination, either normal or user imposed.
c
      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcn(x,fvec,fjac,ldfjac,iflag,Y)

      return
      end
c       subroutine lmder1

      double precision function enorm(n,x)

      integer n
      double precision x(n)
c     **********
c
c     function enorm
c
c     given an n-vector x, this function calculates the
c     euclidean norm of x.
c
c     the euclidean norm is computed by accumulating the sum of
c     squares in three different sums. the sums of squares for the
c     small and large components are scaled so that no overflows
c     occur. non-destructive underflows are permitted. underflows
c     and overflows do not occur in the computation of the unscaled
c     sum of squares for the intermediate components.
c     the definitions of small, intermediate and large components
c     depend on two constants, rdwarf and rgiant. the main
c     restrictions on these constants are that rdwarf**2 not
c     underflow and rgiant**2 not overflow. the constants
c     given here are suitable for every known computer.
c
c     the function statement is
c
c       double precision function enorm(n,x)
c
c     where
c
c       n is a positive integer input variable.
c
c       x is an input array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i
      double precision agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,
     *                 x1max,x3max,zero
      data one,zero,rdwarf,rgiant /1d0, 0d0, 3.834d-20, 1.304d19/

      enorm = -1d0
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n
      agiant = rgiant/floatn
      do 90 i = 1, n
         xabs = dabs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30
c
c              sum for large components.
c
               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue
c
c              sum for small components.
c
               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue
c
c           sum for intermediate components.
c
            s2 = s2 + xabs**2
   80    continue
   90    continue
c
c     calculation of norm.
c
      if (s1 .eq. zero) go to 100
         enorm = x1max*dsqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max)
     *         enorm = dsqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max)
     *         enorm = dsqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*dsqrt(s3)
  120    continue
  130 continue

      return
      end
c   function enorm

      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)

      integer m,n,lda,lipvt
      integer ipvt(lipvt)
      logical pivot
      double precision a(lda,n),rdiag(n),acnorm(n),wa(n)
c     **********
c
c     subroutine qrfac
c
c     this subroutine uses householder transformations with column
c     pivoting (optional) to compute a qr factorization of the
c     m by n matrix a. that is, qrfac determines an orthogonal
c     matrix q, a permutation matrix p, and an upper trapezoidal
c     matrix r with diagonal elements of nonincreasing magnitude,
c     such that a*p = q*r. the householder transformation for
c     column k, k = 1,2,...,min(m,n), is of the form
c
c                           t
c           i - (1/u(k))*u*u
c
c     where u has zeros in the first k-1 positions. the form of
c     this transformation and the method of pivoting first
c     appeared in the corresponding linpack subroutine.
c
c     the subroutine statement is
c
c       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a contains the matrix for
c         which the qr factorization is to be computed. on output
c         the strict upper trapezoidal part of a contains the strict
c         upper trapezoidal part of r, and the lower trapezoidal
c         part of a contains a factored form of q (the non-trivial
c         elements of the u vectors described above).
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       pivot is a logical input variable. if pivot is set true,
c         then column pivoting is enforced. if pivot is set false,
c         then no column pivoting is done.
c
c       ipvt is an integer output array of length lipvt. ipvt
c         defines the permutation matrix p such that a*p = q*r.
c         column j of p is column ipvt(j) of the identity matrix.
c         if pivot is false, ipvt is not referenced.
c
c       lipvt is a positive integer input variable. if pivot is false,
c         then lipvt may be as small as 1. if pivot is true, then
c         lipvt must be at least n.
c
c       rdiag is an output array of length n which contains the
c         diagonal elements of r.
c
c       acnorm is an output array of length n which contains the
c         norms of the corresponding columns of the input matrix a.
c         if this information is not needed, then acnorm can coincide
c         with rdiag.
c
c       wa is a work array of length n. if pivot is false, then wa
c         can coincide with rdiag.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm
c
c       fortran-supplied ... dmax1,dsqrt,min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kmax,minmn
      double precision ajnorm,epsmch,one,p05,sum,temp,zero
c     double precision dpmpar,enorm
      double precision enorm

      double precision   FLTMIN, FLTMAX, EPSMIN
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, epsmch
      save   /MACHFD/

      data one,p05,zero /1.0d0,5.0d-2,0.0d0/
c     epsmch is the machine precision.
c
c     epsmch = dpmpar(1)
c
c     compute the initial column norms and initialize several arrays.
c
      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue
c
c     reduce a to r with householder transformations.
c
      minmn = min0(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40
c
c        bring the column of largest norm into the pivot position.
c
         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue
c
c        compute the householder transformation to reduce the
c        j-th column of a to a multiple of the j-th unit vector.
c
         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
         a(j,j) = a(j,j) + one
c
c        apply the transformation to the remaining columns
c        and update the norms.
c
         jp1 = j + 1
         if (n .lt. jp1) go to 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
   60          continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*dsqrt(dmax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue

      return
      end
c   subroutine qrfac

      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,
     *                 wa2)

      integer n,ldr
      integer ipvt(n)
      double precision delta,par
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa1(n),
     *                 wa2(n)
c     **********
c
c     subroutine lmpar
c
c     given an m by n matrix a, an n by n nonsingular diagonal
c     matrix d, an m-vector b, and a positive number delta,
c     the problem is to determine a value for the parameter
c     par such that if x solves the system
c
c           a*x = b ,     sqrt(par)*d*x = 0 ,
c
c     in the least squares sense, and dxnorm is the euclidean
c     norm of d*x, then either par is zero and
c
c           (dxnorm-delta) .le. 0.1*delta ,
c
c     or par is positive and
c
c           abs(dxnorm-delta) .le. 0.1*delta .
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then lmpar expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. on output
c     lmpar also provides an upper triangular matrix s such that
c
c            t   t                   t
c           p *(a *a + par*d*d)*p = s *s .
c
c     s is employed within lmpar and may be of separate interest.
c
c     only a few iterations are generally needed for convergence
c     of the algorithm. if, however, the limit of 10 iterations
c     is reached, then the output par will contain the best
c     value obtained so far.
c
c     the subroutine statement is
c
c       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
c                        wa1,wa2)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       delta is a positive input variable which specifies an upper
c         bound on the euclidean norm of d*x.
c
c       par is a nonnegative variable. on input par contains an
c         initial estimate of the levenberg-marquardt parameter.
c         on output par contains the final estimate.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, sqrt(par)*d*x = 0,
c         for the output par.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa1 and wa2 are work arrays of length n.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm,qrsolv
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iter,j,jm1,jp1,k,l,nsing
      double precision dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,
     *                 sum,temp,zero
c     double precision dpmpar,enorm
      double precision enorm

      double precision  FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/   FLTMIN, FLTMAX, EPSMIN, EPSMAX

      data p1,p001,zero /1.0d-1,1.0d-3,0.0d0/

c     dwarf is the smallest positive magnitude.
c
c     dwarf = dpmpar(2)
      dwarf = FLTMIN
c
c     compute and store in x the gauss-newton direction. if the
c     jacobian is rank-deficient, obtain a least squares solution.
c
      nsing = n
      do 10 j = 1, n
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa1(j) = zero
   10    continue
      if (nsing .lt. 1) go to 50
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 30
         do 20 i = 1, jm1
            wa1(i) = wa1(i) - r(i,j)*temp
   20       continue
   30    continue
   40    continue
   50 continue
      do 60 j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
   60    continue
c
c     initialize the iteration counter.
c     evaluate the function at the origin, and test
c     for acceptance of the gauss-newton direction.
c
      iter = 0
      do 70 j = 1, n
         wa2(j) = diag(j)*x(j)
   70    continue
      dxnorm = enorm(n,wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220
c
c     if the jacobian is not rank deficient, the newton
c     step provides a lower bound, parl, for the zero of
c     the function. otherwise set this bound to zero.
c
      parl = zero
      if (nsing .lt. n) go to 120
      do 80 j = 1, n
         l = ipvt(j)
         wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
      do 110 j = 1, n
         sum = zero
         jm1 = j - 1
         if (jm1 .lt. 1) go to 100
         do 90 i = 1, jm1
            sum = sum + r(i,j)*wa1(i)
   90       continue
  100    continue
         wa1(j) = (wa1(j) - sum)/r(j,j)
  110    continue
      temp = enorm(n,wa1)
      parl = ((fp/delta)/temp)/temp
  120 continue
c
c     calculate an upper bound, paru, for the zero of the function.
c
      do 140 j = 1, n
         sum = zero
         do 130 i = 1, j
            sum = sum + r(i,j)*qtb(i)
  130       continue
         l = ipvt(j)
         wa1(j) = sum/diag(l)
  140    continue
      gnorm = enorm(n,wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = dwarf/dmin1(delta,p1)
c
c     if the input par lies outside of the interval (parl,paru),
c     set par to the closer endpoint.
c
      par = dmax1(par,parl)
      par = dmin1(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm
c
c     beginning of an iteration.
c
  150 continue
         iter = iter + 1
c
c        evaluate the function at the current value of par.
c
         if (par .eq. zero) par = dmax1(dwarf,p001*paru)
         temp = dsqrt(par)
         do 160 j = 1, n
            wa1(j) = temp*diag(j)
  160       continue
         call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n
            wa2(j) = diag(j)*x(j)
  170       continue
         dxnorm = enorm(n,wa2)
         temp = fp
         fp = dxnorm - delta
c
c        if the function is small enough, accept the current value
c        of par. also test for the exceptional cases where parl
c        is zero or the number of iterations has reached 10.
c
         if (dabs(fp) .le. p1*delta
     *       .or. parl .eq. zero .and. fp .le. temp
     *            .and. temp .lt. zero .or. iter .eq. 10) go to 220
c
c        compute the newton correction.
c
         do 180 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180       continue
         do 210 j = 1, n
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            jp1 = j + 1
            if (n .lt. jp1) go to 200
            do 190 i = jp1, n
               wa1(i) = wa1(i) - r(i,j)*temp
  190          continue
  200       continue
  210       continue
         temp = enorm(n,wa1)
         parc = ((fp/delta)/temp)/temp
c
c        depending on the sign of the function, update parl or paru.
c
         if (fp .gt. zero) parl = dmax1(parl,par)
         if (fp .lt. zero) paru = dmin1(paru,par)
c
c        compute an improved estimate for par.
c
         par = dmax1(parl,par+parc)
c
c        end of an iteration.
c
         go to 150
  220 continue
c
c     termination.
c
      if (iter .eq. 0) par = zero
      return
      end
c   subroutine lmpar.


      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)

      integer n,ldr
      integer ipvt(n)
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n)
c     **********
c
c     subroutine qrsolv
c
c     given an m by n matrix a, an n by n diagonal matrix d,
c     and an m-vector b, the problem is to determine an x which
c     solves the system
c
c           a*x = b ,     d*x = 0 ,
c
c     in the least squares sense.
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then qrsolv expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. the system
c     a*x = b, d*x = 0, is then equivalent to
c
c                  t       t
c           r*z = q *b ,  p *d*p*z = 0 ,
c
c     where x = p*z. if this system does not have full rank,
c     then a least squares solution is obtained. on output qrsolv
c     also provides an upper triangular matrix s such that
c
c            t   t               t
c           p *(a *a + d*d)*p = s *s .
c
c     s is computed within qrsolv and may be of separate interest.
c
c     the subroutine statement is
c
c       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, d*x = 0.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa is a work array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kp1,l,nsing
      double precision cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero
      data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/
c
c     copy r and (q transpose)*b to preserve input and initialize s.
c     in particular, save the diagonal elements of r in x.
c
      do 20 j = 1, n
         do 10 i = j, n
            r(i,j) = r(j,i)
   10       continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20    continue
c
c     eliminate the diagonal matrix d using a givens rotation.
c
      do 100 j = 1, n
c
c        prepare the row of d to be eliminated, locating the
c        diagonal element using p from the qr factorization.
c
         l = ipvt(j)
         if (diag(l) .eq. zero) go to 90
         do 30 k = j, n
            sdiag(k) = zero
   30       continue
         sdiag(j) = diag(l)
c
c        the transformations to eliminate the row of d
c        modify only a single element of (q transpose)*b
c        beyond the first n, which is initially zero.
c
         qtbpj = zero
         do 80 k = j, n
c
c           determine a givens rotation which eliminates the
c           appropriate element in the current row of d.
c
            if (sdiag(k) .eq. zero) go to 70
            if (dabs(r(k,k)) .ge. dabs(sdiag(k))) go to 40
               cotan = r(k,k)/sdiag(k)
               sin = p5/dsqrt(p25+p25*cotan**2)
               cos = sin*cotan
               go to 50
   40       continue
               tan = sdiag(k)/r(k,k)
               cos = p5/dsqrt(p25+p25*tan**2)
               sin = cos*tan
   50       continue
c
c           compute the modified diagonal element of r and
c           the modified element of ((q transpose)*b,0).
c
            r(k,k) = cos*r(k,k) + sin*sdiag(k)
            temp = cos*wa(k) + sin*qtbpj
            qtbpj = -sin*wa(k) + cos*qtbpj
            wa(k) = temp
c
c           accumulate the tranformation in the row of s.
c
            kp1 = k + 1
            if (n .lt. kp1) go to 70
            do 60 i = kp1, n
               temp = cos*r(i,k) + sin*sdiag(i)
               sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
               r(i,k) = temp
   60          continue
   70       continue
   80       continue
   90    continue
c
c        store the diagonal element of s and restore
c        the corresponding diagonal element of r.
c
         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100    continue
c
c     solve the triangular system for z. if the system is
c     singular, then obtain a least squares solution.
c
      nsing = n
      do 110 j = 1, n
         if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa(j) = zero
  110    continue
      if (nsing .lt. 1) go to 150
      do 140 k = 1, nsing
         j = nsing - k + 1
         sum = zero
         jp1 = j + 1
         if (nsing .lt. jp1) go to 130
         do 120 i = jp1, nsing
            sum = sum + r(i,j)*wa(i)
  120       continue
  130    continue
         wa(j) = (wa(j) - sum)/sdiag(j)
  140    continue
  150 continue
c
c     permute the components of z back to components of x.
c
      do 160 j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
  160    continue
      return
      end
c       subroutine qrsolv.

C ##############################################################################
C FRACDIFF-fdsim


      subroutine fdsim( n, ip, iq, ar, ma, d, rmu, y, s,
     *                  flmin, flmax, epmin, epmax)

      implicit none

c  generates a random time series for use with fracdf
c
c  Input :
c
c  n      integer  length of the time series
c  ip     integer  number of autoregressive parameters
c  ar     real    (ip) autoregressive parameters
c  ma     real    (iq) moving average parameters
c  d      real     fractional differencing parameters
c  rmu    real     time series mean
c  y      real    (n+iq) 1st n : normalized random numbers
c  s      real    (n+iq) workspace
c
c  Output :
c
c  s      real   (n) the generated time series

c-----------------------------------------------------------------------------
c
c        Simulates a series of length n from an ARIMA (p,d,q) model
c        with fractional d (0 < d < 0.5).
c
c-----------------------------------------------------------------------------

      integer            n, ip, iq
c     real               ar(ip), ma(iq), rmu, d
      double precision   ar(ip), ma(iq), rmu, d

      double precision   g0, vk, amk, sum, dk1, dk1d, dj, temp
c     real               y(n+iq), s(n+iq)
      double precision   y(*), s(*)

      double precision   flmin, flmax, epmin, epmax

      double precision   dgamr, dgamma

      external           dgamr, dgamma

      integer            k, j, i

      double precision   FLTMIN, FLTMAX, EPSMIN, EPSMAX
      common /MACHFD/    FLTMIN, FLTMAX, EPSMIN, EPSMAX
      save   /MACHFD/

      integer            IGAMMA, JGAMMA
      common /GAMMFD/    IGAMMA, JGAMMA
      save   /GAMMFD/

      double precision zero, one, two
      parameter        (zero = 0d0, one = 1d0, two = 2d0)

*--------------------------------------------------------------------------

        IGAMMA = 0
        JGAMMA = 0

        FLTMIN  = flmin
        FLTMAX  = flmax
        EPSMIN  = epmin
        EPSMAX  = epmax
c
c    Calculate g0

        temp = dgamr(one-d)
        if (IGAMMA .ne. 0) then
          do i = 1, n
            s(i) = zero
          end do
          return
        end if

        g0   = dgamma(one-two*d)*(temp*temp)
        if (IGAMMA .ne. 0) then
          do i = 1, n
            s(i) = zero
          end do
          return
        end if
c
c    Generate y(1)
c
        y(1) = y(1)*sqrt(g0)
c
c    Generate y(2) and initialise vk,phi(j)
c
        temp  = d / (one-d)
        vk    = g0*(one-(temp*temp))

        amk   = temp*y(1)
        s(1)  = temp
        y(2)  = amk + y(2)*sqrt(vk)
c
c    Generate y(3),...,y(n+iq)
c
        do k = 3, n + iq
          dk1  = real(k) - one
          dk1d = dk1 - d
c
c    Update the phi(j) using the recursion formula on W498
c
          do j = 1, k-2
            dj   = dk1 - real(j)
            s(j) = s(j)*(dk1*(dj-d)/(dk1d*dj))
          end do

          temp   = d / dk1d
          s(k-1) = temp
c
c    Update vk
c
        vk = vk * (one-(temp*temp))
c
c    Form amk
c
        amk = zero
        do j = 1, k-1
            amk = amk + s(j)*y(k-j)
        end do
c
c    Generate y(k)
c
        y(k) = amk + y(k)*sqrt(vk)

        end do
c
c    We now have an ARIMA (0,d,0) realisation of length n+iq in
c    y(k),k=1,n+iq. We now run this through an inverse ARMA(p,q)
c    filter to get the final output in x(k),k=1,n.
c

        do k = 1, n

        sum = zero

          do i = 1, ip
        if (k .le. i) go to 10
        sum = sum + ar(i)*s(k-i)
          end do

10        continue

          do j = 1, iq
        sum = sum-ma(j)*y(k+iq-j)
          end do

        s(k) = sum + y(k+iq)

        end do

        if (rmu .ne. zero) then
          do i = 1, n
            s(i) = s(i) + rmu
          end do
        end if

       return
       end

        