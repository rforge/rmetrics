CDW The multivariate integration package adapt was added to for use in the 
CDW Rmetrics package fCopula. Thanks to Prof. Alan Genz who put his code 
CDW for the use in fCopulae under the GPL-2 License.
CDW Message-ID: <4AD7A74B.3020108@math.wsu.edu>
CDW Date: Thu, 15 Oct 2009 15:50:51 -0700
CDW From: Alan Genz <genz@math.wsu.edu>
CDW User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.21) 
CDW     Gecko/20090402 SeaMonkey/1.1.16
CDW MIME-Version: 1.0
CDW To: Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
CDW CC: Alan C Genz <alangenz@wsu.edu>
CDW Subject: Re: adapt
CDW References: <4AD3032B.4090801@itp.phys.ethz.ch>
CDW In-Reply-To: <4AD3032B.4090801@itp.phys.ethz.ch>
CDW Content-Type: text/plain; charset=ISO-8859-1; format=flowed
CDW Content-Transfer-Encoding: 7bit
CDW Status:  O
CDW Dear Prof. Wuertz,
CDW Thank you for your message and your interest in my adaptive integration
CDW Fortran code. I would be pleased if you included my code in your open
CDW source R fCopulae package under the Gnu GPL2 license. You have my
CDW permission to do this.
CDW Sincerely,
CDW Alan Genz 


cMM this is the original adapt code with one modification.
cMM instead of calling the external function "functn", a fixed
cMM external routine adphlp is always called, and passed a pointer
cMM to the external s function.
cMM                              Michael Meyer, October 1989.

      subroutine adapt(ndim,a,b,minpts,maxpts,eps,relerr,
     *         lenwrk,wrkstr,finest,ifail)
c***begin prologue adapt
c  adaptive multidimensional integration subroutine
c          author: A. C. Genz, Washington State University
c                 19 March 1984
c**************       parameters for adapt  ********************************
c***** input parameters
c  ndim         number of variables, must exceed 1, but not exceed 20
c  a         real array of lower limits, with dimension ndim
c  b         real array of upper limits, with dimension ndim
c  minpts  minimum number of function evaluations to be allowed.
c         on the first call to adapt minpts should be set to a
c         non negative value (caution... minpts is altered by adapt).
c         It is possible to continue a calculation to greater accuracy
c         by calling adapt again by decreasing eps (described below)
c         and resetting minpts to any negative value.
c         minpts must not exceed maxpts.
c  maxpts  maximum number of function evaluations to be allowed,
c         which must be at least rulcls, where
c         rulcls =  2**ndim+2*ndim**2+6*ndim+1
c
c             for ndim =  2   3   4   5   6   7   8   9   10      12    15      20
c      maxpts >= rulcls = 25  45  73 113 173 269 433 729 1285 4457 33309 1049497
c
c        a SUGGESTED value for maxpts is 100 times the above values.
c
c  functn  externally declared user defined function to be integrated.
c         it must have parameters (ndim,z), where z is a real array
c         of dimension ndim.
cTSL this function has been replaced by the fixed function adhlp
c  eps         required relative accuracy
c  lenwrk  length of array wrkstr of working storage, the routine
c         needs (2*ndim+3)*(1+maxpts/rulcls)/2 for lenwrk if
c         maxpts function calls are used.
c         for guidance, if you set maxpts to 100*rulcls (see table
c         above) then acceptable values for lenwrk are
c
c           for ndim = 2    3        4    5    6       7    8          9
c           lenwrk =  357  561       1785 3417 6681 13209 26265 52377
c
c***** OUTPUT parameters
c
c  minpts  actual number of function evaluations used by adapt
c  wrkstr  real array of working storage of dimension (lenwrk).
c  relerr  estimated relative accuracy of finest
c  finest  estimated value of integral ["FINal ESTimate"]
c  ifail : return code
c
c      ifail=0 for normal exit, when estimated relative accuracy
c            relerr is less than eps with maxpts or less function
c            calls made.
c      ifail=1 if maxpts was too small for adapt to obtain the
c            required relative accuracy eps.
c            In this case adapt returns a value of finest
c            with estimated relative accuracy relerr.
c      ifail=2 if lenwrk too small for maxpts function calls.
c            In this case adapt returns a value of finest with
c            estimated accuracy relerr using the working storage
c            available, but relerr will be greater than eps.
c      ifail=3 if ndim < 2, ndim > 20,
c      ifail=4 if minpts > maxpts,
c      ifail=5 if maxpts < rulcls or other memory problems
c                  (which will only be found later)
c***********************************************************************
c***end prologue adapt

      implicit none

C-- Arguments:
C      double precision functn
C      external functn

      integer ndim, minpts,maxpts, lenwrk, ifail
      double precision a(ndim), b(ndim), eps, relerr, wrkstr(lenwrk),
     &         finest

C-- Local Variables:
      double precision center(20), width(20)
      double precision errmin, rgnerr, rgnval, half, zero,one,two

      integer divaxo, divaxn, divflg, funcls, index1, index2,
     *     j, k,  maxcls, rgnstr, rulcls, sbrgns, sbtmpp, subrgn, subtmp

      data zero/0d0/, one/1d0/, two/2d0/

c Check arguments; fail w/ code '3' or '4'
      relerr=one
      funcls=0
      ifail=3
      if(ndim.lt.2.or.ndim.gt.20) goto 990
      ifail=4
      if(minpts.gt.maxpts) goto 990
      ifail=5
c
c*****      initialisation of subroutine
c
      half=one/two
      rgnstr =2*ndim+3
      errmin = zero
      maxcls = 2**ndim + 2*ndim**2 + 6*ndim+1
      maxcls = min0(maxcls,maxpts)
      divaxo=0
c
c*****      end subroutine initialisation
      if(minpts.lt.0) then
         sbrgns=wrkstr(lenwrk-1)
         goto 280
      endif

      do 30 j=1,ndim
       width(j)=(b(j)-a(j))*half
 30       center(j)=a(j)+width(j)
      finest=zero
      wrkstr(lenwrk)=zero
      divflg=1
      subrgn=rgnstr
      sbrgns=rgnstr

C-- REPEAT --- (outermost loop) -------
   40 call bsrl(ndim,center,width,maxcls,rulcls,
     *         errmin,rgnerr,rgnval,divaxo,divaxn)
      finest=finest+rgnval
      wrkstr(lenwrk)=wrkstr(lenwrk)+rgnerr
      funcls = funcls + rulcls
c
c*****      place results of basic rule into partially ordered list
c*****      according to subregion error
      if(divflg .eq. 0) then
c
c*****      when divflg=0 start at top of list and move down list tree to
c     find correct position for results from first half of recently
c     divided subregion
 200       subtmp=2*subrgn
       if(subtmp.le.sbrgns) then
          if(subtmp.ne.sbrgns) then
             sbtmpp=subtmp+rgnstr
             if(wrkstr(subtmp).lt.wrkstr(sbtmpp)) subtmp=sbtmpp
          endif
 210          if(rgnerr.lt.wrkstr(subtmp)) then
             do 220 k=1,rgnstr
              index1=subrgn-k+1
              index2=subtmp-k+1
              wrkstr(index1)=wrkstr(index2)
 220             continue
             subrgn=subtmp

             goto 200
          endif
       endif

      else
c
c*****when divflg=1 start at bottom right branch and move up list
c     tree to find correct position for results from second half of
c     recently divided subregion
 230       subtmp=(subrgn/(rgnstr*2))*rgnstr
       if(subtmp.ge.rgnstr) then
          if(rgnerr.gt.wrkstr(subtmp)) then
             do 240 k=1,rgnstr
              index1=subrgn-k+1
              index2=subtmp-k+1
              wrkstr(index1)=wrkstr(index2)
 240             continue
             subrgn=subtmp
             goto 230
          endif
       endif
      endif

c*****      store results of basic rule in correct position in list
  250 wrkstr(subrgn)=rgnerr
      wrkstr(subrgn-1)=rgnval
      wrkstr(subrgn-2)=divaxn
      do 260 j=1,ndim
      subtmp=subrgn-2*(j+1)
      wrkstr(subtmp+1)=center(j)
      wrkstr(subtmp)=width(j)
 260  continue
      if(divflg .eq. 0) then
c***  when divflg=0 prepare for second application of basic rule
       center(divaxo)=center(divaxo)+two*width(divaxo)
       sbrgns=sbrgns+rgnstr
       subrgn=sbrgns
       divflg=1
c***  loop back to apply basic rule to other half of subregion
       go to 40
      endif
c
c*****      end ordering and storage of basic rule results
c*****      make checks for possible termination of routine
c
  270 relerr=one
      if(wrkstr(lenwrk).le.zero) wrkstr(lenwrk)=zero
      if(dabs(finest).ne.zero) relerr=wrkstr(lenwrk)/dabs(finest)
      if(relerr.gt.one) relerr=one

      if(sbrgns+rgnstr.gt.lenwrk-2) ifail=2
      if(funcls+funcls*rgnstr/sbrgns.gt.maxpts) ifail=1
      if(relerr.lt.eps.and.funcls.ge.minpts) ifail=0
      if(ifail.lt.3) goto 990
c
c*****      prepare to use basic rule on each half of subregion with largest
c      error
  280 divflg=0
      subrgn=rgnstr
      subtmp = 2*sbrgns/rgnstr
      maxcls = maxpts/subtmp
      errmin = dabs(finest)*eps/dfloat(subtmp)
      wrkstr(lenwrk)=wrkstr(lenwrk)-wrkstr(subrgn)
      finest=finest-wrkstr(subrgn-1)
      divaxo=wrkstr(subrgn-2)
      do 290 j=1,ndim
      subtmp=subrgn-2*(j+1)
      center(j)=wrkstr(subtmp+1)
  290      width(j)=wrkstr(subtmp)
      width(divaxo)=width(divaxo)*half
      center(divaxo)=center(divaxo)-width(divaxo)
c
c*****      loop back to apply basic rule
c
      goto 40
c
c*****      termination point
c
  990 minpts=funcls
      wrkstr(lenwrk-1)=sbrgns
      return
      end
      subroutine bsrl(s, center,hwidth, maxvls,funcls,
     *                  errmin,errest,basest,divaxo,divaxn)

      implicit none
C-- Arguments:
      integer s
      double precision center(s), hwidth(s)
      integer maxvls,funcls, divaxo,divaxn
      double precision errmin, errest, basest
C
      EXTERNAL adphlp
      double precision adphlp
C-- Local Variables:
      double precision  intvls(20), z(20), fulsms(200), weghts(200)

      integer intcls, i, mindeg, maxdeg, maxord, minord
      integer ifail

      double precision zero, one, two, ten, dif, errorm,
     *     sum0, sum1, sum2, difmax, x1, x2

      data zero/0d0/, one/1d0/, two/2d0/, ten/10d0/

      maxdeg = 12
      mindeg = 4
      minord = 0
      do 10 maxord = mindeg,maxdeg
         call symrl(s, center, hwidth, minord, maxord, intvls,
     *        intcls, 200, weghts, fulsms, ifail)
         if (ifail.eq.2) goto 20
         errest = dabs(intvls(maxord)  -intvls(maxord-1))
         errorm = dabs(intvls(maxord-1)-intvls(maxord-2))
         if (errest.ne.zero)
     &        errest = errest*
     &        dmax1(one/ten,errest/dmax1(errest/two,errorm))
         if (errorm.le. 5.*errest) goto 20
         if (2*intcls.gt.maxvls) goto 20
         if (errest.lt.errmin) goto 20
 10   continue
 20   difmax = -1
      x1 = one/two**2
      x2 = 3.*x1
      do 30 i = 1,s
         z(i) = center(i)
 30   continue
cmmm
      sum0 = adphlp(s,z)
      do 40 i = 1,s
         z(i) = center(i) - x1*hwidth(i)
cmmm
         sum1 = adphlp(s,z)
         z(i) = center(i) + x1*hwidth(i)

         sum1 = sum1 + adphlp(s,z)
         z(i) = center(i) - x2*hwidth(i)

         sum2 = adphlp(s,z)
         z(i) = center(i) + x2*hwidth(i)

         sum2 = sum2 + adphlp(s,z)
         z(i) = center(i)

         dif = dabs((sum1-two*sum0) - (x1/x2)**2*(sum2-two*sum0))
         if (dif.ge.difmax) then
            difmax = dif
            divaxn = i
         endif
 40   continue
      if (sum0.eq.sum0+difmax/two) divaxn = mod(divaxo,s) + 1
      basest = intvls(minord)
      funcls = intcls + 4*s
      return
      end
      double precision function flsm(s,center,hwidth,x,m,mp,maxord,
     * g,sumcls)
c
c***  function to compute fully symmetric basic rule sum
c
      integer s, m(s), mp(s), maxord, sumcls, ixchng, lxchng, i, l,
     * ihalf, mpi, mpl
      double precision g(maxord), x(s), intwgt, zero, one,two, intsum,
     * center(s), hwidth(s)
      double precision adphlp

      zero = 0
      one = 1
      two = 2

      intwgt = one
      do 10 i=1,s
        mp(i) = m(i)
        if (m(i).ne.0) intwgt = intwgt/two
        intwgt = intwgt*hwidth(i)
   10 continue
      sumcls = 0
      flsm = zero
c
c*******  compute centrally symmetric sum for permutation mp
   20 intsum = zero
      do 30 i=1,s
        mpi = mp(i) + 1
        x(i) = center(i) + g(mpi)*hwidth(i)
   30 continue
   40 sumcls = sumcls + 1
cmmm
      intsum = intsum + adphlp(s,x)
      do 50 i=1,s
        mpi = mp(i) + 1
        if(g(mpi).ne.zero) hwidth(i) = -hwidth(i)
        x(i) = center(i) + g(mpi)*hwidth(i)
        if (x(i).lt.center(i)) go to 40
   50 continue
c*******  end integration loop for mp
c
      flsm = flsm + intwgt*intsum
      if (s.eq.1) return
c
c*******  find next distinct permutation of m and loop back
c          to compute next centrally symmetric sum
      do 80 i=2,s
        if (mp(i-1).le.mp(i)) go to 80
        mpi = mp(i)
        ixchng = i - 1
        if (i.eq.2) go to 70
        ihalf = ixchng/2
        do 60 l=1,ihalf
          mpl = mp(l)
          imnusl = i - l
          mp(l) = mp(imnusl)
          mp(imnusl) = mpl
          if (mpl.le.mpi) ixchng = ixchng - 1
          if (mp(l).gt.mpi) lxchng = l
   60   continue
        if (mp(ixchng).le.mpi) ixchng = lxchng
   70   mp(i) = mp(ixchng)
        mp(ixchng) = mpi
        go to 20
   80 continue
c*****  end loop for permutations of m and associated sums
c
      return
      end
      subroutine nxprt(prtcnt, s, m)
c
c***  subroutine to compute the next s partition
c
      implicit none
      integer s, m(s), prtcnt

      integer i,k, msum

      if (prtcnt.gt.0) go to 20
      do 10 i=1,s
        m(i) = 0
   10 continue
      prtcnt = 1
      return
   20 prtcnt = prtcnt + 1
      msum = m(1)
      if (s.eq.1) go to 60
      do 50 i=2,s
        msum = msum + m(i)
        if (m(1).le.m(i)+1) go to 40
        m(1) = msum - (i-1)*(m(i)+1)
        do 30 k=2,i
          m(k) = m(i) + 1
   30   continue
        return
   40   m(i) = 0
   50 continue
   60 m(1) = msum + 1
      return
      end
      subroutine symrl(s, center, hwidth, minord, maxord, intvls,
     *     intcls, numsms, weghts, fulsms, fail)
c  multidimensional fully symmetric rule integration subroutine
c
c   this subroutine computes a sequence of fully symmetric rule
c   approximations to a fully symmetric multiple integral.
c   written by a. genz, mathematical institute, university of kent,
c   canterbury, kent ct2 7nf, england
c
c**************       parameters for symrl  ********************************
c*****input parameters
c  s         integer number of variables, must exceed 0 but not exceed 20
c  f         externally declared user defined real function integrand.
c         it must have parameters (s,x), where x is a real array
c         with dimension s.
c  minord  integer minimum order parameter.  on entry minord specifies
c         the current highest order approximation to the integral,
c         available in the array intvls.  for the first call of symrl
c         minord should be set to 0.  otherwise a previous call is
c         assumed that computed intvls(1), ... , intvls(minord).
c         on exit minord is set to maxord.
c  maxord  integer maximum order parameter, must be greater than minord
c         and not exceed 20. the subroutine computes intvls(minord+1),
c         intvls(minord+2),..., intvls(maxord).
c  g         real array of dimension(maxord) of generators.
c         all generators must be distinct and nonnegative.
c  numsms  integer length of array fulsms, must be at least the sum of
c         the number of distinct partitions of length at most s
c         of the integers 0,1,...,maxord-1.  an upper bound for numsms
c         when s+maxord is less than 19 is 200
c******output parameters
c  intvls  real array of dimension(maxord).  upon successful exit
c         intvls(1), intvls(2),..., intvls(maxord) are approximations
c         to the integral.  intvls(d+1) will be an approximation of
c         polynomial degree 2d+1.
c  intcls  integer total number of f values needed for intvls(maxord)
c  weghts  real working storage array with dimension (numsms). on exit
c         weghts(j) contains the weight for fulsms(j).
c  fulsms  real working storage array with dimension (numsms). on exit
c         fulsms(j) contains the fully symmetric basic rule sum
c         indexed by the jth s-partition of the integers
c         0,1,...,maxord-1.
c  fail         integer failure output parameter
c         fail=0 for successful termination of the subroutine
c         fail=1 when numsms is too small for the subroutine to
c               continue.  in this case weghts(1), weghts(2), ...,
c               weghts(numsms), fulsms(1), fulsms(2), ...,
c               fulsms(numsms) and intvls(1), intvls(2),...,
c               intvls(j) are returned, where j is maximum value of
c               maxord compatible with the given value of numsms.
c         fail=2 when parameters s,minord, maxord or g are out of
c               range
c***********************************************************************
cmmm        external f
ctsl      real f
ctsl       double precision f
c***  for double precision change real to double precision
c      in the next statement
      integer d, i, fail, k(20), intcls, prtcnt, l, m(20), maxord,
     * minord, modofm, numsms, s, sumcls
      double precision intvls(maxord), center(s), hwidth(s), gisqrd,
     * glsqrd,
     * intmpa, intmpb, intval, one, fulsms(numsms), weghts(numsms),
     * two, momtol, momnkn, momprd(20,20), moment(20), zero, g(20)
      double precision flsm, wht
c      patterson generators
      data g(1), g(2) /0.0000000000000000,0.7745966692414833/
      data g(3), g(4) /0.9604912687080202,0.4342437493468025/
      data g(5), g(6) /0.9938319632127549,0.8884592328722569/
      data g(7), g(8) /0.6211029467372263,0.2233866864289668/
      data g(9), g(10), g(11), g(12) /0.1, 0.2, 0.3, 0.4/
c
c***  parameter checking and initialisation
      fail = 2
      maxrdm = 20
      maxs = 20
      if (s.gt.maxs .or. s.lt.1) return
      if (minord.lt.0 .or. minord.ge.maxord) return
      if (maxord.gt.maxrdm) return
      zero = 0
      one = 1
      two = 2
      momtol = one
   10 momtol = momtol/two
      if (momtol+one.gt.one) go to 10
      hundrd = 100
      momtol = hundrd*two*momtol
      d = minord
      if (d.eq.0) intcls = 0
c***  calculate moments and modified moments
      do 20 l=1,maxord
      floatl = l + l - 1
      moment(l) = two/floatl
   20 continue
      if (maxord.ne.1) then
         do 40 l=2,maxord
            intmpa = moment(l-1)
            glsqrd = g(l-1)**2
            do 30 i=l,maxord
               intmpb = moment(i)
               moment(i) = moment(i) - glsqrd*intmpa
               intmpa = intmpb
 30         continue
            if (moment(l)**2.lt.(momtol*moment(1))**2) moment(l) = zero
 40      continue
      endif
      do 70 l=1,maxord
      if (g(l).lt.zero) return
      momnkn = one
      momprd(l,1) = moment(1)
      if (maxord.eq.1) go to 70
      glsqrd = g(l)**2
      do 60 i=2,maxord
        if (i.le.l) gisqrd = g(i-1)**2
        if (i.gt.l) gisqrd = g(i)**2
        if (glsqrd.eq.gisqrd) return
        momnkn = momnkn/(glsqrd-gisqrd)
        momprd(l,i) = momnkn*moment(i)
   60      continue
   70 continue
      fail = 1
c
c***  begin LOOP
c      for each d find all distinct partitions m with mod(m))=d
c
   80 prtcnt = 0
      intval = zero
      modofm = 0
      call nxprt(prtcnt, s, m)
   90 if (prtcnt.gt.numsms) return
c
c***  calculate weight for partition m and fully symmetric sums
c***       when necessary
c
      if (d.eq.modofm) weghts(prtcnt) = zero
      if (d.eq.modofm) fulsms(prtcnt) = zero
      fulwgt = wht(s,moment,m,k,modofm,d,maxrdm,momprd)
      sumcls = 0
      if (weghts(prtcnt).eq.zero .and. fulwgt.ne.zero) fulsms(prtcnt) =
     * flsm(s, center, hwidth, moment, m, k, maxord, g, sumcls)
      intcls = intcls + sumcls
      intval = intval + fulwgt*fulsms(prtcnt)
      weghts(prtcnt) = weghts(prtcnt) + fulwgt
      call nxprt(prtcnt, s, m)
      if (m(1).gt.modofm) modofm = modofm + 1
      if (modofm.le.d) go to 90
c
c***  end loop for each d
      if (d.gt.0) intval = intvls(d) + intval
      intvls(d+1) = intval
      d = d + 1
      if (d.lt.maxord) go to 80
c
c***  set failure parameter and return
      fail = 0
      minord = maxord
      return
      end
      double precision function wht(s, intrps, m, k, modofm, d,
     *     maxrdm, momprd)
c***  subroutine to calculate weight for partition m
c
      integer s, m(s), k(s), d, maxrdm, mi, ki, m1, k1, modofm
      double precision intrps(s), zero, momprd(maxrdm,maxrdm)

      zero = 0
      do 10 i=1,s
        intrps(i) = zero
        k(i) = 0
   10 continue
      m1 = m(1) + 1
      k1 = d - modofm + m1
   20 intrps(1) = momprd(m1,k1)
      if (s.eq.1) go to 40
      do 30 i=2,s
        mi = m(i) + 1
        ki = k(i) + mi
        intrps(i) = intrps(i) + momprd(mi,ki)*intrps(i-1)
        intrps(i-1) = zero
        k1 = k1 - 1
        k(i) = k(i) + 1
        if (k1.ge.m1) go to 20
        k1 = k1 + k(i)
        k(i) = 0
   30 continue
   40 wht = intrps(s)
      return
      end
