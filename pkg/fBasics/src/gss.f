      subroutine dcoef (s, lds, nobs, nnull, qraux, jpvt, z, q, ldq, 
     *nlaht, c, d, info, twk)
      integer lds, nobs, nnull, jpvt(*), ldq, info
      double precision s(lds,*), qraux(*), z(*), q(ldq,*), nlaht, c(*), 
     *d(*), twk(2,*)
      double precision dum, ddot
      integer n, n0
      info = 0
      if(.not.( nnull .lt. 1 .or. nnull .ge. nobs .or. nobs .gt. lds 
     *.or. nobs .gt. ldq ))goto 23000
      info = -1
      return
23000 continue
      n0 = nnull
      n = nobs - nnull
      call dset (n, 10.d0 ** nlaht, twk(2,1), 2)
      call daxpy (n, 1.d0, q(n0+1,n0+1), ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(n0+1,n0+2), ldq+1, twk(1,2), 2)
      call dpbfa (twk, 2, n, 1, info)
      if(.not.( info .ne. 0 ))goto 23002
      info = -2
      return
23002 continue
      call dpbsl (twk, 2, n, 1, z(n0+1))
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, twk, 1)
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, twk, z(n0+2), z(n0+2), 
     *dum, dum, dum, dum, 10000, info)
      call dset (n0, 0.d0, c, 1)
      call dcopy (n, z(n0+1), 1, c(n0+1), 1)
      call dqrsl (s, lds, nobs, nnull, qraux, c, c, dum, dum, dum, dum, 
     *10000, info)
      j=1
23004 if(.not.(j.le.n0))goto 23006
      d(j) = z(j) - ddot (n, z(n0+1), 1, q(n0+1,j), 1)
      j=j+1
      goto 23004
23006 continue
      call dtrsl (s, lds, n0, d, 01, info)
      call dprmut (d, n0, jpvt, 1)
      return
      end
      subroutine dcore (vmu, q, ldq, nobs, nnull, tol, z, job, limnla, 
     *nlaht, score, varht, info, twk, work)
      character*1 vmu
      integer ldq, nobs, nnull, job, info
      double precision q(ldq,*), tol, z(*), limnla(2), nlaht, score(*), 
     *varht, twk(2,*), work(*)
      double precision dum, low, upp, dasum, mchpr
      integer n0, n, j
      info = 0
      if(.not.( vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u' ))
     *goto 23000
      info = -3
      return
23000 continue
      if(.not.( nnull .lt. 1 .or. nobs .le. nnull .or. nobs .gt. ldq ))
     *goto 23002
      info = -1
      return
23002 continue
      n0 = nnull
      n = nobs - nnull
      call dsytr (q(n0+1,n0+1), ldq, n, tol, info, work)
      if(.not.( info .ne. 0 ))goto 23004
      return
23004 continue
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, work, 1)
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, work, z(n0+2), dum, z(n0+
     *2), dum, dum, dum, 01000, info)
      if(.not.( job .eq. 0 ))goto 23006
      mchpr = 1.d0
23008 if(.not.( 1.d0 + mchpr .gt. 1.d0 ))goto 23009
      mchpr = mchpr / 2.d0
      goto 23008
23009 continue
      mchpr = mchpr * 2.d0
      limnla(2) = dmax1 (dasum (n, q(n0+1,n0+1), ldq+1) * 1.d2, mchpr)
      limnla(1) = limnla(2) * mchpr
      limnla(2) = dlog10 (limnla(2))
      limnla(1) = dlog10 (limnla(1))
23006 continue
      low = limnla(1)
      upp = limnla(2)
      if(.not.( job .le. 0 ))goto 23010
      call dgold (vmu, q(n0+1,n0+1), ldq, n, z(n0+1), low, upp, nlaht, 
     *score(1), varht, info, twk, work)
      if(.not.( vmu .eq. 'v' ))goto 23012
      score(1) = score(1) * dfloat (nobs) / dfloat (n)
23012 continue
      if(.not.( vmu .eq. 'm' ))goto 23014
      score(1) = score(1) * dfloat (n) / dfloat (nobs)
23014 continue
      if(.not.( vmu .eq. 'u' ))goto 23016
      score(1) = score(1) * dfloat (n) / dfloat (nobs) + 2.d0 * varht
23016 continue
      goto 23011
23010 continue
      call deval (vmu, q(n0+1,n0+1), ldq, n, z(n0+1), job, low, upp, 
     *nlaht, score, varht, info, twk, work)
      dum = dfloat (nobs) / dfloat (n)
      j=1
23018 if(.not.(j.le.job+1))goto 23020
      if(.not.( vmu .eq. 'v' ))goto 23021
      score(j) = score(j) * dum
23021 continue
      if(.not.( vmu .eq. 'm' ))goto 23023
      score(j) = score(j) / dum
23023 continue
      if(.not.( vmu .eq. 'u' ))goto 23025
      score(j) = score(j) / dum + 2.d0 * varht
23025 continue
      j=j+1
      goto 23018
23020 continue
23011 continue
      return
      end
      subroutine dcrdr (s, lds, nobs, nnull, qraux, jpvt, q, ldq, nlaht,
     & r, ldr, nr, cr, ldcr, dr, lddr, wk, info)
      integer lds, nobs, nnull, jpvt(*), ldq, ldr, nr, ldcr, lddr, info
      double precision s(lds,*), qraux(*), q(ldq,*), nlaht, r(ldr,*), 
     &cr(ldcr,*), dr(lddr,*), wk(2,*)
      double precision dum, ddot
      integer i, j, n, n0
      info = 0
      if(.not.( nnull .lt. 1 .or. nnull .ge. nobs .or. nobs .gt. lds 
     &.or. nobs .gt. ldq .or. ldr .lt. nobs .or. nr .lt. 1 .or. ldcr 
     &.lt. nobs .or. lddr .lt. nnull ))goto 23000
      info = -1
      return
23000 continue
      n0 = nnull
      n = nobs - nnull
      j=1
23002 if(.not.(j.le.nr))goto 23004
      call dcopy (nobs, r(1,j), 1, cr(1,j), 1)
      j=j+1
      goto 23002
23004 continue
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, wk, 1)
      j=1
23005 if(.not.(j.le.nr))goto 23007
      call dqrsl (s, lds, nobs, nnull, qraux, cr(1,j), dum, cr(1,j), 
     &dum, dum, dum, 01000, info)
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, wk, cr(n0+2,j), dum, cr(
     &n0+2,j), dum, dum, dum, 01000, info)
      j=j+1
      goto 23005
23007 continue
      call dset (n, 10.d0 ** nlaht, wk(2,1), 2)
      call daxpy (n, 1.d0, q(n0+1,n0+1), ldq+1, wk(2,1), 2)
      call dcopy (n-1, q(n0+1,n0+2), ldq+1, wk(1,2), 2)
      call dpbfa (wk, 2, n, 1, info)
      if(.not.( info .ne. 0 ))goto 23008
      info = -2
      return
23008 continue
      j=1
23010 if(.not.(j.le.nr))goto 23012
      call dpbsl (wk, 2, n, 1, cr(n0+1,j))
      j=j+1
      goto 23010
23012 continue
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, wk, 1)
      j=1
23013 if(.not.(j.le.nr))goto 23015
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, wk, cr(n0+2,j), cr(n0+2,
     &j), dum, dum, dum, dum, 10000, info)
      j=j+1
      goto 23013
23015 continue
      j=1
23016 if(.not.(j.le.nr))goto 23018
      i=1
23019 if(.not.(i.le.n0))goto 23021
      dr(i,j) = cr(i,j) - ddot (n, cr(n0+1,j), 1, q(n0+1,i), 1)
      i=i+1
      goto 23019
23021 continue
      call dtrsl (s, lds, n0, dr(1,j), 01, info)
      call dprmut (dr(1,j), n0, jpvt, 1)
      j=j+1
      goto 23016
23018 continue
      j=1
23022 if(.not.(j.le.nr))goto 23024
      call dset (n0, 0.d0, cr(1,j), 1)
      call dqrsl (s, lds, nobs, nnull, qraux, cr(1,j), cr(1,j), dum, 
     &dum, dum, dum, 10000, info)
      j=j+1
      goto 23022
23024 continue
      return
      end
      subroutine ddeev (vmu, nobs, q, ldqr, ldqc, n, nq, u, ldu, uaux, 
     *t, x, theta, nlaht, score, varht, hes, ldh, gra, hwk1, hwk2, gwk1,
     * gwk2, kwk, ldk, work1, work2, work3, info)
      character*1 vmu
      integer nobs, ldqr, ldqc, n, nq, ldu, ldh, ldk, info
      double precision q(ldqr,ldqc,*), u(ldu,*), uaux(*), t(2,*), x(*), 
     *theta(*), nlaht, score, varht, hes(ldh,*), gra(*), hwk1(nq,*), 
     *hwk2(nq,*), gwk1(*), gwk2(*), kwk(ldk,ldk,*), work1(*), work2(*), 
     *work3(*)
      double precision trc, det, dum, ddot
      integer i, j, m
      info = 0
      call dset (nq, 0.d0, gra, 1)
      call dset (nq*nq, 0.d0, hes, 1)
      if(.not.( vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u' ))
     *goto 23000
      info = -3
      return
23000 continue
      if(.not.( nobs .lt. n .or. ldqr .lt. n .or. ldqc .lt. n .or. nq 
     *.le. 0 .or. ldu .lt. n-1 .or. ldh .lt. nq .or. ldk .lt. n ))goto 2
     *3002
      info = -1
      return
23002 continue
      i=2
23004 if(.not.(i.le.nq))goto 23006
      if(.not.( theta(i) .le. -25.d0 ))goto 23007
      goto 23005
23007 continue
      j=1
23009 if(.not.(j.le.n))goto 23011
      call dcopy (n-j+1, q(j,j,i), 1, kwk(j,j,i), 1)
      call dscal (n-j+1, 10.d0 ** theta(i), kwk(j,j,i), 1)
      j=j+1
      goto 23009
23011 continue
      call dqrslm (u, ldu, n-1, n-2, uaux, kwk(2,2,i), n, 0, info, 
     *work1)
      call dqrsl (u, ldu, n-1, n-2, uaux, kwk(2,1,i), dum, kwk(2,1,i), 
     *dum, dum, dum, 01000, info)
23005 i=i+1
      goto 23004
23006 continue
      call dcopy (n, t(2,1), 2, kwk(1,1,1), n+1)
      call dcopy (n-1, t(1,2), 2, kwk(2,1,1), n+1)
      j=1
23012 if(.not.(j.lt.n-1))goto 23014
      call dset (n-j-1, 0.d0, kwk(j+2,j,1), 1)
      j=j+1
      goto 23012
23014 continue
      i=2
23015 if(.not.(i.le.nq))goto 23017
      if(.not.( theta(i) .le. -25.d0 ))goto 23018
      goto 23016
23018 continue
      j=1
23020 if(.not.(j.le.n))goto 23022
      call daxpy (n-j+1, -1.d0, kwk(j,j,i), 1, kwk(j,j,1), 1)
      j=j+1
      goto 23020
23022 continue
23016 i=i+1
      goto 23015
23017 continue
      i=1
23023 if(.not.(i.le.nq))goto 23025
      if(.not.( theta(i) .le. -25.d0 ))goto 23026
      goto 23024
23026 continue
      j=1
23028 if(.not.(j.lt.n))goto 23030
      call dcopy (n-j, kwk(j+1,j,i), 1, kwk(j,j+1,i), n)
      j=j+1
      goto 23028
23030 continue
23024 i=i+1
      goto 23023
23025 continue
      call dset (n, 10.d0 ** nlaht, work1, 1)
      call daxpy (n, 1.d0, work1, 1, t(2,1), 2)
      call dpbfa (t, 2, n, 1, info)
      if(.not.( info .ne. 0 ))goto 23031
      info = -2
      return
23031 continue
      i=1
23033 if(.not.(i.le.nq))goto 23035
      if(.not.( theta(i) .le. -25.d0 ))goto 23036
      goto 23034
23036 continue
      j=1
23038 if(.not.(j.le.n))goto 23040
      call dpbsl (t, 2, n, 1, kwk(1,j,i))
      j=j+1
      goto 23038
23040 continue
23034 i=i+1
      goto 23033
23035 continue
      call dcopy (n, x, 1, work1, 1)
      call dpbsl (t, 2, n, 1, work1)
      if(.not.( vmu .ne. 'm' ))goto 23041
      call dcopy (n, work1, 1, work2, 1)
      call dscal (n, 2.d0, work2, 1)
      goto 23042
23041 continue
      call dcopy (n, x, 1, work2, 1)
23042 continue
      i=1
23043 if(.not.(i.le.nq))goto 23045
      if(.not.( theta(i) .le. -25.d0 ))goto 23046
      goto 23044
23046 continue
      call dgemv ('t', n, n, 1.d0, kwk(1,1,i), n, work2, 1, 0.d0, work3,
     * 1)
      gwk1(i) = - ddot (n, work1, 1, work3, 1)
23044 i=i+1
      goto 23043
23045 continue
      i=1
23048 if(.not.(i.le.nq))goto 23050
      gwk2(i) = 0.d0
      if(.not.( theta(i) .le. -25.d0 ))goto 23051
      goto 23049
23051 continue
      j=1
23053 if(.not.(j.le.n))goto 23055
      if(.not.( vmu .ne. 'm' ))goto 23056
      call dcopy (n, kwk(1,j,i), 1, work1, 1)
      call dpbsl (t, 2, n, 1, work1)
      gwk2(i) = gwk2(i) - work1(j)
      goto 23057
23056 continue
      gwk2(i) = gwk2(i) - kwk(j,j,i)
23057 continue
      j=j+1
      goto 23053
23055 continue
23049 i=i+1
      goto 23048
23050 continue
      if(.not.( vmu .ne. 'm' ))goto 23058
      call dcopy (n, x, 1, work1, 1)
      call dpbsl (t, 2, n, 1, work1)
      i=1
23060 if(.not.(i.le.nq))goto 23062
      if(.not.( theta(i) .le. -25.d0 ))goto 23063
      goto 23061
23063 continue
      call dgemv ('n', n, n, 1.d0, kwk(1,1,i), n, work1, 1, 0.d0, work2,
     * 1)
      j=1
23065 if(.not.(j.le.i))goto 23067
      if(.not.( theta(j) .le. -25.d0 ))goto 23068
      goto 23066
23068 continue
      call dgemv ('n', n, n, 1.d0, kwk(1,1,j), n, work1, 1, 0.d0, work3,
     * 1)
      hwk1(i,j) = 2.d0 * ddot (n, work2, 1, work3, 1)
      call dgemv ('t', n, n, 1.d0, kwk(1,1,j), n, work1, 1, 0.d0, work3,
     * 1)
      hwk1(i,j) = hwk1(i,j) + 2.d0 * ddot (n, work2, 1, work3, 1)
23066 j=j+1
      goto 23065
23067 continue
      call dgemv ('t', n, n, 1.d0, kwk(1,1,i), n, work1, 1, 0.d0, work2,
     * 1)
      j=1
23070 if(.not.(j.le.i))goto 23072
      if(.not.( theta(j) .le. -25.d0 ))goto 23073
      goto 23071
23073 continue
      call dgemv ('n', n, n, 1.d0, kwk(1,1,j), n, work1, 1, 0.d0, work3,
     * 1)
      hwk1(i,j) = hwk1(i,j) + 2.d0 * ddot (n, work2, 1, work3, 1)
23071 j=j+1
      goto 23070
23072 continue
23061 i=i+1
      goto 23060
23062 continue
      goto 23059
23058 continue
      call dcopy (n, x, 1, work1, 1)
      call dpbsl (t, 2, n, 1, work1)
      i=1
23075 if(.not.(i.le.nq))goto 23077
      if(.not.( theta(i) .le. -25.d0 ))goto 23078
      goto 23076
23078 continue
      call dgemv ('n', n, n, 1.d0, kwk(1,1,i), n, work1, 1, 0.d0, work2,
     * 1)
      j=1
23080 if(.not.(j.le.i))goto 23082
      if(.not.( theta(j) .le. -25.d0 ))goto 23083
      goto 23081
23083 continue
      call dgemv ('t', n, n, 1.d0, kwk(1,1,j), n, x, 1, 0.d0, work3, 1)
      hwk1(i,j) = 2.d0 * ddot (n, work2, 1, work3, 1)
23081 j=j+1
      goto 23080
23082 continue
23076 i=i+1
      goto 23075
23077 continue
23059 continue
      i=1
23085 if(.not.(i.le.nq))goto 23087
      if(.not.( theta(i) .le. -25.d0 ))goto 23088
      goto 23086
23088 continue
      hwk1(i,i) = hwk1(i,i) + gwk1(i)
23086 i=i+1
      goto 23085
23087 continue
      i=1
23090 if(.not.(i.le.nq))goto 23092
      if(.not.( theta(i) .le. -25.d0 ))goto 23093
      goto 23091
23093 continue
      m=1
23095 if(.not.(m.le.i))goto 23097
      hwk2(i,m) = 0.d0
      if(.not.( theta(m) .le. -25.d0 ))goto 23098
      goto 23096
23098 continue
      j=1
23100 if(.not.(j.le.n))goto 23102
      if(.not.( vmu .ne. 'm' ))goto 23103
      call dcopy (n, kwk(1,j,m), 1, work1, 1)
      call dpbsl (t, 2, n, 1, work1)
      hwk2(i,m) = hwk2(i,m) + 2.d0 * ddot (n, kwk(j,1,i), n, work1, 1)
      goto 23104
23103 continue
      hwk2(i,m) = hwk2(i,m) + ddot (n, kwk(j,1,i), n, kwk(1,j,m), 1)
23104 continue
      j=j+1
      goto 23100
23102 continue
23096 m=m+1
      goto 23095
23097 continue
23091 i=i+1
      goto 23090
23092 continue
      i=1
23105 if(.not.(i.le.nq))goto 23107
      if(.not.( theta(i) .le. -25.d0 ))goto 23108
      goto 23106
23108 continue
      hwk2(i,i) = hwk2(i,i) + gwk2(i)
23106 i=i+1
      goto 23105
23107 continue
      if(.not.( vmu .eq. 'v' ))goto 23110
      trc = dfloat (nobs) * 10.d0 ** (-nlaht) * varht / score
      i=1
23112 if(.not.(i.le.nq))goto 23114
      if(.not.( theta(i) .le. -25.d0 ))goto 23115
      goto 23113
23115 continue
      gra(i) = gwk1(i) / trc / trc - 2.d0 * score * gwk2(i) / trc / 
     *dfloat(nobs)
23113 i=i+1
      goto 23112
23114 continue
      call dscal (nq, dfloat (nobs), gra, 1)
23110 continue
      if(.not.( vmu .eq. 'u' ))goto 23117
      dum = 10.d0 ** nlaht
      i=1
23119 if(.not.(i.le.nq))goto 23121
      if(.not.( theta(i) .le. -25.d0 ))goto 23122
      goto 23120
23122 continue
      gra(i) = dum * dum * gwk1(i) - 2.d0 * varht * dum * gwk2(i)
23120 i=i+1
      goto 23119
23121 continue
      call dscal (nq, 1.d0/dfloat (n), gra, 1)
23117 continue
      if(.not.( vmu .eq. 'm' ))goto 23124
      det = 10.d0 ** (-nlaht) * varht / score
      i=1
23126 if(.not.(i.le.nq))goto 23128
      if(.not.( theta(i) .le. -25.d0 ))goto 23129
      goto 23127
23129 continue
      gra(i) = gwk1(i) / det - dfloat (nobs) / dfloat (n) * score * 
     *gwk2(i)
23127 i=i+1
      goto 23126
23128 continue
      call dscal (nq, 1.d0 / dfloat (nobs), gra, 1)
23124 continue
      if(.not.( vmu .eq. 'v' ))goto 23131
      i=1
23133 if(.not.(i.le.nq))goto 23135
      if(.not.( theta(i) .le. -25.d0 ))goto 23136
      goto 23134
23136 continue
      j=1
23138 if(.not.(j.le.i))goto 23140
      if(.not.( theta(j) .le. -25.d0 ))goto 23141
      goto 23139
23141 continue
      hes(i,j) = hwk1(i,j) / trc / trc - 2.d0 * gwk1(i) * gwk2(j) / trc 
     *** 3 - 2.d0 * gwk1(j) * gwk2(i) / trc ** 3 - 2.d0 * score * hwk2(
     *i,j) / trc / dfloat (nobs) + 6.d0 * score * gwk2(i) * gwk2(j) / 
     *trc / trc / dfloat (nobs)
23139 j=j+1
      goto 23138
23140 continue
      call dscal (i, dfloat (nobs), hes(i,1), ldh)
23134 i=i+1
      goto 23133
23135 continue
23131 continue
      if(.not.( vmu .eq. 'u' ))goto 23143
      i=1
23145 if(.not.(i.le.nq))goto 23147
      if(.not.( theta(i) .le. -25.d0 ))goto 23148
      goto 23146
23148 continue
      j=1
23150 if(.not.(j.le.i))goto 23152
      if(.not.( theta(j) .le. -25.d0 ))goto 23153
      goto 23151
23153 continue
      hes(i,j) = dum * dum * hwk1(i,j) - 2.d0 * varht * dum * hwk2(i,j)
23151 j=j+1
      goto 23150
23152 continue
      call dscal (i, 1.d0/dfloat (n), hes(i,1), ldh)
23146 i=i+1
      goto 23145
23147 continue
23143 continue
      if(.not.( vmu .eq. 'm' ))goto 23155
      i=1
23157 if(.not.(i.le.nq))goto 23159
      if(.not.( theta(i) .le. -25.d0 ))goto 23160
      goto 23158
23160 continue
      j=1
23162 if(.not.(j.le.i))goto 23164
      if(.not.( theta(j) .le. -25.d0 ))goto 23165
      goto 23163
23165 continue
      hes(i,j) = hwk1(i,j) / det - gwk1(i) * gwk2(j) / det / dfloat (n) 
     *- gwk1(j) * gwk2(i) / det / dfloat (n) - dfloat (nobs) / dfloat (
     *n) * score * hwk2(i,j) + dfloat (nobs) / dfloat (n) ** 2 * score *
     * gwk2(i) * gwk2(j)
23163 j=j+1
      goto 23162
23164 continue
      call dscal (i, 1.d0 / dfloat (nobs), hes(i,1), ldh)
23158 i=i+1
      goto 23157
23159 continue
23155 continue
      return
      end
      subroutine deval (vmu, q, ldq, n, z, nint, low, upp, nlaht, score,
     * varht, info, twk, work)
      character*1 vmu
      integer ldq, n, nint, info
      double precision q(ldq,*), z(*), low, upp, nlaht, score(*), varht,
     * twk(2,*), work(*)
      double precision tmp, minscr, mlo, varhtwk
      integer j
      info = 0
      if(.not.( upp .lt. low ))goto 23000
      mlo = low
      low = upp
      upp = mlo
23000 continue
      if(.not.( (vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u') 
     *.or. nint .lt. 1 ))goto 23002
      info = -3
      return
23002 continue
      if(.not.( 1 .gt. n .or. n .gt. ldq ))goto 23004
      info = -1
      return
23004 continue
      j=1
23006 if(.not.(j.le.nint+1))goto 23008
      tmp = low + dfloat (j-1) * ( upp - low ) / dfloat (nint)
      call dset (n, 10.d0 ** (tmp), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**tmp
      call dtrev (vmu, twk, 2, n, z, score(j), varht, info, work)
      if(.not.( info .ne. 0 ))goto 23009
      info = -2
      return
23009 continue
      if(.not.( score(j) .le. minscr .or. j .eq. 1 ))goto 23011
      minscr = score(j)
      nlaht = tmp
      varhtwk = varht
23011 continue
      j=j+1
      goto 23006
23008 continue
      varht = varhtwk
      return
      end
      subroutine dgold (vmu, q, ldq, n, z, low, upp, nlaht, score, 
     *varht, info, twk, work)
      character*1 vmu
      integer ldq, n, info
      double precision q(ldq,*), z(*), low, upp, nlaht, score, varht, 
     *twk(2,*), work(*)
      double precision ratio, mlo, mup, tmpl, tmpu
      ratio = ( dsqrt (5.d0) - 1.d0 ) / 2.d0
      info = 0
      if(.not.( upp .lt. low ))goto 23000
      mlo = low
      low = upp
      upp = mlo
23000 continue
      if(.not.( vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u' ))
     *goto 23002
      info = -3
      return
23002 continue
      if(.not.( n .lt. 1 .or. n .gt. ldq ))goto 23004
      info = -1
      return
23004 continue
      mlo = upp - ratio * (upp - low)
      call dset (n, 10.d0 ** (mlo), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**mlo
      call dtrev (vmu, twk, 2, n, z, tmpl, varht, info, work)
      if(.not.( info .ne. 0 ))goto 23006
      info = -2
      return
23006 continue
      mup = low + ratio * (upp - low)
      call dset (n, 10.d0 ** (mup), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**mup
      call dtrev (vmu, twk, 2, n, z, tmpu, varht, info, work)
      if(.not.( info .ne. 0 ))goto 23008
      info = -2
      return
23008 continue
23010 continue
      if(.not.( mup - mlo .lt. 1.d-7 ))goto 23013
      goto 23012
23013 continue
      if(.not.( tmpl .lt. tmpu ))goto 23015
      upp = mup
      mup = mlo
      tmpu = tmpl
      mlo = upp - ratio * (upp - low)
      call dset (n, 10.d0 ** (mlo), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**mlo
      call dtrev (vmu, twk, 2, n, z, tmpl, varht, info, work)
      if(.not.( info .ne. 0 ))goto 23017
      info = -2
      return
23017 continue
      goto 23016
23015 continue
      low = mlo
      mlo = mup
      tmpl = tmpu
      mup = low + ratio * (upp - low)
      call dset (n, 10.d0 ** (mup), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**mup
      call dtrev (vmu, twk, 2, n, z, tmpu, varht, info, work)
      if(.not.( info .ne. 0 ))goto 23019
      info = -2
      return
23019 continue
23016 continue
23011 goto 23010
23012 continue
      nlaht = ( mup + mlo ) / 2.d0
      call dset (n, 10.d0 ** (nlaht), twk(2,1), 2)
      call daxpy (n, 1.d0, q, ldq+1, twk(2,1), 2)
      call dcopy (n-1, q(1,2), ldq+1, twk(1,2), 2)
      twk(1,1) = 10.d0**nlaht
      call dtrev (vmu, twk, 2, n, z, score, varht, info, work)
      if(.not.( info .ne. 0 ))goto 23021
      info = -2
      return
23021 continue
      return
      end
      subroutine dmcdc (a, lda, p, e, jpvt, info)
      integer lda, p, jpvt(*), info
      double precision a(lda,*), e(*)
      double precision beta, delta, theta, tmp, dasum, ddot
      integer i, j, jmax, jtmp, idamax
      info = 0
      if(.not.( lda .lt. p .or. p .lt. 1 ))goto 23000
      info = -1
      return
23000 continue
      tmp = 1.d0
23002 if(.not.( 1.d0 + tmp .gt. 1.d0 ))goto 23003
      tmp = tmp / 2.d0
      goto 23002
23003 continue
      jmax = idamax (p, a, lda+1)
      beta = dmax1 (2.d0 * tmp, dabs (a(jmax,jmax)))
      tmp = dsqrt (dfloat (p*p-1))
      if(.not.( tmp .lt. 1.d0 ))goto 23004
      tmp = 1.d0
23004 continue
      j=2
23006 if(.not.(j.le.p))goto 23008
      jmax = idamax (j-1, a(1,j), 1)
      beta = dmax1 (beta, dabs (a(jmax,j)) / tmp)
      j=j+1
      goto 23006
23008 continue
      delta = dasum (p, a, lda+1) / dfloat (p) * 1.d-7
      delta = dmax1 (delta, 1.d-10)
      j=1
23009 if(.not.(j.le.p))goto 23011
      jpvt(j) = j
      j=j+1
      goto 23009
23011 continue
      j=1
23012 if(.not.(j.le.p))goto 23014
      jmax = idamax (p-j+1, a(j,j), lda+1) + j - 1
      if(.not.( jmax .ne. j ))goto 23015
      call dswap (j-1, a(1,j), 1, a(1,jmax), 1)
      call dswap (jmax-j-1, a(j,j+1), lda, a(j+1,jmax), 1)
      call dswap (p-jmax, a(j,jmax+1), lda, a(jmax,jmax+1), lda)
      tmp = a(j,j)
      a(j,j) = a(jmax,jmax)
      a(jmax,jmax) = tmp
      jtmp = jpvt(j)
      jpvt(j) = jpvt(jmax)
      jpvt(jmax) = jtmp
23015 continue
      i=1
23017 if(.not.(i.lt.j))goto 23019
      a(i,j) = a(i,j) / a(i,i)
      i=i+1
      goto 23017
23019 continue
      i=j+1
23020 if(.not.(i.le.p))goto 23022
      a(j,i) = a(j,i) - ddot (j-1, a(1,j), 1, a(1,i), 1)
      i=i+1
      goto 23020
23022 continue
      if(.not.( j .eq. p ))goto 23023
      theta = 0.d0
      goto 23024
23023 continue
      jmax = idamax (p-j, a(j,j+1), lda) + j
      theta = dabs (a(j,jmax))
23024 continue
      tmp = dmax1 (delta, dabs (a(j,j)), theta ** 2 / beta)
      e(j) = tmp - a(j,j)
      a(j,j) = tmp
      i=j+1
23025 if(.not.(i.le.p))goto 23027
      a(i,i) = a(i,i) - a(j,i) ** 2 / a(j,j)
      i=i+1
      goto 23025
23027 continue
      j=j+1
      goto 23012
23014 continue
      j=1
23028 if(.not.(j.le.p))goto 23030
      a(j,j) = dsqrt (a(j,j))
      call dscal (p-j, a(j,j), a(j,j+1), lda)
      j=j+1
      goto 23028
23030 continue
      return
      end
      subroutine dmudr (vmu, s, lds, nobs, nnull, q, ldqr, ldqc, nq, y, 
     *tol, init, prec, maxite, theta, nlaht, score, varht, c, d, wk, 
     *info)
      integer lds, nobs, nnull, ldqr, ldqc, nq, init, maxite, info
      double precision s(lds,*), q(ldqr,ldqc,*), y(*), tol, prec, theta(
     **), nlaht, score, varht, c(*), d(*), wk(*)
      character*1 vmu
      integer n, n0
      integer iqraux, itraux, itwk, iqwk, iywk, ithewk, ihes, igra, 
     *ihwk1, ihwk2, igwk1, igwk2, ikwk, iwork1, iwork2, ijpvt, ipvtwk
      n = nobs
      n0 = nnull
      iqraux = 1
      itraux = iqraux + n0
      itwk = itraux + (n-n0-2)
      iqwk = itwk + 2 * (n-n0)
      iywk = iqwk + n * n
      ithewk = iywk + n
      ihes = ithewk + nq
      igra = ihes + nq * nq
      ihwk1 = igra + nq
      ihwk2 = ihwk1 + nq * nq
      igwk1 = ihwk2 + nq * nq
      igwk2 = igwk1 + nq
      ikwk = igwk2 + nq
      iwork1 = ikwk + (n-n0) * (n-n0) * nq
      iwork2 = iwork1 + n
      ijpvt = iwork2 + n
      ipvtwk = ijpvt + n0
      call dmudr1 (vmu, s, lds, nobs, nnull, q, ldqr, ldqc, nq, y, tol, 
     *init, prec, maxite, theta, nlaht, score, varht, c, d, wk(iqraux), 
     *wk(ijpvt), wk(itwk), wk(itraux), wk(iqwk), wk(iywk), wk(ithewk), 
     *wk(ihes), wk(igra), wk(ihwk1), wk(ihwk2), wk(igwk1), wk(igwk2), 
     *wk(ipvtwk), wk(ikwk), wk(iwork1), wk(iwork2), info)
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine dmudr0 (vmu, s, lds, nobs, nnull, q, ldqr, ldqc, nq, y,
     * tol, init, prec, maxite, theta, nlaht, score, varht, c, d, wk, in
     *fo)
      integer vmu
      integer lds, nobs, nnull, ldqr, ldqc, nq, init, maxite, info
      double precision s(lds,*), q(ldqr,ldqc,*), y(*), tol, prec, theta(
     **), nlaht, score, varht, c(*), d(*), wk(*)
      character*1 vmu1
      if( vmu .eq. 1 )then
      vmu1 = 'v'
      endif
      if( vmu .eq. 2 )then
      vmu1 = 'm'
      endif
      if( vmu .eq. 3 )then
      vmu1 = 'u'
      endif
      call dmudr (vmu1, s, lds, nobs, nnull, q, ldqr, ldqc, nq, y, tol, 
     *init, prec, maxite, theta, nlaht, score, varht, c, d, wk, info)
      return
      end
      subroutine dmudr1 (vmu, s, lds, nobs, nnull, q, ldqr, ldqc, nq, y,
     & tol, init, prec, maxite, theta, nlaht, score, varht, c, d, qraux,
     & jpvt, twk, traux, qwk, ywk, thewk, hes, gra, hwk1, hwk2, gwk1, 
     &gwk2, pvtwk, kwk, work1, work2, info)
      integer lds, nobs, nnull, ldqr, ldqc, nq, init, maxite, jpvt(*), 
     &pvtwk(*), info
      double precision s(lds,*), q(ldqr,ldqc,*), y(*), tol, prec, theta(
     &*), nlaht, score, varht, c(*), d(*), qraux(*), traux(*), twk(2,*),
     & qwk(nobs,*), ywk(*), thewk(*), hes(nq,*), gra(*), hwk1(nq,*), 
     &hwk2(nq,*), gwk1(*), gwk2(*), kwk(nobs-nnull,nobs-nnull,*), work1(
     &*), work2(*)
      character*1 vmu
      double precision alph, scrold, scrwk, nlawk, limnla(2), tmp, 
     &dasum, ddot
      integer n, n0, i, j, iwk, maxitwk, idamax, job
      info = 0
      n0 = nnull
      n = nobs - nnull
      maxitwk = maxite
      if(.not.( (vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u') 
     &.or. (init .ne. 0 .and. init .ne. 1) .or. (maxitwk .le.0) .or. (
     &prec .le. 0.d0) ))goto 23000
      info = -3
      return
23000 continue
      if(.not.( lds .lt. nobs .or. nobs .le. n0 .or. n0 .lt. 1 .or. 
     &ldqr .lt. nobs .or. ldqc .lt. nobs .or. nq .le. 0 ))goto 23002
      info = -1
      return
23002 continue
      call dstup (s, lds, nobs, n0, qraux, jpvt, y, q, ldqr, ldqc, nq, 
     &info, work1)
      if(.not.( info .ne. 0 ))goto 23004
      return
23004 continue
      if(.not.( init .eq. 1 ))goto 23006
      call dcopy (nq, theta, 1, thewk, 1)
      goto 23007
23006 continue
      i=1
23008 if(.not.(i.le.nq))goto 23010
      thewk(i) = dasum (n, q(n0+1,n0+1,i), ldqr+1)
      if(.not.( thewk(i) .gt. 0.d0 ))goto 23011
      thewk(i) = 1.d0 / thewk(i)
23011 continue
      i=i+1
      goto 23008
23010 continue
      j=1
23013 if(.not.(j.le.nobs))goto 23015
      call dset (nobs-j+1, 0.d0, qwk(j,j), 1)
      j=j+1
      goto 23013
23015 continue
      i=1
23016 if(.not.(i.le.nq))goto 23018
      j=1
23019 if(.not.(j.le.nobs))goto 23021
      call daxpy (nobs-j+1, thewk(i), q(j,j,i), 1, qwk(j,j), 1)
      j=j+1
      goto 23019
23021 continue
      i=i+1
      goto 23016
23018 continue
      call dcopy (nobs, y, 1, ywk, 1)
      call dcore (vmu, qwk, nobs, nobs, n0, tol, ywk, 0, limnla, nlawk, 
     &scrwk, varht, info, twk, work1)
      if(.not.(info .ne. 0 ))goto 23022
      return
23022 continue
      call dcoef (s, lds, nobs, n0, qraux, jpvt, ywk, qwk, nobs, nlawk, 
     &c, d, info, twk)
      call dqrsl (s, lds, nobs, n0, qraux, c, tmp, c, tmp, tmp, tmp, 
     &01000, info)
      i=1
23024 if(.not.(i.le.nq))goto 23026
      call dsymv('l', n, thewk(i), q(n0+1,n0+1,i), ldqr, c(n0+1), 1, 0.
     &d0, work1, 1)
      thewk(i) = ddot (n, c(n0+1), 1, work1, 1) * thewk(i)
      if(.not.( thewk(i) .gt. 0.d0 ))goto 23027
      thewk(i) = dlog10 (thewk(i))
      goto 23028
23027 continue
      thewk(i) = -25.d0
23028 continue
      i=i+1
      goto 23024
23026 continue
23007 continue
      scrold = 1.d10
      job = 0
23029 continue
      if(.not.( nq .eq. 1 ))goto 23032
      theta(1) = 0.d0
      goto 23031
23032 continue
      j=1
23034 if(.not.(j.le.nobs))goto 23036
      call dset (nobs-j+1, 0.d0, qwk(j,j), 1)
      j=j+1
      goto 23034
23036 continue
      i=1
23037 if(.not.(i.le.nq))goto 23039
      if(.not.( thewk(i) .le. -25.d0 ))goto 23040
      goto 23038
23040 continue
      j=1
23042 if(.not.(j.le.nobs))goto 23044
      call daxpy (nobs-j+1, 10.d0 ** thewk(i), q(j,j,i), 1, qwk(j,j), 1)
      j=j+1
      goto 23042
23044 continue
23038 i=i+1
      goto 23037
23039 continue
      call dcopy (nobs, y, 1, ywk, 1)
      call dcore (vmu, qwk, nobs, nobs, n0, tol, ywk, job, limnla, 
     &nlawk, scrwk, varht, info, twk, work1)
      if(.not.(info .ne. 0 ))goto 23045
      return
23045 continue
      if(.not.( scrold .lt. scrwk ))goto 23047
      tmp = dabs (gwk1(idamax (nq, gwk1, 1)))
      if(.not.( alph * tmp .gt. - prec ))goto 23049
      info = -5
      return
23049 continue
      alph = alph / 2.d0
      i=1
23051 if(.not.(i.le.nq))goto 23053
      thewk(i) = theta(i) + alph * gwk1(i)
      i=i+1
      goto 23051
23053 continue
      goto 23030
23047 continue
      maxitwk = maxitwk - 1
      call dcopy (n-2, qwk(n0+2,n0+1), nobs+1, traux, 1)
      call dcopy (n, qwk(n0+1,n0+1), nobs+1, twk(2,1), 2)
      call dcopy (n-1, qwk(n0+1,n0+2), nobs+1, twk(1,2), 2)
      call ddeev (vmu, nobs, q(n0+1,n0+1,1), ldqr, ldqc, n, nq, qwk(n0+
     &2,n0+1), nobs, traux, twk, ywk(n0+1), thewk, nlawk, scrwk, varht, 
     &hes, nq, gra, hwk1, hwk2, gwk1, gwk2, kwk, n, work1, work2, c, 
     &info)
      iwk = 0
      i=1
23054 if(.not.(i.le.nq))goto 23056
      if(.not.( thewk(i) .le. -25.d0 ))goto 23057
      goto 23055
23057 continue
      iwk = iwk + 1
      call dcopy (nq, hes(1,i), 1, hes(1,iwk), 1)
23055 i=i+1
      goto 23054
23056 continue
      iwk = 0
      i=1
23059 if(.not.(i.le.nq))goto 23061
      if(.not.( thewk(i) .le. -25.d0 ))goto 23062
      goto 23060
23062 continue
      iwk = iwk + 1
      call dcopy (nq, hes(i,1), nq, hes(iwk,1), nq)
      gwk1(iwk) = gra(i)
      work2(iwk) = gra(i)
23060 i=i+1
      goto 23059
23061 continue
      i=1
23064 if(.not.(i.lt.iwk))goto 23066
      call dcopy (iwk-i, hes(i+1,i), 1, hes(i,i+1), nq)
      i=i+1
      goto 23064
23066 continue
      call dmcdc (hes, nq, iwk, gwk2, pvtwk, info)
      call dprmut (gwk1, iwk, pvtwk, 0)
      call dposl (hes, nq, iwk, gwk1)
      call dprmut (gwk1, iwk, pvtwk, 1)
      alph = -1.d0
      j = iwk
      i=nq
23067 if(.not.(i.ge.1))goto 23069
      if(.not.( thewk(i) .le. -25.0 ))goto 23070
      gwk1(i) = 0.d0
      goto 23071
23070 continue
      gwk1(i) = gwk1(iwk)
      iwk = iwk - 1
23071 continue
      i=i-1
      goto 23067
23069 continue
      call dscal (nq, 1.d0/dlog(1.d1), gwk1, 1)
      tmp = dabs (gwk1(idamax (nq, gwk1, 1)))
      if(.not.( tmp .gt. 1.d0 ))goto 23072
      call dscal (nq, 1.d0/tmp, gwk1, 1)
23072 continue
      i=1
23074 if(.not.(i.le.nq))goto 23076
      if(.not.( thewk(i) .le. -25.d0 ))goto 23077
      goto 23075
23077 continue
      thewk(i) = thewk(i) - nlawk
23075 i=i+1
      goto 23074
23076 continue
      call dcopy (nq, thewk, 1, theta, 1)
      tmp = gra(idamax (nq, gra, 1)) ** 2
      if(.not.( tmp .lt. prec ** 2 .or. scrold - scrwk .lt. prec * (
     &scrwk + 1.d0) .and. tmp .lt. prec * (scrwk + 1.d0) ** 2 ))goto 230
     &79
      goto 23031
23079 continue
      if(.not.( maxitwk .lt. 1 ))goto 23081
      info = -4
      return
23081 continue
      scrold = scrwk
      i=1
23083 if(.not.(i.le.nq))goto 23085
      thewk(i) = thewk(i) + alph * gwk1(i)
      i=i+1
      goto 23083
23085 continue
      job = -1
      limnla(1) = -1.d0
      limnla(2) = 1.d0
23030 goto 23029
23031 continue
      j=1
23086 if(.not.(j.le.nobs))goto 23088
      call dset (nobs-j+1, 0.d0, qwk(j,j), 1)
      j=j+1
      goto 23086
23088 continue
      i=1
23089 if(.not.(i.le.nq))goto 23091
      if(.not.( theta(i) .le. -25.d0 ))goto 23092
      goto 23090
23092 continue
      j=1
23094 if(.not.(j.le.nobs))goto 23096
      call daxpy (nobs-j+1, 10.d0 ** theta(i), q(j,j,i), 1, qwk(j,j), 1)
      j=j+1
      goto 23094
23096 continue
23090 i=i+1
      goto 23089
23091 continue
      call dcopy (nobs, y, 1, ywk, 1)
      call dcore (vmu, qwk, nobs, nobs, n0, tol, ywk, job, limnla, 
     &nlaht, score, varht, info, twk, work1)
      if(.not.(info .ne. 0 ))goto 23097
      return
23097 continue
      call dcoef (s, lds, nobs, n0, qraux, jpvt, ywk, qwk, nobs, nlaht, 
     &c, d, info, twk)
      return
      end
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
     *info)
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine dnewton1 (cd, nxis, q, nxi, rs, nobs, cntsum, cnt, qdrs
     *, nqd, qdwt, prec, maxiter, mchpr, mrs, wt, fit, mu, v, jpvt, cdne
     *w, wtnew, fitnew, wk, info)
      integer nxis, nxi, nobs, cntsum, cnt(*), nqd, maxiter, jpvt(*), in
     *fo
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,*), qdwt(*)
     *, prec, mchpr, mrs(*), wt(*), fit(*), mu(*), v(nxis,*), cdnew(*), 
     *wtnew(*), fitnew(*), wk(*)
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
      disc0 = dmax1 ((mumax/(1.d0+lkhd))**2, dabs(lkhd-lkhdnew)/(1.d0+da
     *bs(lkhd)))
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
      subroutine dqrslm (x, ldx, n, k, qraux, a, lda, job, info, work)
      integer ldx, n, k, lda, job, info
      double precision x(ldx,*), qraux(*), a(lda,*), work(*)
      double precision tmp, alph, ddot
      integer i, j, step
      info = 0
      if(.not.( lda .lt. n .or. n .lt. k .or. k .lt. 1 ))goto 23000
      info = -1
      return
23000 continue
      if(.not.( job .ne. 0 .and. job .ne. 1 ))goto 23002
      info = 1
      return
23002 continue
      if(.not.( job .eq. 0 ))goto 23004
      j = 1
      step = 1
      goto 23005
23004 continue
      j = k
      step = -1
23005 continue
23006 if(.not.( j .ge. 1 .and. j .le. k ))goto 23007
      if(.not.( qraux(j) .eq. 0.0d0 ))goto 23008
      j = j + step
      goto 23006
23008 continue
      tmp = x(j,j)
      x(j,j) = qraux(j)
      i=1
23010 if(.not.(i.lt.j))goto 23012
      alph = - ddot (n-j+1, x(j,j), 1, a(j,i), 1) / x(j,j)
      call daxpy (n-j+1, alph, x(j,j), 1, a(j,i), 1)
      i=i+1
      goto 23010
23012 continue
      alph = 1.d0 / x(j,j)
      call dsymv ('l', n-j+1, alph, a(j,j), lda, x(j,j), 1, 0.d0, work(
     *j), 1)
      alph = - ddot (n-j+1, work(j), 1, x(j,j), 1) / 2.d0 / x(j,j)
      call daxpy (n-j+1, alph, x(j,j), 1, work(j), 1)
      call dsyr2 ('l', n-j+1, -1.d0, x(j,j), 1, work(j), 1, a(j,j), lda)
      x(j,j) = tmp
      j = j + step
      goto 23006
23007 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine drkl (cd, nxis, qdrs, nqd, qdwt, wt0, mchpr, wt, eta, m
     *u, v, jpvt, wk, cdnew, wtnew, prec, maxiter, info)
      integer nxis, nqd, jpvt(*), maxiter, info
      double precision cd(*), qdrs(nqd,*), qdwt(*), wt0(*), mchpr, wt(*)
     *, eta(*), mu(*), v(nxis,*), wk(*), cdnew(*), wtnew(*), prec
      integer i, j, k, iter, flag, idamax, infowk
      double precision wtsum, ddot, rkl, mumax, wtsumnew, rklnew, disc, 
     *disc0
      info = 0
      wtsum = 0.d0
      rkl = 0.d0
      i=1
23000 if(.not.(i.le.nqd))goto 23002
      wt0(i) = qdwt(i) * wt0(i)
      eta(i) = ddot (nxis, qdrs(i,1), nqd, cd, 1)
      wt(i) = qdwt(i) * dexp (eta(i))
      wtsum = wtsum + wt(i)
      rkl = rkl + dlog(wt0(i)/wt(i)) * wt0(i)
23001 i=i+1
      goto 23000
23002 continue
      rkl = rkl + dlog(wtsum)
      iter = 0
      flag = 0
23003 continue
      iter = iter + 1
      i=1
23006 if(.not.(i.le.nxis))goto 23008
      mu(i) = ddot (nqd, wt, 1, qdrs(1,i), 1) / wtsum
23007 i=i+1
      goto 23006
23008 continue
      i=1
23009 if(.not.(i.le.nxis))goto 23011
      j=i
23012 if(.not.(j.le.nxis))goto 23014
      v(i,j) = 0.d0
      k=1
23015 if(.not.(k.le.nqd))goto 23017
      v(i,j) = v(i,j) + wt(k) * qdrs(k,i) * qdrs(k,j)
23016 k=k+1
      goto 23015
23017 continue
      v(i,j) = v(i,j) / wtsum - mu(i) * mu(j)
23013 j=j+1
      goto 23012
23014 continue
23010 i=i+1
      goto 23009
23011 continue
      i=1
23018 if(.not.(i.le.nxis))goto 23020
      mu(i) = ddot (nqd, wt0, 1, qdrs(1,i), 1) - mu(i)
23019 i=i+1
      goto 23018
23020 continue
      mumax = dabs(mu(idamax(nxis, mu, 1)))
      i=1
23021 if(.not.(i.le.nxis))goto 23023
      jpvt(i) = 0
23022 i=i+1
      goto 23021
23023 continue
      call dmcdc (v, nxis, nxis, wk, jpvt, infowk)
23024 continue
      call dcopy (nxis, mu, 1, cdnew, 1)
      call dprmut (cdnew, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, cdnew, 11, infowk)
      call dtrsl (v, nxis, nxis, cdnew, 01, infowk)
      call dprmut (cdnew, nxis, jpvt, 1)
      call daxpy (nxis, 1.d0, cd, 1, cdnew, 1)
      wtsumnew = 0.d0
      rklnew = 0.d0
      i=1
23027 if(.not.(i.le.nqd))goto 23029
      eta(i) = ddot (nxis, qdrs(i,1), nqd, cdnew, 1)
      if(eta(i).gt.3.d2)then
      flag = flag + 1
      goto 23029
      endif
      wtnew(i) = qdwt(i) * dexp (eta(i))
      wtsumnew = wtsumnew + wtnew(i)
      rklnew = rklnew + dlog(wt0(i)/wtnew(i)) * wt0(i)
23028 i=i+1
      goto 23027
23029 continue
      rklnew = rklnew + dlog (wtsumnew)
      if(flag.eq.1)then
      call dset (nxis, 0.d0, cd, 1)
      wtsum = 0.d0
      rkl = 0.d0
      i=1
23034 if(.not.(i.le.nqd))goto 23036
      wt(i) = qdwt(i)
      wtsum = wtsum + wt(i)
      rkl = rkl + dlog(wt0(i)/wt(i)) * wt0(i)
23035 i=i+1
      goto 23034
23036 continue
      rkl = rkl + dlog (wtsum)
      iter = 0
      goto 23026
      endif
      if(rklnew-rkl.lt.1.d1*(1.d0+dabs(rkl))*mchpr)then
      goto 23026
      endif
      call dscal (nxis, .5d0, mu, 1)
      if(dabs(mu(idamax(nxis, mu, 1))/mumax).lt.1.d1*mchpr)then
      goto 23026
      endif
23025 goto 23024
23026 continue
      if(flag.eq.1)then
      flag = 2
      goto 23004
      endif
      if(flag.eq.3)then
      info = 1
      return
      endif
      disc = 0.d0
      i=1
23045 if(.not.(i.le.nqd))goto 23047
      disc = dmax1 (disc, dabs(wt(i)-wtnew(i))/(1.d0+dabs(wt(i))))
23046 i=i+1
      goto 23045
23047 continue
      disc = dmax1 (disc, (mumax/(1.d0+rkl))**2)
      disc0 = dmax1 ((mumax/(1.d0+rkl))**2, dabs(rkl-rklnew)/(1+dabs(rkl
     *)))
      call dcopy (nxis, cdnew, 1, cd, 1)
      call dcopy (nqd, wtnew, 1, wt, 1)
      wtsum = wtsumnew
      rkl = rklnew
      if(disc0.lt.prec)then
      goto 23005
      endif
      if(disc.lt.prec)then
      goto 23005
      endif
      if(iter.lt.maxiter)then
      goto 23004
      endif
      if(flag.eq.0)then
      call dset (nxis, 0.d0, cd, 1)
      wtsum = 0.d0
      rkl = 0.d0
      i=1
23056 if(.not.(i.le.nqd))goto 23058
      wt(i) = qdwt(i)
      wtsum = wtsum + wt(i)
      rkl = rkl + dlog(wt0(i)/wt(i)) * wt0(i)
23057 i=i+1
      goto 23056
23058 continue
      rkl = rkl + dlog (wtsum)
      iter = 0
      flag = 2
      else
      info = 2
      goto 23005
      endif
23004 goto 23003
23005 continue
      wtsum = 0.d0
      i=1
23059 if(.not.(i.le.nqd))goto 23061
      wt0(i) = dexp (ddot (nxis, qdrs(i,1), nqd, cd, 1))
      wtsum = wtsum + qdwt(i) * wt0(i)
23060 i=i+1
      goto 23059
23061 continue
      call dscal (nqd, 1.d0/wtsum, wt0, 1)
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
      subroutine dsidr (vmu, s, lds, nobs, nnull, y, q, ldq, tol, job, 
     *limnla, nlaht, score, varht, c, d, qraux, jpvt, wk, info)
      character*1 vmu
      integer lds, nobs, nnull, ldq, job, jpvt(*), info
      double precision s(lds,*), y(*), q(ldq,*), tol, limnla(2), nlaht, 
     *score(*), varht, c(*), d(*), qraux(*), wk(*)
      info = 0
      if(.not.( nnull .lt. 1 .or. nnull .ge. nobs .or. nobs .gt. lds 
     *.or. nobs .gt. ldq ))goto 23000
      info = -1
      return
23000 continue
      if(.not.( vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u' ))
     *goto 23002
      info = -3
      return
23002 continue
      call dstup (s, lds, nobs, nnull, qraux, jpvt, y, q, ldq, nobs, 1, 
     *info, wk)
      if(.not.( info .ne. 0 ))goto 23004
      return
23004 continue
      call dcore (vmu, q, ldq, nobs, nnull, tol, y, job, limnla, nlaht, 
     *score, varht, info, wk, wk(2*nobs+1))
      if(.not.( info .ne. 0 ))goto 23006
      return
23006 continue
      call dcoef (s, lds, nobs, nnull, qraux, jpvt, y, q, ldq, nlaht, c,
     * d, info, wk)
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine dsidr0 (vmu, s, lds, nobs, nnull, y, q, ldq, tol, job, 
     *limnla, nlaht, score, varht, c, d, qraux, jpvt, wk, info)
      integer vmu
      integer lds, nobs, nnull, ldq, job, jpvt(*), info
      double precision s(lds,*), y(*), q(ldq,*), tol, limnla(2), nlaht, 
     *score(*), varht, c(*), d(*), qraux(*), wk(*)
      character*1 vmu1
      if( vmu .eq. 1 )then
      vmu1 = 'v'
      endif
      if( vmu .eq. 2 )then
      vmu1 = 'm'
      endif
      if( vmu .eq. 3 )then
      vmu1 = 'u'
      endif
      call dsidr (vmu1, s, lds, nobs, nnull, y, q, ldq, tol, job, limnla
     *, nlaht, score, varht, c, d, qraux, jpvt, wk, info)
      return
      end
      subroutine dsms (s, lds, nobs, nnull, jpvt, q, ldq, nlaht, sms, 
     *ldsms, wk, info)
      integer lds, nobs, nnull, jpvt(*), ldq, ldsms, info
      double precision s(lds,*), q(ldq,*), nlaht, sms(ldsms,*), wk(2,*)
      double precision dum, ddot
      integer i, j, n, n0
      info = 0
      if(.not.( nnull .lt. 1 .or. nnull .ge. nobs .or. nobs .gt. lds 
     *.or. nobs .gt. ldq .or. ldsms .lt. nnull ))goto 23000
      info = -1
      return
23000 continue
      n0 = nnull
      n = nobs - nnull
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, wk, 1)
      j=1
23002 if(.not.(j.le.n0))goto 23004
      call dcopy (n, q(n0+1,j), 1, q(j,n0+1), ldq)
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, wk, q(n0+2,j), dum, q(n0+
     *2,j), dum, dum, dum, 01000, info)
      j=j+1
      goto 23002
23004 continue
      call dset (n, 10.d0 ** nlaht, wk(2,1), 2)
      call daxpy (n, 1.d0, q(n0+1,n0+1), ldq+1, wk(2,1), 2)
      call dcopy (n-1, q(n0+1,n0+2), ldq+1, wk(1,2), 2)
      call dpbfa (wk, 2, n, 1, info)
      if(.not.( info .ne. 0 ))goto 23005
      info = -2
      return
23005 continue
      j=1
23007 if(.not.(j.le.n0))goto 23009
      call dpbsl (wk, 2, n, 1, q(n0+1,j))
      j=j+1
      goto 23007
23009 continue
      call dcopy (n-2, q(n0+2,n0+1), ldq+1, wk, 1)
      j=1
23010 if(.not.(j.le.n0))goto 23012
      call dqrsl (q(n0+2,n0+1), ldq, n-1, n-2, wk, q(n0+2,j), q(n0+2,j),
     * dum, dum, dum, dum, 10000, info)
      j=j+1
      goto 23010
23012 continue
      i=1
23013 if(.not.(i.le.n0))goto 23015
      j=1
23016 if(.not.(j.lt.i))goto 23018
      sms(i,j) = sms(j,i)
      j=j+1
      goto 23016
23018 continue
      j=i
23019 if(.not.(j.le.n0))goto 23021
      sms(i,j) = q(j,i) - ddot (n, q(n0+1,j), 1, q(i,n0+1), ldq)
      j=j+1
      goto 23019
23021 continue
      sms(i,i) = sms(i,i) + 10.d0**nlaht
      i=i+1
      goto 23013
23015 continue
      j=1
23022 if(.not.(j.le.n0))goto 23024
      call dtrsl (s, lds, n0, sms(1,j), 01, info)
      j=j+1
      goto 23022
23024 continue
      i=1
23025 if(.not.(i.le.n0))goto 23027
      call dcopy (n0, sms(i,1), ldsms, wk, 1)
      call dtrsl (s, lds, n0, wk, 01, info)
      call dprmut (wk, n0, jpvt, 1)
      call dcopy (n0, wk, 1, sms(i,1), ldsms)
      i=i+1
      goto 23025
23027 continue
      j=1
23028 if(.not.(j.le.n0))goto 23030
      call dprmut (sms(1,j), n0, jpvt, 1)
      j=j+1
      goto 23028
23030 continue
      j=1
23031 if(.not.(j.le.n0))goto 23033
      call dcopy (n, q(j,n0+1), ldq, q(n0+1,j), 1)
      j=j+1
      goto 23031
23033 continue
      return
      end
      subroutine dstup (s, lds, nobs, nnull, qraux, jpvt, y, q, ldqr, 
     *ldqc, nq, info, work)
      integer lds, nobs, nnull, jpvt(*), ldqr, ldqc, nq, info
      double precision s(lds,*), y(*), qraux(*), q(ldqr,ldqc,*), work(*)
      double precision dum
      integer j
      info = 0
      if(.not.( nobs .lt. 1 .or. nobs .gt. lds .or. nobs .gt. ldqr .or. 
     *nobs .gt. ldqc ))goto 23000
      info = -1
      return
23000 continue
      j=1
23002 if(.not.(j.le.nnull))goto 23004
      jpvt(j) = 0
      j=j+1
      goto 23002
23004 continue
      call dqrdc (s, lds, nobs, nnull, qraux, jpvt, work, 1)
      call dqrsl (s, lds, nobs, nnull, qraux, y, dum, y, work, dum, dum,
     * 01100, info)
      if(.not.( info .ne. 0 ))goto 23005
      return
23005 continue
      j=1
23007 if(.not.(j.le.nq))goto 23009
      call dqrslm (s, lds, nobs, nnull, qraux, q(1,1,j), ldqr, 0, info, 
     *work)
      j=j+1
      goto 23007
23009 continue
      return
      end
      subroutine dsytr (x, ldx, n, tol, info, work)
      integer ldx, n, info
      double precision x(ldx,*), tol, work(*)
      double precision nrmtot, nrmxj, alph, toltot, tolcum, toluni, dn, 
     *ddot
      integer j
      info = 0
      if(.not.( ldx .lt. n .or. n .le. 2 ))goto 23000
      info = -1
      return
23000 continue
      nrmtot = ddot (n, x, ldx+1, x, ldx+1)
      j=1 
23002 if(.not.(j.lt.n))goto 23004
      nrmtot = nrmtot + 2.d0 * ddot (n-j, x(j+1,j), 1, x(j+1,j), 1)
       j=j+1 
      goto 23002
23004 continue
      toltot = 1.d0
23005 if(.not.( 1.d0 + toltot .gt. 1.d0 ))goto 23006
      toltot = toltot / 2.d0
      goto 23005
23006 continue
      toltot = 4.d0 * toltot ** 2
      if(.not.( toltot .lt. tol ))goto 23007
      toltot = tol
23007 continue
      toltot = toltot * nrmtot
      dn = dfloat (n)
      toluni = toltot * 6.d0 / dn / ( dn - 1.d0 ) / ( 2.d0 * dn - 1.d0 )
      tolcum = 0.d0
      j=1 
23009 if(.not.(j.lt.n-1))goto 23011
      nrmtot = nrmtot - x(j,j) * x(j,j)
      nrmxj = ddot (n-j, x(j+1,j), 1, x(j+1,j), 1)
      dn = dfloat (n-j)
      tolcum = tolcum + toluni * dn * dn
      if(.not.( 2.d0 * nrmxj .le. tolcum ))goto 23012
      x(j,j+1) = 0.d0
      call dscal (n-j, 0.d0, x(j+1,j), 1)
      tolcum = tolcum - 2.d0 * nrmxj
      toltot = toltot - 2.d0 * nrmxj
      goto 23010
23012 continue
      if(.not.( x(j+1,j) .lt. 0.d0 ))goto 23014
      x(j,j+1) = dsqrt (nrmxj)
      goto 23015
23014 continue
      x(j,j+1) = - dsqrt (nrmxj)
23015 continue
      nrmtot = nrmtot - 2.d0 * nrmxj
      call dscal (n-j, -1.d0/x(j,j+1), x(j+1,j), 1)
      x(j+1,j) = 1.d0 + x(j+1,j)
      alph = 1.d0 / x(j+1,j)
      call dsymv ('l', n-j, alph, x(j+1,j+1), ldx, x(j+1,j), 1, 0.d0, 
     *work(j+1), 1)
      alph = - ddot (n-j, work(j+1), 1, x(j+1,j), 1) / 2.d0 / x(j+1,j)
      call daxpy (n-j, alph, x(j+1,j), 1, work(j+1), 1)
      call dsyr2 ('l', n-j, -1.d0, x(j+1,j), 1, work(j+1), 1, x(j+1,j+1)
     *, ldx)
23010  j=j+1 
      goto 23009
23011 continue
      x(n-1,n) = x(n,n-1)
      return
      end
      subroutine dtrev (vmu, t, ldt, n, z, score, varht, info, work)
      character*1 vmu
      integer n, info
      double precision t(ldt,*), z(*), score, varht, work(*)
      double precision nume, deno, tmp, alph, la, dasum, ddot
      integer j
      info = 0
      if(.not.( vmu .ne. 'v' .and. vmu .ne. 'm' .and. vmu .ne. 'u' ))
     *goto 23000
      info = -3
      return
23000 continue
      la = t(1,1)
      alph = dfloat (n) / dasum (n, t(2,1), ldt)
      call dscal (n, alph, t(2,1), ldt)
      call dscal (n-1, alph, t(1,2), ldt)
      call dpbfa (t, ldt, n, 1, info)
      if(.not.( info .ne. 0 ))goto 23002
      return
23002 continue
      call dcopy (n, z, 1, work, 1)
      call dpbsl (t, ldt, n, 1, work)
      if(.not.( vmu .eq. 'v' ))goto 23004
      tmp = 1.d0 / t(2,n) / t(2,n)
      deno = tmp
      j=n-1
23006 if(.not.(j.gt.0))goto 23008
      tmp = ( 1.d0 + t(1,j+1) * t(1,j+1) * tmp ) / t(2,j) / t(2,j)
      deno = deno + tmp
      j=j-1
      goto 23006
23008 continue
      nume = ddot (n, work, 1, work, 1) / dfloat (n)
      deno = deno / dfloat (n)
      varht = alph * la * nume / deno
      score = nume / deno / deno
23004 continue
      if(.not.( vmu .eq. 'm' ))goto 23009
      deno = dlog (t(2,n))
      j=n-1
23011 if(.not.(j.gt.0))goto 23013
      deno = deno + dlog (t(2,j))
      j=j-1
      goto 23011
23013 continue
      nume = ddot (n, z, 1, work, 1) / dfloat (n)
      varht = alph * la * nume
      score = nume * dexp (2.d0 * deno / dfloat (n))
23009 continue
      if(.not.( vmu .eq. 'u' ))goto 23014
      nume = ddot (n, work, 1, work, 1) / dfloat (n)
      tmp = 1.d0 / t(2,n) / t(2,n)
      deno = tmp
      j=n-1
23016 if(.not.(j.gt.0))goto 23018
      tmp = ( 1.d0 + t(1,j+1) * t(1,j+1) * tmp ) / t(2,j) / t(2,j)
      deno = deno + tmp
      j=j-1
      goto 23016
23018 continue
      deno = deno / dfloat (n)
      score = alph * alph * la * la * nume - 2.d0 * varht * alph * la * 
     *deno
23014 continue
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
C Output from Public domain Ratfor, version 1.0
      subroutine hrkl (cd, nxis, qdrs, nqd, nx, qdwt, wt0, mchpr, wt, mu
     *, mu0, v, jpvt, wk, cdnew, wtnew, prec, maxiter, info)
      integer nxis, nqd, nx, jpvt(*), maxiter, info
      double precision cd(*), qdrs(nqd,nxis,*), qdwt(nqd,*), wt0(nqd,*),
     * mchpr, wt(nqd,*), mu(*), mu0(*), v(nxis,*), wk(*), cdnew(*), wtne
     *w(nqd,*), prec
      integer i, j, k, kk, idamax, iter, flag, infowk
      double precision tmp, ddot, dasum, rkl, mumax, rklnew, disc, disc0
      info = 0
      call dset (nxis, 0.d0, mu0, 1)
      kk=1
23000 if(.not.(kk.le.nx))goto 23002
      i=1
23003 if(.not.(i.le.nxis))goto 23005
      mu0(i) = mu0(i) + ddot (nqd, wt0(1,kk), 1, qdrs(1,i,kk), 1)
23004 i=i+1
      goto 23003
23005 continue
23001 kk=kk+1
      goto 23000
23002 continue
      rkl = 0.d0
      kk=1
23006 if(.not.(kk.le.nx))goto 23008
      i=1
23009 if(.not.(i.le.nqd))goto 23011
      tmp = ddot (nxis, qdrs(i,1,kk), nqd, cd, 1)
      wt(i,kk) = qdwt(i,kk) * dexp (tmp)
      rkl = rkl + (wt(i,kk) - wt0(i,kk)*tmp)
23010 i=i+1
      goto 23009
23011 continue
23007 kk=kk+1
      goto 23006
23008 continue
      iter = 0
      flag = 0
23012 continue
      iter = iter + 1
      call dset (nxis, 0.d0, mu, 1)
      call dset (nxis*nxis, 0.d0, v, 1)
      kk=1
23015 if(.not.(kk.le.nx))goto 23017
      i=1
23018 if(.not.(i.le.nxis))goto 23020
      mu(i) = mu(i) - ddot (nqd, wt(1,kk), 1, qdrs(1,i,kk), 1)
      j=i
23021 if(.not.(j.le.nxis))goto 23023
      k=1
23024 if(.not.(k.le.nqd))goto 23026
      v(i,j) = v(i,j) + wt(k,kk) * qdrs(k,i,kk) * qdrs(k,j,kk)
23025 k=k+1
      goto 23024
23026 continue
23022 j=j+1
      goto 23021
23023 continue
23019 i=i+1
      goto 23018
23020 continue
23016 kk=kk+1
      goto 23015
23017 continue
      call daxpy (nxis, 1.d0, mu0, 1, mu, 1)
      mumax = dabs(mu(idamax(nxis, mu, 1)))
      i=1
23027 if(.not.(i.le.nxis))goto 23029
      jpvt(i) = 0
23028 i=i+1
      goto 23027
23029 continue
      call dmcdc (v, nxis, nxis, wk, jpvt, infowk)
23030 continue
      call dcopy (nxis, mu, 1, cdnew, 1)
      call dprmut (cdnew, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, cdnew, 11, infowk)
      call dtrsl (v, nxis, nxis, cdnew, 01, infowk)
      call dprmut (cdnew, nxis, jpvt, 1)
      call daxpy (nxis, 1.d0, cd, 1, cdnew, 1)
      rklnew = 0.d0
      kk=1
23033 if(.not.(kk.le.nx))goto 23035
      i=1
23036 if(.not.(i.le.nqd))goto 23038
      tmp = ddot (nxis, qdrs(i,1,kk), nqd, cdnew, 1)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23038
      endif
      wtnew(i,kk) = qdwt(i,kk) * dexp (tmp)
      rklnew = rklnew + (wtnew(i,kk) - wt0(i,kk)*tmp)
23037 i=i+1
      goto 23036
23038 continue
23034 kk=kk+1
      goto 23033
23035 continue
      if(flag.eq.1)then
      call dset (nxis, 0.d0, cd, 1)
      rkl = dasum (nqd*nx, qdwt, 1)
      call dcopy (nqd*nx, qdwt, 1, wt, 1)
      iter = 0
      goto 23032
      endif
      if(rklnew-rkl.lt.1.d1*(1.d0+dabs(rkl))*mchpr)then
      goto 23032
      endif
      call dscal (nxis, .5d0, mu, 1)
      if(dabs(mu(idamax(nxis, mu, 1))/mumax).lt.1.d1*mchpr)then
      goto 23032
      endif
23031 goto 23030
23032 continue
      if(flag.eq.1)then
      flag = 2
      goto 23013
      endif
      if(flag.eq.3)then
      info = 1
      return
      endif
      disc = 0.d0
      kk=1
23051 if(.not.(kk.le.nx))goto 23053
      i=1
23054 if(.not.(i.le.nqd))goto 23056
      disc = dmax1 (disc, dabs(wt(i,kk)-wtnew(i,kk))/(1.d0+dabs(wt(i,kk)
     *)))
23055 i=i+1
      goto 23054
23056 continue
23052 kk=kk+1
      goto 23051
23053 continue
      disc = dmax1 (disc, (mumax/(1.d0+rkl))**2)
      disc0 = dmax1 ((mumax/(1.d0+rkl))**2, dabs(rkl-rklnew)/(1+dabs(rkl
     *)))
      call dcopy (nxis, cdnew, 1, cd, 1)
      call dcopy (nqd*nx, wtnew, 1, wt, 1)
      rkl = rklnew
      if(disc0.lt.prec)then
      goto 23014
      endif
      if(disc.lt.prec)then
      goto 23014
      endif
      if(iter.lt.maxiter)then
      goto 23013
      endif
      if(flag.eq.0)then
      call dset (nxis, 0.d0, cd, 1)
      rkl = dasum (nqd*nx, qdwt, 1)
      call dcopy (nqd*nx, qdwt, 1, wt, 1)
      iter = 0
      flag = 2
      else
      info = 2
      goto 23014
      endif
23013 goto 23012
23014 continue
      kk=1
23065 if(.not.(kk.le.nx))goto 23067
      i=1
23068 if(.not.(i.le.nqd))goto 23070
      wt0(i,kk) = dexp (ddot (nxis, qdrs(i,1,kk), nqd, cd, 1))
23069 i=i+1
      goto 23068
23070 continue
23066 kk=kk+1
      goto 23065
23067 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine hzdaux1 (cd, nxis, q, nxi, qdrs, nqd, qdwt, nx, mchpr, 
     *wt, v, vwk, jpvt)
      integer nxis, nxi, nqd, nx, jpvt(*)
      double precision cd(*), q(nxi,*), qdrs(nqd,nxis,*), qdwt(nqd,*), m
     *chpr, wt(nqd,*), v(nxis,*), vwk(nxis,*)
      integer i, j, k, kk, rkv
      double precision ddot
      kk=1
23000 if(.not.(kk.le.nx))goto 23002
      i=1
23003 if(.not.(i.le.nqd))goto 23005
      wt(i,kk) = qdwt(i,kk) * dexp (ddot (nxis, qdrs(i,1,kk), nqd, cd, 1
     *))
23004 i=i+1
      goto 23003
23005 continue
23001 kk=kk+1
      goto 23000
23002 continue
      call dset (nxis*nxis, 0.d0, v, 1)
      kk=1
23006 if(.not.(kk.le.nx))goto 23008
      i=1
23009 if(.not.(i.le.nxis))goto 23011
      j=i
23012 if(.not.(j.le.nxis))goto 23014
      vwk(i,j) = 0.d0
      k=1
23015 if(.not.(k.le.nqd))goto 23017
      vwk(i,j) = vwk(i,j) + wt(k,kk) * qdrs(k,i,kk) * qdrs(k,j,kk)
23016 k=k+1
      goto 23015
23017 continue
23013 j=j+1
      goto 23012
23014 continue
23010 i=i+1
      goto 23009
23011 continue
      call daxpy (nxis*nxis, 1.d0, vwk, 1, v, 1)
23007 kk=kk+1
      goto 23006
23008 continue
      i=1
23018 if(.not.(i.le.nxi))goto 23020
      j=i
23021 if(.not.(j.le.nxi))goto 23023
      v(i,j) = v(i,j) + q(i,j)
23022 j=j+1
      goto 23021
23023 continue
23019 i=i+1
      goto 23018
23020 continue
      i=1
23024 if(.not.(i.le.nxis))goto 23026
      jpvt(i) = 0
23025 i=i+1
      goto 23024
23026 continue
      call dchdc (v, nxis, nxis, vwk, jpvt, 1, rkv)
23027 if(v(rkv,rkv).lt.v(1,1)*dsqrt(mchpr))then
      rkv = rkv - 1
      goto 23027
      endif
23028 continue
      i=rkv+1
23029 if(.not.(i.le.nxis))goto 23031
      v(i,i) = v(1,1)
      call dset (i-rkv-1, 0.d0, v(rkv+1,i), 1)
23030 i=i+1
      goto 23029
23031 continue
      return
      end
      subroutine hzdaux2 (v, nxis, jpvt, r, nr, se)
      double precision v(nxis,*), r(nxis,*), se(*)
      integer nxis, jpvt(*), nr
      double precision ddot
      integer i, infowk
      i=1
23032 if(.not.(i.le.nr))goto 23034
      call dprmut (r(1,i), nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, r(1,i), 11, infowk)
      se(i) = dsqrt (ddot (nxis, r(1,i), 1, r(1,i), 1))
23033 i=i+1
      goto 23032
23034 continue
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine hzdnewton (cd, nxis, q, nxi, rs, nt, nobs, cntsum, cnt,
     * qdrs, nqd, qdwt, nx, prec, maxiter, mchpr, wk, info)
      integer nxis, nxi, nt, nobs, cntsum, cnt(*), nqd, nx, maxiter, inf
     *o
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,nxis,*), qd
     *wt(nqd,*), prec, mchpr, wk(*)
      integer imrs, iwt, ifit, imu, imuwk, iv, ivwk, ijpvt, icdnew, iwtn
     *ew, ifitnew, iwk
      imrs = 1
      iwt = imrs + max0 (nxis, 2)
      ifit = iwt + nqd*nx
      imu = ifit + nt
      imuwk = imu + nxis
      iv = imuwk + nxis
      ivwk = iv + nxis*nxis
      ijpvt = ivwk + nxis*nxis
      icdnew = ijpvt + nxis
      iwtnew = icdnew + nxis
      ifitnew = iwtnew + nqd*nx
      iwk = ifitnew + nt
      call hzdnewton1 (cd, nxis, q, nxi, rs, nt, nobs, cntsum, cnt, qdrs
     *, nqd, qdwt, nx, prec, maxiter, mchpr, wk(imrs), wk(iwt), wk(ifit)
     *, wk(imu), wk(imuwk), wk(iv), wk(ivwk), wk(ijpvt), wk(icdnew), wk(
     *iwtnew), wk(ifitnew), wk(iwk), info)
      return
      end
C Output from Public domain Ratfor, version 1.0
      subroutine hzdnewton1 (cd, nxis, q, nxi, rs, nt, nobs, cntsum, cnt
     *, qdrs, nqd, qdwt, nx, prec, maxiter, mchpr, mrs, wt, fit, mu, muw
     *k, v, vwk, jpvt, cdnew, wtnew, fitnew, wk, info)
      integer nxis, nxi, nt, nobs, cntsum, cnt(*), nqd, nx, maxiter, jpv
     *t(*), info
      double precision cd(*), q(nxi,*), rs(nxis,*), qdrs(nqd,nxis,*), qd
     *wt(nqd,*), prec, mchpr, mrs(*), wt(nqd,*), fit(*), mu(*), muwk(*),
     * v(nxis,*), vwk(nxis,*), cdnew(*), wtnew(nqd,*), fitnew(*), wk(*)
      integer i, j, k, kk, iter, flag, rkv, idamax, infowk
      double precision tmp, ddot, fitmean, dasum, lkhd, mumax, lkhdnew, 
     *disc, disc0, trc
      info = 0
      i=1
23000 if(.not.(i.le.nxis))goto 23002
      mrs(i) = 0.d0
      j=1
23003 if(.not.(j.le.nt))goto 23005
      if(cntsum.eq.0)then
      mrs(i) = mrs(i) + rs(i,j)
      else
      mrs(i) = mrs(i) + rs(i,j) * dfloat (cnt(j))
      endif
23004 j=j+1
      goto 23003
23005 continue
      mrs(i) = mrs(i) / dfloat (nobs)
23001 i=i+1
      goto 23000
23002 continue
      kk=1
23008 if(.not.(kk.le.nx))goto 23010
      i=1
23011 if(.not.(i.le.nqd))goto 23013
      wt(i,kk) = qdwt(i,kk) * dexp (ddot (nxis, qdrs(i,1,kk), nqd, cd, 1
     *))
23012 i=i+1
      goto 23011
23013 continue
23009 kk=kk+1
      goto 23008
23010 continue
      fitmean = 0.d0
      i=1
23014 if(.not.(i.le.nt))goto 23016
      tmp = ddot (nxis, rs(1,i), 1, cd, 1)
      fit(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23015 i=i+1
      goto 23014
23016 continue
      fitmean = fitmean / dfloat (nobs) - dasum (nqd*nx, wt, 1)
      call dsymv ('u', nxi, 1.d0, q, nxi, cd, 1, 0.d0, wk, 1)
      lkhd = ddot (nxi, cd, 1, wk, 1) / 2.d0 - fitmean
      iter = 0
      flag = 0
23019 continue
      iter = iter + 1
      call dset (nxis, 0.d0, mu, 1)
      call dset (nxis*nxis, 0.d0, v, 1)
      kk=1
23022 if(.not.(kk.le.nx))goto 23024
      i=1
23025 if(.not.(i.le.nxis))goto 23027
      muwk(i) = - ddot (nqd, wt(1,kk), 1, qdrs(1,i,kk), 1)
      j=i
23028 if(.not.(j.le.nxis))goto 23030
      vwk(i,j) = 0.d0
      k=1
23031 if(.not.(k.le.nqd))goto 23033
      vwk(i,j) = vwk(i,j) + wt(k,kk) * qdrs(k,i,kk) * qdrs(k,j,kk)
23032 k=k+1
      goto 23031
23033 continue
23029 j=j+1
      goto 23028
23030 continue
23026 i=i+1
      goto 23025
23027 continue
      call daxpy (nxis, 1.d0, muwk, 1, mu, 1)
      call daxpy (nxis*nxis, 1.d0, vwk, 1, v, 1)
23023 kk=kk+1
      goto 23022
23024 continue
      i=1
23034 if(.not.(i.le.nxi))goto 23036
      j=i
23037 if(.not.(j.le.nxi))goto 23039
      v(i,j) = v(i,j) + q(i,j)
23038 j=j+1
      goto 23037
23039 continue
23035 i=i+1
      goto 23034
23036 continue
      call daxpy (nxis, 1.d0, mrs, 1, mu, 1)
      call dsymv ('u', nxi, -1.d0, q, nxi, cd, 1, 1.d0, mu, 1)
      mumax = dabs(mu(idamax(nxis, mu, 1)))
      i=1
23040 if(.not.(i.le.nxis))goto 23042
      jpvt(i) = 0
23041 i=i+1
      goto 23040
23042 continue
      call dchdc (v, nxis, nxis, wk, jpvt, 1, rkv)
23043 if(v(rkv,rkv).lt.v(1,1)*dsqrt(mchpr))then
      rkv = rkv - 1
      goto 23043
      endif
23044 continue
      i=rkv+1
23045 if(.not.(i.le.nxis))goto 23047
      v(i,i) = v(1,1)
      call dset (i-rkv-1, 0.d0, v(rkv+1,i), 1)
23046 i=i+1
      goto 23045
23047 continue
23048 continue
      call dcopy (nxis, mu, 1, cdnew, 1)
      call dprmut (cdnew, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, cdnew, 11, infowk)
      call dset (nxis-rkv, 0.d0, cdnew(rkv+1), 1)
      call dtrsl (v, nxis, nxis, cdnew, 01, infowk)
      call dprmut (cdnew, nxis, jpvt, 1)
      call daxpy (nxis, 1.d0, cd, 1, cdnew, 1)
      kk=1
23051 if(.not.(kk.le.nx))goto 23053
      i=1
23054 if(.not.(i.le.nqd))goto 23056
      tmp = ddot (nxis, qdrs(i,1,kk), nqd, cdnew, 1)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23056
      endif
      wtnew(i,kk) = qdwt(i,kk) * dexp (tmp)
23055 i=i+1
      goto 23054
23056 continue
      if((flag.eq.1).or.(flag.eq.3))then
      goto 23053
      endif
23052 kk=kk+1
      goto 23051
23053 continue
      if((flag.eq.0).or.(flag.eq.2))then
      fitmean = 0.d0
      i=1
23063 if(.not.(i.le.nt))goto 23065
      tmp = ddot (nxis, rs(1,i), 1, cdnew, 1)
      if(tmp.gt.3.d2)then
      flag = flag + 1
      goto 23065
      endif
      fitnew(i) = dexp (tmp)
      if(cntsum.ne.0)then
      tmp = tmp * dfloat (cnt(i))
      endif
      fitmean = fitmean + tmp
23064 i=i+1
      goto 23063
23065 continue
      fitmean = fitmean / dfloat (nobs) - dasum (nqd*nx, wtnew, 1)
      call dsymv ('u', nxi, 1.d0, q, nxi, cdnew, 1, 0.d0, wk, 1)
      lkhdnew = ddot (nxi, cdnew, 1, wk, 1) / 2.d0 - fitmean
      endif
      if(flag.eq.1)then
      call dset (nxis, 0.d0, cd, 1)
      call dcopy (nqd*nx, qdwt, 1, wt, 1)
      fitmean = - dasum (nqd*nx, wt, 1)
      lkhd = - fitmean
      iter = 0
      goto 23050
      endif
      if(flag.eq.3)then
      goto 23050
      endif
      if(lkhdnew-lkhd.lt.1.d1*(1.d0+dabs(lkhd))*mchpr)then
      goto 23050
      endif
      call dscal (nxis, .5d0, mu, 1)
      if(dabs(mu(idamax(nxis, mu, 1))/mumax).lt.1.d1*mchpr)then
      goto 23050
      endif
23049 goto 23048
23050 continue
      if(flag.eq.1)then
      flag = 2
      goto 23020
      endif
      if(flag.eq.3)then
      info = 1
      return
      endif
      disc = 0.d0
      kk=1
23082 if(.not.(kk.le.nx))goto 23084
      i=1
23085 if(.not.(i.le.nqd))goto 23087
      disc = dmax1 (disc, dabs(wt(i,kk)-wtnew(i,kk))/(1.d0+dabs(wt(i,kk)
     *)))
23086 i=i+1
      goto 23085
23087 continue
23083 kk=kk+1
      goto 23082
23084 continue
      i=1
23088 if(.not.(i.le.nt))goto 23090
      disc = dmax1 (disc, dabs(fit(i)-fitnew(i))/(1.d0+dabs(fit(i))))
23089 i=i+1
      goto 23088
23090 continue
      disc = dmax1 (disc, (mumax/(1.d0+dabs(lkhd)))**2)
      disc0 = dmax1 ((mumax/(1.d0+lkhd))**2, dabs(lkhd-lkhdnew)/(1.d0+da
     *bs(lkhd)))
      call dcopy (nxis, cdnew, 1, cd, 1)
      call dcopy (nqd*nx, wtnew, 1, wt, 1)
      call dcopy (nt, fitnew, 1, fit, 1)
      lkhd = lkhdnew
      if(disc0.lt.prec)then
      goto 23021
      endif
      if(disc.lt.prec)then
      goto 23021
      endif
      if(iter.lt.maxiter)then
      goto 23020
      endif
      if(flag.eq.0)then
      call dset (nxis, 0.d0, cd, 1)
      call dcopy (nqd*nx, qdwt, 1, wt, 1)
      fitmean = - dasum (nqd*nx, wt, 1)
      lkhd = - fitmean
      iter = 0
      flag = 2
      else
      info = 2
      goto 23021
      endif
23020 goto 23019
23021 continue
      i=1
23099 if(.not.(i.le.nt))goto 23101
      call dprmut (rs(1,i), nxis, jpvt, 0)
      if(cntsum.ne.0)then
      call dscal (nxis, dsqrt(dfloat(cnt(i))), rs(1,i), 1)
      endif
      call dtrsl (v, nxis, nxis, rs(1,i), 11, infowk)
23100 i=i+1
      goto 23099
23101 continue
      call dprmut (mrs, nxis, jpvt, 0)
      call dtrsl (v, nxis, nxis, mrs, 11, infowk)
      trc = ddot (nxis*nt, rs, 1, rs, 1) - dfloat (nobs) * ddot (nxis, m
     *rs, 1, mrs, 1)
      trc = trc / dfloat(nobs) / (dfloat(nobs)-1.d0)
      mrs(1) = fitmean
      mrs(2) = trc
      kk=1
23104 if(.not.(kk.le.nx))goto 23106
      i=1
23107 if(.not.(i.le.nqd))goto 23109
      wt(i,kk) = dexp (ddot (nxis, qdrs(i,1,kk), nqd, cd, 1))
23108 i=i+1
      goto 23107
23109 continue
23105 kk=kk+1
      goto 23104
23106 continue
      return
      end
C Output from Public domain Ratfor, version 1.01
      subroutine reg (sr, nobs, nnull, q, nxi, y, method, alpha, varht, 
     *score, dc, mchpr, v, mu, jpvt, wk, rkv, info)
      double precision sr(nobs,*), q(nxi,*), y(*), alpha, varht, score, 
     *dc(*), mchpr, v(nnull+nxi,*), mu(*), wk(*)
      integer nobs, nnull, nxi, method, jpvt(*), rkv, info
      double precision ddot, dasum, rss, trc, dum
      integer i, j, nn, idamax, infowk
      info = 0
      nn = nnull + nxi
      i=1
23000 if(.not.(i.le.nn))goto 23002
      mu(i) = ddot (nobs, sr(1,i), 1, y, 1)
      j=i
23003 if(.not.(j.le.nn))goto 23005
      v(i,j) = ddot (nobs, sr(1,i), 1, sr(1,j), 1)
      if(i.gt.nnull)then
      v(i,j) = v(i,j) + q(i-nnull,j-nnull)
      endif
23004 j=j+1
      goto 23003
23005 continue
23001 i=i+1
      goto 23000
23002 continue
      infowk = 0
      i=1
23008 if(.not.(i.le.nn))goto 23010
      infowk = infowk + jpvt(i)
23009 i=i+1
      goto 23008
23010 continue
      call dchdc (v, nn, nn, wk, jpvt, 1, rkv)
      j = idamax (rkv-infowk, v(infowk+1,infowk+1), nn+1)
23011 if(v(rkv,rkv).lt.v(infowk+j,infowk+j)*dsqrt(mchpr))then
      rkv = rkv - 1
      goto 23011
      endif
23012 continue
      i=rkv+1
23013 if(.not.(i.le.nn))goto 23015
      v(i,i) = v(j,j)
      call dset (i-rkv-1, 0.d0, v(rkv+1,i), 1)
23014 i=i+1
      goto 23013
23015 continue
      call dcopy (nn, mu, 1, dc, 1)
      call dprmut (dc, nn, jpvt, 0)
      call dtrsl (v, nn, nn, dc, 11, infowk)
      call dset (nn-rkv, 0.d0, dc(rkv+1), 1)
      call dtrsl (v, nn, nn, dc, 01, infowk)
      call dprmut (dc, nn, jpvt, 1)
      if(method.eq.4)then
      return
      endif
      i=1
23018 if(.not.(i.le.nobs))goto 23020
      wk(i) = y(i) - ddot (nn, sr(i,1), nobs, dc, 1)
23019 i=i+1
      goto 23018
23020 continue
      if(method.eq.5)then
      wk(nobs+1) = ddot (nobs, wk, 1, wk, 1) / dfloat (nobs)
      i=1
23023 if(.not.(i.le.nobs))goto 23025
      call dcopy (nn, sr(i,1), nobs, mu, 1)
      call dprmut (mu, nn, jpvt, 0)
      call dtrsl (v, nn, nn, mu, 11, infowk)
      wk(i) = ddot (nn, mu, 1, mu, 1)
23024 i=i+1
      goto 23023
23025 continue
      return
      endif
      if(method.eq.3)then
      rss = ddot (nobs, y, 1, wk, 1)
      if(nnull.gt.0)then
      call dqrdc (sr, nobs, nobs, nnull, wk, dum, dum, 0)
      i=1
23030 if(.not.(i.le.nxi))goto 23032
      call dqrsl (sr, nobs, nobs, nnull, wk, sr(1,nnull+i), dum, sr(1,nn
     *ull+i), dum, dum, dum, 01000, infowk)
23031 i=i+1
      goto 23030
23032 continue
      endif
      call dcopy (nxi, q, nxi+1, wk, 1)
      i=1
23033 if(.not.(i.le.nxi))goto 23035
      j=i
23036 if(.not.(j.le.nxi))goto 23038
      q(i,j) = q(i,j) + ddot (nobs-nnull, sr(nnull+1,nnull+i), 1, sr(nnu
     *ll+1,nnull+j), 1)
23037 j=j+1
      goto 23036
23038 continue
23034 i=i+1
      goto 23033
23035 continue
      i=1
23039 if(.not.(i.le.nxi))goto 23041
      j=i
23042 if(.not.(j.le.nxi))goto 23044
      sr(i,j) = q(i,j)
      sr(j,i) = q(i,j)
      q(i,j) = q(j,i)
23043 j=j+1
      goto 23042
23044 continue
23040 i=i+1
      goto 23039
23041 continue
      call dcopy (nxi, wk, 1, q, nxi+1)
      call rs (nobs, nxi, sr, mu, 0, dum, wk, y, info)
      trc = 0.d0
      i=1
23045 if(.not.(i.le.rkv-nnull))goto 23047
      trc = trc + dlog (mu(nxi-i+1))
23046 i=i+1
      goto 23045
23047 continue
      call rs (nxi, nxi, q, mu, 0, dum, wk, y, info)
      i=1
23048 if(.not.(i.le.rkv-nnull))goto 23050
      trc = trc - dlog (mu(nxi-i+1))
23049 i=i+1
      goto 23048
23050 continue
      score = rss / dfloat (nobs) * dexp (trc/dfloat(nobs-nnull))
      varht = rss / dfloat (nobs-nnull)
      else
      rss = ddot (nobs, wk, 1, wk, 1) / dfloat (nobs)
      i=1
23051 if(.not.(i.le.nobs))goto 23053
      call dcopy (nn, sr(i,1), nobs, mu, 1)
      call dprmut (mu, nn, jpvt, 0)
      call dtrsl (v, nn, nn, mu, 11, infowk)
      wk(i) = ddot (nn, mu, 1, mu, 1)
23052 i=i+1
      goto 23051
23053 continue
      trc = dasum (nobs, wk, 1) / dfloat (nobs)
      if(method.eq.2)then
      score = rss / (1.d0-alpha*trc)**2
      varht = rss / (1.d0-trc)
      else
      score = rss + 2.d0 * varht * alpha * trc
      endif
      endif
      wk(1) = rss
      wk(2) = trc
      return
      end
      subroutine regaux (v, nn, jpvt, rkv, r, nr, sms, nnull, wk)
      double precision v(nn,*), r(nn,*), sms(nnull,*), wk(nn,*)
      integer nn, jpvt(*), rkv, nr, nnull
      double precision ddot
      integer i, j, infowk
      i=1
23056 if(.not.(i.le.nr))goto 23058
      call dprmut (r(1,i), nn, jpvt, 0)
      call dtrsl (v, nn, nn, r(1,i), 11, infowk)
      call dset (nn-rkv, 0.d0, r(rkv+1,i), 1)
      call dtrsl (v, nn, nn, r(1,i), 01, infowk)
      call dprmut (r(1,i), nn, jpvt, 1)
23057 i=i+1
      goto 23056
23058 continue
      call dset (nn*nnull, 0.d0, wk, 1)
      call dset (nnull, 1.d0, wk, nn+1)
      i=1
23059 if(.not.(i.le.nnull))goto 23061
      call dtrsl (v, nn, nn, wk(1,i), 11, infowk)
23060 i=i+1
      goto 23059
23061 continue
      i=1
23062 if(.not.(i.le.nnull))goto 23064
      j=i
23065 if(.not.(j.le.nnull))goto 23067
      sms(i,j) = ddot (nn, wk(1,i), 1, wk(1,j), 1)
      sms(j,i) = sms(i,j)
23066 j=j+1
      goto 23065
23067 continue
23063 i=i+1
      goto 23062
23064 continue
      return
      end
