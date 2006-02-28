C ##############################################################################
C MARS-dcalcvar

      subroutine calcvar(nx,n,px,qr,qrank,qpivot,cov,tmpcov,work)
      implicit double precision (a-h,o-z)
      integer n,px,qrank,qpivot(px)
      double precision qr(nx,px),cov(px,px), tmpcov(px,px),work(1)
      double precision dsum
      integer i,j,km
      do 23000 i=1,qrank
      do 23002 j=1,qrank
      tmpcov(i,j)=0d0
      cov(i,j)=qr(i,j)
23002 continue
      tmpcov(i,i)=1e0
23000 continue
      info=0
c R version has different args
c      call dbksl(cov,px,qrank,tmpcov,px,info)
      do 20 j = 1, qrank
 20      call dtrsl(cov, px, qrank, tmpcov(1,j), 01, info)
      do 23004 i=1,qrank
      do 23006 j=i,qrank
      dsum=0e0
      km=max(i,j)
      k=km
23008 if(.not.(k.le.qrank))goto 23010
      dsum=dsum+tmpcov(i,k)*tmpcov(j,k)
      k=k+1
      goto 23008
23010 continue
      tmpcov(i,j)=dsum
      tmpcov(j,i)=dsum
23006 continue
23004 continue
      do 23011 i=1,qrank
      do 23013 j=1,qrank
      cov(i,j)=tmpcov(i,j)
23013 continue
23011 continue
      return
      end

C ##############################################################################
C MARS-dmarss

      subroutine marss(nx,n,p,nclass,y,x,w,tagx,maxorder,mmax,penalty,
     &   thresh,forwstep,interms,prune,bx,fullin,lenb, bestgcv, bestin, 
     &   flag,cut,dir,res,alpha,beta,scrat,iscrat,trace)
      implicit double precision (a-h,o-z)
      integer nx, n,p,nclass,tagx(nx,p),maxorder,mmax,bestin(mmax),
     &     flag(mmax,p),fullin(mmax)
      double precision y(n,nclass),x(nx,p),w(n),bx(nx,mmax),bestgcv,
     &      cut(mmax,p),dir(mmax,p),res(nx,nclass),alpha(nclass),
     &      beta(mmax,nclass)
      double precision scrat(*)
      integer iscrat(*)
      logical forwstep, prune, trace, tracec
      common tracec
      tracec=trace
      len1=n*mmax
      len2=mmax
      len3=mmax*mmax
      len4=mmax*nclass
      len5=nclass
      len6=mmax
      len7=mmax
      len8=nclass
      len9=n
      len10=n*mmax
      len11=mmax*mmax
      len12=mmax*nclass
      len13=mmax*mmax
      len14=mmax*mmax
      n1=1
      n2=n1+len1
      n3=n2+len2
      n4=n3+len3
      n5=n4+len4
      n6=n5+len5
      n7=n6+len6
      n8=n7+len7
      n9=n8+len8
      n10=n9+len9
      n11=n10+len10
      n12=n11+len11
      n13=n12+len12
      n14=n13+len13
      n15=n14+len14
      call marsnew1(nx, n, p, nclass, y, x, w, tagx, maxorder, mmax, 
     &   bx, bestgcv, bestin, fullin, lenb, flag, cut, dir, res, 
     &   alpha, beta, penalty, thresh, forwstep, interms, prune, 
     &   scrat, scrat(n2), scrat(n3), scrat(n4), scrat(n5), scrat(n6), 
     &   scrat(n7), scrat(n8), scrat(n9), scrat(n10), scrat(n11), 
     &   scrat(n12), scrat(n13), scrat(n14), scrat(n15), iscrat, 
     &   iscrat(1+mmax), iscrat(1+2*mmax), iscrat(1+3*mmax))
      return
      end

      subroutine marsnew1(nx, n, p, nclass, y, x, w, tagx, maxorder, 
     &   mmax, bx, bestgcv, bestin, fullin, lenb, flag, cut, dir, 
     &   res, alpha, beta, penalty,  thresh, forwstep, interms, 
     &   prune, bxorth, bxorthm, cov, covsy, ybar, scr1, scr5, scr6,  
     &   temp, bxsc, r, betasc, varsc, var, work, termlen, in,  
     &   tempin, qpivot)
      implicit double precision (a-h,o-z)
      integer n,nterms2,p,mmax,flag(mmax,p),tagx(nx,p),termlen(mmax), 
     &   nclass,fullin(mmax)
      double precision cov(mmax,mmax),covsy(mmax,nclass),critmax,
     &    x(nx,p),bx(nx,mmax),bxorth(n,mmax),bxorthm(mmax),
     &    y(n,nclass),ybar(nclass),scr1(mmax),scr5(mmax),scr6(nclass)
      double precision temp(n),w(n), cut(mmax,p),dir(mmax,p),
     &    alpha(nclass),beta(mmax,nclass), bxsc(n,mmax), r(mmax,mmax), 
     &    dofit, res(nx,nclass),betasc(mmax,nclass), varsc(mmax,mmax), 
     &    var(mmax,mmax), stopfac, work(*)
      integer tempin(mmax), bestin(mmax),qrank, qpivot(mmax)
      logical forwstep,go, prune, newform, cvar, trace
      common trace
      double precision rtemp(4)
      integer itemp(4)
      tolbx=.01
      stopfac=10.0
      prevcrit=10e9
      if(.not.(interms.eq.1))goto 23000
      dofit=0
      goto 23001
23000 continue
      dofit=0
      do 23002 j=2,lenb 
      dofit=dofit+fullin(j)
23002 continue
      nterms=interms
23001 continue
      if(.not.(forwstep))goto 23004
      fullin(1)=1
      do 23006 i=2,mmax
      fullin(i)=0
23006 continue
      do 23008 i=1,n 
      w(i)=1
23008 continue
      do 23010 i=1, mmax
      termlen(i)=0
      do 23012 j=1, p
      flag(i,j)=0
      cut(i,j)=0
23012 continue
23010 continue
      nterms=1
      nterms2=2
      do 23014 i=1,n 
      bx(i,1)=1
      bxorth(i,1)=1.0/dsqrt(dfloat(n))
23014 continue
      bxorthm(1)=1/dsqrt(dfloat(n))
      do 23016 i=1,n 
      do 23018 j=1, mmax
      bx(i,j)=0.0
23018 continue
23016 continue
      do 23020 i=1,n 
      bx(i,1)=1
23020 continue
      do 23022 k=1, nclass
      ybar(k)=0.0
      do 23024 i=1,n 
      ybar(k)=ybar(k)+y(i,k)/n
23024 continue
23022 continue
      if(.not.(interms.eq.1))goto 23026
      rssnull=0.0
      do 23028 k=1, nclass
      do 23030 i=1,n 
      rssnull=rssnull+(y(i,k)-ybar(k))**2
23030 continue
23028 continue
      goto 23027
23026 continue
      rssnull=0.0
      do 23032 k=1, nclass
      do 23034 i=1,n 
      rssnull=rssnull+res(i,k)**2
23034 continue
23032 continue
23027 continue
      rss=rssnull
      cmm= (1+dofit) + penalty*(.5*dofit)
      gcvnull=(rssnull/n)/(1.0-cmm/n)**2
      if(.not.(trace))goto 23036
      call dblepr("initial rss=",11,rssnull,1)
23036 continue
      if(.not.(trace))goto 23038
      call dblepr("initial gcv=",11,gcvnull,1)
23038 continue
      lenb=1
      ii=interms-1
      go=.true.
23040 if(.not.( (ii.lt.(mmax-1)).and.((rss/rssnull).gt.thresh).and.go))
     &     goto 23041
      ii=ii+2
      do 23042 i1=1, nterms
      do 23044 i2=1, nterms
      cov(i1,i2)=0
23044 continue
23042 continue
      do 23046 j=1, nterms
      cov(j,j)=0.0
      do 23048 i=1,n 
      cov(j,j) = cov(j,j) + 
     %           (bxorth(i,j)-bxorthm(j)) * (bxorth(i,j)-bxorthm(j))
23048 continue
23046 continue
      do 23050 k=1,nclass
      do 23052 j=1, nterms
      covsy(j,k)=0.0
      do 23054 i=1,n 
      covsy(j,k)=covsy(j,k)+(y(i,k)-ybar(k))*bxorth(i,j)
23054 continue
23052 continue
23050 continue
      do 23056 ik=1,mmax 
      tempin(ik)=fullin(ik)
23056 continue
      call addtrm(nx,bx,tempin,bxorth,bxorthm,p,n,nclass,rss,prevcrit, 
     &     cov,covsy,y,ybar,x,tagx,w,termlen,mmax,tolbx, nterms,flag,
     &     maxorder,scr1,scr5,scr6,imax,jmax,kmax,critmax, newform,
     &     bxsc, r, betasc, temp)
      doftemp=dofit
      doftemp=doftemp+1
      if(.not.((imax.gt.1).and.(newform)))goto 23058
      doftemp=doftemp+1
23058 continue
      temprss=rss-critmax
      cmm= (1+doftemp) + penalty*(.5*doftemp)
      gcv=(temprss/n)/(1.0-cmm/n)**2
      go=.false.
      if (.not.(((critmax/rss).gt.thresh).and.
     &          ((gcv/gcvnull).lt.stopfac))) goto 23060
      go=.true.
      dofit=doftemp
      rss=rss-critmax
      kk=tagx(imax,jmax)
256   format(" ","adding term"," jmax=",i3, "  imax=",i3 ,"  kmax=",i3, 
     &  "  critmax= ",f8.2,"  cutp=", f9.5," rss=",f8.2, " gcv=",f8.2, 
     &  " dofit=",f9.3)
      itemp(1)=jmax
      itemp(2)=imax
      itemp(3)=kmax
      rtemp(1)=critmax
      rtemp(2)=x(kk,jmax)
      rtemp(3)=rss
      rtemp(4)=gcv
      if(.not.(trace))goto 23062
      call intpr("adding term ",12,ii,1)
23062 continue
      if(.not.(trace))goto 23064
      call intpr("var, sp index, parent",21,itemp,3)
23064 continue
      if(.not.(trace))goto 23066
      call dblepr("critmax cut rss gcv",19,rtemp,4)
23066 continue
      prevcrit=critmax
      do 23068 j=1,p 
      flag(ii,j)=flag(kmax,j)
      flag(ii+1,j)=flag(kmax,j)
      cut(ii,j)=cut(kmax,j)
      cut(ii+1,j)=cut(kmax,j)
      dir(ii,j)=dir(kmax,j)
      dir(ii+1,j)=dir(kmax,j)
23068 continue
      termlen(ii)=termlen(kmax)+1
      termlen(ii+1)=termlen(kmax)+1
      do 23070 i=1,n 
      temp(i)=x(tagx(i,jmax),jmax)
23070 continue
      temp1=temp(imax)
      fullin(ii)=1
      if(.not.((imax.gt.1).and.(newform)))goto 23072
      fullin(ii+1)=1 
23072 continue
      flag(ii,jmax)=1
      flag(ii+1,jmax)=1
      cut(ii,jmax)=temp1
      cut(ii+1,jmax)=temp1
      dir(ii,jmax)=1
      dir(ii+1,jmax)=-1
      if(.not.(fullin(ii+1).eq.0))goto 23074
      termlen(ii+1)=maxorder+1
23074 continue
      do 23076 i=1,n 
      if(.not.( (x(i,jmax)-temp1).gt.0))goto 23078
      bx(i,ii)=bx(i,kmax)*(x(i,jmax)-temp1)
23078 continue
      if(.not.((temp1-x(i,jmax)).ge.0))goto 23080
      bx(i,ii+1)=bx(i,kmax)*(temp1-x(i,jmax))
23080 continue
23076 continue
      if(.not.(nterms.eq.1))goto 23082
      temp1=0.0
      do 23084 i=1,n 
      temp1=temp1+bx(i,2)/n
23084 continue
      do 23086 i=1,n 
      bxorth(i,2)=bx(i,2)-temp1
23086 continue
      goto 23083
23082 continue
      call orthreg(n,n,nterms,bxorth,fullin, bx(1,ii),bxorth(1,nterms2))
23083 continue
      if(.not.(fullin(ii+1).eq.1))goto 23088
      call orthreg(n,n,nterms+1,bxorth,fullin, bx(1,ii+1),
     &       bxorth(1,nterms2+1))
      goto 23089
23088 continue
      do 23090 i=1,n 
      bxorth(i,nterms2+1)=0
23090 continue
23089 continue
      bxorthm(nterms2)=0.0 
      bxorthm(nterms2+1)=0.0
      do 23092 i=1,n 
      bxorthm(nterms2)=bxorthm(nterms2)+bxorth(i,nterms2)/n
      bxorthm(nterms2+1)=bxorthm(nterms2+1)+bxorth(i,nterms2+1)/n
23092 continue
      temp1=0.0
      temp2=0.0
      do 23094 i=1,n 
      temp1=temp1+bxorth(i,nterms2)**2
      temp2=temp2+bxorth(i,nterms2+1)**2 
23094 continue
      if(.not.(temp1.gt.0.0))goto 23096
      do 23098 i=1,n 
      bxorth(i,nterms2) =bxorth(i,nterms2)/dsqrt(temp1) 
23098 continue
23096 continue
      if(.not.(temp2.gt.0.0))goto 23100
      do 23102 i=1,n 
      bxorth(i,nterms2+1)=bxorth(i,nterms2+1)/dsqrt(temp2) 
23102 continue
23100 continue
      lenb=lenb+2
      nterms=nterms+2
      nterms2=nterms2+2
23060 continue
      goto 23040
23041 continue
      rtemp(1)=rss/rssnull
      rtemp(2)=critmax/rss
      rtemp(3)=gcv/gcvnull
      if(.not.(trace))goto 23104
      call dblepr("stopping forw step; rss crit and gcv ratios",43,
     &     rtemp,3)
23104 continue
      if(.not.(trace))goto 23106
      if(.not.((rss/rssnull).le.thresh))goto 23108
      call dblepr("rss ratio=",10,rss/rssnull,1)
23108 continue
      if(.not.((critmax/rss).le.thresh))goto 23110
      call dblepr ("crit ratio=",11,critmax/rss,1)
23110 continue
      call dblepr("critmax",7,critmax,1)
      call dblepr("rss",3,rss,1)
      if(.not.((gcv/gcvnull).gt.stopfac))goto 23112
      call dblepr("gcv ratio=",10,gcv/gcvnull,1)
23112 continue
23106 continue
23004 continue
      dofit= -1
      do 23114 i=1,nterms
      bestin(i)=fullin(i)
      dofit=dofit+fullin(i)
23114 continue
      if(.not.(trace))goto 23116
      call intpr("aft forw step",13,nterms,1)
23116 continue
      call qrreg(nx,n,mmax,lenb,nclass,bx,bxsc,bestin,y,qpivot,qrank,
     &     beta,res,rss,cvar,var,varsc,scr1, work)
      nt=dofit+1
      if(.not.(qrank.lt. nt))goto 23118
      do 23120 i=qrank+1,nt
      bestin(qpivot(i))=0
      fullin(qpivot(i))=0
      dofit=dofit-1
23120 continue
23118 continue
      cvar=.true.
      rssfull=rss
      cmm= (1+dofit) + penalty*(.5*dofit)
      bestgcv=(rss/n)/(1.0-cmm/n)**2
      rtemp(1)=bestgcv
      rtemp(2)=rssfull
      rtemp(3)=dofit
      if(.not.(trace))goto 23122
      call dblepr("full model: gcv rss dofit",25,rtemp,3)
23122 continue
      if(.not.(trace))goto 23124
      call intpr("terms",5,fullin,lenb)
23124 continue
      if(.not.(prune))goto 23126
c Need var calculated to do drop-one calculations from t values.
      cvar=.true.
      call qrreg(nx,n,mmax,lenb,nclass,bx,bxsc,tempin,y,qpivot,qrank,
     &     beta,res,rss,cvar,var,varsc,scr1,work)
      do 23128 i=1,mmax
      tempin(i)=bestin(i)
23128 continue
23130 if(.not.(dofit.gt.0 ))goto 23131
      jo=1
      rsstemp=10d99
      minterm=0
      do 23132 ii=2, lenb 
      if(.not.(tempin(ii).eq.1))goto 23134
      jo=jo+1
      temp7=0.0
      do 23136 kc=1,nclass
      temp7=temp7+beta(jo,kc)**2/var(jo,jo)
23136 continue
      if(.not.(temp7 .lt. rsstemp))goto 23138
      minterm=ii
      rsstemp=temp7
23138 continue
23134 continue
23132 continue
      rss=rss+rsstemp
      dofit=dofit-1
      cmm= (1.0+dofit) + penalty*(.5*dofit)
      gcv=(rss/n)/(1.0-cmm/n)**2
      tempin(minterm)=0
100   format(" ","pruning, minterm= ",i4, " gcv=",f9.3,2x, " rss=",f9.3,
     &     2x," dof=",f9.3," model= ",60(i1,1x))
      if(.not.(gcv.lt. bestgcv))goto 23140
      bestgcv=gcv
      do 23142 i=1,mmax 
      bestin(i)=tempin(i)
23142 continue
23140 continue
      if(.not.(dofit .gt. 0))goto 23144
      cvar=.true.
      call qrreg(nx,n,mmax,lenb,nclass,bx,bxsc,tempin,y,qpivot,qrank,
     &     beta,res,rss,cvar,var,varsc,scr1,work)
23144 continue
      goto 23130
23131 continue
      call qrreg(nx,n,mmax,lenb,nclass,bx,bxsc,bestin,y,qpivot,qrank,
     &     beta,res,rss,cvar,var,varsc,scr1, work)
101   format(" ","best model gcv=",f9.3," rss=",f9.3,2x,"model= ",
     &             60(i1,1x))
      if(.not.(trace))goto 23146
      call intpr("best model",10,bestin,lenb)
23146 continue
      if(.not.(trace))goto 23148
      call dblepr(" gcv=",4,bestgcv,1)
23148 continue
23126 continue
      return
      end
      subroutine addtrm(nx,bx,tempin,bxorth,bxorthm,p,n,nclass,rss,
     &     prevcrit,cov,covsy,y,ybar,x,tagx,w,termlen,mmax,tolbx,
     &     nterms,flag, maxorder,scr1,scr5,scr6,imax,jmax,kmax,
     &     critmax, newform,bxsc,r, betasc, scrat)
      implicit double precision (a-h,o-z)
      integer n,nterms,nterms2,p,mmax,flag(mmax,p),v,tagx(nx,p),
     &        termlen(mmax), nclass, tempin(mmax), minspan, iendspan
      double precision cov(mmax,mmax),covsy(mmax,nclass),critmax,
     &     x(nx,p),bx(nx,mmax),bxorth(n,mmax),bxorthm(mmax),
     &     y(n,nclass),ybar(nclass),scr1(mmax),scr5(mmax),scr6(nclass), 
     &     bxsc(n,mmax), r(mmax,mmax),betasc(mmax,nclass), scrat(n),
     &     w(n)
      double precision temp1, temp2, scr2,sumb, sumbx, su, st, tem
      logical newform, tnewform, trace
      common trace
      critmax=0
      jmax=0
      imax=0
      kmax=0
      do 23150 m=1,nterms 
      nm=0
      do 23152 jjj=1,n 
      if(.not.(bx(jjj,m).gt.0))goto 23154
      nm=nm+1
23154 continue
23152 continue
      tem=-(1d0/(n*nm))*dlog(1d0 - 5d-2)
      minspan= -1d0*(dlog(tem)/dlog(2d0))/2.5
      tem=(5d-2)/n
      iendspan=3d0-dlog(tem)/dlog(2d0)
      if(.not.(termlen(m).lt. maxorder))goto 23156
      do 23158 v=1,p 
      if(.not.(flag(m,v).eq.0))goto 23160
      tnewform=.true.
      mm=1
23162 if(.not.((mm.le.nterms).and.tnewform))goto 23163
      mm=mm+1
      if(.not.(tempin(mm).eq.1))goto 23164
      tnewform=.false.
      if(.not.(flag(mm,v).ne.1))goto 23166
      tnewform=.true.
      go to 9911
23166 continue
      do 23168 j=1,p 
      if(.not.(j.ne.v))goto 23170
      if(.not.(flag(mm,j).ne.flag(m,j)))goto 23172
      tnewform=.true.
      go to 9911 
23172 continue
23170 continue
23168 continue
23164 continue
9911  continue
      goto 23162
23163 continue
      if(.not.(tnewform))goto 23174
      nterms2=nterms+1
      do 23176 i=1,n 
      scrat(i)=x(i,v)*bx(i,m)
23176 continue
      if(.not.(nterms.gt.1))goto 23178
      call orthreg(n,n,nterms,bxorth,tempin, scrat,bxorth(1,nterms2))
      goto 23179
23178 continue
      tem=0
      do 23180 i=1,n 
      tem=tem+scrat(i)/n
23180 continue
      do 23182 i=1,n
      bxorth(i,2)=scrat(i)-tem
23182 continue
23179 continue
      bxorthm(nterms2)=0.0 
      do 23184 i=1,n 
      bxorthm(nterms2)=bxorthm(nterms2)+bxorth(i,nterms2)/n
23184 continue
      temp1=0.0
      do 23186 i=1,n 
      temp1=temp1+bxorth(i,nterms2)**2
23186 continue
      if(.not.(temp1.gt.tolbx))goto 23188
      do 23190 i=1,n 
      bxorth(i,nterms2)=bxorth(i,nterms2)/dsqrt(temp1) 
23190 continue
      goto 23189
23188 continue
      do 23192 i=1,n 
      bxorth(i,nterms2)=0
23192 continue
      tnewform=.false.
23189 continue
      do 23194 i1=1, nterms2
      cov(i1,nterms2)=0.0
      cov(nterms2, i1)=0.0
23194 continue
      cov(nterms2,nterms2)=1
      do 23196 kc=1,nclass
      covsy(nterms2,kc)=0.0
      do 23198 i=1,n 
      covsy(nterms2,kc) = covsy(nterms2,kc)+(y(i,kc)-ybar(kc)) * 
     &                                               bxorth(i,nterms2)
23198 continue
23196 continue
      critnew=0.0
      do 23200 kc=1,nclass 
      temp1=0
      do 23202 i=1,n 
      temp1=temp1+y(i,kc)*bxorth(i,nterms2)
23202 continue
      critnew=critnew+temp1**2
23200 continue
      if(.not.(critnew.gt.critmax))goto 23204
      jmax=v
      critmax=critnew
      imax=1
      kmax=m
23204 continue
23174 continue
      if(.not.(tnewform))goto 23206
      nterms2=nterms+1
      nterms21=nterms+2
      goto 23207
23206 continue
      nterms2=nterms
      nterms21=nterms+1
      critnew=0.0
23207 continue
      do 23208 kc=1, nclass
      covsy(nterms21,kc)=0
23208 continue
      do 23210 ii=1,nterms21 
      cov(ii,nterms21)=0
      cov(nterms21,ii)=0
23210 continue
      do 23212 kc=1,nclass 
      scr6(kc)=0
23212 continue
      do 23214 ii=1,nterms21
      scr1(ii)=0
23214 continue
      scr2=0
      su=0
      st=0
      sumbx2=0
      sumb=0.0
      sumbx=0.0
      k=n-1
23216 if(.not.(k.gt.0))goto 23218
      do 23219 i=1,nterms2 
      kk=tagx(k,v)
      kk1=tagx(k+1,v)
      scr1(i)=scr1(i)+(bxorth(kk1,i)-bxorthm(i))*bx(kk1,m)
      cov(i,nterms21)=cov(i,nterms21)+ (x(kk1,v)-x(kk,v))*scr1(i)
      cov(nterms21,i)=cov(i,nterms21)
23219 continue
      scr2=scr2+(bx(kk1,m)**2)*x(kk1,v)
      sumbx2=sumbx2+bx(kk1,m)**2
      sumb=sumb+bx(kk1,m)
      sumbx=sumbx+bx(kk1,m)*x(kk1,v)
      su=st
      st=sumbx-sumb*x(kk,v)
      cov(nterms21,nterms21)= cov(nterms21,nterms21)+ (x(kk1,v)-x(kk,v))
     &     *(2*scr2-sumbx2*(x(kk,v)+x(kk1,v)))+ ( (su*su)-(st*st) )/n
      crittemp=critnew
      do 23221 kc=1, nclass
      scr6(kc)=scr6(kc)+(y(kk1,kc)-ybar(kc))*bx(kk1,m)
      covsy(nterms21,kc)=covsy(nterms21,kc )+(x(kk1,v)-x(kk,v))*scr6(kc)
      temp1=covsy(nterms21,kc)
      temp2=cov(nterms21,nterms21)
      do 23223 jk=1,nterms2 
      temp1=temp1-covsy(jk,kc)*cov(jk,nterms21)
      temp2=temp2-cov(jk,nterms21)*cov(jk,nterms21)
23223 continue
      if(.not.(cov(nterms21,nterms21).gt.0))goto 23225
      if(.not.((temp2/cov(nterms21,nterms21)) .gt. tolbx))goto 23227
      critadd=(temp1*temp1)/temp2
      goto 23228
23227 continue
      critadd=0.0
23228 continue
      goto 23226
23225 continue
      critadd=0
23226 continue
      crittemp=crittemp+critadd
      if(.not.(crittemp.gt.(1.01*rss)))goto 23229
      crittemp=0.0
23229 continue
      if(.not.(crittemp.gt.(2*prevcrit)))goto 23231
      crittemp=0.0
23231 continue
23221 continue
      if(.not.(k.gt.1))goto 23233
      k0=tagx(k-1,v)
23233 continue
      if(.not.((crittemp.gt.critmax) .and. 
     &         (mod(k,minspan).eq.0) .and.
     &               (k.ge.iendspan) .and.
     &           (k.le.(n-iendspan)) .and.
     &              (bx(kk1,m).gt.0) .and. 
     & (.not.( (k.gt.1) .and. (x(kk,v).eq.x(k0,v)))   ))) goto 23235
      jmax=v
      critmax=crittemp
      imax=k
      kmax=m
      newform=tnewform
23235 continue
      k=k-1
      goto 23216
23218 continue
23160 continue
9999  continue
23158 continue
23156 continue
23150 continue
      return
      end


C ##############################################################################
C MARS-dorthreg

      subroutine orthreg(nx,n,p,x,in, y,res)
      implicit double precision (a-h,o-z)
      integer n,nx,p, in(p)
      double precision x(nx,p),y(n),res(n)
      do 23000 i=1,n 
      res(i)=y(i)
23000 continue
      do 23002 j=1,p 
      if(.not.(in(j).eq.1))goto 23004
      temp1=0
      temp2=0
      do 23006 i=1,n 
      temp1=temp1+res(i)*x(i,j)
      temp2=temp2+x(i,j)*x(i,j)
23006 continue
      beta=temp1/temp2
      do 23008 i=1,n 
      res(i)=res(i)-beta*x(i,j)
23008 continue
23004 continue
23002 continue
      return
      end

      
C ##############################################################################
C MARS-dqrreg
 
      subroutine qrreg(nx,n,px,p,nclass,x,xsc,in,y,qpivot,qrank,beta,
     &     res,rss,cvar,var,varsc,scr1,work)
      implicit double precision (a-h,o-z)
      integer nx,n,p,px, qpivot(p),qrank,nclass,in(p)
      double precision x(nx,p), xsc(n,p), y(n,nclass),res(nx,nclass),
     &     beta(px,nclass),work(*),scr1(p),var(px,p),varsc(px,p)
      logical cvar
      ii=0
      do 23000 j=1,p 
      if(.not.(in(j).eq.1))goto 23002
      ii=ii+1
      do 23004 i=1,n 
      xsc(i,ii)=x(i,j)
23004 continue
23002 continue
23000 continue
      nt=ii
      ijob=101
      info=1
      temp3=1d-2
      do 23006 i=1,p 
      qpivot(i)=i
23006 continue
      call dqrdc2(xsc,n,n,nt,temp3,qrank,scr1,qpivot,work)
      rss=0.0
      do 23008 k=1,nclass
      call dqrsl(xsc,n,n,qrank,scr1,y(1,k),work(1),work(1),beta(1,k),
     &     work(1),res(1,k),ijob,info)
      do 23010 i=1,n 
      res(i,k)=y(i,k)-res(i,k)
      rss=rss+res(i,k)*res(i,k)
23010 continue
23008 continue
      if(.not.(cvar))goto 23012
      call calcvar(nx,n,px,xsc,qrank,qpivot,var,varsc,work)
23012 continue
      return
      end

      