
c ##############################################################################
c XtsTools - dv subroutine 


C This library is free software; you can redistribute it and/or
C modify it under the terms of the GNU Library General Public
C License as published by the Free Software Foundation; either
C version 2 of the License, or (at your option) any later version.
C
C This library is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
C GNU Library General Public License for more details.
C
C You should have received a copy of the GNU Library General 
C Public License along with this library; if not, write to the 
C Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
C MA  02111-1307  USA


C
C Bin-Zhou's Devolatilization Procedure
C
C Subroutine written by Diethelm Wuertz
C www.rmetrics.org
C
C  s(nt)  original time series of log prices
C  ms(nt) index
C  nt     their lengths
C  k      length of averaging interval
C  vmax   volatility threshold 

      subroutine dv(vmax, s, ms, nt, ns, k)
C
C>>> DECLARATIONS:
      integer nt, ms(nt), k, ns, m, j
      real*8 v, vmax, s(nt)
      real*8 s1, s2, ok, tks
C
C>>> SETTINGS:
      ns=0
      m=k
      ok=1.0d0/real(k)
      tks=2.0d0/real(k*k)
      do i=1, nt
         ms(i)=-1
      enddo
C
C>>> DEVOLATILIZATION:
 11   continue
      v=0.0d0
      do i=m+1, nt
         v=v+ok*(s(i-k)-s(i))**2
         do j=0, k-1
            s1=s(i-j-1)-s(i-j)
            s2=s(i-j-2)-s(i-j-1)
            v=v+tks*s1*s2
         enddo
         if (v.ge.vmax) then
            ns=ns+1
            m=i
            if (ns.lt.nt) ms(i)=+1
            if(m.lt.nt) goto 11
         endif
      enddo
C
      return
      end
      
      
c ##############################################################################
c XtsTools - fxfilter subroutine

C
C
C This library is free software; you can redistribute it and/or
C modify it under the terms of the GNU Library General Public
C License as published by the Free Software Foundation; either
C version 2 of the License, or (at your option) any later version.
C
C This library is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
C GNU Library General Public License for more details.
C
C You should have received a copy of the GNU Library General 
C Public License along with this library; if not, write to the 
C Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
C MA  02111-1307  USA
C
C Copyright Diethelm Wuertz
C
C
C*************************************************************
C     Filterprogram
C       A Fortran subroutine written by Diethelm Wuertz
C       License GPL
C 		www.rmetrics.org
C*************************************************************
C
      subroutine fxfilter(xjulian,bid,ask,fparm,index,nd)
C
C>>> DECLARATIONS:
      implicit real*8 (a-h,o-z)
      dimension xjulian(nd),bid(nd),ask(nd)      
      dimension fparm(9)
      dimension index(nd)
C
C>>> STANDARD FILTERS STRONG/WEAK:
C     D,  S,  A,   Q,   C,  T,   U,  V,      W
C    /0.5,2.0,0.18,0.25,1.3,45.0,4.0,0.00010,0.025/  ! strong
C    /0.5,2.2,0.27,0.40,1.5,75.0,5.5,0.00008,0.004/  ! weak
C 
C>>> FILTER PARAMETERS:
      D=fparm(1)
      S=fparm(2)
      A=fparm(3)
      Q=fparm(4)
      C=fparm(5)
      T=fparm(6)
      U=fparm(7)
      V=fparm(8)
      W=fparm(9)
      A=A*(1.0d0)/((24.0d0*60.0d0)**D)
      T=T*(1.0d0)/((24.0d0*60.0d0)**D)
C     
C>>> FILTER PROCESS:
C     !read first data record and assume it to be validated
       n=1
       nacc=1
       xjul1=xjulian(n)
       vbid=bid(n)
       vask=ask(n)
       p1=0.5*(vbid+vask)              !initial midprice
       bid1=vbid                       !initial bidprice
       valbid=dlog(vbid)               !validated log-price
       valspr=dlog(vask)-dlog(vbid)    !validated log-spread
       index(n)=n
C
       do n=2,nd
          if (ask(n).le.bid(n)) goto 441     !skip wrong prices
          if (bid(n).eq.0.)     goto 441     !zero bid price
          if (ask(n).eq.0.)     goto 441     !zero ask price
          ttime=(xjulian(n)-xjul1+1.0d0)  !time difference in min
          naccept=0   
C        !filtering of the price:
          tbid=dlog(bid(n))               
          test1=dabs(tbid-valbid)      
            if(test1.lt.Q) naccept=naccept+1
            if(test1.lt.S*valspr+A*(ttime**D)) naccept=naccept+1
C        !filtering of the spread:
          tspr=dlog(ask(n))-dlog(bid(n))
            if(tspr.ge.V) naccept=naccept+1
            if(tspr.lt.W) naccept=naccept+1
          tlog=dabs(dlog(tspr/valspr))
            if(tlog.lt.U) naccept=naccept+1
            if(tlog.lt.C+T*(ttime**D)) naccept=naccept+1
C        !filtering results:
          if(naccept.eq.6) then
            xjul1=xjulian(n)
            valbid=tbid
            valspr=tspr
            vbid=bid(n)  
            vask=ask(n)  
            nacc=nacc+1
            index(n)=n
          else
            index(n)=-n
          endif
 441      continue
        enddo
C
      return
      end
C
      subroutine fxvarmin(bid,ask,vbid,vask,julian,icount,nx,nv)
C
C>>> DECLARATIONS:
      implicit real*8 (a-h,o-z)
      dimension julian(nx),bid(nx),ask(nx)      
      dimension vbid(nv), vask(nv)
      dimension icount(nv)
C
      do i=1,nv
         vbid(i)=0.0
         vask(i)=0.0
         icount(i)=0
      enddo
C
      ic=1
      jindex=julian(1)
      vbid(ic)=vbid(ic)+bid(1)
      vask(ic)=vask(ic)+ask(1)
      icount(ic)=1
C
      do n=2,nx
         if(julian(n).gt.jindex) then
            jindex=julian(n)
            ic=ic+1
            if(ic.gt.nv) return
         endif
         vbid(ic)=vbid(ic)+bid(n)
         vask(ic)=vask(ic)+ask(n)
         icount(ic)=icount(ic)+1
      enddo
C
      do i=1,nv
         vbid(i)=vbid(i)/icount(i)
         vask(i)=vask(i)/icount(i)
      enddo
C
      return
      end

      
C ------------------------------------------------------------------------------

