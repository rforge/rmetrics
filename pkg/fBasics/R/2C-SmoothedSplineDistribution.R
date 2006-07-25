
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTIONS:        DESCRIPTION:
#   dssd             Returns smoothed spline density estimate
#   pssd             Returns smoothed spline probability estimate
#   qssd             Returns smoothed spline quantiles estimate
#   rssd             Returns smoothed spline random variates 
# INTERNAL:         DESCRIPTION:         
#  .dssden           Internal function called by dssd
#  .pssden           Internal function called by pssd
#  .qssden           Internal function called by qssd
################################################################################


################################################################################
# IMPORTANT NOTE:
# The required contributed R-package is not available as Debian package,
# see http://packages.debian.org/unstable/math, therefore we have implemented
# here the required functionality ftrom this package
# Last Check: 2006-07-13 DW


################################################################################
#   dssd             Returns smoothed spline density estimate
#   pssd             Returns smoothed spline probability estimate
#   qssd             Returns smoothed spline quantiles estimate
#   rssd             Returns smoothed spline random variates 


dssd = 
function(x, param) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #  Evaluate density using smoothing spline ANOVA model 
    
    # Arguments:
    #   param - an S3 object of class "ssd" as returned from the
    #       function 'ssdFit'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    class(param) = "ssden"
    .dssden(object = param, x = x) 
}   


# ------------------------------------------------------------------------------

 
pssd = 
function(q, param) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Evaluate probability using smoothing spline ANOVA model 
    
    # Arguments:
    #   param - an S3 object of class "ssd" as returned from the
    #       function 'ssdFit'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    class(param) = "ssden"
    .pssden(object = param, q = q) 
}


# ------------------------------------------------------------------------------


qssd = 
function(p, param) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Evaluate quantiles using smoothing spline ANOVA model   
    
    # Arguments:
    #   param - an S3 object of class "ssd" as returned from the
    #       function 'ssdFit'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    class(param) = "ssden"
    .qssden(object = param, p = p) 
}


# ------------------------------------------------------------------------------


rssd = 
function(n, param) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generate random deviates using smoothing spline ANOVA model 
    
    # Arguments:
    #   param - an S3 object of class "ssd" as returned from the
    #       function 'ssdFit'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    class(param) = "ssden"
    .qssden(object = param, p = runif(n)) 
}


################################################################################
#  .dssden           Internal function called by dssd
#  .pssden           Internal function called by pssd
#  .qssden           Internal function called by qssd


# Code Copied from:
#   Package: gss
#   Version: 0.9-3
#   Depends: R (>= 1.7.0)
#   Title: General Smoothing Splines
#   Author: Chong Gu <chong@stat.purdue.edu>
#   Maintainer: Chong Gu <chong@stat.purdue.edu>
#   Description: A comprehensive package for structural multivariate
#           function estimation using smoothing splines.
#   License: GPL
#   Packaged: Thu Sep 23 16:28:03 2004


.dssden = 
function (object, x) 
{   # A copy from contributed R package gss
 
    # Description:
    #   Evaluate density estimate
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Density:
    if (class(object) != "ssden") {
        stop("error in .dssden: not a ssden object")
    }
    if (dim(object$mf)[2] == 1 & is.vector(x)) {
        x = data.frame(x)
        colnames(x) = colnames(object$mf)
    }
    
    s = NULL
    r = matrix(0,dim(x)[1], length(object$id.basis))
    nq = 0
    for (label in object$terms$labels) {
        xx = object$mf[object$id.basis,object$terms[[label]]$vlist]
        x.new = x[,object$terms[[label]]$vlist]
        nphi = object$terms[[label]]$nphi
        nrk = object$terms[[label]]$nrk
        if (nphi) {
            phi =  object$terms[[label]]$phi
            for (i in 1:nphi) {
                s = cbind(s,phi$fun(x.new, nu = i, env = phi$env))
            }
        }
        if (nrk) {
            rk = object$terms[[label]]$rk
            for (i in 1:nrk) {
                nq = nq + 1
                r = r + 10^object$theta[nq] * 
                    rk$fun(x.new, xx, nu = i, env = rk$env, out = TRUE)
            }
        }
    }
    
    # Return Value:
    as.vector(exp(cbind(s,r) %*% c(object$d, object$c)) / object$int)
}


# ------------------------------------------------------------------------------


.pssden = 
function(object, q) 
{   # A copy from contributed R package gss

    # Description
    #   Compute cdf for univariate density estimate
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Probability:
    if (class(object) != "ssden") {
        stop("error in .pssden: not a ssden object")
    }
    if (dim(object$mf)[2] != 1) {
        stop("error in .pssden: not a 1-D density")
    }
    
    mn = min(object$domain)
    mx = max(object$domain)
    order.q = rank(q)
    p = q = sort(q)
    q.dup = duplicated(q)
    p[q<=mn] = 0
    p[q>=mx] = 1
    kk = (1:length(q))[q>mn&q<mx]
    for (i in kk) {
        if (q.dup[i]) {
            p[i] = p.dup
            next
        }
        nqd.l = max(20,ceiling((q[i]-mn)/(mx-mn)*200))
        qd.l = .ssden.gauss.quad(nqd.l, c(mn, q[i]))
        p.l = sum(.dssden(object,qd.l$pt) * qd.l$wt)
        nqd.u = max(20,ceiling((mx-q[i]) / (mx-mn)*200))
        qd.u = .ssden.gauss.quad(nqd.u, c(q[i],mx))
        p.u = sum(.dssden(object, qd.u$pt)*qd.u$wt)
        p[i] = p.dup = p.l / (p.l+p.u)
    }
    
    # Return Value:
    p[order.q]
}


# ------------------------------------------------------------------------------


.qssden = 
function(object, p) 
{   # A copy from contributed R package gss
 
    # Description:
    #   Compute quantiles for univariate density estimate
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Quantiles:
    if (class(object) != "ssden") {
        stop("error in .qssden: not a ssden object")
    }
    if (dim(object$mf)[2] != 1) {
        stop("error in .qssden: not a 1-D density")
    }
    
    mn = min(object$domain)
    mx = max(object$domain)
    order.p = rank(p)
    q = p = sort(p)
    p.dup = duplicated(p)
    q[p <= 0] = mn
    q[p >= 1] = mx
    kk = (1:length(p))[p>0&p<1]
    q.wk = object$quad$pt[,1]
    p.wk = cumsum(object$quad$wt * .dssden(object, q.wk))
    
    for (i in kk) {
        if (p.dup[i]) {
            q[i] = q.dup
            next
        }
        j = which.min(abs(p[i]-p.wk))
        q0 = q.wk[j]
        p0 = .pssden(object, q0)
        if (p0 == p[i]) {
            q[i] = q0
            next
        }
        if (p0 < p[i]) {
            q.l = q0
            p.l = p0
            while (p0 < p[i]) {
                j = j+1
                q0 = ifelse(is.null(q.wk[j]), mx, q.wk[j])
                p0 = .pssden(object, q0)
            }
            q.u = q0
            p.u = p0
        } else {
            q.u = q0
            p.u = p0
            while (p0 > p[i]) {
                j = j-1
                q0 = ifelse(is.null(q.wk[j]), mn, q.wk[j])
                p0 = .pssden(object, q0)
            }
            q.l = q0
            p.l = p0
        }
        while (abs(p0-p[i]) > 1.0e-10) {
            q0 = q.l+(p[i]-p.l) / (p.u-p.l) * (q.u-q.l)
            p0 = .pssden(object,q0)
            if (p0>p[i]) {
                q.u = q0
                p.u = p0
            } else {
                q.l = q0
                p.l = p0
            }
        }
        q[i] = q.dup = q0
    }
    
    # Return Value:
    q[order.p]
}


################################################################################

