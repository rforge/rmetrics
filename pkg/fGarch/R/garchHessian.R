
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:               PARAMETER ESTIMATION:
#  .garchRCDAHessian       Computes R coded CDA Hessian matrix
#  .garchFCDAHessian       Computes Fortran coded CDA Hessian matrix
#  .garchFFDAHessian       Computes Fortran coded FDA Hessian matrix
################################################################################


.garchRCDAHessian <-
    function(par, .params, .series, eps = 1.0e-4)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute R coded CDA (central difference approximated) Hessian

    # Reference:
    #   http://status.sph.umich.edu/computing/manuals/sas8/stat/chap46/sect26.htm

    # FUNCTION:

    # Starttime
    .StartHessian <- Sys.time()

    # Algorithm:
    algorithm = .params$control$algorithm[1]
    .trace = FALSE

    # Compute Hessian:
    eps = eps * par
    n = length(par)
    H = matrix(0, ncol = n, nrow = n)
    for (i in 1:n) {
        for (j in 1:n) {
            x1 = x2 = x3 = x4 = par
            x1[i] = x1[i] + eps[i]
            x1[j] = x1[j] + eps[j]
            x2[i] = x2[i] + eps[i]
            x2[j] = x2[j] - eps[j]
            x3[i] = x3[i] - eps[i]
            x3[j] = x3[j] + eps[j]
            x4[i] = x4[i] - eps[i]
            x4[j] = x4[j] - eps[j]
            H[i, j] = (
                .garchLLH(x1, .trace) -
                .garchLLH(x2, .trace) -
                .garchLLH(x3, .trace) +
                .garchLLH(x4, .trace) ) /
                    (4*eps[i]*eps[j])
        }
    }
    colnames(H) = rownames(H) = names(par)
    time = Sys.time() - .StartHessian

    # Attribute Exdecution time
    attr(H, "time") = time

    # Return Value:
    H
}


# ------------------------------------------------------------------------------


.garchFCDAHessian <-
    function(par, .params, .series, eps = 1.0e-4)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute central difference approximated Hessian

    # FUNCTION:

    # Algorithm:
    algorithm = .params$control$algorithm[1]
    .trace = FALSE

    # Compute Hessian:
    N = length(.series$x)
    NF = length(par)
    if(.params$includes["delta"]) {
        XDELTA = par["delta"]
    } else {
        XDELTA = .params$delta
    }
    if(.params$includes["skew"]) {
        XSKEW = par["skew"]
    } else {
        XSKEW = .params$skew
    }
    if(.params$includes["shape"]) {
        XSHAPE = par["shape"]
    } else {
        XSHAPE = .params$shape
    }
    DPARM = c(XDELTA, XSKEW, XSHAPE)
    MDIST = c(norm = 10, snorm = 11, std = 20, sstd = 21, ged = 30,
        sged = 31)[.params$cond.dist]                # Which Distribution
    REC = 1
    if(.series$init.rec == "uev") REC = 2
    MYPAR = c(
        REC   = REC,                                  # How to initialize
        LEV   = as.integer(.params$leverage),         # Include Leverage 0|1
        MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1
        DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1
        SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1
        SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1
        ORDER = .series$order)                        # Order of ARMA-GARCH
    # Compute Hessian:
    .StartHessian <- Sys.time()
    ans = .Fortran("garchhess",
        N = as.integer(N),
        Y = as.double(.series$x),
        Z = as.double(rep(0, times = N)),
        H = as.double(rep(0, times = N)),
        NF = as.integer(NF),
        X = as.double(par),
        DPARM = as.double(DPARM),
        MDIST = as.integer(MDIST),
        MYPAR = as.integer(MYPAR),
        E0 = as.double(eps),
        HESS = as.double(rep(0, times = NF*NF)),
        PACKAGE = "fGarch2")
    H = matrix(ans[["HESS"]], ncol = NF)
    colnames(H) = rownames(H) = names(par)
    time = Sys.time() - .StartHessian

    # Attribute Exdecution time
    attr(H, "time") = time

    # Return Value:
    H
}


# ------------------------------------------------------------------------------

.garchFFDAHessian <-
    function(par, .params, .series, eps = 1.0e-4)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute forward difference approximated Hessian

    # Note
    #   scaling has not yet been implemented ...

    # FUNCTION:

    # Algorithm:
    algorithm = .params$control$algorithm[1]
    .trace = FALSE

    # Compute Hessian:
    N = length(.series$x)
    NF = length(par)
    if(.params$includes["delta"]) {
        XDELTA = par["delta"]
    } else {
        XDELTA = .params$delta
    }
    if(.params$includes["skew"]) {
        XSKEW = par["skew"]
    } else {
        XSKEW = .params$skew
    }
    if(.params$includes["shape"]) {
        XSHAPE = par["shape"]
    } else {
        XSHAPE = .params$shape
    }
    DPARM = c(XDELTA, XSKEW, XSHAPE)
    MDIST = c(norm = 10, snorm = 11, std = 20, sstd = 21, ged = 30,
        sged = 31)[.params$cond.dist]                # Which Distribution
    REC = 1
    if(.series$init.rec == "uev") REC = 2
    MYPAR = c(
        REC   = REC,                                  # How to initialize
        LEV   = as.integer(.params$leverage),         # Include Leverage 0|1
        MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1
        DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1
        SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1
        SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1
        ORDER = .series$order)                        # Order of ARMA-GARCH
    # Compute Hessian:
    .StartHessian <- Sys.time()
    ans = .Fortran("gfdhess",
        N = as.integer(N),
        Y = as.double(.series$x),
        Z = as.double(rep(0, times = N)),
        H = as.double(rep(0, times = N)),
        NF = as.integer(NF),
        X = as.double(par),
        DPARM = as.double(DPARM),
        MDIST = as.integer(MDIST),
        MYPAR = as.integer(MYPAR),
        E0 = as.double(eps),
        HESS = as.double(rep(0, times = NF*NF)),
        SCL = as.double(rep(1, times = NF)),
        STPSZ = as.double(rep(0, times = NF)),
        FNBR = as.double(rep(0, times = NF)),
        PACKAGE = "fGarch2")
    H = matrix(ans[["HESS"]], ncol = NF)
    colnames(H) = rownames(H) = names(par)
    time = Sys.time() - .StartHessian

    # Attribute Exdecution time
    attr(H, "time") = time

    # Return Value:
    H
}


################################################################################

