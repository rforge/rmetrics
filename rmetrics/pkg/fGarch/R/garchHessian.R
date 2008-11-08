
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
#  .garchRoptimhess        Uses R internal optimhess function
#  .garchRCDAHessian       Computes R coded CDA Hessian matrix
################################################################################

.garchRoptimhess <-
    function(par, .params, .series, eps = 1.0e-4)
{
    .StartHessian <- Sys.time()

    con <- list(trace = 0, fnscale = 1, parscale = rep.int(1,
        length(par)), ndeps = rep.int(0.001, length(par)), maxit = 100L,
        abstol = -Inf, reltol = sqrt(.Machine$double.eps), alpha = 1,
        beta = 0.5, gamma = 2, REPORT = 10, type = 1, lmm = 5,
        factr = 1e+07, pgtol = 0, tmax = 10, temp = 10)

    H <- .Internal(optimhess(par, .garchLLH, NULL, con))
    H <- 0.5 * (H + t(H))
    nm <- names(par)
    dimnames(H) <- list(nm, nm)

    time = Sys.time() - .StartHessian
    attr(H, "time") = time

    H
}

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

################################################################################

