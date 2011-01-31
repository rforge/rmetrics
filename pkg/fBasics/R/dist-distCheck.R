
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  .distCheck                Checks consistency of distributions
################################################################################


distCheck <-
function(fun = "norm", n = 1000, robust = TRUE, subdivisions = 1000,
         tol = 1e-4, doMoments = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks consistency of distributions

    # Arguments:
    #   fun - a character string denoting the name of the distribution
    #   n - an integer specifying the number of random variates to be
    #       generated
    #   robust -  a logical flag, should robust estimates be used? By
    #       default \code{TRUE}
    #   subdivisions - an integer specifying the numbers of subdivisions
    #       in integration
    #   ... - the distributional parameters

    # Examples:
    #   .distCheck("norm", mean = 1, sd = 1)
    #   .distCheck("t", df = 4)
    #   .distCheck("exp", rate = 2)
    #   .distCheck("weibull", shape = 1)

    # FUNCTION:

    # Distribution Functions:
    cat("\nDistribution Check for:", fun, "\n ")
    CALL = match.call()
    cat("Call: ")
    cat(paste(deparse(CALL), sep = "\n", collapse = "\n"), "\n", sep = "")
    dfun = match.fun(paste("d", fun, sep = ""))
    pfun = match.fun(paste("p", fun, sep = ""))
    qfun = match.fun(paste("q", fun, sep = ""))
    rfun = match.fun(paste("r", fun, sep = ""))

    # Range:
    xmin = qfun(p = 0.01, ...)
    xmax = qfun(p = 0.99, ...)

    nIntegrate <- function(f, ...)
        integrate(f, lower = -Inf, upper = Inf,
                  subdivisions = subdivisions, stop.on.error = FALSE,
                  rel.tol = tol, ...)

    # Check 1 - Normalization:
    NORM <- nIntegrate(dfun, ...)
    cat("\n1. Normalization Check:\n NORM ")
    print(NORM)
    normCheck = (abs(NORM[[1]]-1) < 0.01)

    # Check 2:
    cat("\n2. [p-pfun(qfun(p))]^2 Check:\n ")
    p <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    P <- pfun(qfun(p, ...), ...)
    print(round(rbind(p, P), 3))
    RMSE = sd(p-P)
    print(c(RMSE = RMSE))
    rmseCheck = (abs(RMSE) < 0.0001)

    # Check 3:
    cat("\n3. r(", n, ") Check:\n", sep = "")
    r = rfun(n = n, ...)
    if (!robust) {
        SAMPLE.MEAN = mean(r)
        SAMPLE.VAR = var(r)
    } else {
        cMcd = MASS::cov.mcd(r, quantile.used = floor(0.95*n))
        SAMPLE.MEAN = cMcd$center
        SAMPLE.VAR = cMcd$cov[1,1]
    }
    print(signif(rbind(SAMPLE =
                       c(MEAN = SAMPLE.MEAN, VAR = SAMPLE.VAR)), 3))
    if(doMoments) {
        fun1 = function(x, ...) { x * dfun(x, ...) }
        fun2 = function(x, ...) { x^2 * dfun(x, ...) }
        MEAN <- nIntegrate(fun1, ...); cat("   X   "); print(MEAN)
        X2   <- nIntegrate(fun2, ...); cat("   X^2 "); print(X2)
        Var <- X2[[1]] - (MU <- MEAN[[1]])^2
        print(signif(rbind(EXACT = c(MEAN = MU, VAR = Var)),  3))
        meanvarCheck = (abs(SAMPLE.VAR - Var)/Var < 0.10)
        cat("\n")
    } else meanvarCheck <- NA

    # Done:
    c(normCheck = normCheck,
      rmseCheck = rmseCheck,
      meanvarCheck = meanvarCheck)
}


# ------------------------------------------------------------------------------


.distCheck <- distCheck

    # Keep for older Rmetrics Versions



################################################################################

