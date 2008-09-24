
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 ASSETS NORMALITY TESTS:
#  .mvenergyTest             Multivariate Energy Test
# REQUIREMENTS:             DESCRIPTION:
#  energy                    Contributed R-package "energy"
#  boot                      Contributed R-package "boot"
################################################################################


.mvenergyTest <- 
    function(x, Replicates = 99, title = NULL, description = NULL)
{
    # Description:
    #   Computes E-statistics test for multivariate variables

    # Arguments:
    #   x - a timeSeries object or any other rectangular object which
    #       can be tranformed by the function as.matrix into a numeric
    #       matrix
    #   Replicates - an integer value, the number of replicates to be
    #       bootstrapped
    
    # Note:
    #   Reimplemented function, doesn't require the contributed
    #   R package energy, we only use the C Program here.

    # Source:
    #   Maria L. Rizzo <mrizzo @ bgnet.bgsu.edu> and
    #   Gabor J. Szekely <gabors @ bgnet.bgsu.edu>
    #   License: GPL 2.0 or later

    # Example:
    #   mvenergyTest(x = assetsSim(100))

    # FUNCTION:

    # Transform:
    x = as.matrix(x)

    # Test:
    R = Replicates

    # RVs:
    n <- nrow(x)
    d <- ncol(x)
    ran.gen = function(x, y) return(matrix(rnorm(n * d), nrow = n, ncol = d))

    # Parametric Mini Boot:
    strata = rep(1, n)
    n <- nrow(x)
    temp.str <- strata
    strata <- tapply(1:n, as.numeric(strata))
    t0 <- .mvnorm.e(x)
    lt0 <- length(t0)
    t.star <- matrix(NA, sum(R), lt0)
    pred.i <- NULL
    for(r in 1:R) t.star[r, ] <- .mvnorm.e(ran.gen(x, NULL))

    # Result:
    test <- list(
        statistic = c("E-Statistic" = t0),
        p.value = 1 - mean(t.star < t0),
        method = "Energy Test",
        data.name = paste("x, obs ", n, ", dim ", d, ", reps ", R, sep = ""))
    names(test$p.value) = ""
    class(test) = "list"

    # Add:
    if (is.null(title)) title = test$method
    if (is.null(description)) description = .description()

    # Return Value:
    new("fHTEST",
        call = match.call(),
        data = list(x = x),
        test = test,
        title = title,
        description = description)
}


# ------------------------------------------------------------------------------


.mvnorm.e <-
function(x)
{
    z <- scale(x, scale = FALSE)
    ev <- eigen(var(x), symmetric = TRUE)
    P <- ev$vectors
    y <- z %*% (P %*% diag(1 / sqrt(ev$values)) %*% t(P))
    e <- .C("mvnEstat", y = as.double(t(y)), byrow = as.integer(TRUE),
        nobs = as.integer(nrow(x)), dim = as.integer(ncol(x)),
        stat = as.double(0), PACKAGE = "fAssets2")$stat

    # Return Value:
    e
}


################################################################################

