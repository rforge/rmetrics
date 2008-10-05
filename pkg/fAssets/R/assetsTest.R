
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


################################################################################
# FUNCTION:                 ASSETS NORMALITY TESTS:
#  assetsTest                Tests for multivariate Normal Assets
#  mvshapiroTest             Multivariate Shapiro Test
################################################################################


################################################################################
# FUNCTION:                 ASSETS NORMALITY TESTS:
#  assetsTest                Tests for multivariate Normal Assets
#   method = "shapiro"       ... calling Shapiro test
#   method = "energy"        ... calling E-Statistic (energy) test
# FUNCTION:                 INTERNAL UTILITY FUNCTIONS:
#  .mvenergyTest             Multivariate Energy Test
#  .mvshapiroTest            Multivariate Shapiro Test   
# REQUIREMENTS:             DESCRIPTION:  
#  energy                    Contributed R-package "energy"
#  boot                      Contributed R-package "boot"
################################################################################


assetsTest =
function(x, method = c("shapiro", "energy"), Replicates = 100, 
title = NULL, description = NULL)
{
    # Description:
    #   Tests for multivariate Normal Assets
    
    # Example:
    #   .mvnormTest(x = assetsSim(100))
    #   .mvnormTest(x = assetsSim(100), method = "e", Replicates = 99)
    
    # FUNCTION:
    
    # Test:
    method = match.arg(method)
    if (method == "shapiro") {
        test = .mvshapiroTest(x)
    } 
    if (method == "energy") {
        test = .mvenergyTest(x, Replicates = Replicates)
    }
    
    # Return Value:
    test    
}


################################################################################


.mvenergyTest =
function(x, Replicates = 99, title = NULL, description = NULL)
{
    # Description:
    #   Computes E-statistics test for multivariate variables
    
    # Note:
    #   Reimplemented function, doesn't require the contributed 
    #   R package energy, we only use the C Program here.
    
    # Source:
    #   Maria L. Rizzo <mrizzo @ bgnet.bgsu.edu> and 
    #   Gabor J. Szekely <gabors @ bgnet.bgsu.edu>
    #   License: GPL 2.0 or later
    
    # Example:
    #   .mvenergyTest(x = assetsSim(100), 99)
    
    # FUNCTION:
    
    # Transform:
    if (class(x) == "timeSeries") x = series(x)
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

  
################################################################################
# Package: mvnormtest
# Version: 0.1-6
# Date: 2005-04-02
# Title: Normality test for multivariate variables
# Author: Slawomir Jarek
# Maintainer: Slawomir Jarek <slawomir.jarek@gallus.edu.pl>
# Description: Generalization of shapiro-wilk test for multivariate variables.

# Depends: stats


.mvshapiroTest = 
function(x, title = NULL, description = NULL)
{   
    # Description:
    #   Computes Shapiro's normality test for multivariate variables
    
    # Note:
    #   Reimplemented function, doesn't require the contributed R package 
    #   mvnormtest
    
    # Author: 
    #   Slawomir Jarek
    #   License: GPL
    
    # Example:
    #   .mvshapiroTest(x = assetsSim(100))
    
    # FUNCTION:
    
    # Transform:
    U = t(as.matrix(x))

    # Test:
    n = ncol(U)
    if (n < 3 || n > 5000) stop("sample size must be between 3 and 5000")
    rng = range(U)
    rng = rng[2] - rng[1]
    if (rng == 0)
    stop("all `U[]' are identical")
    Us = apply(U, 1, mean)
    R = U-Us
    M.1 = solve(R %*% t(R), tol = 1e-18)
    Rmax = diag(t(R) %*% M.1 %*% R)
    C = M.1 %*% R[, which.max(Rmax)]
    Z = t(C) %*% U
    test = shapiro.test(Z)
    names(test$p.value) = ""
    class(test) = "list"
    
    # Add title and description:
    if (is.null(title)) title = "Multivariate Shapiro Test"
    if (is.null(description)) description = .description()
    
    # Return Value:
    new("fHTEST", 
        call = match.call(), 
        data = list(x = x), 
        test = test, 
        title = title, 
        description = description)
}


################################################################################



