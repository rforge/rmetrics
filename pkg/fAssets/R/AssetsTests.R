
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
# Package: energy
# Title: E-statistics (energy statistics) tests of fit, independence, clustering
# Version: 1.0-6
# Date: 2007-04-02
# Author: Maria L. Rizzo <mrizzo @ bgnet.bgsu.edu> and 
#  Gabor J. Szekely <gabors @ bgnet.bgsu.edu>
# Description: E-statistics (energy) tests for comparing distributions: 
#  multivariate normality, multivariate k-sample test for equal distributions, 
#  hierarchical clustering by e-distances, multivariate independence test, 
#  goodness-of-fit tests. Energy-statistics concept based on a generalization of 
#  Newton's potential energy is due to Gabor J. Szekely.
# Maintainer: Maria Rizzo <mrizzo @ bgnet.bgsu.edu>
# Depends: boot
# License: GPL 2.0 or later
# Packaged: Mon Apr  2 22:30:11 2007; rizzo


.mvenergyTest =
function(x, Replicates = 99, title = NULL, description = NULL)
{
    # Description:
    
    # Note:
    #   Requires Contributed R packags "energy and "boot".
    #   Unfortunately, "boot" conflicts with "robustbase".
    #   What we are doing here is to load the packages not 
    #   at startup, we load it here, and detach it after usage.
    #   It is important to know, that require can only load 
    #   an installed package, and this is detected by having a 
    #   ‘DESCRIPTION’ file. ".require" is used here that it
    #   is not detected  by R CMD. - Can we do this better ? 
    
    # Example:
    #   .mvenergyTest(x = assetsSim(100), 99)
    
    # FUNCTION:
    
    # Transform:
    if (class(x) == "timeSeries") x = seriesData(x)
    x = as.matrix(x)
    
    # Test:  
    mvnorm.etest = function(x = x, R = Replicates) { NA }
    .require = require
    .require("energy")
    rm(mvnorm.etest)
    test = mvnorm.etest(x = x, R = Replicates)
    detach("package:energy")
    detach("package:boot")
    names(test$p.value) = ""
    class(test) = "list"
    
    # Add:
    if (is.null(title)) title = test$method
    if (is.null(description)) description = date()
    
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
# License: GPL
# Depends: stats


.mvshapiroTest = 
function(x, title = NULL, description = NULL)
{   
    # Description:
    #   Computes Shapiro's normality test for multivariate variables
    
    # Note:
    #   Builtin Function, doesn't require the contributed R package 
    #   mvnormtest
    
    # Author: 
    #   Slawomir Jarek

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
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fHTEST", 
        call = match.call(), 
        data = list(x = x), 
        test = test, 
        title = title, 
        description = description)
}


################################################################################

