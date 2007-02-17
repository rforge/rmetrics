
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
# FUNCTION:             ASSETS NORMALITY TESTS:
#  assetsTest            Tesst for multivariate Normal Assets
#   method = "shapiro"    ... calling Shapiro test
#   method = "energy"     ... calling E-Statistic (energy) test
#  .mvenergyTest         Multivariate Energy Test
#   .mvnorm.etest         Internal Function used by assetsTest
#   .mvnorm.e             Internal Function used by assetsTest
#   .normal.e             Internal Function used by assetsTest
#   .mvnormBoot           Internal Function used by assetsTest
#  .mvshapiroTest        Multivariate Shapiro Test     
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
    method = method[1]
    if (method == "shapiro" | method == "s") {
        test = .mvshapiroTest(x)
    } else if (method == "energy" | method == "e") {
        test = .mvenergyTest(x, Replicates = Replicates)
    } else {
        stop("No valid method specified")
    }
    
    # Return Value:
    test    
}


# ------------------------------------------------------------------------------


.mvenergyTest =
function(x, Replicates = 99, title = NULL, description = NULL)
{
    # Description:
    
    # Example:
    #   .mvenergyTest(x = assetsSim(100), 99)
    
    # FUNCTION:
    
    # Transform:
    if (class(x) == "timeSeries") x = seriesData(x)
    x = as.matrix(x)
    
    # Test:
    test = .mvnorm.etest(x = x, R = Replicates)
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
# Package: energy
# Title: E-statistics (energy statistics) tests of fit, independence, 
#   clustering
# Version: 1.0-3
# Date: 2005-10-02
# Author: Maria L. Rizzo <rizzo@math.ohiou.edu> and 
#   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
# Description: E-statistics (energy) tests for comparing distributions: 
#   multivariate normality, multivariate k-sample test for equal 
#   distributions, hierarchical clustering by e-distances, multivariate 
#   independence test, Poisson test. Energy-statistics concept based on 
#   a generalization of Newton's potential energy is due to Gabor J. Szekely.
# Maintainer: Maria Rizzo <rizzo@math.ohiou.edu>
# License: GPL 2.0 or later
# Packaged: Sun Oct  2 16:55:19 2005; rizzo


.mvnorm.etest = 
function(x, R) 
{
    # Description:
    #   Parametric bootstrap E-test for multivariate normality
    
    # Author: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>

    # FUNCTION:
    
    # Test:
    if (is.vector(x)) {
        n = length(x)
        d = 1
        bootobj = .mvnormBoot(x, statistic = .normal.e, R = R,  
            ran.gen = function(x, y) {return(rnorm(n)) })
    } else {
        n = nrow(x)
        d = ncol(x)
        bootobj = .mvnormBoot(x, statistic = .mvnorm.e, R = R,  
            ran.gen = function(x, y) { return(matrix(rnorm(n * d), 
            nrow = n, ncol = d)) })
    }
    p = 1 - mean(bootobj$t < bootobj$t0)
    names(bootobj$t0) = "E-statistic"
    e = list(statistic = bootobj$t0,
        p.value = p,
        method = "Energy Test of Multivariate Normality",
        data.name = paste("x, sample size ", n, ", dimension ", 
            d, ", replicates ", R, sep = ""))
    class(e) = "htest"    
    
    # Return Value:    
    e                 
}


# ------------------------------------------------------------------------------


.mvnorm.e = 
function(x) 
{
    # Description:
    #   E-statistic for multivariate normality
    
    # Author: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
    
    # FUNCTION:
    
    # Statistic:
    if (is.vector(x)) return(normal.e(x))
    n = nrow(x)
    d = ncol(x)
    if (n < 2) return(normal.e(x))
    # subtract column means and
    z = scale(x, scale = FALSE) 
    # compute S^(-1/2)    
    ev = eigen(var(x), symmetric = TRUE)    
    P = ev$vectors
    lambda = ev$values    
    y = z %*% (P %*% diag(1 / sqrt(lambda)) %*% t(P))
    if (any(!is.finite(y))) return (NA)
    stat = 0
    e = .C("mvnEstat", y = as.double(t(y)), byrow = as.integer(TRUE),
        nobs = as.integer(n), dim = as.integer(d), 
        stat = as.double(stat), PACKAGE = "fPortfolio")$stat
            
    # Return Value:
    e
}


# ------------------------------------------------------------------------------


.normal.e = 
function(x) 
{
    # Description:
    #   Statistic for univariate Normality
    
    # Authors: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
    
    # FUNCTION:
    
    # Statistic:
    x = as.vector(x)
    y = sort(x)
    n = length(y)
    if (y[1] == y[n]) return (NA)
    y = scale(y) 
    K = seq(1 - n, n - 1, 2)
    e = 2 * (sum(2 * y * pnorm(y) + 2 * dnorm(y)) - n / sqrt(pi) - mean(K * y))
   
    # Return Value:
    e
}


################################################################################
# Package: boot
# Priority: recommended
# Version: 1.2-24
# Date: 2005-12-09
# Author: S original <http://statwww.epfl.ch/davison/BMA/library.html>
#   by Angelo Canty <cantya@mcmaster.ca>.  
#   R port by  Brian Ripley <ripley@stats.ox.ac.uk>.
# Maintainer: Brian Ripley <ripley@stats.ox.ac.uk>
# Description: functions and datasets for bootstrapping from the
#   book "Bootstrap Methods and Their Applications" by A. C. Davison and 
#   D. V. Hinkley (1997, CUP).
# Title: Bootstrap R (S-Plus) Functions (Canty)
# Depends: R (>= 2.0.0), graphics, stats
# Suggests: survival
# LazyData: yes
# License: Unlimited distribution.
# Packaged: Thu Dec  8 21:19:17 2005; ripley


.mvnormBoot = 
function(data, statistic, R, strata = rep(1, n), L = NULL, m = 0, 
weights = NULL, ran.gen=function(d, p) d, mle = NULL, ...)
{    
    # Author: 
    #   S original <http://statwww.epfl.ch/davison/BMA/library.html>
    #   by Angelo Canty <cantya@mcmaster.ca>  
    #   R port by  Brian Ripley <ripley@stats.ox.ac.uk>

    # R replicates of bootstrap applied to  statistic(data)
    # Various auxilliary functions find the indices to be used for the
    # bootstrap replicates and then this function loops over those replicates.
     
    isBootMatrix = function(x) {length(dim(x)) == 2}
    call = match.call()
    if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
    seed = get(".Random.seed", envir=.GlobalEnv, inherits = FALSE)
    if (isBootMatrix(data)) n = nrow(data) else n = length(data)
    temp.str = strata
    strata = tapply(1:n,as.numeric(strata))
    if ((n == 0) || is.null(n)) stop("no data in call to boot")  
    t0 = statistic(data,...)
    lt0 = length(t0)
    t.star = matrix(NA, sum(R), lt0)
    pred.i = NULL
    for(r in 1:R) t.star[r,] = statistic(ran.gen(data, mle),...)      
    dimnames(t.star) = NULL
    if (is.null(weights)) weights = 1/tabulate(strata)[strata]
    out = list(t0 = t0, t = t.star, R = R, data = data, seed = seed,
        statistic = statistic, call = call)
     out = c(out, list(ran.gen = ran.gen, mle=mle) )
    class(out) = "boot"
    out
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
    Us = apply(U,1,mean)
    R = U-Us
    M.1 = solve(R%*%t(R),tol=1e-18)
    Rmax = diag(t(R)%*%M.1%*%R)
    C = M.1%*%R[,which.max(Rmax)]
    Z = t(C)%*%U
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

