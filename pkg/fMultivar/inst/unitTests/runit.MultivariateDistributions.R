
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
# FUNCTION:           DESCRIPTION:
#  dmvsnorm            Multivariate Skew Normal Density Function
#  pmvsnorm            Multivariate Skew Normal Probability Function
#  rmvsnorm            Multivariate Skew Normal Random Deviates
# FUNCTION:           DESCRIPTION:
#  dmvst               Multivariate Skew Sudent-t Density Function
#  pmvst               Multivariate Skew Sudent-t Probability Function
#  rmvst               Multivariate Skew Sudent-t Random Deviates
# FUNCTION:           DESCRIPTION:
#  fMV                 S4 Object of class 'fMV'
#  mvFit               Fits a MV Normal or Student-t Distribution
#  print.fMV           S3: Print method for objects of class 'fMV'
#  plot.fMV            S3: Plot method for objects of class 'fMV'
#  summary.fMV         S3: Summary method for objects of class 'fMV'
################################################################################


test.dmvsnorm =
function()
{
    # Multivariate Skew Normal
    
    # Bivariate Density:
    x = y = seq(-4, 4, length = 81)
    G = grid2d(x)
    X = cbind(G$x, G$y)
    z = dmvsnorm(X, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = rep(0, 2))
    Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
    
    # Plot:
    par(mfrow = c(1, 1), ask = FALSE)
    persp(Z, theta = -40, phi = 30, col = "steelblue")
       
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.pmvsnorm =
function()
{
    # Multivariate Skew Normal
    
    # Bivariate Density:
    x = y = seq(-4, 4, length = 21)
    G = grid2d(x)
    X = cbind(G$x, G$y)
    z = pmvsnorm(X, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = rep(0, 2))
    Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
    
    # Plot:
    par(mfrow = c(1, 1), ask = FALSE)
    persp(Z, theta = -40, phi = 30, col = "steelblue")
       
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.rmvsnorm =
function()
{
    # Multivariate Skew Normal
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    N = 5000
    z = rmvsnorm(N, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = rep(1, 2))
    
    # Scatterplot:
    par(mfrow = c(1, 1), ask = FALSE)
    plot(z, pch = 19, col = "steelblue")
    grid()
       
    # Return Value:
    return()    
}


################################################################################


test.dmvst =
function()
{
    # Multivariate Skew Sudent-t
    args(dmvst)
    # dmvst(x, dim = 2, mu = rep(0, dim), Omega = diag(dim), 
    #   alpha = rep(0, dim), df = 4) 

    # Bivariate Density:
    x = y = seq(-4, 4, length = 81)
    G = grid2d(x)
    X = cbind(G$x, G$y)
    z = dmvst(X, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = c(-1, 1))
    Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
    
    # Plot:
    par(mfrow = c(1, 1), ask = FALSE)
    persp(Z, theta = -40, phi = 30, col = "steelblue")
       
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.pmvst =
function()
{
    # Multivariate Skew Sudent-t
    
    # Bivariate Density:
    x = y = seq(-4, 4, length = 21)
    G = grid2d(x)
    X = cbind(G$x, G$y)
    z = pmvst(X, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = c(-1, 1))
    Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
    
    # Plot:
    par(mfrow = c(1, 1), ask = FALSE)
    persp(Z, theta = -40, phi = 30, col = "steelblue")
    .perspPlot(Z)
       
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.rmvst =
function()
{
    # Multivariate Skew Sudent-t
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    N = 5000
    z = rmvsnorm(N, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = c(-1, 1))
    
    # Scatterplot:
    par(mfrow = c(1, 1), ask = FALSE)
    plot(z, pch = 19, col = "steelblue")
    grid()
       
    # Return Value:
    return()    
}


################################################################################


#  fMV                 S4 Object of class 'fMV'


################################################################################


test.mvFit.mvsnorm =
function()
{
    # mvFit - Fits a MV Normal or Student-t Distribution
    # mvFit(x, method = c("snorm", "st"), fixed.df = NA, title = NULL, 
    #   description = NULL, trace = FALSE, ...) 

    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    N = 5000
    z = rmvsnorm(N, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = c(-1, 1))
    
    # Fit:
    fit = mvFit(x = z, method = "snorm")
    
    # Print:
    print(fit)
    
    # Interactive Plot:
    # plot(fit, which = "ask")
    
    # Scatterplot:
    par(mfrow = c(1, 1))
    plot(fit, which = 1)
    
    # Normal QQ Plot of Mahalanobis Distances:
    par(mfrow = c(2, 2))
    plot(fit, which = 2)
    
    # Skew Normal QQ Plot of Mahalanobis Distances:
    plot(fit, which = 3)
    
    # Normal PP Plot of Mahalanobis Distances:
    plot(fit, which = 4)
    
    # Skew Normal PP Plot of Mahalanobis Distances:
    plot(fit, which = 5)
       
    # Summary:
    summary(fit, doplot = FALSE)

    # Return Value:
    return()    
}


################################################################################


test.mvFit.mvst =
function()
{
    # mvFit - Fits a MV Normal or Student-t Distribution
    # mvFit(x, method = c("snorm", "st"), fixed.df = NA, title = NULL, 
    #   description = NULL, trace = FALSE, ...) 

    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    N = 1000
    z = rmvst(N, dim = 2, mu = rep(0, 2), Omega = diag(2), alpha = c(-1, 1), 
        df = 4)
    
    # Fit:
    # fit = mvFit(x = z, method = "st", trace = TRUE)       
    fit = mvFit(x = z, method = "st")                    

    # Print:
    print(fit)
    
    # Interactive Plot:
    # plot(fit, which = "ask")
    
    # Scatterplot:
    par(mfrow = c(1, 1))
    plot(fit, which = 1)
    
    # Normal QQ Plot of Mahalanobis Distances:
    par(mfrow = c(2, 2))
    plot(fit, which = 2)
    
    # Skew Normal QQ Plot of Mahalanobis Distances:
    plot(fit, which = 3)
    
    # Normal PP Plot of Mahalanobis Distances:
    plot(fit, which = 4)
    
    # Skew Normal PP Plot of Mahalanobis Distances:
    plot(fit, which = 5)
       
    # Summary:
    summary(fit, doplot = FALSE)

    # Return Value:
    return()    
}


################################################################################
    
