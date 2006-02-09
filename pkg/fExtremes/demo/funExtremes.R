
#
# fExtremes Functions Addon:
#
#   1 Moments for the GEV and GPD distributions
#   2 Law of Large Numbers and Law of the iterated logarithm
#
# Author:
#   Diethelm Wuertz, GPL
#


################################################################################
# 1 Moments for the GEV and GPD distributions


################################################################################
# FUNCTION           DESCRIPTION:
#  gevMoments         Computes true statistics for GEV distribution
#  gpdMoments         Computes true statistics for GPD distribution
################################################################################


gevMoments = 
function(xi, mu = 0, beta = 1)
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Compute true statistics for Generalized Extreme Value distribution
    
    # Value:
    #   Returns true mean for xi < 1 and variance for xi < 1/2
    #   of GEV distribution, otherwise NaN is returned

    # FUNCTION: 
    
    # MEAN: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    # For xi = the result is eulers constant
    euler = 0.57721566490153286060651209008240243104    
    xi0 = c(0, mu+beta*euler, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn = -1)
    gevMean = mu + beta * xinv * (gamma(1-xi)-1) * g[sign(xi-1)+2] +
        xi0[(sign(xi)+2)]
    options(warn = 0)
    
    # VAR: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    xi0 = c(0, (beta*pi)^2 / 6, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn=-1)
    gevVar = (beta*xinv)^2 * (gamma(1-2*xi) - gamma(1-xi)^2 ) * 
        g[sign(2*xi-1)+2] + xi0[(sign(xi)+2)]
    options(warn = 0)     

    # Return Value:
    list(mean = gevMean, var = gevVar)      
}


# ------------------------------------------------------------------------------


gpdMoments = 
function(xi, mu = 0, beta = 1)
{   # A function implemented by Diethelm Wuertz
 
    # Description:
    #   Compute true statistics for Generalized Pareto distribution
    
    # Value:
    #   Returns true mean of Generalized Pareto distribution 
    #   for xi < 1 else NaN
    #   Returns true variance of Generalized Pareto distribution 
    #   for xi < 1 else NaN

    # FUNCTION: 
    
    # MEAN: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdMean = beta/(1-xi)*a[sign(xi-1)+2]
    
    # VAR: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdVar = beta*beta/(1-xi)^2/(1-2*xi) * a[sign(2*xi-1)+2]

    # Return Value:
    list(mean = gevMean, var = gevVar)              
}


################################################################################
# 2 Law of Large Numbers and Law of the iterated logarithm


################################################################################
# FUNCTION:         DESCRIPTION:
#  sllnPlot          Verify Kolmogorov's Strong Law of large Numbers
#  lilPlot           Verify Hartman-Wintner's Law of the iterated logarithm
################################################################################


sllnPlot =  
function (x, mean = NULL, main = "SLLN", ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Verify Kolmogorov's Strong Law of large Numbers
    
    # Arguments:
    #   x - sequence of iid non-degenerate rvs.
    
    # References:
    #   Embrechts et al. p. 61, Theorem 2.1.3
    
    # FUNCTION:
    
    # Verify SLLN:
    if (is.null(mean)) mean=mean(cumsum(x)/(1:length(x)))
    nx  =  length(x)
    plot(cumsum(x)/(1:nx), xlab = "n", ylab = "x", type = "l", main = main, ...)
    lines(c(0, nx), c(mu, mu), col = 2)
    y  =  cumsum(x)/(1:nx)
    
    # Return Value:
    invisible(y)
}


# ------------------------------------------------------------------------------


lilPlot =  
function (x, mean = NULL, sd = NULL, main = "LIL", ...)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Verify Hartman-Wintner's Law of the iterated logarithm
            
    # Arguments:
    #   x - sequence of iid non-degenerate rvs.

    # References:
    #   Embrechts et al. p. 67. Theorem 2.1.13
    
    # FUNCTION:
    
    # Verify LIL:
    lx  =  length(x)
    nx  =  1:lx
    fact  =  sqrt(2*nx*log(log(nx)))
    if (is.null(mean)) mean  =  mean(cumsum(x))
    if (is.null(sd)) sd  =  sqrt(var(x))
    y  =  (cumsum(x)-mean*nx)/fact/sd
    plot(x = nx, y = y, xlab = "n", ylab = "x", 
        ylim = range(y[!is.na(y)], -1, 1), type = "l", main = main, ...)
    lines(c(0,lx), c(1,1), col=2)
    lines(c(0,lx), c(-1,-1), col=2)
    
    # Return Value:
    y
}


# ------------------------------------------------------------------------------



