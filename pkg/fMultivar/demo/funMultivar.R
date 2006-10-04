
#
# Example:
#   fSeries Functions Addons
#
# Description:
#   This is a collection of frunctions which support the following
#   tasks:
#    3 Absolute Moment Statistics
#    6 OLS Functions
#    8 Time Series Filter
#   12 Time series disaggregation from low to high frequency
#
# Author:
#   (C) 2002, Diethelm Wuertz, GPL
#



################################################################################
# 3 ABSOLUTE MOMENT STATISTICS


################################################################################
# FUNCTION:       DESCRIPTION:
#  absMoments      Absolute moments of a standardized symmetric distribution
################################################################################


absMoments =
function(n, density, ...)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the absolute moments of a standardized
    #   symmetric distribution function.
    
    # Arguments:
    #   n - a vector of integers i, to compute M_i
    #   density  - a character string denoting the density
    #       "norm", "ged", "std" or any other
    #   ... - optional parameters to be passed 
    
    # Value:
    #   Returns a numeric vector of moments M_i.
    #   Stores globally errors in the variable absMoment.error
    #     if the moments were comuted numerically.
    
    # FUNCTION:
              
    # norm - Normal Distribution:
    if (density == "dnorm") {
        return (sqrt(2)^n * gamma((n+1)/2) / sqrt(pi)) }

    # ged - Generalized Error Distribution:
    if (density == "dged") {
        parm = function(n, nu) {
            lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
            mu2r = (2^(1/nu)*lambda)^n * gamma((n+1)/nu) / gamma(1/nu)
            return(mu2r) }
        return(parm(n, ...)) }

    # std - Student-t Distribution:
    # Note: nu > 2*n
    if (density == "dstd") {
        parm = function(n, nu) {
            r = n/2
            mu2r = ((nu-2)^r/2) * beta((r+1)/2, (nu-r)/2) / beta(1/2, nu/2)
            return(mu2r) }
        return(parm(n, ...)) }

    # Any other standardized symmetric Distribution ...
    fun = match.fun(density)
    moments = function(x, n, ...) { 2 * x^n * fun(x, ...) }
    M = absMoments.error <<- NULL
    for (i in n) {
        I = integrate(moments, 0, Inf, n=i, ...)
        M = c(M, I$value)
        absMoments.error <<- c(absMoments.error, I$abs.error) }
        return(M)

    # Return Value:
    invisible()
}


################################################################################
# 6 OLS FUNCTIONS


################################################################################
# FUNCTION:       DESCRIPTION:
#  OLS            Fit an OLS regression model
#   print.OLS      S3 Print method for an OLS regression model
#   plot.OLS       S3 Plot method for an OLS regression model
#   summary.OLS    S3 Summary method for an OLS regression model
################################################################################


OLS = 
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   OLS Estimator
    
    # FUNCTION:
    
    # Estimate:
    fit = lm(formula = formula, data = data, ...)
    fit$call = match.call()
    fit$formula = formula
    fit$data<- data
    
    # Return Value:
    class(fit) = "OLS"
    fit 
}

# ------------------------------------------------------------------------------


print.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method
    
    # FUNCTION:
    
    # Print:
    class(object) = "lm"
    print.lm(object, ...) 
}
    

# ------------------------------------------------------------------------------


plot.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Plot Method
    
    # FUNCTION:
    
    # Plot:
    class(object) = "lm"
    plot.lm(object, ...)
}


# ------------------------------------------------------------------------------

    
summary.OLS = function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method
    
    # FUNCTION:
    
    # Summary:
    class(object) = "lm"
    summary.lm(object, ...) 
}


################################################################################
# 8 TIME SERIES FILTER


################################################################################
# FUNCTION:       DESCRIPTION:
#  hpFilter        Decompose a series using the Hodrick-Prescott filter
################################################################################


hpFilter = 
function(y, lambda = 1600) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the Hodrick-Prescott decomposition of a macroeconomic time 
    #   series into a smooth trend component and a cyclical component. 
    
    # Arguments:
    #   x - a vector, or a "timeSeries" object of the original data. 
    #       Usually this is represented on a logarithmic scale.
    #   lambda - a positive number representing the smoothness 
    #       parameter. 
    
    # Details:
    #   A larger number for the smoothness parameter "lambda" results 
    #   in more smoothing. A rule of thumb is to use 100 for annual, 
    #   1600 for quarterly, and 14400 for monthly data. However, Ravn  
    #   and Uhlig (2002) recently proposed a different smoothing 
    #   parameter of 6.25 for yearly data.
    #   This function is implemented along the recommendations given
    #   in Posch [2002].
        
    # Value: 
    #   a vector, or a "timeSeries" with the same length as x, representing 
    #   the smooth trend component. The cyclical (business cycle) component 
    #   is the difference between the original data and the trend component. 

    # References:
    #   Hodrick R.J., Prescott E.C. (1997); 
    #       Postwar U.S. business cycles: an empirical investigation. 
    #       Journal of Money, Credit and Banking, 29:1--16. 
    #   Kydland F.E., Prescott, E.C. (1990); 
    #       Business cycles: real facts and a monetary myth. 
    #       Federal Reserve Bank of Minneapolis Quarterly Review, 
    #       Spring(1990):3--18.
    #   Posch O. (2002);
    #       The HP Filter and its R implementation
    #       University of Dresden, October 02, 2002.
    #   Ravn M.O., Uhlig, H. (2002);
    #       On Adjusting the Hodrick-Prescott Filter
    #       for the Frequency of Observations,
    #       Review of Economics and Statistics, 84, 371--76.
    
    # FUNCTION:
    
    # Number of observations:
    n = length(y) 
    # Creates an identity matrix:
    I = diag(n) 
    # Second order differences:
    D = diff(I, lag=1, d=2) 
    # Solve focs:
    result = solve(I + lambda * crossprod(D) , y) 
    
    # Return Value:
    result 
}


# PASTECS FILTER:
#  decaverage
#  deccensus
#  decdiff
#  decevf
#  decloess
#  decmedian
#  decreg


################################################################################
# 12 DISSAGGREGATE


################################################################################
# FUNCTION:      DESCRIPTION:
#  disaggregate   Disaggregates time series from low to high frequency
################################################################################


disaggregate = 
function(data, k, 
method = c("linear", "constant", "fmm", "spline", "natural", "periodic"), 
how = NA, x = NA, out.positions = NA, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time series disaggregation/distribution from low frequency
    #   to high frequency.
    
    # Arguments:
    #   data - a vector, or a matrix, or a "timeSeries" object 
    #       that represents the low frequency time series to 
    #       be disaggregated. 
    #   k - a positive integer specifying the number of time 
    #       periods to distribute data into. For example, to 
    #       disaggregate an annual series into a quarterly 
    #       series, you set k to 4. 

    # Value:
    #   Returns a vector, or a matrix, or a "timeSeries" object that 
    #   represents the disaggregated high frequency time series. 

    # FUNCTION:
    
    # Method:
    method = method[1]
    
    # Data
    x = 1:length(data)
    y = as.vector(data)
    xout = seq(1, length(data), length=k*length(data))
    
    # "linear" / "constant" Interpolation:  
    if (method == "linear" || method == "constant") 
        ans = approx(x = x, y = y, xout, method = "linear")
    
    # "fmm" / "natural" / "periodic" Spline interpolation:
    if (method == "spline") method = "fmm"
    if (method == "fmm" | method == "natural" | method == "periodic") 
        ans = spline(x = x, y = y, n = k*length(x), method = "fmm",
            xmin = min(x), xmax = max(x))   
    
    # Return Value:
    ans
}


################################################################################

