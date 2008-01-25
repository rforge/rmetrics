
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
# FUNCTION:                 ASSETS STATISTICS:
#  assetsStats               Computes basic statistics of a set of assets  
################################################################################


assetsStats <- 
    function(x)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes benchmark statistics for a data set of assets with
    #   monthly data records. 
    
    # Details:
    #   The computed statistics values are:
    #       records - number of records (length of time series)
    #       paMean - annualized (pa, per annum) Mean of Returns
    #       paAve - annualized Average of Returns
    #       paVola - annualized Volatility (standard Deviation)
    #       paSkew - Skewness of Returns
    #       paKurt - Kurtosis of Returns
    #       maxDD - maximum Drawdown
    #       TUW - Time under Water
    #       mMaxLoss - Monthly maximum Loss
    #       mVaR - Monthly 99% Value-at-Risk 
    #       mModVaR - Monthly 99% Modified Value-at-Risk 
    #       mSharpe - Monthly Sharpe Ratio
    #       mModSharpe - Monthly Modified Sharpe Ratio
    #       skPrice - Skewness/Kurtosis Price
    #   The statistics are implemented based on the formulas from
    #   "Extreme Metrics". They reflect risk measures as used in 
    #   the hedge fund software from "www.AlternativeSoft.com".

    # Arguments:
    #   x - asset data set, a matrix (or vector) where the rows
    #       are numbered by "time", and the columns belong to the
    #       individual assets. Monthly values are expected.
    
    # Value:
    #   The function returns a data frame with the values of the
    #   12 statistics for each asset.
    
    # Reference:
    #   "ExtremeMetrics Software", Help Document, Alternative Software,
    #   March 2003, 4 pages.
    
    # FUNCTION:
    
    # If x is a vector, make it a matrix:
    statistics = 14
    if (is.null(dim(x))) {
        n = 1 
        x = matrix(x, length(x)) 
        result = matrix(rep(0, times = statistics), ncol = 1) }
    else {
        n = dim(x)[2] 
        result = matrix(rep(0, times = statistics*n), ncol = n) }
    
    # Give Names to Result Matrix:  
    stat.names = c(
        "Records",      "paMean",   "paAve",    "paVola",
        "paSkew",       "paKurt",   "maxDD",    "TUW",
        "mMaxLoss",     "mVaR",     "mModVaR",  "mSharpe",
        "mModSharpe",   "skPrice")
    dimnames(result) = list(stat.names, dimnames(x)[[2]])   

    # Loop over all Assets:
    for (i in 1:n) {
        r = x[, i]
        # Number of Records:
        result[1, i] = length(r)
        # Annualized mean from monthly returns:
        result[2, i] = annualizedMean = (1 + mean(r))^12 - 1
        # Annualized mean from monthly returns:
        result[3, i] = annualizedAverage = mean(r)*sqrt(12)
        # Annualized volatility from monthly returns:
        result[4, i] = annualizedVolatility = sqrt(var(r))
        # Annualized skewness from monthly returns:
        result[5, i] = annualizedSkewness = skewness(r) 
        # Annualized Kurtosis from monthly returns:
        result[6, i] = annualizedKurtosis = kurtosis(r) 
        # Maximum Drawdown of of monthly returns:
        result[7, i] = maxDrawdown = max(cummax(cumsum(r)) - cumsum(r))
        # Time-Under-Water of monthly returns:
        result[8, i] = timeUnderWater = 
            max(diff(which (diff(cummax(cumsum(r))) != 0)))
        # Maximum Loss of monthly returns:
        result[9, i] = maxMonthlyLoss = min(r)  
        # Monthly Value at Risk:
        zc = 2.33
        result[10, i] = monthlyVaR = annualizedMean - 
            zc * annualizedVolatility   
        # Monthly Modified Value at Risk:
        p = 0.99; s = annualizedSkewness; k = annualizedKurtosis    
        zcf = zc + (zc*zc-1)*s/6 + zc*(zc*zc-3)*k/24 + zc*(2*zc*zc-5)*s*s/36
        result[11, i] = monthlyModVaR = annualizedMean - 
            zcf * annualizedVolatility  
        # Monthly Sharpe Ratio:
        result[12, i] = monthlySharpeRatio = 
            annualizedMean/annualizedVolatility 
        # Monthly Modified Sharpe Ratio:
        result[13, i] = monthlyModSharpeRatio = annualizedMean/monthlyModVaR    
        # Skewness Kurtosis Price:
        result[14, i] = skewnesskurtosisPrice = annualizedMean * 
            ( monthlyModVaR/monthlyVaR - 1) }
    
    # Result:
    ans = as.data.frame(round(result, digits = 3))    
    
    # Return Value:
    ans
} 


################################################################################

