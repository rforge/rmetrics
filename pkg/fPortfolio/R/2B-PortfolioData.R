
# This library is free software, you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation, either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library, if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Rmetrics Foundation, GPL
#   Contact: Diethelm Wuertz <wuertz@phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     EXAMPLE DATA:
#  dutchPortfolioData            Example Data from Engel's Diploma Thesis
#  usPortfolioData               Annual US Economics Portfolio Data
#  sm132PortfolioData            Example from Scherer, Martin: Chapter 1.32
################################################################################


dutchPortfolioData =
function()
{   # A function implemented by Rmetrics

    # Description:
    #   Example Portfolio Data from Engels
    
    # Example:
    #   engelsPortfolioData() 
    
    # FUNCTION:
    
    # Mean Returns:
    mu = c(0.266, 0.274, 0.162, 0.519, 0.394, 0.231, 0.277) / 1000
    names(mu) = c(
        "Elsevier", "Fortis", "Getronics", "Heineken", 
        "Philips", "RoyalDutch", "Unilever")  
    
    # Variance-Covariance Risk:
    Sigma = c(
        0.345, 0.150, 0.183, 0.088, 0.186, 0.090, 0.095,
        0.150, 0.399, 0.204, 0.107, 0.236, 0.130, 0.127,
        0.183, 0.204, 1.754, 0.075, 0.325, 0.110, 0.091,
        0.088, 0.107, 0.075, 0.243, 0.096, 0.064, 0.086,
        0.186, 0.236, 0.325, 0.096, 0.734, 0.147, 0.114,
        0.090, 0.130, 0.110, 0.064, 0.147, 0.221, 0.093,
        0.095, 0.127, 0.091, 0.086, 0.114, 0.093, 0.219)    
    Sigma = matrix(Sigma, ncol = 7)
    colnames(Sigma) = rownames(Sigma) = names(mu)
    
    # Return Value:
    list(mu = mu, Sigma = Sigma)
}


# ------------------------------------------------------------------------------


usPortfolioData =
function()
{   # A function implemented by Rmetrics

    # Description:
    #   Annual US Economics Portfolio Data 
    
    # Example:
    #   usPortfolioData() 
    #   list(mu = round(mean(usPortfolioData()),5),
    #   Sigma = round(var(usPortfolioData()), 5))
    
    # FUNCTION:
    
    # Units:
    Units = c("TBills3m", "LongBonds", "SP500", "Wilshire5000",
        "NASDAQComp", "LehmanBonds", "EAFE", "Gold")
    
    # Time Series Object:
    tS = as.timeSeries(as.data.frame(matrix(c(
        19731231,1.075,0.942,0.852,0.815,0.698,1.023,0.851,1.677,
        19741231,1.084,1.020,0.735,0.716,0.662,1.002,0.768,1.722,
        19751231,1.061,1.056,1.371,1.385,1.318,1.123,1.354,0.760,
        19761231,1.052,1.175,1.236,1.266,1.280,1.156,1.025,0.960,
        19771231,1.055,1.002,0.926,0.974,1.093,1.030,1.181,1.200,
        19781231,1.077,0.982,1.064,1.093,1.146,1.012,1.326,1.295,
        19791231,1.109,0.978,1.184,1.256,1.307,1.023,1.048,2.212,
        19801231,1.127,0.947,1.323,1.337,1.367,1.031,1.226,1.296,
        19811231,1.156,1.003,0.949,0.963,0.990,1.073,0.977,0.688,
        19821231,1.117,1.465,1.215,1.187,1.213,1.311,0.981,1.084,
        19831231,1.092,0.985,1.224,1.235,1.217,1.080,1.237,0.872,
        19841231,1.103,1.159,1.061,1.030,0.903,1.150,1.074,0.825,
        19851231,1.080,1.366,1.316,1.326,1.333,1.213,1.562,1.006,
        19861231,1.063,1.309,1.186,1.161,1.086,1.156,1.694,1.216,
        19871231,1.061,0.925,1.052,1.023,0.959,1.023,1.246,1.244,
        19881231,1.071,1.086,1.165,1.179,1.165,1.076,1.283,0.861,
        19891231,1.087,1.212,1.316,1.292,1.204,1.142,1.105,0.977,
        19901231,1.080,1.054,0.968,0.938,0.830,1.083,0.766,0.922,
        19911231,1.057,1.193,1.304,1.342,1.594,1.161,1.121,0.958,
        19921231,1.036,1.079,1.076,1.090,1.174,1.076,0.878,0.926,
        19931231,1.031,1.217,1.100,1.113,1.162,1.110,1.326,1.146,
        19941231,1.045,0.889,1.012,0.999,0.968,0.965,1.078,0.990),
        byrow = TRUE, ncol = 9)))
    colnames(tS)<-Units
    
    # return Value:
    tS
}

# ------------------------------------------------------------------------------

sm132PortfolioData = 
function()
{
    # A function implemented by Rmetrics

    # Description:
    #   Example from Scherer, Martin:  "Modern Portfolio Omtimization":
    #       Cheapter 1.32
    
    # FUNCTION:
    corr = matrix(data =
        c(  1, 0.4, 0.5, 0.5, 0.4, 0.1, 0.1, 0.1,
          0.4, 1.0, 0.3, 0.3, 0.1, 0.4, 0.1, 0.1,
          0.5, 0.3, 1.0, 0.7, 0.1, 0.1, 0.5, 0.1,
          0.5, 0.3, 0.7, 1.0, 0.1, 0.1, 0.1, 0.5,
          0.4, 0.1, 0.1, 0.1, 1.0, 0.0, 0.0, 0.0,
          0.1, 0.4, 0.1, 0.1, 0.0, 1.0, 0.0, 0.0,
          0.1, 0.1, 0.5, 0.1, 0.0, 0.0, 1.0, 0.2,
          0.1, 0.1, 0.1, 0.5, 0.0, 0.0, 0.2, 1.0),
          nrow = 8, ncol = 8)
    vol = diag(c(17, 21, 22, 20, 8, 8, 8, 8))
    Cov = vol %*% corr %*% vol    
    
    # Average return
    mu = c(3, 4, 5, 6, 0.25, 0.5, 0.75, 1)
    
    # Return value:
    list(mu = mu, Sigma = Cov)

    
}


################################################################################

