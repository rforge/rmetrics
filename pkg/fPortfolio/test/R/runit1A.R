
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
# FUNCTION:             ASSETS SELECTION:
#  assetsSelect          Selects individual assets from a set of assets
#   method = "hclust"     hierarchical clustering of returns
#   method = "kmeans"     k-means clustering of returns
#  assetsHistPlot        Displays histograms of individual assets 
#  assetsQQNormPlot      Displays normal qq-plots of individual assets
#  assetsPairsPlot       Displays pairs of scatterplots of individual assets
#  assetsCorTestPlot     Displays and tests pairwise correlations of assets
# FUNCTION:             SIMULATION AND PARAMETER ESTIMATION:
#  'fASSETS'             Class representation for "fASSETS" Objects
#  assetsSim             Simulates a set of artificial assets
#  assetsFit             Estimates the parameters of set of assets
#   method = "norm"       assuming a multivariate Normal distribution
#   method = "snorm"      assuming a multivariate skew-Normal distribution
#   method = "st"         assuming a multivariate skew-Student-t  
#  print.fASSETS         S3: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS
# FUNCTION:             ASSETS STATISTICS:
#  assetsStats           Computes basic statistics of a set of asset  
#  assetsMeanCov         Estimates mean and variance for a set of assets
#   method = "cov"        uses standard covariance estimation
#   method = "mve"        uses
#   method = "mcd"        uses
#   method = "nne"        uses
#   method = "shrink"     uses shrinkage
#   method = "bagged"     uses bagging
#  .isPositiveDefinite    Checks if the matrix x is positive definite
#  .makePositiveDefinite  Forces the matrix x to be positive definite
# FUNCTION:             ASSETS NORMALITY TESTS:
#  assetsTest            Test for multivariate Normal Assets
#   method = "shapiro"    calling Shapiro test
#   method = "energy"     calling E-Statistic (energy) test
#  .mvenergyTest         Multivariate Energy Test
#   .mvnorm.etest         Internal Function used by assetsTest
#   .mvnorm.e             Internal Function used by assetsTest
#   .normal.e             Internal Function used by assetsTest
#   .mvnormBoot           Internal Function used by assetsTest
#  .mvshapiroTest        Multivariate Shapiro Test
# REQUIREMENTS:         DESCRIPTION:
#  .msn.quantities       Function from R package sn [in fMultivar]      
#  copcor                R contributed package copcor
#  covRobust             R contributed package covRobust
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(MarkowitzPortfolio); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.assetsSelect = 
function()
{
    # The data set "berndtInvest" is from Berndt's textbook 
    # "The Practice of Econometrics". It is a data.frame consisting
    # of 18 columns with the following entries:
    #  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
    #  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
    # [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
    # The first column holds the date, the 11th the market rate,
    # and the last (the 18th) the risk free rate.
        
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    class(berndtInvest)
    head(berndtInvest)
    
    # Exclude the Date, Market Returns and Interest Rate Columns 
    # from the data frame, then multiply by 100 for percentual returns ...
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    head(berndtAssets)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.assetsPlots = 
function()
{
    # The data set "berndtInvest" is from Berndt's textbook 
    # "The Practice of Econometrics". It is a data.frame consisting
    # of 18 columns with the following entries:
    #  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
    #  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
    # [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
    # The first column holds the date, the 11th the market rate,
    # and the last (the 18th) the risk free rate.
        
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    class(berndtInvest)
    head(berndtInvest)
    
    # Exclude the Date, Market Returns and Interest Rate Columns 
    # from the data frame, then multiply by 100 for percentual returns ...
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    head(berndtAssets)
    
    
    # FHistogram Plot:
    .assetsHistPlot(x = berndtAssets, method = c("mve", "mcd", "classical"), 
        which = 1:dim(x)[2], labels = TRUE, xlim = NULL) 
    
    # Return Value:
    return()       
}


# ------------------------------------------------------------------------------


test.robustCovariances = 
function()
{
    # The data set "berndtInvest" is from Berndt's textbook 
    # "The Practice of Econometrics". It is a data.frame consisting
    # of 18 columns with the following entries:
    #  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
    #  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
    # [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
    # The first column holds the date, the 11th the market rate,
    # and the last (the 18th) the risk free rate.
        
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    class(berndtInvest)
    head(berndtInvest)
    
    # Exclude the Date, Market Returns and Interest Rate Columns 
    # from the data frame, then multiply by 100 for percentual returns ...
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    head(berndtAssets)
    
    # RobustCovariances:
    #   assetsMeanCov(x, method = c("cov", "mve", "mcd", "nnve", "shrink", 
    #       "bagged"), check = TRUE, force = TRUE, baggedR = 100) 

    assetsMeanCov(berndtAssets, "cov")
    assetsMeanCov(berndtAssets, "mve")
    assetsMeanCov(berndtAssets, "mcd")
    assetsMeanCov(berndtAssets, "nnve")
    assetsMeanCov(berndtAssets, "shrink")
    assetsMeanCov(berndtAssets, "bagged")
    
    # Return Value:
    return()       
}
    

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit1A.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

