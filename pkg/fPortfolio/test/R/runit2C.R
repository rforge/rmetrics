
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
# FUNCTION:                 EFFICIENT FRONTIER:
#  'fPFOLIO'                 S4 Portfolio Class
#  frontierMarkowitz         Efficient frontier of mean-var portfolio
#  montecarloMarkowitz       Adds randomly created portfolios
#  portfolioMarkowitz        Mean-variance Markowitz (target) portfolio
# METHODS:                  DESCRIPTION:   
#  print.fPFOLIO             S3: Print method for objects of class fPFOLIO
#  plot.fPFOLIO              S3: Plot method for objects of class fPFOLIO
#  summary.fPFOLIO           S3: Summary method for objects of class fPFOLIO
#  .frontier.default         S3: Extract points on the efficient frontier
# FUNCTIONS:                SPECIAL PORTFOLIOS:
#  .tangencyMarkowitz        Adds tangency portfolio
#  .equalweightsMarkowitz    Adds equal weights Portfolio
#  .frontierShortSelling     Efficient Frontier of Short Selling Markowitz
# FUNCTIONS:                BUILTIN FROM PACKAGE:
#  .portfolio.optim          Function from R-package tseries
#  .solve.QP                 Function from R-package quadprog
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


test.berndtInvest = 
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
    ###
        
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    class(berndtInvest)
    head(berndtInvest)
    
    # Exclude the Date, Market Returns and Interest Rate Columns 
    # from the data frame, then multiply by 100 for percentual returns ...
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    head(berndtAssets)
    ###
}


# ------------------------------------------------------------------------------


test.frontierMarkowitz = 
function()
{
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    
    # Optimize Markowitz Portfolio - Calculate Efficient Frontier:
    par(mfrow = c(1, 1))
    myPF = frontierMarkowitz(data = berndtAssets)     
    plot(myPF, which = 1)  
    title(main = "\n\nBerndt's Data Set")
    
    # Use the Mean-Covariance as Input:
    series = seriesData(berndtAssets)
    MeanCov = list(mu = colAvgs(series), Sigma = cov(series))
    myPF = frontierMarkowitz(data = MeanCov)
    plot(myPF, which = 1)  
    
    # Print Method:
    print(myPF) 
    
    # Plot Method - All Plots on one Page: 
    par(mfrow = c(3, 2), cex = 0.7)
    plot(myPF, which = "all")                           # Probelm !!!
    ###
    
    # Summary Method:
    # summary(myPF)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.montecarloMarkowitz = 
function()
{ 
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    
    # Monte Carlo Simulation:
    # montecarloMarkowitz(object, mc = 5000, doplot = FALSE, add = TRUE, ...)
    myPF = montecarloMarkowitz(myPF, mc = 5000, doplot = TRUE)
    
    # Add Monte Carlo Simulation to Frontier Plot: 
    myPF = frontierMarkowitz(data = berndtAssets)    
    plot(myPF, which = 1) 
    myPF = montecarloMarkowitz(myPF, mc = 5000, add = TRUE)
    
    # Return Value:
    return()    
}  


# ------------------------------------------------------------------------------


test.tangencyMarkowitz = 
function()
{
    # Internal Function - Tangency Portfolio
    
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    
    # Add to Frontier: 
    myPF = frontierMarkowitz(data = berndtAssets)    
    plot(myPF, which = 1) 
    
    # Tangency Portfolio:
    .tangencyMarkowitz(myPF, Rf = 0, add = TRUE) 
    
    # Return Value:
    return()    
}      


# ------------------------------------------------------------------------------  
    

test.portfolioMarkowitz = 
function(x, targetReturn, title = NULL, description = NULL) 
{   
    # Optimize a mean-var portfolio for a given desired return
    
    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    berndtAssets = 100 * berndtInvest[, -c(10, 17)]
    
    # portfolioMarkowitz(x, targetReturn, title = NULL, description = NULL) 
    myPF = portfolioMarkowitz(berndtAssets, targetReturn = 2.2)
    class(myPF)
    print(myPF)
    
    # Return Value:
    return()    
}  


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit2C.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

