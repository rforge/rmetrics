
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                      DESCRIPTION:
#  frontierTwoAssetsMarkowitz     Computes efficient frontier for Markowitz PF
#  frontierTwoAssetsCVaR          Computes efficient frontier for CVaR PF        
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TwoAssetsPortfolio); return() }
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
    
    # Select IBM and MOBIL - use percentual Returns:
    berndtAssets = 100 * berndtInvest[, c("IBM", "MOBIL")]
    head(berndtAssets)
    ###
}


# ------------------------------------------------------------------------------


test.frontierTwoAssetsMarkowitz = 
function()
{   
    # frontierTwoAssetsMarkowitz(
    #   x, length = 100, title = NULL, description = NULL) 

    # Load the Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    berndtAssets = 100 * berndtInvest[, c("IBM", "MOBIL")]
    
    myPF = frontierTwoAssetsMarkowitz(berndtAssets)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit2B.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

