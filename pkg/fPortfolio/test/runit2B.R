
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
# FUNCTION:                     EXAMPLE DATA:
#  dutchPortfolioData            Example Data from Engel's Diploma Thesis
#  usPortfolioData               Annual US Economics Portfolio Data
#  sm132PortfolioData            Example from Scherer, Martin: Chapter 1.32
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioData, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dutchPortfolioData =
function()
{
    # mu/Sigma List:
    x = dutchPortfolioData()
    print(x)
    mu = x$mu
    Sigma = x$Sigma
    target = round(mean(mu) + mean(Sigma), 3)
    current = 0.194
    checkIdentical(target, current)
        
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.sm132PortfolioData =
function()
{
    # Data Frame:
    df = usPortfolioData()
    print(df)
    mu = colMeans(as.matrix(df))
    Sigma = cov(as.matrix(df))
    target = round(mean(mu) + mean(Sigma), 3)
    current = 1.122
    checkIdentical(target, current)
  
    # Convert to timeSeries:
    tS = as.timeSeries(df)
    tS
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.sm132PortfolioData =
function()
{
    # mu/Sigma List:
    x = sm132PortfolioData()
    print(x)
    mu = x$mu
    Sigma = x$Sigma
    target = round(mean(mu) + mean(Sigma), 3)
    current = 80.744
    checkIdentical(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

