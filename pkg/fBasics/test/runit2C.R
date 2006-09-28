
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 


################################################################################
# FUNCTIONS:        DESCRIPTION:
#   dssd             Returns smoothed spline density estimate
#   pssd             Returns smoothed spline probability estimate
#   qssd             Returns smoothed spline quantiles estimate
#   rssd             Returns smoothed spline random variates 
# INTERNAL:         DESCRIPTION:      
#  .dssden           Computes density function
#  .pssden           Computes probability function
#  .qssden           Computes quantile function
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(SmoothedSplineDistribution); return()}
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ssd = 
function()
{
    # ssdFit Distribution:
    set.seed(1953)
    X = rnorm(100)     
    fit = ssdFit(x = X, alpha = 1.4, seed = 4711)
    test = .distCheck("ssd", n = 100, param = fit)
    # Add Unit Test ...
    
    # Histogram Plot:
    hist(rssd(100, fit), probability = TRUE, breaks = "FD",
        col = "steelblue", border = "white")
    s = seq(-3, 3, length = 201)
    lines(s, dnorm(s), col = "orange")
    # Add Unit Test ...
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit2C.R")
    printTextProtocol(testResult)
}


################################################################################

