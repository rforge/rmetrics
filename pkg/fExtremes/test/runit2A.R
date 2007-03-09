
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
# FUNCTION:             GEV DISTRIBUTION FAMILY: [CALLING EVD]
#  dgev                  Density for the GEV Distribution 
#   pgev                  Probability for the GEV Distribution
#   qgev                  Quantiles for the GEV Distribution
#   rgev                  Random variates for the GEV Distribution
#  gevMoments            Computes true statistics for GEV distribution
#  gevSlider             Displays distribution and rvs for GEV distribution
# FUNCTION:             GEV DISTRIBUTION FAMILY: [USE FROM EVD]
#  .devd                 Density for the GEV Distribution 
#   .pevd                 Probability for the GEV Distribution
#   .qevd                 Quantiles for the GEV Distribution
#   .revd                 Random variates for the GEV Distribution
#  .evdMoments           Computes true statistics for GEV distribution
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GevDistribution, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.gev =
function()
{
    # Check Distribution:
    set.seed(1985)
    .distCheck(fun = "gev", n = 1000, xi = 0.0, mu = 0, beta = 1)
    .distCheck(fun = "gev", n = 1000, xi = 1/4, mu = 0, beta = 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.gevMoments = 
function()
{
    # gevMoments(xi = 0, mu = 0, beta = 1) 
    
    # Compute Moments:
    xi = seq(-4.5, 1.5, by = 0.25)
    mom = gevMoments(xi)
    print(mom)
    
    # Plot Mean:
    par(mfrow = c(2, 1), cex = 0.7)
    xi = seq(-5, 2, length = 351)
    mom = gevMoments(xi)
    plot(xi, mom$mean, main = "Mean", pch = 19, col = "steelblue")
    abline(v = 1, col = "red", lty = 3)
    abline(h = 0, col = "red", lty = 3)
    
    # Plot Variance:
    plot(xi, log(mom$var), main = "log Variance", pch = 19, col = "steelblue")
    abline(v = 1/2, col = "red", lty = 3)
    abline(h = 0.0, col = "red", lty = 3)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.gevSlider = 
function()
{
    # Distribution Slider:
    print("Activate Slider manually!")
    # gevSlider(method = "dist")
    
    # Random Variates Slider:
    print("Activate Slider manually!")
    # gevSlider(method = "rvs")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fExtremes/test/runit2A.R")
    printTextProtocol(testResult)
}


################################################################################

