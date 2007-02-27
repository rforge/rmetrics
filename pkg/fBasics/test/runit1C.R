
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
# FUNCTION:           DESCRIPTION:
# .akimaInterp         Interpolates and Smoothes Irregularly Distributed Points
# .krigeInterp         Interpolates and Smoothes Irregularly Distributed Points
# FUNCTION:           DESCRIPTION:
# .squareBinning       Squatre Binning of Irregularly Distributed Points
# .sqarePlot           Plots Square Binned Data Points
# FUNCTION:           DESCRIPTION:
# .hexBinning          Hexagonal Binning of Irregularly Distributed Points
# .hexPlot             Plots Hexagonal Binned Data Points
# FUNCTION:           DESCRIPTION:
# .surfacePlot         Perspective Plot of Irregularly Distributed Points
# .levelPlot           Contour Plot of Irregularly Distributed Points
# .circles2Plot        Circles Plot of Irregularly Distributed Points
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(Tailored3DPlots, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# TAILORED PLOT FUNCTIONS:


test.tailoredPlots = 
function()
{
    set.seed(101)
    x = rnorm(10000)
    y = rnorm(10000) + X*(X+1)/4
    
    par(mfrow = c(1, 1))
    
    .hexPlot(x, y, bins = 30)
    grid()
    
    .hexPlot(x, y, bins = 30, rainbow(255))
    grid()
    
    .hexPlot(x, y, bins = 30, rev(greyPalette(256-17)))
    grid()
    
    plot(x, y, pch = ".", col = "steelblue")
    grid()

    .hexPlot(x, y, bins = 30)
    grid()

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit1C.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

