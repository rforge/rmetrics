
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
# FUNCTION:           BIVARIATE GRIDDED INTERPOLATION:
#  linearInterp        Interpolates Linearly Irregularly Distributed Data Points
#  linearInterpp       Interpolates Linearly pointwise
#  akimaInterp         Interpolates and Smoothes Irregularly Distributed Points
#  akimaInterpp        Interpolates and Smoothes pointwise
#  krigeInterp         Kriges Irregularly Distributed Data Points
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(BivariateInterpolation, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")
        
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.linearInterp = 
function()
{
    #  Interpolates Linearly Irregularly Distributed Data Points

    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = runif(999)-0.5
    y = runif(999)-0.5
    z = cos(2*pi*(x^2+y^2))
    ans = linearInterp(x, y, z, gridPoints = 41)
    persp(ans, theta = -50, phi = 30, col = "steelblue")
    
    title(main = "Linear Interpolation")
    mtext(text = 
        "x=runif(999)-0.5  |  y=runif(999)-0.5  |  z=cos(2*pi*(x^2+y^2))", 
        side = 1, line = 1)
    mtext(text = "", side = 1, line = 0)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.krigeInterp = 
function()
{
    # Kriges Irregularly Distributed Data Points

    require(spatial)
    
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = runif(999)-0.5
    y = runif(999)-0.5
    z = cos(2*pi*(x^2+y^2))
    ans = krigeInterp(x, y, z, extrap = FALSE)
    persp(ans, theta = -50, phi = 30, col = "steelblue")
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

    
if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fEcofin/test/runit5B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}   


################################################################################



