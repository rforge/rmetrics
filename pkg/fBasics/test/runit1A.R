
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
# FUNCTION:                     DESCRIPTION:   
#  .interp                       Does Akima Spline Interpolation
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ISO8601Standard); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.interp = 
function()
{
    akima = list(
    x = c(
    11.16, 24.20, 12.85, 19.85, 10.35, 24.65, 19.72, 15.91,  0.00, 20.87, 
     6.71,  3.45, 19.99, 14.26, 10.28,  4.51, 17.43, 22.80,  0.00,  7.58, 
    16.70,  6.08,  1.99, 25.00, 14.90,  3.22,  0.00,  9.66,  2.56,  5.22, 
    11.77, 17.25, 15.10, 25.00, 12.13, 25.00, 22.33, 11.52, 14.59, 15.20,  
     7.54,  5.23, 17.32,  2.14,  0.51, 22.69, 25.00,  5.47, 21.67,  3.31),
    y = c(
     1.24, 16.23,  3.06, 10.72,  4.11,  2.40,  1.39,  7.74, 20.00, 20.00, 
     6.26, 12.78,  4.62, 17.87, 15.16, 20.00,  3.46, 12.39,  4.48,  1.98, 
    19.65,  4.58,  5.60, 11.87,  3.12, 16.78,  0.00, 20.00,  3.02, 14.66, 
    10.47, 19.57, 17.19,  3.87, 10.79,  0.00,  6.21,  8.53,  8.71,  0.00, 
    10.69, 10.72, 13.78, 15.03,  8.37, 19.63, 20.00, 17.13, 14.36,  0.13),
    z = c(
    22.15,  2.83, 22.11,  7.97, 22.33, 10.25, 16.83, 15.30, 34.60,  7.54,
    30.97, 41.24, 14.72, 10.74, 21.59, 15.61, 18.60,  5.47, 61.77, 29.87,  
     6.31, 35.74, 51.81,  4.40, 21.70, 39.93, 58.20,  4.73, 50.55, 40.36, 
    13.62,  6.43, 12.57,  8.74, 13.71, 12.00, 10.25, 15.74, 14.81, 21.60, 
    19.31, 26.50, 12.11, 53.10, 49.43,  3.25,  0.60, 28.63,  5.52, 44.08))

    # Interpolation:
    akima.lin = .interp(akima$x, akima$y, akima$z)
    Z = mean(akima.lin$z)
    checkSun = 21.70316
    checkEquals(target = Z, current = checkSum, tolerance = 0.00001)
    
    # Plots:
    image  (akima.lin, add = FALSE)
    contour(akima.lin, add = TRUE)
    points (akima, pch = 19)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit1A.R")
    printTextProtocol(testDate = testResult, fileName = "")
}


# ------------------------------------------------------------------------------

