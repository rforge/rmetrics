
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
# FUNCTION:      DESCRIPTION: 
#  removeNA       Remove NAs from a matrix object
#  substituteNA   Substitute NAs by zero, the column mean or median
#  interpNA       Interpolate NAs using R's "approx" function
#  knnNA          Impute NAs by the "knn"-Algorithm from R's EMV package
#  .knn           Internal function from package EMV
################################################################################


### Uncomplete - Under Development ###


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(MissingValues); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.remove =
function()
{
    NA
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.substitute =
function()
{
    NA 
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.interp =
function()
{
    NA
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.knn =
function()
{
    NA
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit6B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
