
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
# FUNCTION:           DESCRIPTION:
#  squareBinning       Square binning of irregularly distributed data sets
#  plot                S3 Method for plotting square binned data sets
# FUNCTION:           DESCRIPTION:
#  hexBinning          Hexagonal binning of irregularly distributed data sets
#  plot                S3 Method for plotting hexagonal binned data sets
################################################################################



test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(BivariateBinning); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")
    return() 
}


# ------------------------------------------------------------------------------


test.squareBinning = 
function()
{
    #  squareBinning       Square binning of irregularly distributed data sets
    #  plot                S3 Method for plotting square binned data sets

    # Generate Grid Data:
    sB = squareBinning(x = rnorm(1000), y = rnorm(1000))
    
    # Plot:
    plot(sB)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.hexBinning = 
function()
{
    #  hexBinning          Hexagonal binning of irregularly distributed data sets
    #  plot                S3 Method for plotting hexagonal binned data sets
    
    # Generate Grid Data:
    set.seed(1953)
    hB = hexBinning(x = rnorm(1000), y = rnorm(1000))
    
    # Plot:
    plot(hB)
      
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

    
if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fEcofin/test/runit5B.R")
    printTextProtocol(testResult)
}   


################################################################################



