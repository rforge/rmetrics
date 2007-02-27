
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
# FUNCTION:          MC KINNON'S PROBABILIY AND QUANTILES:
#  punitroot          Returns cumulative probability for unit root distributions
#  qunitroot          Returns quantiles for unit root distributions
#  unitrootTable      Returns McKinnon's unitroot finite sample test table
# FUNCTION:          INTERNAL UTILITY FUNCTIONS:
#  .strsplitUrcval    Implements string split function for S-Plus compatibility
#  .urcval            Implements unit root statists
#  .probsUrcval       Implements probability values
# FUNCTION:          INTERNAL DATA SETS:
#  .urc1 ... .urc12   Statistical values for unitroot data
################################################################################



test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(McKinnonPValues, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")
        
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------

    
if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fEcofin/test/runit4E.R")
    printTextProtocol(testResult)
}   


################################################################################

