
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
# FUNCTION:                 TIME SERIES TESTS
#  lmTest                   Linear Modelling Test, select from:
#   bgTest                   Breusch-Godfrey Test
#   bpTest                   Breusch-Pagan Test
#   dwTest                   Durbin-Watson Test
#   gqTest                   Goldfeld-Quandt Test
#   harvTest                 Harvey-Collier Test
#   hmcTest                  Harrison-McCabe Test
#   rainTest                 Rainbow Test
#   resetTest                Ramsey's RESET Test
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(RegressionTests, ask = FALSE)
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
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit2B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
