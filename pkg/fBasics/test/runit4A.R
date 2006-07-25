
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
# FUNCTION:                 PORTABLE INNOVATIONS:
#  set.lcgseed               Set initial random seed
#  get.lcgseed               Get the current valus of the random seed
#  runif.lcg                 Uniform linear congruational generator
#  rnorm.lcg                 Normal linear congruational generator
#  rt.lcg                    Student-t linear congruational generator
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortableRandomInnovations); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

   
test.portableInnovations = 
function()
{
    # set.lcgseed -
    seed = set.lcgseed(seed = 65890)
    
    # runif.lcg - rnorm.lcg - rt.lcg -
    DF = cbind(runif.lcg(10), rnorm.lcg(10), rt.lcg(10, df = 4))
    print(DF)
    checkEqualsNumeric(target = round(sum(DF), 4), current = 3.3164 )
    
    # get.lcgseed -
    seed = get.lcgseed()
    print(seed)
    checkIdentical(target = seed, current = 1743389204 ) 
    
    # Note, to overwrite rnorm, use
    # rnorm = rnorm.lcg
    # Going back to rnorm
    # rm(rnorm) 
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit4A.R")
    printTextProtocol(testResult)
}


################################################################################

