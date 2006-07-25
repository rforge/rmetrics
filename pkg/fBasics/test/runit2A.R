
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
# FUNCTION:             DESCRIPTION:
#  dsymstb               Returns density for symmetric stable DF
#  psymstb               Returns probabilities for symmetric stable DF
#  qsymstb               Returns quantiles for symmetric stable DF
#  rsymstb               Returns random variates for symmetric stable DF
# FUNCTIONS:            DESCRIPTION:
#  stableMode            Computes stable mode
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
# FUNCTION:             DESCRIPTION:
#  symstbSlider          Displays symmetric stable distribution function
#  stableSlider          Displays stable distribution function
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(StableDistribution); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.symstb = 
function()
{
    # rsymstb -
    ans = .distCheck("symstb", alpha = 1.9)
    print(ans)
    cat("\n")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stable = 
function()
{
    # stable -
    ans = .distCheck("stable", alpha = 1.9, beta = 0.3)
    print(ans)
    cat("\n")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stableS1 = 
function()
{
    # stable - S1:
    ans = .distCheck("stable", alpha = 1.9, beta = 0.3, pm = 1)
    print(ans)
    cat("\n")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit013A.R")
    printTextProtocol(testResult)
}

       
################################################################################

