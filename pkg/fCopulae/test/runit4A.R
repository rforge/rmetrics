
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
# FUNCTION:                 EXTREME VALUE COPULAE PARAMETER:
#  evList                    Returns list of implemented extreme value copulae
#  evParam                   Sets Default parameters for an extreme value copula
#  evCheck                   Checks if parameters are in the valid range
#  evRange                   Returns the range of valid parameter values
# FUNCTION:                 EXTREME VALUE COPULAE GENERATOR FUNCTION:
#  Afunc                     Computes Dependence function
#  AfuncSlider               Displays interactively dependence function
#################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ExtremeValueCopula, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.evList = 
function()
{
    # Arguments ?
    args(evList)
    
    # List:
    evList()

    # Return Value:
    return()    
}

    
# ------------------------------------------------------------------------------


test.evParam = 
function()
{
    # 
    NA

    # Return Value:
    return()    
}

    
# ------------------------------------------------------------------------------


test.evRange = 
function()
{
    NA
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.evCheck = 
function()
{
    NA
    
    # Return Value:
    return()    
}

   
# ------------------------------------------------------------------------------


test.Afunc = 
function()
{
    NA
    
    # Return Value:
    return()    
}

   
# ------------------------------------------------------------------------------

    
test.AfuncSlider = 
function()
{
    NA
    
    # Return Value:
    return()    
}

        
# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/test/runit4A.R")
    printTextProtocol(testResult)
}
 
  
################################################################################

