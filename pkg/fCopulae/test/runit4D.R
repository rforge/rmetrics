
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
# FUNCTION:                  EXTREME VALUE COPULAE GENERATOR FUNCTION:
#  Afunc                      Computes Dependence function
#  AfuncSlider                Displays interactively dependence function
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


test.evCopulae = 
function()
{
    # No. 1 - 5
    TYPES = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
    for (type in TYPES) {
        param = .evParam(type)
        print(unlist(param))
        cat("\n")
    }

    # Return Value:
    return()    
}

    
# ------------------------------------------------------------------------------


test.dependenceMeasures = 
function()
{
    # TYPES = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
    # A, A', A'':
    AfuncSlider() 
    
    # Return Value:
    return()    
}

    
# ------------------------------------------------------------------------------


test.kendallsTau = 
function()
{
    NA
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.spearmansRho = 
function()
{
    NA
    
    # Return Value:
    return()    
}

   
# ------------------------------------------------------------------------------
    

test.revCopula = 
function()
{
    
    # Return Value:
    return()    
}

       
# ------------------------------------------------------------------------------


test.pevCopula = 
function()
{        
    # u - single input value:
    pevCopula()
    pevCopula(0.5)
    pevCopula(0.5, 0.25)
    
    # u - input vector:
    U = (0:10)/10
    V = rev(U)
    pevCopula(U)
    pevCopula(u = U, v = V)
    
    # u - input matrix:
    pevCopula(cbind(U,V))
    
    # u - input list:
    u = grid2d()
    u
    pevCopula(u) # output = "vector"
    pevCopula(u, output = "list")
    pevCopula(u) - pevCopula(u, alternative = TRUE)
    
    # Slider:
    pevSlider() # type = "persp"   
    pevSlider(type = "contour")  
    
    # Return Value:
    return()    
}

   
# ------------------------------------------------------------------------------


test.devCopula = 
function()
{
    devSlider() # type = "persp" 
    devSlider(type = "contour") 
    
    # Return Value:
    return()    
}

   
# ------------------------------------------------------------------------------

    
test.parameterFitting = 
function()
{
    
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

