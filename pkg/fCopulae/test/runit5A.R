
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
#                       X     NOT YET IMPEMENTED
# FUNCTION:                  ARMAX COPULAE PARAMETER:
#  .armaxParam          X     Sets parameters for an armax copula
#  .armaxRange          X     Returns the range of valid parameter values
#  .armaxCheck          X     Checks if the parameters are in the valid range
# FUNCTION:                  ARMAX COPULAE GENERATOR FUNCTION:
#  armax*               X
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  armaxTau             X     Returns Kendall's tau for armax copulae
#  armaxRho             X     Returns Spearman's rho for armax copulae
# FUNCTION:                  ARMAX COPULAE TAIL COEFFICIENT:
#  armaxTailCoeff       X     Computes tail dependence for armax copulae
#  armaxTailPlot        X     Plots armax tail dependence function
# FUNCTION:                  ARMAX COPULAE RANDOM VARIATES:
#  rarmaxCopula         X     Generates armax copula random variates 
#  rarmaxSlider         X     Generates interactive plots of random variates
# FUNCTION:                  ARMAX COPULAE PROBABILITY:
#  parmaxlCopula              Computes armax copula probability
#  parmaxSlider         X     Generates interactive plots of probability
# FUNCTION:                  ARMAX COPULAE DENSITY:
#  darmaxCopula         X     Computes armax copula density 
#  darmaxSlider         X     Generates interactive plots of armax density
# FUNCTION:                  ARMAX COPULAE PARAMETER FITTING:
#  armaxCopulaSim       X     Simulates bivariate extreme value copula
#  armaxCopulaFit       X     Fits the paramter of an extreme value copula
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ArchmaxCopulae, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


################################################################################
# ARMAX - ARCHIMEDEAN EXTREME VALE COPULAE:


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/test/runit1A.R")
    printTextProtocol(testResult)
}
 

  
################################################################################