
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


################################################################################
# FUNCTION:           DENSITIES:
#  dlognorm            log-Normal density an derivatives
#  plognorm            log-Normal, synonyme for plnorm
#  dgam                Gamma density, synonyme for dgamma
#  pgam                Gamma probability, synonyme for pgamma
#  drgam               Reciprocal-Gamma density
#  prgam               Reciprocal-Gamma probability
#  djohnson            Johnson Type I density
#  pjohnson            Johnson Type I probability
# FUNCTION :          MOMENTS:
#  mnorm               Moments of Normal density
#  mlognorm            Moments of log-Normal density
#  mrgam               Moments of reciprocal-Gamma density
#  masian              Moments of Asian Option density
#  .DufresneMoments     Internal Function called by 'masian'
# FUNCTION:           NUMERICAL DERIVATIVES:
#  derivative          First and second numerical derivative
# FUNCTION:           ASIAN DENSITY BY DOUBLE INTEGRATION:
#  .thetaEBM
#  .psiEBM
#  d2EBM
# FUNCTION:           ASIAN DENSITY BY SINGLE INTEGRATION:
#  .gxuEBM
#  .gxt
#  .gxtu
#  dEBM
#  pEBM
# FUNCTION:           ASYMPTOTIC EXPANSION OF ASIAN DENSITY:
#  dasymEBM
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(EBMDistribution); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.X = 
function()
{
    ##

    # Return Value:
    return()    
}



# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fOptions/test/runit3A.R")
    printTextProtocol(testResult)
}


################################################################################

