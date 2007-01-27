
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
# FUNCTION:       DESCRIPTION:
#  erf             Error function
#  [gamma]         Gamma function 
#  [lgamma]        LogGamma function, returns log(gamma)
#  [digamma]       First derivative of of LogGamma, dlog(gamma(x))/dx
#  [trigamma]      Second derivative of of LogGamma, dlog(gamma(x))/dx
#  {tetragamma}    Third derivative of of LogGamma, dlog(gamma(x))/dx
#  {pentagamma}    Fourth derivative of LogGamma, dlog(gamma(x))/dx
#  [beta]*         Beta function
#  [lbeta]*        LogBeta function, returns log(Beta)
#  Psi             Psi(x) (Digamma) function
#  igamma          P(a,x) Incomplete Gamma Function
#  cgamma          Gamma function for complex arguments
#  Pochhammer      Pochhammer symbol
# NOTES:
#  Functions in [] paranthesis are part of the R's and SPlus' base distribution
#  Functions in {} paranthesis are only availalble in R
#  Function marked by []* are compute through the gamma function in SPlus
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GammaFunctions); return() }
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
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fOptions/test/runit3B.R")
    printTextProtocol(testResult)
}


################################################################################

