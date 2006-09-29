
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
# FUNCTION:           DESCRIPTION:
#  dmvsnorm            Multivariate Skew Normal Density Function
#  pmvsnorm            Multivariate Skew Normal Probability Function
#  rmvsnorm            Multivariate Skew Normal Random Deviates
# FUNCTION:           DESCRIPTION:
#  dmvst               Multivariate Skew Sudent-t Density Function
#  pmvst               Multivariate Skew Sudent-t Probability Function
#  rmvst               Multivariate Skew Sudent-t Random Deviates
# FUNCTION:           DESCRIPTION:
#  fMV                 S4 Object of class 'fMV'
#  mvFit               Fits a MV Normal or Student-t Distribution
#  print.fMV           S3: Print method for objects of class 'fMV'
#  plot.fMV            S3: Plot method for objects of class 'fMV'
#  summary.fMV         S3: Summary method for objects of class 'fMV'
# INTERNAL:           DESCRIPTION:
#  .mvsnormFit         Fits a Multivariate Normal Distribution
#  .mvstFit            Fits a Multivariate Student-t Distribution
#  .mvsnormPlot        Plots for Multivariate Normal Distribution
#  .mvstPlot           Plots for Multivariate Student-t Distribution
################################################################################


### Uncomplete - Under Development ###


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(MultivariateDistribution); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.mvsnorm =
function()
{
    # Bivariate Density:
    x = y = (-40:40)/10
    G = grid2d(x)
    X = cbind(G$x, G$y)
    z = dmvsnorm(X, 2, mu = rep(0, 2), Omega = diag(2), alpha = rep(0, 2))
    Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
    .perspPlot(Z)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit7B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
