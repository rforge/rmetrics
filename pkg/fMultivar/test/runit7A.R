
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
# FUNCTION:             DESCRIPTION:
#  grid2d                Creates from two vectors x-y grid coordinates
#  density2d             Returns 2D Kernel Density Estimates
#  hist2d                Returns 2D Histogram Counts
#  integrate2d           Integrates over a two dimensional unit square
# FUNCTION:             BIVARIATE DISTRIBUTIONS:
#  pnorm2d               Computes bivariate Normal probability function
#  dnorm2d               Computes bivariate Normal density function
#  rnorm2d               Generates bivariate normal random deviates
#  pcauchy2d             Computes bivariate Cauchy probability function
#  dcauchy2d             Computes bivariate Cauchy density function
#  rcauchy2d             Generates bivariate Cauchy random deviates
#  pt2d                  Computes bivariate Student-t probability function
#  dt2d                  Computes bivariate Student-t density function
#  rt2d                  Generates bivariate Student-t random deviates
# FUNCTION:             ELLIPTICAL DISTRIBUTIONS:
#  delliptical2d         Computes density for elliptical distributions
#  .gfunc2d              Generator Function for elliptical distributions
#  .delliptical2dSlider  Slider for bivariate densities
#  .delliptical2d.RUnit  RUnit Test for elliptical distributions
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(BivariateTools); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

test.xxx =
function()
{
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit7A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
