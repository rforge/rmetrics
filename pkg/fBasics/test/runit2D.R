
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
# FUNCTION:           DESCRIPTION:    
#  fDISTFIT            Class Representation
#  normFit             Fits Parameters of a Normal Density
#  tFit                Fits parameters of a Student-t Density
#  ghFit               Fits parameters of a generalized hyperbolic Density
#  hypFit              Fits parameters of a hyperbolic Density
#  nigFit              Fits parameters of a normal inverse Gaussian Density
#  ssdFit              Fits probability densities using smoothing spline ANOVA
#   print.ssd           S3 Print Method
# IFUNCTION:          INTERNAL USED BY SMOOTHED SPLINE DISTRIBUTION:         
#  .ssden              ... Internal functions which are required by ssdFit
#  .mkterm.cubic1
#  .mkphi.cubic
#  .mkrk.cubic
#  .gauss.quad
#  .sspdsty
#  .nlm0
# FUNCTION:           FUNCTIONS FOR SPLUS VERSION:
#  much.fun
#  which.min
#  which.max
################################################################################
    

test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(DistributionFits); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.distFit = 
function()
{ 
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
     
    # normFit -
    # Simulated normal random variates:
    set.seed(1953)
    s = rnorm(n = 1000) 
    ans = .normFit(x = s)
    print(ans)

    # tFit -
    # Simulated random variates t(4):
    set.seed(1953)
    s = rt(n = 1000, df = 4) 
    ans = tFit(x = s, df = 2*var(s)/(var(s)-1), trace = FALSE)
    print(ans)
    
    # ghFit -
    set.seed(1953)
    s = rgh(n = 1000, alpha = 0.8, beta = 0.2, delta = 1.5, mu = 1, lambda = 2) 
    ans = ghFit(x = s, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1,
        trace = FALSE) 
    print(ans)
    # CHECK WARNINGS!
    
    # hypFit -
    set.seed(1953)
    s = rhyp(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1) 
    ans = hypFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), trace = FALSE)
    print(ans)
    # CHECK WARNINGS!
    
    # nigFit -
    set.seed(1953)
    s = rnig(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 
    ans = nigFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), trace = FALSE)
    print(ans)
    print(class(ans))
    # CHECK WARNINGS!
    
    # ssdFit -
    set.seed(1953)
    s = rnorm(1000)
    ans = ssdFit(x = s)
    print(ans)
    print(class(ans))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnits)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit2D.R")
    printTextProtocol(testResult)
}
   
   
################################################################################

