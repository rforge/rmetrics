
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

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
# FUNCTION:                    DESCRIPTION: 
#  solveShortExact              Solves Analytically Unlimited Short Portfolio 
################################################################################


.DEBUG = NA


# ------------------------------------------------------------------------------


solveShortExact <- 
    function(data, spec, constraints)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Solves Analyticallu Unlimited Short Portfolio 
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)

    # Get Target Return - if NULL use Tangency Portfolio:
    targetReturn = spec@portfolio$targetReturn 
    if (is.null(targetReturn)) targetreturn = (a/b)*C0     
    
    # Get Target Risk:
    targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
    
    # Get Weights:
    weights = as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    
    # Prepare Output List:
    ans = list(
        weights = weights, 
        targetReturn = targetReturn, targetRisk = targetRisk)
        
    # For Debugging ...
    .DEBUG <<- ans

    # Return Value:
    ans
}


################################################################################

