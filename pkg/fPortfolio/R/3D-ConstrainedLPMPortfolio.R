
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
# FUNCTION:                          SINGLE PORTFOLIOS:
#  .feasibleConstrainedLPMPortfolio    Returns a constrained feasible MV-PF
#  .tangencyConstrainedLPMPortfolio    Returns constrained tangency MV-PF
#  .cmlConstrainedLPMPortfolio         Returns constrained CML-Portfolio
#  .minvarianceConstraineLPMPortfolio Returns constrained min-Variance-PF
#  .efficientConstrainedLPMPortfolio    Returns a constrained frontier MV-PF
# FUNCTION:                          PORTFOLIO FRONTIER:
#  .portfolioConstrainedLPMFrontier    Returns the EF of a constrained MV-PF
################################################################################


.feasibleConstrainedLPMPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   In contrast to the functions *Portfolio(), which only require either 
    #   the statistics or the series the functions .*Portfolio() require both 
    #   as input.

    # Example:
    #   .feasibleConstrainedLPMPortfolio()
    
    # FUNCTION:
    
    stop("NYI")

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Return Value:
    stop(".feasibleConstrainedLPMPortfolio NYI")
}


#-------------------------------------------------------------------------------


.tangencyConstrainedLPMPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk, Return and Weight for the tangency portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .tangencyConstrainedLPMPortfolio()
    
    # FUNCTION:
 
    # Set Risk Free Rate:
    spec@portfolio$riskFreeRate = 0
    
    # Call cmlPorfolio unction:
    ans = .cmlConstrainedLPMPortfolio(data, spec, constraints)
    ans@call = match.call()
    ans@title = "Tangency Portfolio"
    
    # Return value:
    ans
}


#-------------------------------------------------------------------------------  

 
.cmlConstrainedLPMPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Computes Risk, Return and Weight for CML portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .cmlConstrainedLPMPortfolio()
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # FUNCTION:

    # Return Value:
    stop("cml.ConstrainedLPMPortfolio NYI")
}


#-------------------------------------------------------------------------------

   
.minvarianceConstrainedLPMPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Risk, Return and Weight for minimum variance portfolio
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Example:
    #   .minvarianceConstrainedLPMPortfolio()
    
    # FUNCTION:

    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Return Value:
    stop(".minvarianceConstrainedLPMPortfolio NYI")
}


#-------------------------------------------------------------------------------   


.efficientConstrainedLPMPortfolio = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Optimizes a LPM portfolio for a given desired return and a set of
    #   box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints


    # FUNCTION:
       
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma

    # Portfolio Optimization:
    ans = .efficientConstrainedMVPortfolio(data, spec, constraints)
    
    # Compute True Returns and Risk as Stabdard Deviation:
    weights = getWeights(ans)
    ans@portfolio$targetMean = weights %*% colMeans(x)
    ans@portfolio$targetStdev = sqrt(weights %*% cov(x) %*% weights)  
    
    # Update Title:
    ans@title = "Constrained LPM Portfolio"

    # Return Value:
    ans 
}


################################################################################


.portfolioConstrainedLPMFrontier = 
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Evaluates the EF for a given set of box and or sector constraints
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # FUNCTION:

    # Get Statistics:
    # if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    # mu = data$statistics$mu
    # Sigma = data$statistics$Sigma
    
    # Series:
    x = as.matrix(data$series)
    mu = colMeans(x)
    Sigma = cov(x)
    
    # Compute Efficient Frontier:
    ans = .portfolioConstrainedMVFrontier(data, spec, constraints)
    
    # Loop over Frontier:
    weights = getWeights(ans)
    targetMean = targetStdev = rep(0, times = dim(weights)[2])

    K = dim(weights)[1]
    for (k in 1:K) {
        targetWeight = weights[k,]
        targetMean[k] = mu %*% targetWeight 
        targetStdev[k] = sqrt ( targetWeight %*% Sigma %*% targetWeight )
    }
      
    # Update Title: 
    ans@call = match.call()
    ans@portfolio$targetMean = targetMean
    ans@portfolio$targetStdev = targetStdev
    ans@title = "Constrained LPM Portfolio"
    
    # Return Value:
    ans
}


################################################################################

