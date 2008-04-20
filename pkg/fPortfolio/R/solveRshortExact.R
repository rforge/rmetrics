
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION: 
#  solveRshortExact              Solves Analytically Unlimited Short Portfolio 
################################################################################


solveRshortExact <- 
    function(data, spec, constraints)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Solves Analyticallu Unlimited Short Portfolio 
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # FUNCTION:
    
    # Get Statistics:
    if(!inherits(data, "fPFOLIODATA")) 
        data = portfolioData(data, spec)
    
    # Trace:
    trace = getTrace(spec)
    if(trace) 
        cat("\nPortfolio Optimiziation:\n Unlimited Short Exact ...\n")
    
    # What to optimize target risk or target return ?
    optimize = NA
    if (is.null(getWeights(spec)) & is.null(getTargetReturn(spec)))
    {
        optimize = "target return"
        if (is.null(getTargetRisk(spec))) 
            stop("Either target return or target risk must be specified")
    }
    if (is.null(getWeights(spec)) & is.null(getTargetRisk(spec)))
    {
        optimize = "target risk"
        if (is.null(getTargetReturn(spec))) 
            stop("Either target return or target risk must be specified")
    }
    if (is.na(optimize))
        stop("Weights, target return and target risk are inconsistent!")
    if (trace)
        cat("\nProblem:\n Optimize", optimize, "\n")
        
    # Covariance:
    mu = getMu(data)
    Sigma = getSigma(data)
    
    # Parameter Settings:
    C0 = 1
    one = rep(1, times = length(mu))
    invSigma = solve(Sigma)
    a = as.numeric(mu %*% invSigma %*% mu)
    b = as.numeric(mu %*% invSigma %*% one)
    c = as.numeric(one %*% invSigma %*% one)
    d = as.numeric(a*c - b^2)

    if (optimize == "target risk") 
    {
        # Get Target Return:
        # Note: for the Tangency Portfolio we have targetReturn = (a/b)*C0     
        targetReturn = getTargetReturn(spec) 
    
        # Compute Target Risk:
        targetRisk = sqrt((c*targetReturn^2 - 2*b*C0*targetReturn + a*C0^2) / d)
        
        # Objective:
        # DW: added 2008-04-20
        objective = targetRisk
        
        # trace:
        if (trace) {
            cat("\nTarget Return:\n ", targetReturn, "\n")    
            cat("\nTarget Risk:\n ", targetRisk, "\n") 
            cat("\nobjective:\n ", objective, "\n") 
        }   
    
    } else if (optimize == "target return")  {
        
        # DW 2008-02-12 added
        
        # Get Target Risk:
        targetRisk = getTargetRisk(spec)    
    
        # Compute Target Return:
        aq = c
        bq = -2*b*C0
        cq = a*C0^2 - d*targetRisk^2
        targetReturn = ( -bq + sqrt(bq^2 - 4*aq*cq) ) / (2*aq)
        
        # Objective:
        # DW: added 2008-04-20
        objective = targetReturn
        
        # trace:
        if (trace) {
            cat("\nTarget Return:\n ", targetReturn, "\n")    
            cat("\nTarget Risk:\n ", targetRisk, "\n") 
            cat("\nobjective:\n ", objective, "\n") 
        }   
    }
    
    # Compute Weights:
    weights = 
        as.vector(invSigma %*% ((a-b*mu)*C0 + (c*mu-b)*targetReturn )/d)
    if (trace) {
        cat("\nWeights:\n", weights, "\n\n")    
    }   
            
    # Prepare Output List:
    ans = list(
        solver = "solveRshortExact",
        optim = NA,
        weights = weights, 
        targetReturn = targetReturn, 
        targetRisk = targetRisk,
        status = 0, 
        objective = objective)

    # Return Value:
    ans
}


################################################################################

