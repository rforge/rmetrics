
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
#  solveRsocp                   Seconde order cone programming solver
################################################################################


solveRsocp <- 
    function(data, spec, constraints)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Solves by second order cone programming
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # FUNCTION:
    
    stop("Not yet implemented")

    # Return Value:
    NA
}


################################################################################


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
#  solveRsocp                   Second Order Cone Programming
################################################################################



# *** TESTING PHASE ***


.SqrtMatrix <- 
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Square Root of a quadratic Matrix:
    
    # Example:
    #   A = matrix(c(1,.2,.2,.2,1,.2,.2,.2,1), ncol = 3)
    #   round(Sqrt(A) %*% Sqrt(A) - A, digits = 12)
    
    # FUNCTION:
    
    # Check if matrix is square:
    stopifnot(NCOL(x) == NROW(x))
    
    # One-dimensional ?
    if (NCOL(x) == 1) return(sqrt(as.vector(x)))
    
    # Square Root of a matrix:
    e <- eigen(x)
    V <- e$vectors
    ans <- V %*% diag(sqrt(e$values)) %*% t(V)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.solveRsocp <- 
    function(data, spec, constraints)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Second Order Cone Programming
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Note:
    #   This function is thought to maximize MV return for a fixed risk
    
    # FUNCTION:
    
    # Test Implementation for "LongOnly" MV Portfolio
    stopifnot(constraints == "LongOnly")
    
    # Get Statistics:
    if(!inherits(data, "fPFOLIODATA")) 
        data = portfolioData(data, spec)
    
    # Trace:
    trace = getTrace(spec)
    if(trace) 
        cat("\nPortfolio Optimiziation:\n Using Rquadprog ...\n\n")
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    
    # Number of Assets:
    nAssets = getNAssets(data)

    # Extracting Target Risk from Specification:
    targetRisk = getTargetRisk(spec)  
    stopifnot(is.numeric(targetRisk)) 
    
    # Optimize Portfolio:
    #if (nAssets == 2) {
        # Two Assets Portfolio:
        # ...
    #} else { 
        
        # Objective Function:
        f <- -mu
        
        # Long Only Constraints:
        A = rbind(
            .SqrtMatrix(Sigma),                        
            matrix(rep(0, times = nAssets), ncol = nAssets),
            matrix(rep(0, times = nAssets^2), ncol = nAssets))
        b = c(
            rep(0, nAssets),      # xCx
            0,                    # sum(x)
            rep(0, nAssets))      # x[i]>0
        C = rbind(
            rep(0, nAssets),      # xCx
            rep(-1, nAssets),     # sum(x)
            diag(nAssets))        # x[i]>0
        d = c(
            targetRisk,           # xCx = risk
            +1,                   # sum(x) <= 1
            rep(0, nAssets))      # x[i] > 0
        N <- c(
            nAssets,              # dim(C)
            1,                    # Full Investment
            rep(1, nAssets))      # Long
            
        # Control List:
        #   abs_tol = 1e-8, rel_tol = 1e-6, target = 0,
        #   max_iter = 500, Nu = 10, out_mode = 0,
        #   BigM_K = 2, BigM_iter = 5
        
        # Optimize:   
        fit = socp(f, A, b, C, d, N, control =
            list(
                abs_tol = 1e-16, 
                rel_tol = 1e-16, 
                target = 0,
                max_iter = 5000, 
                Nu = 10, 
                out_mode = 0,
                BigM_K = 2, 
                BigM_iter = 5
            ))

        # Prepare Output List:
        ans = list(
            fit = fit,
            weights = fit$x, 
            solver = "socp")
    #}

    # Return Value:
    ans
}


################################################################################

