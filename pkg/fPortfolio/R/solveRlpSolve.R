
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
#  solveRlpSolve                Calls linear programming solver
################################################################################


.DEBUG = NA


# ------------------------------------------------------------------------------


solveRlpSolve =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Linear Solver from R package lpSolve for Mean-CVaR Problems
    
    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints
    
    # Value of lp():
    #   direction - optimization direction, as entered 
    #   x.count - number of variables in objective function 
    #   objective - vector of objective function coefficients, 
    #       as entered 
    #   const.count - number of constraints entered 
    #   constraints - constraint matrix, as entered (not 
    #       returned by lp.assign or lp.transport) 
    #   int.count - number of integer variables 
    #   int.vec - vector of integer variables' indices, as entered 
    #   objval - value of objective function at optimum} 
    #   solution - vector of optimal coefficients 
    #   status - numeric indicator: 0 = success, 2 = no feasible 
    #       solution 
   
    # Note:
    #   This function requires to load the contributed R package
    #   lpSolve explicitely!
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    
    # Get Specifications:
    mu = getMu(data) 
    Sigma = getSigma(data)
    nAssets = getNumberOfAssets(data)

    # Extracting data from spec:
    targetReturn = getTargetReturn(spec)  
    stopifnot(is.numeric(targetReturn))
    
    # Get quantile measure alpha:
    targetAlpha = spec@portfolio$targetAlpha
    
    # Scenarios:
    Data = getSeries(data)
    colNames = colnames(Data)
    rowNames = rownames(Data)
    assets = dim(Data)
    m = assets[1]
    w = assets[2]
    
    if (nAssets == 2) {
        # Two Assets Portfolio:
        stopifnot(targetReturn >= min(mu))
        stopifnot(targetReturn <= max(mu))  
        names(targetReturn) <- spec@model$estimator[1]
        weights = (targetReturn-mu[2]) / (mu[1]-mu[2])
        weights = c(weights, 1- weights)
        ans = list(
            weights = weights, 
            VaR = .varRisk(Data, weights, targetAlpha),
            solution = .varRisk(Data, weights, targetAlpha),
            CVaR = -.cvarRisk(Data, weights, targetAlpha),
            objval = .cvarRisk(Data, weights, targetAlpha),
            ierr = 0,
            solver = "twoAssetsCVaR",
            targetAlpha = targetAlpha)
    } else {
        # Compose objective function:
        Names = c("VaR", paste("e", 1:m, sep = ""), colNames)      
        f.obj = c(-1, rep(-1/(targetAlpha*m), m), rep(0, w))
        names(f.obj) = Names
        
        # Info on constraints - Constraint matrix:
        #   Example m=8 Data Records, and w=4 Assets
        #   
        #   VaR  es            weights          exposure
        #   x1   x2  ...  x9   x10 ... x13
        #       
        #    0    0       0    mu1     mu4      >= Mu
        #    0    0       0    1       1        == 1
        #               
        #   -1    1       0    r1.1    r4.1     >= 0
        #   -1    0  1    0    r1.2    r4.2     >= 0
        #   
        #   -1    0    1  0    r1.8    r4.8     >= 0
        #   -1    0       1    r1.9    r4.9     >= 0
        #   
        #   x2   >= 0    ...   x9   >= 0  
        #   x10  >= 0    ...   x13  >= 0
        
        # Compose Constraint Matrix:
        nX = 1 + m + w
        nY = 2 + m
        f.con = matrix(rep(0, nX*nY), ncol = nX)
        rownames(f.con) = c("Budget", "Return", rowNames)
        colnames(f.con) = c("VaR", paste("e", 1:m, sep = ""), colNames) 
        f.con[1, (2+m):(2+m+w-1)] = as.numeric(mu)
        f.con[2, (2+m):(2+m+w-1)] = 1 
        f.con[3:(m+2), 1] = 1 
        f.con[3:(m+2), 2:(m+1)] = diag(m)
        f.con[3:(m+2), (2+m):(2+m+w-1)] = seriesData(Data)
        
        # Box and Group Constraints:
        tmpConstraints = .setConstraints(data, spec, constraints)
        nConstraints = dim(tmpConstraints)
        append = cbind(matrix(0, ncol = 1+m, nrow = (nConstraints[1]-2)),
            tmpConstraints[3:nConstraints[1], 1:(nConstraints[2]-1)])
        f.con = rbind(f.con, append)
       
        # Set Directions:
        f.dir = c("==", "==", rep(">=", m))
        f.dir = c(f.dir, rep(">=", (nConstraints[1]-2)))
        names(f.dir) = rownames(f.con)
      
        # Compose Right Hand Side Vector:
        f.rhs = c(targetReturn, 1, rep(0, m))
        f.rhs = c(f.rhs, tmpConstraints[3:(nConstraints[1]), nConstraints[2]])
        names(f.rhs) = rownames(f.con)
        
        # Optimize Portfolio:
        ans = lp("max", f.obj, f.con, f.dir, f.rhs)
        class(ans) <- "list"
        
        # Prepare Output List:
        ans$weights = ans$solution[(m+2):(m+1+w)] 
        for(i in 1:w) {
            if(abs(ans$weights[i]) < sqrt(.Machine$double.eps)) 
                ans$weights[i] = 0
        }
        attr(ans$weights, "error") <- ans$ierr
        ans$weights[as.logical(ans$ierr)] = NA
        ans$VaR = ans$solution[1]
        ans$CVaR = -ans$objval 
        ans$ierr = ans$status
        ans$solver = "lpSolve"
        ans$targetAlpha = targetAlpha
    }
    
    # For Debugging ...
    .DEBUG <<- ans

    # Return Value:
    ans
}


################################################################################

