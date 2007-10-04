
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
#  solveRQuadprog               Calls Goldfarb and Idnani's QP solver
#  solveRDonlp2                 Calls Spelucci's donlp2 solver
#  solveRlpSolve                Calls linear programming solver                    
################################################################################


test.solveShortExact =
function()
{ 
    # Tod:
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
    
test.solveRQuadprog =
function()
{ 
    # Direct Access:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification
    spec = portfolioSpec()
    spec
    
    # Default Constraints:
    constraints = "LongOnly"
    constraints
    
    # Quadprog:
    setTargetReturn(spec) = mean(as.matrix(data))
    ans = solveRQuadprog(data, spec, constraints)
    ans
    
    # Check Termination Error:
    setTargetReturn(spec) = mean(as.matrix(10*data))
    ans = solveRQuadprog(10*data, spec, constraints)
    ans
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRQuadprog.twoAssets =
function()
{ 
    # Direct Access:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Default Constraints:
    constraints = "LongOnly"
    constraints
    
    # Quadprog:
    setTargetReturn(spec) = mean(as.matrix(data))
    ans = solveRQuadprog(data, spec, constraints) 
    ans
    
    # Check Termination Error:
    setTargetReturn(spec) = mean(as.matrix(10*data))
    ans = solveRQuadprog(10*data, spec, constraints)
    ans 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solverRDonlp2 =
function()
{ 
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Load Data:   
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Long Only Constraints:
        constraints = NULL
        constraints
        
        # Donlp2:
        setTargetReturn(spec) = mean(as.matrix(data))
        ans = solveRDonlp2(data, spec, constraints) 
        ans
        
        # Check Termination Error:
        setTargetReturn(spec) = mean(as.matrix(10*data))
        ans10 = solveRDonlp2(10*data, spec, constraints)
        ans10
        
        # Compare:
        ans$weights
        ans10$weights
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
 
test.solverRDonlp2.budgetConsatraints =
function()
{     
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Load Data:   
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        setTargetReturn(spec) = mean(as.matrix(data))
        spec
        
        # Add Budget Constraints:
        constraints = c("minW[1:4]=0", "maxB[1:4]=1")
        constraints
        ans = solveRDonlp2(data, spec, constraints)
        ans$weights
        
        # Scaled Donlp2 - Add Budget Constraints:
        constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
        constraints
        ans = solveRDonlp2(data, spec, constraints)
        ans$weights
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRDonlp2.twoAssets =
function()
{ 
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Direct Access:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(as.matrix(data))
        
        # Default Constraints:
        constraints = "LongOnly"
        constraints
        
        # RDonlp2:
        ans = solveRDonlp2(data, spec, constraints) 
        ans 
        ans$weights
        
    }
    
    # Return Value:
    return()
}



# ------------------------------------------------------------------------------


test.solveRlpSolve = 
function()
{
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(colAvgs(data))
    setTargetAlpha(spec) = 0.05
    setSolver(spec) <- "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    
    # CVaR Portfolio Optimization:  
    ans = solveRlpSolve(data, spec, constraints)
    ans$weights
    
    # Return Value:
    return()
}


################################################################################

