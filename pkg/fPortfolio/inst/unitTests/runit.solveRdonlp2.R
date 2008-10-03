
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION: 
#  test.solveRdonlp2.LongOnly  
#  test.solveRdonlp2.CovRiskBudgets 
#  test.solveRdonlp2.twoAssets  
#  test.solveRdonlp2.boxConstraints 
################################################################################


if (FALSE) {
# Check before use ...


# ------------------------------------------------------------------------------


test.solveRdonlp2.LongOnly <- 
    function()
{ 
    # Given Target Return Minimize Risk
    
    # Requires Package Rdonlp2:
    if (require(Rdonlp2)) {
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))

        # Specification:
        spec = portfolioSpec()
        setType(spec) = "MV"
        setOptimize(spec) = "minRisk"
        setTargetReturn(spec) = mean(colMeans(data))
        setSolver(spec) = "solveRdonlp2"
        print(spec)
        
        # Default Constraints:
        constraints = "LongOnly"
        print(constraints)
     
        # Optimization:
        portfolio = solveRdonlp2(data, spec, constraints)
        print(portfolio)
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
 
test.solveRdonlp2.CovRiskBudgets <- 
    function()
{     
    # Given Target Return Minimize Risk
    #   ... but now we have a Quadratic Covariance Risk Budget Constraint!
    
    # Requires Package Rdonlp2:
    if (require(Rdonlp2)) {
        
        # Load Data:   
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))
        
        # Specification:
        spec = portfolioSpec()
        setType(spec) = "MV"
        setOptimize(spec) = "minRisk"
        setSolver(spec) = "solveRdonlp2"
        print(spec)
        
        # Add Covariance Budget Constraints:
        setTargetReturn(spec) = mean(colMeans(data))
        constraints = c("minW[1:4]=0", "maxB[1:4]=1")
        ans = solveRdonlp2(data, spec, constraints)
        setWeights(spec) = ans$weights
        portfolio = feasiblePortfolio(data, spec, constraints)
        print(portfolio)
        
        # Make Covariance Risk Budget Constraints active:
        setTargetReturn(spec) = mean(colMeans(data))
        constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
        ans = solveRdonlp2(data, spec, constraints)
        setWeights(spec) = ans$weights
        portfolio = feasiblePortfolio(data, spec, constraints)
        print(portfolio)
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRdonlp2.twoAssets =
    function()
{ 
    # Requires Package Rdonlp2:
    if (require(Rdonlp2)) {
        
        # Direct Access:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG")]
        print(head(data))
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(colMeans(data))
        print(spec)
        
        # Default Constraints:
        constraints = "LongOnly"
        print(constraints)
        
        # RDonlp2:
        ans = solveRdonlp2(data, spec, constraints) 
        print(ans) 
        print(ans$weights)
        
    }
    
    # Return Value:
    return()
}


################################################################################


test.solveRdonlp2.boxConstraints =
function()
{
    if (require(Rdonlp2)) {
  
        # Load Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
       
        # Set Specifications:
        spec = portfolioSpec() 
        setSolver(spec) = "solveRdonlp2"
        setTargetReturn(spec) = mean(colMeans(data))
        
        # Set Constraints:
        constraints = c(
            "minB[1:4]=0.15",
            "maxB[1:4]=0.35")
        
        # Solve:
        ans = solveRdonlp2(data, spec, constraints)
        print(ans)
        
    }

    # Return Value:
    return()
}


} # Check not yet done ...


################################################################################

