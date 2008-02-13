
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
# FUNCTION:                    DESCRIPTION:   
#  solveRdonlp2                 Calls Spelucci's donlp2 solver                  
################################################################################


test.solveRdonlp2.MV.LongOnly.MinRisk <- 
    function()
{ 
    # Given Target Return Minimize Risk
    
    if (require(Rdonlp2)) {
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(as.matrix(data))
        setSolver(spec) = "solveRdonlp2"
        setTrace(spec) = TRUE
        spec
        
        # Default Constraints:
        constraints = "LongOnly"
        constraints
     
        # Optimization:
        portfolio = solveRdonlp2(data, spec, constraints)
        portfolio
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
 
test.solverRdonlp2.CovRiskBudgets.MinRisk <- 
    function()
{     
    # Given Target Return Minimize Risk
    #   ... but now we have a Quadratic Covariance Risk Budget Constraint!
    
    if (require(Rdonlp2)) {
        
        # Load Data:   
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(as.matrix(data))
        setSolver(spec) = "solveRdonlp2"
        setTrace(spec) = FALSE
        spec
        
        # Add Large Covariance Budget Constraints:
        constraints = c("minW[1:4]=0", "maxB[1:4]=1")
        ans = solveRdonlp2(data, spec, constraints)
        setWeights(spec) = ans$weights
        portfolio = feasiblePortfolio(data, spec, constraints)
        portfolio
        
        # Make Covariance Risk Budget Constraints active:
        setTargetReturn(spec) = mean(as.matrix(data))
        constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
        ans = solveRdonlp2(data, spec, constraints)
        setWeights(spec) = ans$weights
        portfolio = feasiblePortfolio(data, spec, constraints)
        portfolio
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRdonlp2.twoAssets =
    function()
{ 
    if (require(Rdonlp2)) {
        
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


################################################################################




# ------------------------------------------------------------------------------


test.solveRDonlp2 =
function()
{
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Load Data:
        Data = as.timeSeries(data(smallcap.ts))
        Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
        head(Data)
       
        # Set Specifications:
        Spec = portfolioSpec() 
        setSolver(Spec) = "Rdonlp2"
        setTargetReturn(Spec) = mean(Data@Data)
        
        # Set Constraints:
        Constraints = c(
            "minB[1:4]=0.15",
            "maxB[1:4]=0.35")
            
        ## still to be checked ...
        
        # Solve:
        ## ans = solveRDonlp2(Data, Spec, Constraints)
        ## ans = efficientPortfolio(Data, Spec, Constraints)  # Does not work !!
        
        # Plot:
        ## par(mfrow = c(2, 2), cex = 0.7)
        ## weightsPlot(ans)
        ## attributesPlot(ans)
        ## covRiskBudgetsPlot(ans)
    
        # Get Weights:
        ## getWeights(ans)
        
        # Get Risk Budgets:
        # getRiskBudgets(ans)                               # Does not work !!!!
        
    }

    # Return Value:
    return()
}


################################################################################

