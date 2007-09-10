
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
# FUNCTION:                    RISK BUDGETS:               
################################################################################


test.riskBudgets =
function()
{ 
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Set Default Specifications:
    Spec = portfolioSpec() 
    
    # Set Portfolio Constraints:
    Constraints = NULL
    portfolioConstraints(Data, Spec, Constraints)
    
    # Set Portfolio Constraints - Returns NULL:
    Constraints = "LongOnly"
    portfolioConstraints(Data, Spec, Constraints)
    
    # Set Portfolio Constraints - Returns "Short":
    Constraints = "Short"
    portfolioConstraints(Data, Spec, Constraints)
    
    # Set Portfolio Constraints - Returns Constraint String:
    Constraints = c("minW[1:4]=0.1", "maxW[1:4]=0.9")
    portfolioConstraints(Data, Spec, Constraints)                    # Check !!!
    
    # Set Risk Basket Constraints:
    Constraints = c("minB[1:4]", "maxB[1:4]")
    portfolioConstraints(Data, Spec, Constraints)                    # Check !!!
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioLongOnly =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Specifications:
    Spec = portfolioSpec() 
    setNFrontierPoints(Spec) <- 30
    
    # Set Constraints:
    Constraints = NULL
    ans = portfolioFrontier(Data, Spec, Constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(ans)
    attributesPlot(ans)
    covRiskBudgetsPlot(ans)

    # Get Weights:
    getWeights(ans)
    
    # Extract Risk Budgets:
    # getRiskBudgets(ans)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioLongOnly.BoxGroupConstraints =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Specifications:
    Spec = portfolioSpec() 
    setNFrontierPoints(Spec) <- 10
    
    # Set Constraints:
    Constraints = c(
        "minW[1:4]=0.1",
        "maxW[1:4]=0.9", 
        "minsumW[1:2]=0.2", 
        "maxsumW[3:4]=0.8")
    ans = portfolioFrontier(Data, Spec, Constraints)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPlot(ans)
    attributesPlot(ans)
    covRiskBudgetsPlot(ans)

    # Get Weights:
    getWeights(ans)
    
    # Get Risk Budgets:
    # getRiskBudgets(ans)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioLongOnly.RiskBudgetConstraints =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Specifications:
    Spec = portfolioSpec() 
    setSolver(Spec) = "Rdonlp2"
    setNFrontierPoints(Spec) <- 10
    
    # Set Constraints:
    Constraints = c(
        "minW[1:4]=0.1",
        "maxW[1:4]=0.9",
        "minsumW[1:4]=0.0",
        "maxsumW[1:4]=1.0",
        "minB[1:4]=0.05",
        "maxB[1:4]=0.35")
         
    # still to be done ...
        
    ## setTargetReturn(Spec) <- mean(Data@Data)   
    ## solveRDonlp2(Data, Spec, Constraints)
    
    ## ans = .efficientConstrainedMVPortfolio(Data, Spec, Constraints)
    
    ## efficientPortfolio(Data, Spec, Constraints)
    
    ## ans = portfolioFrontier(Data, Spec, Constraints)
    
    # Plot:
    ## par(mfrow = c(2, 2), cex = 0.7)
    ## weightsPlot(ans)
    ## attributesPlot(ans)
    ## covRiskBudgetsPlot(ans)

    # Get Weights:
    ## getWeights(ans)
    
    # Get Risk Budgets:
    # getRiskBudgets(ans)                                   # Does not work !!!!

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solveRDonlp2 =
function()
{
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
    ## ans = efficientPortfolio(Data, Spec, Constraints)       # Does not work !!!!
    
    # Plot:
    ## par(mfrow = c(2, 2), cex = 0.7)
    ## weightsPlot(ans)
    ## attributesPlot(ans)
    ## covRiskBudgetsPlot(ans)

    # Get Weights:
    ## getWeights(ans)
    
    # Get Risk Budgets:
    # getRiskBudgets(ans)                                   # Does not work !!!!

    # Return Value:
    return()
}
   

################################################################################

