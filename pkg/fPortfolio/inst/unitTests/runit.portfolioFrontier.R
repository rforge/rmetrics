
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Founbdattion, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
################################################################################


test.portfolioFrontier.MV.Short <- 
    function()
{ 
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setNFrontierPoints = 10
    spec
    
    # Constraints:
    constraints = "Short" 
    constraints
    
    # Frontier:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


test.portfolioFrontier.MV.LonglOnly <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 5
    spec
    
    # Constraints: 
    constraints = "LongOnly"
    constraints
    
    # Portfolio Frontier:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioFrontier.MV.LongOnly.RDonlp2 =
function()
{   
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        setNFrontierPoints(spec) = 5
        spec
        
        # Constraints: 
        constraints = "LongOnly"                        
        constraints
        
        # Portfolio Frontier:
        Frontier = portfolioFrontier(data, spec, constraints)
        Frontier 
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioFrontier.LongOnly.boxGroupConstraints <-
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


test.portfoliFrontier.MV.LongOnly.covRiskBudgetConstraints <- 
    function()
{    
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        setNFrontierPoints(spec) = 10
        spec
        
        # Add Risk Budgets:
        constraints = c("minW[1:4]=0", "maxB[1:4]=0.8")
        constraints
           
        # Frontier:
        Frontier = portfolioFrontier(data, spec, constraints)
        Frontier
    
    }
      
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioVrontier.MV.LongOnly.covRiskBudgetConstraints <- 
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
        # getRiskBudgets(ans)                               # Does not work !!!!
        
    }

    # Return Value:
    return()
}


################################################################################


test.portfolioFrontier.CVaR.LongOnly = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setNFrontierPoints(spec) = 10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioFrontier.CVaR.LongOnly.TwoAssets =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Frontier = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


################################################################################

