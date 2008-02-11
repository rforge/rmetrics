
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
# FUNCTION:                     PORTFOLIO CLASS:
#  'fPORTFOLIO'                  S4 Portfolio Class
#  portfolioFrontier             Returns the efficient frontier of a portfolio
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  efficientPortfolio            Returns a frontier portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
# FUNCTION:                     PRINT AND PLOT METHODS: 
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects          
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
#  summary.fPORTFOLIO            S3 Summary method for 'fPORTFOLIO' objects
# FUNCTION:                     EDUCATIONAL PORTFOLIO SLIDERS: 
#  weightsSlider                 Weights Slider           
#  frontierSlider                Efficient Frontier Slider                   
################################################################################


test.fPORTFOLIO =
function()
{ 
    # Class:
    getClass("fPORTFOLIO")
  
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioFrontier =
function()
{ 
    # Arguments:
    # portfolioFrontier(data, spec = portfolioSpec(), constraints = NULL, 
    #   title = NULL, description = NULL) 
   
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 10
    spec
   
    # Calculation of Long Only Minimum Variance Portfolio
    Frontier = portfolioFrontier(data, spec)
    Frontier
    
    # Return Value:
    return()
}


################################################################################


test.feasiblePortfolio =
function()
{ 
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    # Get Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, times = 4)
    spec
    
    # Optimize Long Only Minimum Variance Portfolio:
    Portfolio = feasiblePortfolio(data, spec)  
    Portfolio                                                           
     
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.RDonlp2 =
function()
{ 
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Get Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
       
        # Set Default Specifications - Long Only MV Portfolio
        spec = portfolioSpec()
        setWeights(spec) = rep(1/4, times = 4)
        setSolver(spec)<-"Rdonlp2"
        spec
        
        # Constraints:
        constraints = NULL
        constraints
        
        # Optimize Long Only Minimum Variance Portfolio:
        Portfolio = feasiblePortfolio(data, spec, constraints)  
        Portfolio  
        
    }                                                     
     
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.lpsolve =
function()
{ 
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    # Get Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, times = 4)
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # Optimize Long Only Minimum Variance Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)  
    Portfolio                                                       
     
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio =
function()
{ 
    # Arguments:
    # efficientPortfolio(data, spec = portfolioSpec(), constraints = NULL) 
 
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(seriesData(data))
    spec
    
    # Constraints:
    constraints = NULL
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlPortfolio =
function()
{ 
    # Arguments:
    # cmlPortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Specifications:
    spec = portfolioSpec()
    spec
    
    # Portfolio:
    Portfolio = cmlPortfolio(data, spec)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio =
function()
{ 
    # Arguments:
    # tangencyPortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specifications:
    spec = portfolioSpec()
    spec
    
    # Portfolio:
    Portfolio = tangencyPortfolio(data, spec)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio =
function()
{ 
    # Arguments:
    # minvariancePortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specifications:
    spec = portfolioSpec()
    spec
    
    # Portfolio:
    Portfolio = minvariancePortfolio(data, spec)
    Portfolio

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.RDonlp2 =
function()
{ 
    # Arguments:
    # minvariancePortfolio(data, spec = portfolioSpec(), constraints = NULL)
    
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specifications:
        spec = portfolioSpec()
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Portfolio:
        Portfolio = minvariancePortfolio(data, spec)
        Portfolio
        
    }
    
    # Return Value:
    return()
}


################################################################################


test.show =
function()
{ 
    # Load Data::
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 10
    spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec)
    show(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.RQuadprog =
function()
{ 
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
   
    # Specifications:
    spec = portfolioSpec()
    
    # Constraints:
    constraints = "LongOnly"
   
    # Frontier:
    Frontier = object = portfolioFrontier(data, spec, constraints)
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .weightsWheel(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
    
    # Plot Ask:
    # plot(Frontier, which = "ask")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.Rdonlp2 =
function()
{   
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        Data = as.timeSeries(data(smallcap.ts))
        Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
       
        # Specifications:
        Spec = portfolioSpec()
        setSolver(Spec)<-"Rdonlp2"
        
        # Constraints:
        Constraints = "LongOnly"
        
        # Frontier:
        Frontier = portfolioFrontier(Data, Spec, Constraints)
       
        # Plot:
        par(mfrow = c(1, 1))
        frontierPlot(Frontier)
        .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
        .tangencyPlot(Frontier, col = "green") 
        .singleAssetPlot(Frontier, col = "red", cex = 1.5)
        .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
        .twoAssetsPlot(Frontier, col = "grey")
        .weightsWheel(Frontier)
        .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
        .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
        
        # Plot Ask:
        # plot(Frontier, which = "ask")
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.RlpSolve =
function()
{     
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
   
    # Specifications:
    spec = portfolioSpec()
    setType(spec) <- "CVaR"
    
    # Constraints:
    constraints = "LongOnly"
    
    # Frontier:
    Frontier = portfolioFrontier(data, spec, constraints)
   
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .weightsWheel(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
    
    # Plot Ask:
    # plot(Frontier, which = "ask")
    
    # Return Value:
    return()
}


################################################################################


test.weightsSlider =
function()
{ 
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 15
    spec
   
    # Frontier:
    Frontier = portfolioFrontier(data, spec)
    Frontier
    
    # Slider:
    # weightsSlider(Frontier) 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierSlider =
function()
{ 
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 15
    spec
   
    # Frontier:
    Frontier = portfolioFrontier(data, spec)
    Frontier
    
    # Slider:
    # frontierSlider(Frontier)                                            
    NA 
    
    # Return Value:
    return()
}


################################################################################

