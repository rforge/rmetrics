
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
# FUNCTION:                     PORTFOLIO CLASS:
#  'fPORTFOLIO'                  S4 Portfolio Class
#  portfolioFrontier             Returns the efficient frontier of a portfolio
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
#  efficientPortfolio            Returns a frontier portfolio
# FUNCTION:                     PRINT AND PLOT METHODS:           
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
#  summary.fPORTFOLIO            S3 Summary method for 'fPORTFOLIO' objects
# FUNCTION:                     EDUCATIONAL PORTFOLIO SLIDERS: 
#  weightsSlider                 Weights Slider           
#  frontierSlider                Efficient Frontier Slider                   
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioClass, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


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


# ------------------------------------------------------------------------------


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
    
    # Get Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, times = 4)
    require(Rdonlp2)
    setSolver(spec)<-"Rdonlp2"
    spec
    
    # Optimize Long Only Minimum Variance Portfolio:
    Portfolio = feasiblePortfolio(data, spec)  
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
    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    spec
    
    # Calculation of Long Only Minimum Variance Portfolio:
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
    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    spec
    
    # Calculation of Long Only Minimum Variance Portfolio
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
    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    spec
    
    # Calculation of Long Only Minimum Variance Portfolio
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
    
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    require(Rdonlp2)
    setSolver(spec)<-"Rdonlp2"
    spec
    
    # Calculation of Long Only Minimum Variance Portfolio
    Portfolio = minvariancePortfolio(data, spec)
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
    setTargetReturn(spec)<-mean(seriesData(Data))
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


################################################################################


test.plot.RQuadprog =
function()
{ 
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    spec
    
    # Set Constraints:
    constraints = "LongOnly"
    constraints
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = object = portfolioFrontier(data, spec, constraints)
    Frontier
    
    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
    
    # Plot All:
    plot(Frontier, which = "all")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.RDonlp2 =
function()
{     
    # Try RDonlp2:
    require(Rdonlp2)
    
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Default Specifications:
    Spec = portfolioSpec()
    setSolver(Spec)<-"RDonlp2"
    Spec
    
    # Set Constraints:
    Constraints = "LongOnly"
    Constraints
    
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec, Constraints)
    Frontier
   
    # Plot:
    par(mfrow = c(1, 1))
    plot(Frontier, which = 1)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
    
    # Plot All:
    plot(Frontier, which = "all")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.RlpSolve =
function()
{     
    # Try RlpSolve:
    require(lpSolve)
    
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Default Specifications:
    Spec = portfolioSpec()
    setType(Spec) <- "CVaR"
    Spec
    
    # Set Constraints:
    Constraints = "LongOnly"
    Constraints
    
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec, Constraints)
    Frontier
   
    # Plot:
    par(mfrow = c(1, 1))
    plot(Frontier, which = 1)
    .minvariancePlot(Frontier, col = "red", pch = 19, cex = 1.5)  
    .tangencyPlot(Frontier, col = "green") 
    .singleAssetPlot(Frontier, col = "red", cex = 1.5)
    .equalWeightsPlot(Frontier, col = "blue", pch = 19, cex = 1.5)
    .twoAssetsPlot(Frontier, col = "grey")
    .wheelPiePlot(Frontier)
    .monteCarloPlot(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)  
    .sharpeRatioPlot(Frontier, pch = 19, col = "blue") 
    
    # Plot All:
    plot(Frontier, which = "all")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsSlider =
function()
{ 
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Try:
    weightsSlider(Frontier) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierSlider =
function()
{ 
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
   
    # Set Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Try:
    frontierSlider(Frontier)                                            
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    require(lpSolve)
    require(Rdonlp2)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

