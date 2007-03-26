
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
# FUNCTION                      PORTFOLIO CLASS:
#  'fPORTFOLIO'                  S4 Portfolio Class
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
#  efficientPortfolio            Returns a frontier portfolio
# FUNCTION:                     PORTFOLIO FRONTIER:
#  portfolioFrontier             Returns the efficient frontier of a portfolio
# FUNCTION:                     PRINT AND PLOT METHODS:        
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects   
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
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


# ------------------------------------------------------------------------------


test.fPORTFOLIO =
function()
{ 
    NA
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio =
function()
{ 
    # Setting Data:
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    setWeights(Spec) = rep(1/8, times = 8)
    Spec
    
    # Calculation of Long Only Minimum Variance Portfolio:
    Portfolio = feasiblePortfolio(Data, Spec)
    Portfolio                                                             
   
    # Try Rdonlp2:
    require(Rdonlp2)
    setSolver(Spec)<-"Rdonlp2"
    Spec
    Portfolio = feasiblePortfolio(Data, Spec)
    Portfolio
     
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlPortfolio =
function()
{ 
    # Setting Data:
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    Spec
    
    # Calculation of Long Only Minimum Variance Portfolio:
    Portfolio = cmlPortfolio(Data, Spec)
    Portfolio
    
    # Check:
    target = round(sum(getWeights(Portfolio)), 3)
    current = 1
    checkIdentical(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio =
function()
{ 
    # Setting Data
    Data = usPortfolioData()
    Data
    
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    Spec
    
    # Calculation of Long Only Minimum Variance Portfolio
    Portfolio = tangencyPortfolio(Data, Spec)
    Portfolio
    
    # Check:
    target = round(sum(getWeights(Portfolio)), 3)
    current = 1
    checkIdentical(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio =
function()
{ 
    # Setting Data
    Data = usPortfolioData()
    Data
    
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    Spec
    
    # Calculation of Long Only Minimum Variance Portfolio
    Portfolio = minvariancePortfolio(Data, Spec)
    Portfolio
    
    # Check:
    target = round(sum(getWeights(Portfolio)), 3)
    current = 1
    checkIdentical(target, current)
    
    # Try RDonlp2:
    require(Rdonlp2)
    setSolver(Spec)<-"Rdonlp2"
    Spec
    Portfolio = minvariancePortfolio(Data, Spec)
    Portfolio
    
    # Check:
    target = round(sum(getWeights(Portfolio)), 3)
    current = 1
    checkIdentical(target, current)
 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio =
function()
{ 
    # Setting Data
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    setTargetReturn(Spec)<-mean(seriesData(Data))
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio
    Portfolio = efficientPortfolio(Data, Spec)
    Portfolio
    
    # Check:
    target = round(sum(getWeights(Portfolio)), 3)
    current = 1
    checkIdentical(target, current)
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioFrontier =
function()
{ 
    # Setting Data
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications - Long Only MV Portfolio
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Check:
    target = round(sum(rowSums(getWeights(Frontier))))
    current = 32
    checkIdentical(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.show =
function()
{ 
    # Setting Data:
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Show and Print:
    print(Frontier)
    show(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot =
function()
{ 
    # Setting Data:
    Data = sm132PortfolioData()
    Data
   
    # Setting Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Try RDonlp2:
    require(Rdonlp2)
    setSolver(Spec)<-"Rdonlp2"
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Plot:
    # par(mfrow = c(1, 1))
    plot(Frontier)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsSlider =
function()
{ 
    # Setting Data:
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Try:
    # weightsSlider(Frontier)                                            # CHECK
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierSlider =
function()
{ 
    # Setting Data:
    Data = usPortfolioData()
    Data
   
    # Setting Default Specifications:
    Spec = portfolioSpec()
    Spec
   
    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(Data, Spec)
    Frontier
    
    # Try:
    # frontierSlider(Frontier)                                           # CHECK
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

