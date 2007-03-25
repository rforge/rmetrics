
# Load fPortfolio:
require(fPortfolio)


# My directory wher to find the Rmetrics functions ...
if (Sys.getenv("USERNAME") == "myself"){
    DIR = "C:\\Documents and Settings\\myself\\Desktop\\Oliver\\R2\\"
}
if (Sys.getenv("USERNAME") == "Student"){
    DIR = "C:\\dip\\code\\R 0216\\"
}

# Load Portfolio General S4 Extractors:
source(file = paste(DIR, "GeneralS4Extractors.R", sep = ""))

# Load Data, Specification and Constraints:
source(file = paste(DIR, "11-PortfolioData.R", sep = ""))
source(file = paste(DIR, "12-PortfolioSpecifications.R", sep = ""))
source(file = paste(DIR, "13-PortfolioConstraints.R", sep = ""))

# Load Portfolio Class:
source(file = paste(DIR, "21-PortfolioClass.R", sep = ""))  
source(file = paste(DIR, "22-PortfolioExtractors.R", sep = "")) 
source(file = paste(DIR, "23-PortfolioPlots.R", sep = "")) 

# Short Selling Mean-Variance Portfolio Optimization:
source(file = paste(DIR, "31-ShortMVPortfolio.R", sep = ""))
source(file = paste(DIR, "32-ConstrainedMVPortfolio.R", sep = ""))




################################################################################
# Load Portfolio Example Data Set:
#  dutchPortfolioData           Example Data from Egels Diploma Thesis
#  usPortfolioData              Example Data
#  sm132PortfolioData           Data from Scherer, Martin; Cheapter 1.32


    Data = dutchPortfolioData()
    Data
    
    Spec = portfolioSpec()
    Spec
    
    Constraints = NULL
    Constraints


################################################################################   
# Short Selling Mean-Variance Portfolio Optimization:
# .feasibleShortMVPortfolio      Returns a feasible MV portfolio
# .tangencyShortMVPortfolio      Returns the tangency MV portfolio
# .frontierShortMVPortfolio      Returns a frontier MV portfolio
# .minvarianceShortMVPortfolio   Returns the minimum variance portfolio
# .shortMVFrontier               Returns the EF of a short selling MV portfolio


# Test Single Portfolio Functions:

    # Equal Weights Portfolio:
    feasiblePortfolio(Data, Spec)
    .feasibleShortMVPortfolio(Data, Spec)
    
    # Capital Market Line:
    cmlPortfolio(Data, Spec)
    .cmlShortMVPortfolio(Data, Spec)
    
    # Capital Market Line:
    Spec = setRiskFreeRate(riskFreeRate = 1e-4)
    Spec = setRiskFreeRate(Spec, riskFreeRate = 1e-4)
    cmlPortfolio(Data, Spec)
    .cmlShortMVPortfolio(Data, Spec)
    
    # Tangency Portfolio:
    tangencyPortfolio(Data, Spec)
    .tangencyShortMVPortfolio(Data, Spec)
    
    # Minimum Variance Portfolio:
    minvariancePortfolio(Data, Spec)
    .minvarianceShortMVPortfolio(Data, Spec) 
    
    # ... again the tangency Portfolio:
    frontierPortfolio(Data, Spec) 
    .frontierShortMVPortfolio(Data, Spec) 
    
    
# Compute Minimum Variance Portfolio by Optimization:

    minvariancePortfolioFun = function(x, data, spec) {
        spec@portfolio$targetReturn = x
        ans = frontierPortfolio(data = data, spec = spec)
        getTargetRisk(ans)
    }  
    optimize(minvariancePortfolioFun, interval = range(Data$mu), data = Data,
        spec = Spec, tol = .Machine$double.eps^0.5)$objective
    getTargetRisk(minvariancePortfolio(Data, Spec)) 

    
# Compute Tangency Portfolio by Optimization:

    sharpeRatioFun = function(x, data, spec) {
        spec@portfolio$targetReturn = x
        ans = frontierPortfolio(data = data, spec = spec)
        (x - spec@portfolio$riskFreeRate) / getTargetRisk(ans)
    } 
    riskFreeRate = Spec@portfolio$riskFreeRate
    tg = optimize(sharpeRatioFun, interval = range(Data$mu), 
        data = Data, spec = Spec, max = TRUE, tol = .Machine$double.eps^0.5)
    (tg$maximum-riskFreeRate)/tg$objective
    getTargetRisk(cmlPortfolio(Data, Spec))
    
       
# Compute Portfolio Frontier:

    
    # Compare with:
    ans = .portfolioShortMVFrontier(data = Data, spec = Spec)   
    ans  
    
    ans = portfolioFrontier(data = Data, spec = Spec, 
        constraintsString = Constraints)   
    ans  

    
################################################################################
#  show.fPORTFOLIO               Print method for 'fPPORTFOLIO' objects   
#  plot.fPORTFOLIO               Plot method for objects of class fPFOLIO    
#  frontierPlot                  Plots efficient Frontier
#   .sharpeRatioLine              Adds Sharpe Ratio
#   .minimumVariancePlot          Adds Minimum Variance point
#   .tangencyPlot                 Adds Tangency Portfolio point and line
#   .capitalMarketLine            Adds Market Portfolio and Capital Market Line
#   .singleAssetPlot              Adds points of single asset portfolios
#   .equalWeightsPlot             Adds point of equal weights portfolio
#   .twoAssetPlots                Adds EF for all combinations of two assets
#   .wheelPiePlot                 Adds pie chart of weights on EF
#   .monteCarloPlot               Adds randomly produced feasible portfolios
#  weightsPlot                   Plots staggered weights
#  weightsPie
#  portfolioSlider               Portfolio Slider
#  frontierSlider                Efficient Frontier Slider           


# Generic print() Function:

    show(ans)
    print(ans) 


# Test Internal Plot Components:

    par(mfrow = c(1,1))
    N = dim(getWeights(ans))[2]
    frontierPlot(ans, xlim = c(0, 1.5), pch = 19, cex = 0.25)
    # Add ...
    .sharpeRatioPlot(ans, type = "l", col = "red", lty = 3)   
    .minvariancePlot(ans, col = "red", pch = 19, cex = 1.5)
    .tangencyPlot(ans, col = "blue", pch = 17, cex = 1.5)
    .singleAssetPlot(ans, col = rainbow(N), pch = 18, cex = 1.5)    
    .equalWeightsPlot(ans, col = "magenta", pch = 15, cex = 1.5)       
    .twoAssetsPlot(ans, col = "grey")
    .wheelPiePlot(ans)
    .monteCarloPlot(ans, mcSteps = 1000, cex = 0.01)
    
    .singleAssetPlot(ans, col = rainbow(N), pch = 18, cex = 1.5)
    .equalWeightsPlot(ans, col = "magenta", pch = 15, cex = 1.5) 
    grid()


# Try Generic interactive plot() Function:

    plot(ans)


# Display Staggered Barplot of Weights:

    weightsPlot(ans)
    weightsPie(tangencyPortfolio(Data, Spec))


# Test Slider Functions:
    
    weightsSlider(ans)  
    frontierSlider(ans)


################################################################################
# General S4 Extractor Functions:
#  isS4                          Checks if a function is a S4 object
#  getCall                       Extracts call slot from a S4 object 
#  getSpecifiaction              Extracts specification slot from a S4 object
#  getPortfolio   n              Extracts specification slot from a S4 object
#  getTitle                      Extracts title slot from a S4 object
#  getDescription                Extracts the description slot from a S4 object
#  getSlot                       Extracts a specified slot from a S4 object


# Check S4 Class:   

    isS4(ans)


# Extract General Slot by Name:

    getSlot(ans, "call")


# Extract Specific Slot by Function:

    getCall(ans)
    getSpecification(ans)
    getPortfolio(ans)
    getTitle(ans)
    getDescription(ans)    
    
  
################################################################################   
# Portfolio Extractor Functions:
#  getFrontier                      Extracts the efficient frontier
#  getWeights                       Extracts weights from a fPORTFOLIO object
#  getTargetReturn                  Extracts target return from a portfolio
#  getTargetRisk                    Extracts target riks from a portfolio


# Extract Efficient Frontier:

    getFrontier(ans)
    plot(getFrontier(ans))

   
# Extract Portfolio Weights:

    getWeights(ans)

# Extract Portfolio Target Returns and Target Risk:

    getTargetReturn(ans)
    getTargetRisk(ans)


################################################################################
# Constraints:
#  setConstraints          Transforms constraint strings into a list value
#  getConstraints          Transforms a constraint list value into strings



# Set Default Constraints:

    setConstraints(Data, Spec)

# Set Box and Sector Constraints:

    Constraints = c(
        "minW[c(2,4)] = 0.1", 
        "maxW[3] = 0.7", 
        "minsumW[2:3] = 0.2", 
        "maxsumW[c(1,2,4)] = 0.9" )
    ConstraintsMatrix = setConstraints(Data, Spec, Constraints)
    ConstraintsMatrix

# Reconstruct Constraint Strings:

    getConstraints(ConstraintsMatrix)
  
    
################################################################################
# Robust Estimation:


    Data = usPortfolioData()
    portfolioStatistics(Data)
    
    Spec = portfolioSpec()
    
    Spec = setEstimator(Spec, estimator = c(mean = "mcd", cov = "mcd"))
    portfolioStatistics(Data, Spec)
    
    Spec = setEstimator(Spec, estimator = c(mean = "mean", cov = "mcd"))
    portfolioStatistics(Data, Spec)
    
    Spec = setEstimator(Spec, estimator = c(mean = "mcd", cov = "cov"))
    portfolioStatistics(Data, Spec)
    

################################################################################
# Covariance Ellipses Plot:


require(MASS)
X = as.matrix(usPortfolioData())
x = list(Robust = MASS::cov.mcd(X)$cov, Classical = cov(X))
covEllipsesPlot(x[1])
covEllipsesPlot(x)


################################################################################

