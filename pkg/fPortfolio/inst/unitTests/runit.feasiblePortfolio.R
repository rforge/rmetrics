
test.feasiblePortfolio.ConstrainedMV = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
   
 
test.feasiblePortfolio.RandomWeights.LongOnly = 
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification + Random Weights:
    spec = portfolioSpec()
    nAssets = ncol(data)
    Weights = runif(nAssets, 0, 1)
    Weights = Weights/sum(Weights)
    setWeights(spec) <- Weights
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.MV.Short = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints are ignored:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasibleShortMVPortfolio.Short = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints can natursally also defined as:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = .feasibleShortMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}




################################################################################





test.feasiblePortfolio.CVaR.LongOnly.Alpha =
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setWeights(spec) = rep(1/4, 4)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = feasiblePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}

