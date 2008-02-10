


test.efficientPortfolio.MV.LongOnly = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(data@Data)
    setTrace(spec) <- TRUE
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

   
test.efficientPortfolio.LPP.LongOnly <- 
    function()
{   
    # Second Example:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "LPP"
    setEstimator(spec) <- "assetsLPM"
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.CVaR.LongOnly = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setType(spec) <- "CVaR"
    setTargetReturn(spec) <- mean(data@Data)
    setTrace(spec) <- TRUE
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


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


# ------------------------------------------------------------------------------


test.efficientPortfolio <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------