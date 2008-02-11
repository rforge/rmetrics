


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


test.efficientPortfolio.CVaR.LongOnly.Alpha <- 
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
    setAlpha(spec) = 0.10
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


################################################################################
# Capital Market Line:



test.cmlPortfolio.MV <- 
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
    
    # Optimization:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlPortfolio.MV.LongOnly <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # CML Portfolio - Equals Tangency Portfolio:
    constraints = "LongOnly"
    constraints
    
    # Portfolio - Equals tangency Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------



test.cmlPortfolio.CVaR.LongOnly <- 
    function()
{
    # Linear Programming - CVaR Portfolio:
    #   the return is fixed, we minimie the CVaR
    
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
    
    # CVaR Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################
# Tangency Portfolio


test.tangencyPortfolio.MV.LongOnly <- 
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
    
    # Tangency Portfolio:
    Portfolio = tangencyPortfolio(data, spec,  constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.MV.boxConstrained <- 
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
    constraints = "maxW[1:nAssets]=0.3"
    constraints
    
    # Tangency Portfolio:
    Portfolio = tangencyPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.CVaR.LongOnly <- 
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
    setAlpha(spec) = 0.10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Portfolio:
    Portfolio = tangencyPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################
# Minvariance Portfolio


test.minvariancePortfolio.MV.LongOnly = 
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
    
    # Minimum Variance Portfolio:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.MV.boxConstrained = 
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
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Minimum Variance Portfolio:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################

