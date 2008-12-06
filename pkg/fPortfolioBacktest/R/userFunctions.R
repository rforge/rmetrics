################################################################################
# USER FUNCTIONS:
#  equidistWindows          Defines default equal distant rolling windows
#  tangencyStrategy         Defines default tangency strategy portfolio
#  emaSmoother              Defines default EMA weights smoother
################################################################################


equidistWindows <-
function(data, backtest = portfolioBacktest())
{
    # Description:
    #   Defines default equidistant rolling windows
   
    # Arguments:
    #   data - portfolio assets set, an object of class 'timeSeries'
    #   backtest - an object of class 'fPFOLIOBACKTEST'
   
    # Note:
    #   This is an example for a user defined windows function ...
   
    # Example:
    #   equidistWindows(as.timeSeries(data(LPP2005REC)))
   
    # FUNCTION:
   
    # Settings:
    horizon = getWindowsHorizon(backtest)
   
    # Rolling Windows:
    ans = rollingWindows(x = data, period = horizon, by = "1m")
   
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

tangencyStrategy <-
    function(data, spec = portfolioSpec(), constraints = "LongOnly", backtest = portfolioBacktest())
{ 
        strategyPortfolio <- tangencyPortfolio(data, spec, constraints)
        Status = getStatus(strategyPortfolio)
        
        # if tangency portfolio doesn't exist we take the minimum variance portfolio
        if(Status == 1)
            strategyPortfolio <- minvariancePortfolio(data, spec, constraints)
        
        # Return Value:
        strategyPortfolio
}


################################################################################
# PART III: SMOOTHER FUNCTION


emaSmoother <-
    function(weights, spec, backtest)
{
    # Description:
    #   A user defined weights smoother for portfolio backtesting
   
    # Arguments:
    #   weights - a numeric matrix of weights
    #   spec - portfolio spec, an object of class fPFLOLIOSPEC
    #   backtest - portfolio backtest, an object of class fPFLOLIOBACKTEST
   
    # Example:
    #   ans = portfolioBacktesting( ... )
    #   emaSmoother(ans$weights, spec, backtest)
   
    # FUNCTION:
   
    # EMA Function:
    ema <- function (x, lambda) {
        x = as.vector(x)
        lambda = 2/(lambda + 1)
        xlam = x * lambda
        xlam[1] = x[1]
        ema = filter(xlam, filter = (1 - lambda), method = "rec")
        ema[is.na(ema)] <- 0
        as.numeric(ema) }
       
    # Lambda:
    lambda = getSmootherLambda(backtest)
    lambdaLength = as.numeric(substr(lambda, 1, nchar(lambda) - 1))
    lambdaUnit = substr(lambda, nchar(lambda), nchar(lambda))
    stopifnot(lambdaUnit == "m")
    lambda = lambdaLength
   
    # Initial Weights
    nAssets = ncol(weights)
    initialWeights = getSmootherInitialWeights(backtest)
    if (!is.null(initialWeights)) weights[1, ] = initialWeights

    # Compute Exponentially Smoothed Weights:
    smoothWeights1 = NULL
    for (i in 1:nAssets) {
        # print("first smooth")
        EMA = ema(weights[, i], lambda = lambda)
        smoothWeights1 = cbind(smoothWeights1, EMA)
    }
   
    # Double Smoothing ?
    doubleSmooth = getSmootherDoubleSmoothing(backtest)
    if (doubleSmooth) {
        # print("second smooth")
        smoothWeights = NULL
        for (i in 1:nAssets) {
            EMA = ema(smoothWeights1[, i], lambda = lambda)
            smoothWeights = cbind(smoothWeights, EMA)
        }
    } else {
        smoothWeights = smoothWeights1
    }
   
    # Add Names:
    rownames(smoothWeights) = rownames(weights)
    colnames(smoothWeights) = colnames(weights)
   
    # Return Value:
    smoothWeights
}

################################################################################
