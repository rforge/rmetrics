################################################################################
# FUNCTION:               			DESCRIPTION:
#  portfolioBacktest    	 		Returns an object of class fPFOLIOBACKTEST
#
#  portfolioBacktesting     		Performs a portfolio bactest
#  portfolioSmoothing       	Smoothes the weights of a portfolio backtest
################################################################################

portfolioBacktest <-
function(
    windows = list(
         windows = "equidistWindows",
         params = list(
            horizon = "12m")),
    strategy = list(
         strategy = "tangencyStrategy",
         params = list()),
    smoother = list(
         smoother = "emaSmoother",
         params = list(
            doubleSmoothing = TRUE,
            lambda = "3m",
            skip = 0,
            initialWeights = NULL)),
    messages = list() )
{
    # Description:
    #   Specifies a portfolio to be optimized from scratch

    # Example:
    #   portfolioBacktest()
   
    # Arguments:
    #   windows - rolling windows slot:
    #       windows - the name of the rollings windows function
    #       params - parameter list for windows settings:
    #           horizon - length of the rolling windows
    #   strategy - portfolio strategy slot:
    #       strategy - the name of the portfolio strategy function
    #       params - parameter list for strategy settings:
    #   smoother - smoother approach slot:
    #       smoother - the name of the portfolio weights smoother function
    #       params - parameter list for smoother settings:
    #           doubleSmoothing - a flag sould we double smooth the weights?
    #           lambda - length of the ema smoothing parameter
    #           skip - hoqw many periods should be skipped for smoothing ?
    #           initialWeights - vector of initial weights
   
    # FUNCTION:

    # Return Value:
    new("fPFOLIOBACKTEST",
        windows = windows,
        strategy = strategy,
        smoother = smoother,
        messages = messages)
}

################################################################################


portfolioBacktesting <-
    function(formula, data, spec = portfolioSpec(), constraints = "LongOnly", backtest = portfolioBacktest())
{
    # Description:
    #   Backtests a portfolio on rolling windows
   
    # Arguments:
    #   formula - a formula expression to select benchmark and assets
    #       from the data set
    #   data - portfolio data, an object of class fPFLOLIODATA
    #   spec - portfolio spec, an object of class fPFLOLIOSPEC
    #   constraints - portfolio constraints, a vector of character strings
    #   backtest - portfolio backtest, an object of class fPFLOLIOBACKTEST
   
    # Details:
    #   Allows for user specified rolling Windows
    #   Smoothing is separated andcan be usere specified
   
    # Example:
    #   ans = object = portfolioBacktesting(formula, data, spec, constraints, backtest)
   
    # FUNCTION:
   
    trace = TRUE
   
    # Formula, Benchmark and Asset Labels:
    benchmarkName = as.character(formula)[2]
    assetsNames = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    nAssets = length(assetsNames)
   
    # Trace the Specifications and Data Info:
    if(trace) {
   
        cat("\nPortfolio Backtesting:\n")
       
        cat("\nAssets:             ", assetsNames)
        cat("\nBenchmark:          ", benchmarkName)
       
        cat("\nStart Series:       ", as.character(start(data)))
        cat("\nEnd Series:         ", as.character(end(data)))
       
        cat("\n  Type:             ", getType(spec))
        cat("\n  Cov Estimator:    ", getEstimator(spec))
        cat("\n  Solver:           ", getSolver(spec))
       
        cat("\nPortfolio Windows:  ", getWindowsFun(backtest))
        cat("\n  Horizon:          ", getWindowsHorizon(backtest))
       
        cat("\nPortfolio Strategy: ", getStrategyFun(backtest))
       
        cat("\nPortfolio Smoother: ", getSmootherFun(backtest))
        cat("\n  doubleSmoothing:  ", getSmootherDoubleSmoothing(backtest))
        cat("\n  Lambda:           ", getSmootherLambda(backtest))
    }

    # We invest in the "Strategy" or (return) efficient Portfolio:
    if(trace) {
        cat("\n\nPortfolio Optimization:")
        cat("\nOptimization Period\tTarget\tBenchmark\t Weights\n")
    }

    # Create Rolling Windows:
    windowsFun = match.fun(getWindowsFun(backtest))
    rollingWindows = windowsFun(data, backtest)
    from = rollingWindows$from
    to = rollingWindows$to

    # Roll the Portfolio:
    strategyFun = match.fun(getStrategyFun(backtest))
    strategyList = list()
    
    # WC: track the sigma over time:
    Sigma = NULL
     
    for (i in 1:length(from)) {

        # Optimize the Portfolio:
        pfSeries = window(data[, assetsNames], start = from[i], end = to[i])
        bmSeries = window(data[, benchmarkName], start = from[i], end = to[i])
        pfSeries = portfolioData(pfSeries, spec)
        Sigma = c(Sigma, mean(diag(getSigma(pfSeries))))
        strategy = strategyFun(data = pfSeries, spec, constraints, backtest)
        strategyList[[i]] = strategy
   
        # Trace Optionally the Results:
        if (trace) {

            cat(as.character(from[i]), as.character(to[i]))

            spReturn = as.vector(getTargetReturn(strategy))
            cat("\t", round(spReturn[1], digits = 3))

            bmReturn = mean(series(bmSeries))
            cat("\t", round(bmReturn, digits = 3))

            nAssets = length(assetsNames)
            weights = round(getWeights(strategy), digits = 3)
            cat("\t")
            for (i in 1:length(assetsNames)) cat("\t", weights[i])         
            cat("\t  * ", round(sum(weights), 2))

            cat("\n")
        }
       
    }
   
    # Extract Portfolio Investment Weights for the current period:
    weights = NULL
    for (i in 1:length(strategyList)) 
        weights = rbind(weights, getWeights(strategyList[[i]]))
    rownames(weights) = as.character(to)
    colnames(weights) = assetsNames
   
    # Compose Result:
    ans = list(
        formula = formula,
        data = data,
        spec = spec,
        constraints = constraints,
        backtest = backtest,
       
        benchmarkName = benchmarkName,
        assetsNames = assetsNames,
        weights = weights,
       
        strategyList = strategyList,
        
        Sigma = Sigma)
   
    # Return Value:
    invisible(ans)
}

# ------------------------------------------------------------------------------


portfolioSmoothing <-
function(object, backtest)
{
    # Description:
    #   Flexible Weights Smoother Function
   
    # Arguments:
    #   object - an object as returned by the function portfolioBacktesting
    #	backtest - an S4 class object of 'FPFOLIOBACKTEST'
    
    # Example:
    #   object =
    #   portfoloioSmoothing(object)
   
    # FUNCTION:
   
    # Backtest Settings:
    formula = object$formula
    data = object$data
    spec = object$spec
    constraints = object$constraints
    backtest = object$backtest = backtest
    benchmarkName = object$benchmarkName
    assetsNames = object$assetsNames
    weights = object$weights
	skip = getSmootherSkip(backtest)
    if (skip > 0) weights = weights[-(1:skip), ]
    nAssets = ncol(weights)
   
    # Add Smooth weights to Backtest object:
    print("smooth ...")
    smoother = match.fun(getSmootherFun(backtest))
    smoothWeights = object$smoothWeights = smoother(weights, spec, backtest)
   
    # Compute Monthly Assets and Benchmark Returns:
    print("aggregate ...")
    ow <- options("warn")
    options(warn = -1)
    monthlyAssets = object$monthlyAssets =
        applySeries(data[, assetsNames], by = "monthly", FUN = colSums)
    monthlyBenchmark = object$monthlyBenchmark =
        applySeries(data[, benchmarkName], by = "monthly", FUN = colSums)
    options(ow)   
       
    # Compute Offset Return of Rolling Portfolio compared to Benchmark:
    print("offset ...")
    cumX = colCumsums(data[, benchmarkName])
    lastX <- window(cumX, start = start(cumX), end = rownames(weights)[1] )
    offsetReturn = as.vector(lastX[end(lastX),])
    names(offsetReturn) <- as.character(end(lastX))
    object$offsetReturn <- offsetReturn

    # Backtest Return Series:
    Datum = as.vector(rownames(smoothWeights))
    nDatum = length(Datum)
    Portfolio = Benchmark = NULL
    for (i in 1:(nDatum-1)) {
        Portfolio = rbind(Portfolio, as.vector((
            as.matrix(monthlyAssets[Datum[i+1], ]) %*% smoothWeights[Datum[i], ])))
        Benchmark = rbind(Benchmark, as.vector(monthlyBenchmark[Datum[i+1], ]))
    }
   
    P = timeSeries(data = Portfolio, charvec = Datum[-1], units = "Portfolio")
    object$portfolioReturns = portfolio = colCumsums(P)
    object$P = P
   
    B = timeSeries(data = Benchmark, charvec = Datum[-1], units = "Benchmark")
    object$benchmarkReturns = benchmark = colCumsums(B)
    object$B = B
     
    daily = colCumsums(data[, benchmarkName])
    Daily = window(daily, start = start(portfolio), end = end(portfolio))

    portfolio = portfolio - portfolio[1] + Daily[1]
    benchmark = benchmark - benchmark[1] + Daily[1]
   
# Do Plot:
#ylim = range(c(as.vector(benchmark), as.vector(portfolio), as.vector(daily)))
#plot(daily, type = "l", ylim = ylim)
#
#lines(benchmark, lwd = 2, col = "blue")
#lines(portfolio, lwd = 2, col = "red")
   # points(benchmark, lwd = 2, pch = 19, col = "blue")
#    points(portfolio, lwd = 2, pch = 19, col = "red")
   
    # Add to backtest:
    object$portfolio = portfolio
    object$benchmark = benchmark
   
    # Backtest Statistics:
    P = as.vector(P)
    B = as.vector(B)
    Stats = c(sum(P, na.rm = TRUE), sum(B))
    Stats = rbind(Stats, c(mean(P, na.rm = TRUE), mean(B)))
    Stats = rbind(Stats, c(sd(P, na.rm = TRUE), sd(B)))
    Stats = rbind(Stats, c(min(P, na.rm = TRUE), min(B)))
    colnames(Stats) = c(
        "Portfolio",
        "Benchmark")
    rownames(Stats) = c(
        "Total Return",
        "Mean Return",
        "StandardDev Return",
        "Maximum Loss")
    object$stats = Stats
   
  # Annual Lines:
#    YYYY = as.character(1990:2010)
#    for (year in YYYY)
#        abline(v = as.POSIXct(paste(year, "-01-01", sep ="")), col = "green")
   
    # Return Value:
    object
} 

################################################################################
