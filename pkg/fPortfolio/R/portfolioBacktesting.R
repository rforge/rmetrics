
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA


################################################################################
# FUNCTION:                          DESCRIPTION:
#  portfolioBacktesting               Does portfolio backtesting
#  plot.portfolioBacktest             S3 Plot Method
#  print.portfolioBacktes             S3 Print Method
#  summary.portfolioBacktest          S3 Summary Method
################################################################################


portfolioBacktesting <- 
function(formula, data, spec = portfolioSpec(), constraints = "LongOnly",
    portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m",
    warmup = FALSE, trace = TRUE, title = "Backtesting")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Does backtesting on a simple rolling portfolio strategy

    # Arguments:
    #   formula - a formula expression which tells which assets from the
    #       data set have to be analyzed against a given Benchmark, e.g.
    #       LP40 ~ SBI + SPI + SWIIT backtests a portfolio composed of
    #       the Swiss Bond Index SBI, the Swiss Performance Index SPI, and
    #       the Swiss Immofunds Index, against the Pictet Benhmark Index
    #       LP40
    #   data - a multivariate 'timeSeries' object which at least contains
    #       the columns refefrenced in the formula expression.
    #   horizon - the historical investment horizon given in multiples of
    #       months. This is the size of the rolling window on which
    #       perfolio optimization will be performed.
    #   smoothing - the smoothing period of weights. Weights are exponentially
    #       smoothed given by this period measured in multiples of months.
    #   trace - a logical value. Should the backtesting procedure be traced?

    # Details:
    #   The rolling backtesting strategy is the following.
    #       1.  Consider a rolling window of financial returns of length
    #           'horizon'.
    #       2.  Compute the target Return for the portfolio based
    #           on this window and the 'data' listed in the 'formula'
    #           expression.
    #       3.  Extract the weights from the portfolio and
    #           perform an exponential moving average with a smoothing
    #           period defined by the value 'smoothing'.
    #       5.  Do an investing with the obtained weights for the next
    #           month and calculate in the next time step the resulting
    #           gain or loss.

    # FUNCTION:

    # Settings:
    ans = list()
    ans$formula = formula
    ans$data = data
    ans$spec = spec
    ans$constraints = constraints
    ans$portfolio = portfolio
    ans$title = title

    # Get Horizon Window Parameter:
    ans$horizon = horizon
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength

    # Get Smoothing Window Parameter:
    ans$smoothing = smoothing
    smoothingLength = as.numeric(substr(smoothing, 1, nchar(smoothing)-1))
    smoothingUnit = substr(smoothing, nchar(smoothing), nchar(smoothing))
    stopifnot(smoothingUnit == "m")
    smoothing = smoothingLength

    # Formula, Benchmark and Asset Labels:
    ans$benchmark = as.character(formula)[2]
    ans$assets = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    nAssets = length(ans$assets)

    # Trace the Specifications and Data Info:
    if(trace) {
        cat("\nPortfolio Backtesting:\n")
        cat("\nPortfolio Strategy: ", portfolio)
        cat("\nPortfolio Type:     ", getType(spec))
        cat("\nPortfolio Solver:   ", getSolver(spec))
        cat("\nAssets:             ", ans$assets)
        cat("\nBenchmark:          ", ans$benchmark)
        cat("\nInvestment Horizon: ", ans$horizon)
        cat("\nSmoothing Horizon:  ", ans$smoothing)
        cat("\nUpdate Period:      ", "1m")
        cat("\nStart Series:       ", as.character(start(data)))
        cat("\nEnd Series:         ", as.character(end(data)))
    }

    # We invest in the "Strategy" or (return) efficient Portfolio:
    if(trace) {
        cat("\n\nPortfolio Optimization:")
        cat("\nOptimization Period\tTarget\tBenchmark\t Weights\n")
    }

    # Create Rolling Windows:
    rW = rollingWindows(x = data, period = ans$horizon, by = "1m")
    from = rW$from
    to = rW$to
    
    # Optional Warmup:
    if (warmup) {
        rV = rollingWindows(data, "1m", "1m")
        rV$from = rep(rV$from[1], (horizonLength-1))
        rV$to = rV$to[1:(horizonLength-1)]
        from = c(rV$from, from)
        to = c(rV$to, to)
    }

    # Roll the Portfolio:
    portfolioFun = match.fun(portfolio)
    strategyPortfolio = list()
    for (i in 1:length(from)) {

        # Optimize the Portfolio:
        pfSeries = window(data[, ans$assets], start = from[i], end = to[i])
        bmSeries = window(data[, ans$benchmark], start = from[i], end = to[i])
        attr(spec, "bmReturn") <- mean(series(bmSeries))
        attr(spec, "bmRisk") <- sd(series(bmSeries))
        portfolio = portfolioFun(data = pfSeries, spec, constraints)
        strategyPortfolio[i] = portfolio

        # Trace Optionally the Results:
        if (trace) {

            cat(as.character(from[i]), as.character(to[i]))

            spReturn = as.vector(getTargetReturn(portfolio))
            cat("\t", round(spReturn[1], digits = 3))

            bmReturn = mean(series(bmSeries))
            cat("\t", round(bmReturn, digits = 3))

            whichPortfolio = attr(portfolio, "whichPortfolio")
            if (is.null(whichPortfolio)) whichPortfolio = ""
            cat("\t", whichPortfolio)

            weights = round(getWeights(portfolio), digits = 3)
            for (i in 1:nAssets) cat("\t", weights[i])

            cat("\n")
        }
    }
    
    # Add Portfolio to final result:
    ans$tg = strategyPortfolio

    # Extract Portfolio Investment Weights for the current period:
    weights = NULL
    for (i in 1:length(strategyPortfolio)) {
        weights = rbind(weights, getWeights(strategyPortfolio[[i]]))
    }
    rownames(weights) = as.character(to)
    colnames(weights) = ans$assets
    ans$weights = weights

    # Compute Exponentially Smoothed Weights, be sure to be fully invested:
    emaWeights1 = NULL
    for (i in 1:nAssets) {
        emaWeights1 = cbind(emaWeights1, emaTA(weights[, i], lambda = smoothing))
    }
    emaWeights = NULL
    for (i in 1:nAssets) {
        emaWeights = cbind(emaWeights, emaTA(emaWeights1[, i], lambda = smoothing))
    }
    emaWeights = emaWeights / apply(emaWeights, 1, sum)
    rownames(emaWeights) = as.character(to)
    colnames(emaWeights) = ans$assets
    ans$emaWeights = emaWeights

    # Compute Monthly Assets and Benchmark Returns:
    ans$monthlyAssets =
        applySeries(data[, ans$assets], by = "monthly", FUN = colSums)
    ans$monthlyBenchmark =
        applySeries(data[, ans$benchmark], by = "monthly", FUN = colSums)

    # Compute Offset Return of Rolling Portfolio compared to Benchmark:
    cumX = colCumsums(ans$data[, ans$benchmark])
    # lastX = as.vector(
    #     window(cumX, start = start(cumX), end = rownames(ans$weights)[1] ) )
    #ans$offsetReturn = rev(lastX)[1]
    lastX <- window(cumX, start = start(cumX), end = rownames(ans$weights)[1] )
    ans$offsetReturn = as.vector(lastX[end(lastX),])
    names(ans$offsetReturn) <- as.character(end(lastX))

    # Backtest Return Series:
    Datum = as.vector(rownames(emaWeights))
    nDatum = length(Datum)
    Portfolio = Benchmark = NULL
    for (i in 1:(nDatum-1)) {
        Portfolio = rbind(Portfolio,
            as.vector((as.matrix(ans$monthlyAssets[Datum[i+1], ]) %*%
                emaWeights[Datum[i], ])))
        Benchmark = rbind(Benchmark,
            as.vector(ans$monthlyBenchmark[Datum[i+1], ]))
    }
    P = timeSeries(data = Portfolio, charvec = Datum[-1], units = "Portfolio")
    ans$portfolioReturns = colCumsums(P)
    B = timeSeries(data = Benchmark, charvec = Datum[-1], units = "Benchmark")
    ans$benchmarkReturns = colCumsums(B)

    # Backtest Statistics:
    P = as.vector(P)
    B = as.vector(B)
    Stats = c(sum(P, na.rm = TRUE), sum(B))
    Stats = rbind(Stats, c(mean(P, na.rm = TRUE), mean(B)))
    Stats = rbind(Stats, c(sd(P, na.rm = TRUE), sd(B)))
    Stats = rbind(Stats, c(min(P, na.rm = TRUE), min(B)))
    colnames(Stats) = c("Portfolio", "Benchmark")
    rownames(Stats) = c("Total Return", "Mean Return",
        "StandardDev Return", "Maximum Loss")
    ans$stats = Stats

    # Return Value:
    class(ans) = c("list", "portfolioBacktest")
    invisible(ans)
}



# ------------------------------------------------------------------------------


plot.portfolioBacktest <-
    function(x, which = "all", labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # Plot:
    if(which == "1" || which == "all")
        .backtestAssetsPlot(x, labels, ...)
    if(which == "2" || which == "all")
        .backtestWeightsRecommendationPlot(x, labels, ...)
    if(which == "3" || which == "all")
        .backtestWeightsChangesPlot(x, labels, ...)
    if(which == "4" || which == "all")
        .backtestPortfolioPlot(x, labels, ...)

    # Return Value:
    invisible()
}


################################################################################


.backtestAssetsPlot <-
    function(object, labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Assets Plot

    # Arguments:
    #   x - multivariate time series object of class 'timeSeries'
    #   benchmark - the column name of the benchmark
    #   assets - the column names of the assets
    #   labels - a logical flag, should the plot be decorated?
    #   ... - arguments to be passed, e.g. ylim

    # FUNCTION:

    # Settings:
    x = object$data
    benchmark = object$benchmark
    assets = object$assets

    # Labels ?
    if (labels) {
        ylab = "Series"
    } else {
        ylab = ""
    }

    # ylim - Plot Range:
    nAssets = length(assets)
    MAX = -1e99
    for (i in 1:nAssets) MAX = max(c(MAX, cumsum(x[, assets[i]])) )
    MAX = max(MAX, cumsum(x[, benchmark]))
    MIN = 1e99
    for (i in 1:nAssets) MIN = min(MIN, cumsum(x[, assets[i]]))
    MIN = min(MIN, cumsum(x[, benchmark]))
    ylim = c(MIN, MAX)
    
    # xlim - Plot Range:
    xlim = range(time(colCumsums(x[, benchmark])))
    shift = round(0.20 *as.integer(diff(xlim)), 0) * 24 * 60 * 60
    xlim = c(round(xlim[1]-shift), xlim[2])
    Days = 1:as.integer(diff(xlim))
    Time = as.character(xlim[1] + Days*24*60*60)
    range.tS = timeSeries(data = matrix(rep(0, length(Time))), as.character(Time))
    
    # Plot:
    plot(range.tS, xlab = "", ylab = ylab, col = 0, ylim = ylim, ...)
    lines(colCumsums(x[, benchmark]), col = "black")
    lines(colCumsums(x[, benchmark]), col = "black")
    for (i in 1:nAssets)
        lines( colCumsums(x[, assets[i]]), col = i+1)
    if (labels) {
        Benchmark = abbreviate(benchmark, 4)
        Assets = abbreviate(assets, 4)
        assetsTitle = paste(Assets, collapse = " - ", sep = "")
        title(main = object$title)
        mtext(assetsTitle, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
        legend("topleft", 
            legend = c(Benchmark, Assets),
            bty = "n",
            text.col = 1:(nAssets+1), 
            cex = 0.8)
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestWeightsRecommendationPlot <-
    function(object, labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Weights Recommendation Plot:

    # Arguments:
    #   w - vector of weights
    #   assets - the column names of the assets

    # FUNCTION:

    # Settings:
    weights = object$emaWeights
    assets = object$assets
    horizon = object$horizon
    smoothing = object$smoothing

    # Horizon:
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")

    # Labels ?
    if (labels) {
        xlab = ""
        ylab = "Weights %"
        main = "Weights Recommendation"
    } else {
        xlab = ""
        ylab = ""
        main = ""
    }

    # Weights:
    nAssets = length(assets)
    naWeights <-
        matrix(rep(NA, times = horizonLength * nAssets), ncol = nAssets)

    # Plot:
    ### ts.plot(rbind(naWeights, weights), xlim = c(horizonLength,
    ###     horizonLength - 1 + NROW(weights)), ylim = c(0, 1), 
    ###     col = 2:(nAssets+1), gpars = gpars)

    plot(100*timeSeries(weights), ylim = c(0, 100), ann = FALSE,
         col = 2:(nAssets+1), plot.type = "single", ...)

    # Labels ?
    if (labels) {
        title(main = main, xlab = xlab, ylab = ylab)
        text = paste(
            "Horizon = ", horizon, "| Smoothing:", smoothing, "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestWeightsChangesPlot <-
    function(object, labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Weights Changes Plot

    # Arguments:

    # FUNCTION:

    # Settings:
    weights = object$emaWeights
    assets = object$assets
    horizon = object$horizon
    smoothing = object$smoothing

    # Horizon:
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength

    # labels ?
    if (labels) {
        xlab = ""
        ylab = "Weights Changes %"
        main = "Weights Rearrangement"
    } else {
        xlab = ""
        ylab = ""
        main = ""
    }

    # Weights:
    nAssets = length(assets)
    naWeights = matrix(rep(NA, times = horizon * nAssets), ncol = nAssets)
    naWeights = rbind(naWeights, rep(NA, times = nAssets))
    diffWeights = rbind(naWeights, diff(weights))
    absSum <- function(x) { sum(abs(x)) }
    diffWeights = apply(diffWeights, 1, FUN = absSum)
    diffWeights = cbind(diffWeights, rbind(naWeights, diff(weights)))

    ### # Plot:
    ### ts.plot(diffWeights,
    ###     xlim = c(horizonLength, horizonLength - 1 + NROW(weights)),
    ###     col = 1:(nAssets+1), gpars = gpars)

    tS <- 100 * timeSeries(diffWeights[-seq(horizon + 1),],
        charvec = rownames(diffWeights)[-seq(horizon + 1)])
    plot(tS[, 1], type = "h", lwd = 1, col = "darkgrey",
        ylim = range(as.matrix(tS)), xlab = "", ylab = "")
    for (i in 2:NCOL(tS)) lines(tS[, i], col = i)
    
    #lines(tS, col = 1:(nAssets+1), ann = FALSE, plot.type = "single", ...)
    
    # Add Labels"
    if(labels) {
        title(main = main, xlab = xlab, ylab = ylab)
        text = paste(
            "Horizon = ", horizon, "| Smoothing:", smoothing, "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestPortfolioPlot <-
    function(object, labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Portfolio Plot:

    # Arguments:
    #   object
    #   labels

    # FUNCTION:

    # Settings:
    data = object$data
    portfolioReturns = object$portfolioReturns
    benchmarkReturns = object$benchmarkReturns
    benchmark = object$benchmark
    horizon = object$horizon
    smoothing = object$smoothing
    offsetReturn = object$offsetReturn

    # Labels ?
    if (labels) {
        ylab = "Cumulated"
        main = "Portfolio versus Benchmark"
    } else {
        ylab = ""
        main = ""
    }

    # Cumulated Return Series:
    X = data[, benchmark]
    cumX = colCumsums(X)
    cumP = portfolioReturns + offsetReturn
    cumB = benchmarkReturns + offsetReturn
    # we want to start from the benchmark offsetReturn
    offsetTS <- timeSeries(offsetReturn, charvec = names(offsetReturn),
                           units = "offsetReturn")
    cumP <- rbind(offsetTS, cumP)
    cumB <- rbind(offsetTS, cumB)

    # Plot:
    MAX = max(as.vector(series(cumP)), as.vector(series(cumB)), 
            as.vector(series(cumX)))
    MIN = min(as.vector(series(cumP)), as.vector(series(cumB)), 
            as.vector(series(cumX)))
    plot(cumX, type = "l", col = "black", ylim = c(MIN, MAX), ann = FALSE, ...)
    lines(cumP-cumB, type = "h", col = "grey")
    lines(cumP, col = "red", lwd = 2)
    lines(cumB, col = "blue", lwd = 2)

    # Add Labels"
    if(labels) {
        title(main = main, ylab = ylab)
        text = paste(
            "Horizon = ", horizon, "| Smoothing:", smoothing, "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }
    
    # mText:
    mText = Type = getType(object$spec)
    Estimator = getEstimator(object$spec)
    if (Type == "MV") mText = paste(mText, "|", Estimator)
    mtext(mText, side = 4, line = 0, adj = 0, col = "darkgrey", cex = 0.7)

    # Return Value:
    invisible()
}


################################################################################


print.portfolioBacktest <-
    function(x, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes some monthly portfolio performance measures

    # Arguments:
    #   x - an object as returned by the function portfolioBacktesting

    # Notes:
    #   An internal function called by function 'portfolioBacktesting()'

    # FUNCTION:

    # Return Value:
    print(x$stats)
}


# ------------------------------------------------------------------------------


summary.portfolioBacktest <-
    function(object, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # Under Construction ...
    print(x = object, ...)

    # Return Value:
    invisible()
}


################################################################################


.backtest2Plot <- 
    function(x) {
        par(mfrow = c(3, 2), mar = c(2, 5, 5, 3))
        
        plot(x)
        
        plot(rnorm(1), type = "n", 
            xaxt = "n", yaxt = "n", 
            xlab = "", ylab = "", 
            frame = FALSE)
        
        TEXT = paste("Strategy:", x$portfolio)
        mtext(TEXT, side = 3, line =  3, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
            
        TEXT =  capture.output(round(x$stats, 2))
        mtext(TEXT[1], side = 3, line =  2, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[2], side = 3, line =  1, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[3], side = 3, line =  0, adj = 0, cex = 0.65, 
            font = 3, family = "mono") 
        mtext(TEXT[4], side = 3, line = -1, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[5], side = 3, line = -2, adj = 0, cex = 0.65, 
            font = 3, family = "mono") 
            
        TEXT = capture.output(x$spec)[c(2,3,4,5,8)]
        mtext(TEXT[1], side = 3, line = -4, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[2], side = 3, line = -5, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[3], side = 3, line = -6, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[4], side = 3, line = -7, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(TEXT[5], side = 3, line = -8, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
      
        TEXT = capture.output(x$constraints)[1]      
        mtext("Constraints:", side = 3, line = -10, adj = 0, cex = 0.65, 
            font = 3, family = "mono")
        mtext(substr(TEXT[1], 4, 99), side = 3, line = -11, adj = 0, 
            cex = 0.65, font = 3, family = "mono")

        invisible()
    }
    
    
################################################################################

