
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
#  portfolioBacktesting              Does portfolio backtesting
#  plot.portfolioBacktest             S3 Plot Method
#  print.portfolioBacktes             S3 Print Method
#  summary.portfolioBacktest          S3 Summary Method
################################################################################


portfolioBacktesting =
function(formula, data, spec = portfolioSpec(), constraints = NULL,
portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m",
trace = TRUE)
{   # A function implemented by Diethelm Wuertz

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
    #       2.  Compute the target Return for the "safe" portfolio based
    #           on this window and the 'data' listed in the 'formula'
    #           expression.
    #       3.  If the benchmark return is higher than the target return
    #           of the "safe" portfolio, then replace the "safe"
    #           portfolio by the efficient portfolio with a target return
    #           given by the benchmark. We call the resulting portfolio
    #           the "optimal" portfolio
    #       4.  Extract the weights from the optimal portfolio and
    #           perform an exponential moving average with a smoothing
    #           period defined by the value 'smoothing'.
    #       5.  Do an investiment with the obtained weights for the next
    #           month and calculate in the next time step the resulting
    #           gain or loss.

    # FUNCTION:

    #settings:
    ans = list()
    ans$formula = formula
    ans$data = data
    ans$spec = spec
    ans$constraints = constraints
    ans$portfolio = portfolio

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
    rW = rollingWindows(data, ans$horizon, "1m")
    from = rW$from
    to = rW$to

    # Roll the Portfolio:
    portfolioFun = match.fun(portfolio)
    tg = list()
    for (i in 1:length(from)) {

        # Optimize the Portfolio:
        pfSeries = window(data[, ans$assets], from = from[i], to = to[i])
        bmSeries = window(data[, ans$benchmark], from = from[i], to = to[i])
        attr(spec, "bmReturn") <- mean(bmSeries@Data)
        attr(spec, "bmRisk") <- sd(bmSeries@Data)
        portfolio = portfolioFun(data = pfSeries, spec, constraints)
        tg[i] = portfolio

        # Trace Optionally the Results:
        if (trace) {

            cat(as.character(from[i]), as.character(to[i]))

            tgReturn = as.vector(getTargetReturn(portfolio))
            cat("\t", round(tgReturn, digits = 3))

            bmReturn = mean(bmSeries@Data)
            cat("\t", round(bmReturn, digits = 3))

            whichPortfolio = attr(portfolio, "whichPortfolio")
            if (is.null(whichPortfolio)) whichPortfolio = ""
            cat("\t", whichPortfolio)

            weights = round(getWeights(portfolio), digits = 3)
            for (i in 1:nAssets) cat("\t", weights[i])

            cat("\n")
        }
    }
    ans$tg = tg

    # Extract Portfolio Investment Weights for the current period:
    weights = NULL
    for (i in 1:length(tg)) {
        weights = rbind(weights, getWeights(tg[[i]]))
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
    lastX = as.vector(
        window(cumX, from = start(cumX), to = rownames(ans$weights)[1] ) )
    ans$offsetReturn = rev(lastX)[1]

    # Backtest Return Series:
    Datum = as.vector(rownames(emaWeights))
    nDatum = length(Datum)
    Portfolio = Benchmark = NULL
    for (i in 1:(nDatum-1)) {
        Portfolio = rbind(Portfolio,
            as.vector((ans$monthlyAssets[Datum[i+1], ]@Data %*%
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
    Stats = c(sum(P), sum(B))
    Stats = rbind(Stats, c(mean(P), mean(B)))
    Stats = rbind(Stats, c(sd(P), sd(B)))
    Stats = rbind(Stats, c(min(P), min(B)))
    colnames(Stats) = c("Portfolio", "Benchmark")
    rownames(Stats) = c("Total Return", "Mean Return",
        "StandardDev Return", "Minimum Monthly Return")
    ans$stats = Stats

    # Return Value:
    class(ans) = c("list", "portfolioBacktest")
    invisible(ans)
}


# ------------------------------------------------------------------------------


plot.portfolioBacktest =
function(x, which = "all", labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # Plot:
    if(which == "1" || which == "all")
        .backtestAssetsPlot(x, labels)
    if(which == "2" || which == "all")
        .backtestWeightsRecommendationPlot(x, labels)
    if(which == "3" || which == "all")
        .backtestWeightsChangesPlot(x, labels)
    if(which == "4" || which == "all")
        .backtestPortfolioPlot(x, labels)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestAssetsPlot <-
function(object, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

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

    # Plot Range:
    nAssets = length(assets)
    MAX = -1e99
    for (i in 1:nAssets) MAX = max(c(MAX, cumsum(x[, assets[i]]@Data)) )
    MAX = max(MAX, cumsum(x[, benchmark]@Data))
    MIN = 1e99
    for (i in 1:nAssets) MIN = min(MIN, cumsum(x[, assets[i]]@Data))
    MIN = min(MIN, cumsum(x[, benchmark]@Data))
    ylim = c(MIN, MAX)

    # Plot:
    plot(cumsum(x[, benchmark]), type = "l", ylab = ylab, col = "black",
        ylim = ylim, ...)
    for (i in 1:nAssets)
        lines( cumsum(x[, assets[i]]), type = "l", col = i+1)
    if (labels) {
        assetsTitle = paste(assets, collapse = " - ", sep = "")
        title(main = paste(benchmark, "~", assetsTitle))
        grid()
        legend("topleft", legend = c(benchmark, assets), bty = "n",
            text.col = 1:(nAssets+1))
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestWeightsRecommendationPlot <-
function(object, labels = TRUE, gpars = list())
{   # A function implemented by Diethelm Wuertz

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
        ylab = "Weights Factor"
        main = "Weights Recommendation"
    } else {
        xlab = ""
        ylab = ""
        main = ""
    }

    # Weights:
    nAssets = length(assets)
    naWeights = matrix(rep(NA, times = horizonLength * nAssets), ncol = nAssets)

    # Plot:
    ts.plot(rbind(naWeights, weights), xlab = xlab, ylab = ylab,
        ylim = c(0, 1), col = 2:(nAssets+1), main = main, gpars = gpars)

    # Labels ?
    if (labels) {
        text = paste("Horizon = ", horizon, "| Smoothing:", smoothing)
        mtext(text, line = 0.5, cex = 0.7)
        grid()
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestWeightsChangesPlot <-
function(object, labels = TRUE, gpars = list())
{   # A function implemented by Diethelm Wuertz

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
        ylab = "Weights Changes"
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

    # Plot:
    ts.plot(diffWeights, xlab = xlab, ylab = ylab,
        col = 1:(nAssets+1), main = main, gpars = gpars)

    # Add Labels"
    if(labels) {
        text = paste("Horizon = ", horizon, "| Smoothing:", smoothing)
        mtext(text, line = 0.5, cex = 0.7)
        grid()
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.backtestPortfolioPlot <-
    function(object, labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Portfolio Plot:

    # Arguments:

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
        ylab = "Total Percentage Return"
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

    # Plot:
    MAX = max(as.vector(cumP@Data), as.vector(cumB@Data), as.vector(cumX@Data))
    MIN = min(as.vector(cumP@Data), as.vector(cumB@Data), as.vector(cumX@Data))
    plot(cumX, type = "l", col = "black", ylab = ylab, main = main,
        ylim = c(MIN, MAX), ...)
    lines(cumP, col = "red", lwd = 2)
    lines(cumB, col = "blue", lwd = 2)

    lines(cumP-cumB, col = "green", lwd = 2)

    # Add Labels"
    if(labels) {
        text = paste("Horizon = ", horizon, "| Smoothing:", smoothing)
        mtext(text, line = 0.5, cex = 0.7)
        grid()
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


print.portfolioBacktest =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

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


summary.portfolioBacktest =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:

    # Under Construction ...
    print(x = object, ...)

    # Return Value:
    invisible()
}


################################################################################

