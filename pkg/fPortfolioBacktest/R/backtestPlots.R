
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:				      DESCRIPTION:
#  backtestPlots			   Plots all 6 sub plots listed below
#   backtestAssetsPlot     	   Plots assets used in a portfolio backtest   
#   backtestWeightsPlot    	   Plots recommended weights from a backtest
#   backtestRebalancePlot      Plots rebalanced weights of a backtest 
#   backtestPortfolioPlot      Plots benchmark and portfolio series
#   backtestDrawdownPlot       Plots the drawdown of the portfolio backtest
#   backtestReportPlot		   Prints backtest report
################################################################################


backtestPlots <-
    function(object, which = "all", labels = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:
    
    if (any(which == "all"))
    par(mfrow = c(3, 2), mar = c(1.5, 4, 5, 2), oma = c(5,1,0,1))
       
    # Plot:
    if(any(which == "1") || which == "all")
        backtestAssetsPlot (object, labels, ...)
    if(any(which == "2") || which == "all")
        backtestWeightsPlot (object, labels, ...)
    if(any(which == "3") || which == "all")
       backtestRebalancePlot (object, labels, ...)
    if(any(which == "4") || which == "all")
        backtestPortfolioPlot(object, labels, ...)
    if(any(which == "5") || which == "all")
        backtestDrawdownPlot(object, labels, ...)
    if(any(which == "6" )|| which == "all")
    	backtestReportPlot(object, ...)
        
	
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------

   
backtestAssetsPlot <-
    function(object, labels = TRUE, ...)
{
    # Description:
    #   Plots assets used in a portfolio backtest

    # Arguments:
    #   object - an object as returned by the weights smoothing
    #   labels - a logical flag, should automated labels added to the plot

    # FUNCTION:

    # Settings:
    x = object$data
    benchmark = object$benchmarkName
    assets = object$assetsNames

    # Labels ?
    if (labels) {
        ylab = "Series"
    } else {
        ylab = ""
    }

    # ylim - Plot Range:
    nAssets = length(assets)
    MAX = -1.0e99
    for (i in 1:nAssets) MAX = max(c(MAX, cumsum(x[, assets[i]])) )
    MAX = max(MAX, cumsum(x[, benchmark]))
    MIN = 1.0e99
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
    # Plot:
    X = x[, benchmark]
    plot(X, type = "n", xaxt = "n", ylim = ylim, xlab = "", ylab = ylab) 
    # plot(range.tS, xlab = "", ylab = ylab, col = 0, ylim = ylim, ...)
    lines(colCumsums(x[, benchmark]), col = "black")
    lines(colCumsums(x[, benchmark]), col = "black")
    for (i in 1:nAssets)
        lines( colCumsums(x[, assets[i]]), col = i+1)
    if (labels) {
        Benchmark = abbreviate(benchmark, 4)
        Assets = abbreviate(assets, 4)
        assetsTitle = paste(Assets, collapse = " - ", sep = "")
        title(main = "Series")
        mtext(assetsTitle, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
        legend("topleft",
            legend = c(Benchmark, Assets),
            bty = "n",
            text.col = 1:(nAssets+1),
            cex = 0.8)
    }
   
    # Annual Lines:
    YYYY = as.character(1990:2010)
    for (year in YYYY) abline(
        v = as.POSIXct(paste(year, "-01-01", sep ="")),
        lty = 3,
        col = "brown")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


backtestWeightsPlot <-
    function(object, labels = TRUE, ...)
{
    # Description:
    #   Plots recommended weights from a portfolio backtest
   
    # Arguments:
    #   object - an object as returned by the weights smoothing
    #   labels - a logical flag, should automated labels added to the plot

    # FUNCTION:

    # Settings:
    data = object$data
    weights = object$smoothWeights
    assets = object$assetsNames
    benchmark = object$benchmarkName
    horizon = getWindowsHorizon(object$backtest)
    smoothing = getSmootherLambda(object$backtest)
    startup = startup = "1m"

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
    X = data[, benchmark]
    plot(X, type = "n", xaxt = "n", ylim = c(0, 100), xlab = "", ylab = "") 
    tS = 100 * timeSeries(weights)
    for (i in 1:nAssets) lines(tS[, i], col = i+1)

    # Labels ?
    if (labels) {
        title(main = main, xlab = xlab, ylab = ylab)
        text = paste(
            "Horizon = ", horizon,
            "| Smoothing:", smoothing,
            "| Startup:", startup,
            "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }
      
    # Annual Lines:
    YYYY = as.character(1990:2010)
    for (year in YYYY) abline(
        v = as.POSIXct(paste(year, "-01-01", sep ="")),
        lty = 3,
        col = "brown")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


backtestRebalancePlot <-
    function(object, labels = TRUE, ...)
{
    # Description:
    #   Plots rebalanced weights of a backtest

    # Arguments:
    #   object - an object as returned by the weights smoothing
    #   labels - a logical flag, should automated labels added to the plot
   
    # FUNCTION:

    # Settings:
    data = object$data
    weights = object$smoothWeights
    assets = object$assetsNames
    benchmark = object$benchmarkName
  
     horizon = getWindowsHorizon(object$backtest)
    smoothing = getSmootherLambda(object$backtest)
    startup = "1m"

    # Horizon:
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength

    # labels ?
    if (labels) {
        xlab = ""
        ylab = "Weights Changes %"
        main = "Weights Rebalance"
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
    X = data[, benchmark]
    tS <- 100 * timeSeries(diffWeights[-seq(horizon + 1),],
        charvec = rownames(diffWeights)[-seq(horizon + 1)])   
    plot(X, type = "n", xaxt = "n", ylim = range(tS),xlab = "", ylab = "") 
    lines(tS[, 1], type = "h", lwd = 1, col = "darkgrey")
    for (i in 2:NCOL(tS)) lines(tS[, i], col = i)
   
    # Add Labels"
    if(labels) {
        title(main = main, xlab = xlab, ylab = ylab)
        text = paste(
            "Horizon = ", horizon,
            "| Smoothing:", smoothing,
            "| Startup:", startup,
            "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }
     
    # Margin Text:
    mText = paste("Start:", rownames(object$smoothWeights)[1])
    mtext(mText, side = 4, line = 0, adj = 0, col = "darkgrey", cex = 0.65)
   
    # Annual Lines:
    YYYY = as.character(1990:2010)
    for (year in YYYY) abline(
        v = as.POSIXct(paste(year, "-01-01", sep ="")),
        lty = 3,
        col = "brown")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


backtestPortfolioPlot <-
    function(object, labels = TRUE, ...)
{
    # Description:
    #   Plots daily, benchmark and portfolio series of a portfolio backtest

    # Arguments:
    #   object - an object as returned by the weights smoothing
    #   labels - a logical flag, should automated labels added to the plot

    # FUNCTION:

    # Settings:
    data = object$data
    portfolioReturns = object$portfolioReturns
    benchmarkReturns = object$benchmarkReturns
    benchmark = object$benchmarkName
    horizon = getWindowsHorizon(object$backtest)
    smoothing = getSmootherLambda(object$backtest)
    startup = "1m"
    offsetReturn = object$offsetReturn

    # Labels ?
    if (labels) {
        ylab = "Cumulated"
        main = "Portfolio vs Benchmark"
    } else {
        ylab = ""
        main = ""
    }
   
    # Return Series:
    X = data[, benchmark]
    limX = c(as.POSIXct(start(X)), as.POSIXct(end(X)))

    # Cumulated Return Series:
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
    plot(cumX, type = "l", col = "black",
        xlim = limX, ylim = c(MIN, MAX), ann = FALSE, ...)
    #lines(cumB-cumP, type = "h", col = "grey")
    lines(cumP-cumB, type = "h", col = "grey")
    lines(cumP, col = "red", lwd = 2)
    lines(cumB, col = "blue", lwd = 2)

    # Add Labels"
    if(labels) {
        title(main = main, ylab = ylab)
        text = paste(
            "Horizon = ", horizon,
            "| Smoothing:", smoothing,
            "| Startup:", startup,
            "| Shift 1m")
        mtext(text, line = 0.5, cex = 0.7)
        grid(NA, ny = NULL)
    }
   
    # mText:
    mText = Type = getType(object$spec)
    Estimator = getEstimator(object$spec)
    if (Type == "MV") mText = paste(mText, "|", Estimator)
    mtext(mText, side = 4, line = 0, adj = 0, col = "darkgrey", cex = 0.7)
   
    # Annual Lines:
    YYYY = as.character(1990:2010)
    for (year in YYYY) abline(
        v = as.POSIXct(paste(year, "-01-01", sep ="")),
        lty = 3,
        col = "brown")

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


backtestDrawdownPlot = 
	function(object, labels = TRUE, ...)
{
	# A function implemented by Diethelm Wuertz

    # Description:
    #   Backtest Portfolio Plot:

    # Arguments:
    #   object
    #   labels

    # FUNCTION:
	
	# Align Data:
    Data = .align.timeSeries(object$data)/100
    
    # Settings:
    assets = object$assetsNames
    benchmark = object$benchmarkName
    horizon = getWindowsHorizon(object$backtest)
    smoothing = getSmootherLambda(object$backtest)
    startup = getSmootherInitialWeights(object$backtest)
    weights = as.timeSeries(object$smoothWeights)
    
   	# Extract the Time Stamps:
	tS = time(Data)
	tW = time(weights)
		
	# Problem when rebalance day lands on a Weekend - 
	#   need to change the date to the nearest Monday
	if (any(isWeekend(tW))){
        weekend.tW = tW[isWeekend(tW)]
        
        # WC: check timeNdayOnOrAfter function, the nday = 2 is a Monday!???
        tW = sort(c(tW[!isWeekend(tW)], timeNdayOnOrAfter(weekend.tW, 2)))
        # replace old times with new times
        time(weights) = tW
	}
			
	# Extract the Updated Revalance Dates:	
	Dates = time(weights)
	
	# Subsetting the Data:
    data = window(Data, start(weights), end(weights))
    
	# Check whether we have data past the last balance date
	# i.e. last balance date won't take place if we don't have the return series
	if (end(data) < end(weights)){ 
	    n = length(Dates)-1 
	} else {n = length(Dates)
        Dates = c(Dates, end(data))
	}

	# Calculate the portfolio returns for the given weights:
	# assume we start investing the new weights on the rebalance date
	pf = NULL
	a = NULL
	for (i in 1:n){
		temp = window(data, Dates[i], Dates[i+1])[,assets]
		nr = nrow(temp)
		if (i != n) temp = temp[-nr,]
		a = c(a, nrow(temp))
		pf = c(pf, pfolioReturn(temp, as.numeric(weights[i,])))
	}
	
	# Drawdown Plot Settings:
	stopifnot(length(pf) == length(rownames(data)))
	pf = timeSeries(pf, charvec = rownames(data))
	pf.DD = drawdowns(pf)
	benchmark.DD = drawdowns(data[,benchmark]) 
	
	# Return Series:
    X = Data[, benchmark]
    limX = c(as.POSIXct(start(X)), as.POSIXct(end(X)))

	# Plot:
	plot(time(benchmark.DD), benchmark.DD, type = "l", col = "blue", 
	    xlim = limX, ylim = range(c(pf.DD, benchmark.DD)), lwd = 2, 
	    ann = FALSE) #, ...)
	lines(pf.DD, col = "red", lwd = 2)
	
	 # Labels ?
    if (labels) {
        ylab = "Drawdowns"
        main = "Drawdowns | Portfolio vs Benchmark"
    } else {
        ylab = ""
        main = ""
    }
	
	# Add Labels:
    if(labels) {
        title(main = main, ylab = ylab)
		text = paste("(Max)", "Portfolio DD =", round(min(pf.DD),2),
					"|", "Benchmark DD =", round(min(benchmark.DD),2))
        mtext(text, line = 0.5, cex = 0.7)
        #grid(NA, ny = NULL)
    }
    
    # Annual Lines:
    YYYY = as.character(1990:2010)
    for (year in YYYY) abline(
        v = as.POSIXct(paste(year, "-01-01", sep ="")),
        lty = 3,
        col = "brown")
       
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


backtestReportPlot <-
	function(object, ...)
{    
    plot.new()
    plot.window(xlim = c(0,1), ylim = c(0,1))
      
    # Vertical Adjustment:
    z = -2
        
    TEXT = paste("Strategy:", getStrategyFun(object$backtest))
    mtext(TEXT, side = 3, line =  z + 3, adj = 0, ...)
        
    TEXT =  capture.output(round(object$stats, 2))
    mtext(TEXT[1], side = 3, line =  z + 2, adj = 0, ...)
    mtext(TEXT[2], side = 3, line =  z + 1, adj = 0, ...)
    mtext(TEXT[3], side = 3, line =  z + 0, adj = 0, ...) 
    mtext(TEXT[4], side = 3, line = z + -1, adj = 0, ...)
    mtext(TEXT[5], side = 3, line = z + -2, adj = 0, ...) 
        
    TEXT = capture.output(object$spec)[c(2,3,4,5,8)]
    mtext("Portfolio Specification:", side = 3, line = z + -4, adj = 0, ...)
        
    if (length(grep("CVaR",TEXT[2]))!=0) TEXT[2] = 
        gsub("CVaR", paste("CVaR |", getAlpha(object$spec)), TEXT[2])
       
    mtext(TEXT[2], side = 3, line = z + -5, adj = 0, ...)
    mtext(TEXT[3], side = 3, line = z + -6, adj = 0, ...)
    mtext(TEXT[4], side = 3, line = z + -7, adj = 0, ...)
    mtext(TEXT[5], side = 3, line = z + -8, adj = 0, ...)
  
    TEXT = capture.output(object$constraints)[1]      
    mtext("Constraints:", side = 3, line = z + -10, adj = 0, ...)
    TEXT = substr(TEXT[1], 4, 99)   
    mtext(TEXT, side = 3, line = z + -11, adj = 0, ...)
        
    # Model parameters:
    #   mtext("Initial Weights:", side = 3, line = z + -13, 
    #       adj = 0, cex = 0.65,  font = 3, family = "mono")
    #   if (!is.null(getSmootherInitialWeights(object$backtest)))
    #   mtext(paste("\t", 
    #       paste(try(round(getSmootherInitialWeights(object$backtest), 2), 
    #       silent = TRUE),collapse = " "), sep = ""), 
    #       side = 3, line = z + -14, adj = 0, cex = 0.65,  font = 3, 
    #       family = "mono")
       					
 	# Return Value:
 	invisible()
 }


################################################################################

