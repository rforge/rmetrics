
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                    EFFICIENT FRONTIER PLOT AND ADDONS:
#  frontierPlot                 Plots efficient frontier
#   minvariancePoints             Adds minimum variance point
#   cmlPoints                     Adds market portfolio
#   cmlLines                      Adds capital market Line
#   tangencyPoints                Adds tangency portfolio point
#   tangencyLines                 Adds tangency line
#   equalWeightsPoints            Adds point of equal weights portfolio
#   singleAssetPoints             Adds points of single asset portfolios
#   twoAssetsLines                Adds EF for all combinations of two assets
#   sharpeRatioLines              Adds Sharpe ratio line
#   monteCarloPoints              Adds randomly produced feasible portfolios
# FUNCTION:                    DESCRIPTION:
#   .weightsWheel               Adds a pie of weights to frontier plot
#   .attributesWheel            Adds a pie of attributes to frontier plot
# FUNCTION:                    DESCRIPTION:
#   .notStackedWeightsPlot      Plots the not stacked weights of potfolio
#   .addlegend                  Adds legend to sliders
################################################################################


frontierPlot <-
    function(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, labels = TRUE,
    risk = NULL, title = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots the efficient frontier

    # FUNCTION:

    # Check Colors:
    stopifnot(length(col) == 2)

    # Settings:
    frontier = match.arg(frontier)

    # Frontier:
    if (is.null(risk)) {
        risk = "cov"
        auto = TRUE 
    } else {
        auto = FALSE
    }
    fullFrontier = frontierPoints(object, frontier = "both", 
        risk = risk, auto = auto)
    upperFrontier = frontierPoints(object, frontier = "upper", 
        risk = risk, auto = auto)
    lowerFrontier = frontierPoints(object, frontier = "lower", 
        risk = risk, auto = auto)

    # Check for 'xlim' Argument:
    Arg <- match.call(expand.dots = TRUE)
    m <- match(c("xlim", "ylim"), names(Arg), Arg)
    xArg <- as.character(Arg[c(1, m)])[2]
    yArg <- as.character(Arg[c(1, m)])[3]

    # Plot:
    if(xArg == "NULL" & yArg == "NULL") {
        yLim = range(fullFrontier[, 2])
        xRange = range(fullFrontier[, 1])
        xDiff = diff(xRange)
        xLim = c(xRange[1] - 2.5*xDiff/10, xRange[2] + xDiff/10)

        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], xlim = xLim, ylim = yLim,
                    ann = FALSE, ...)
            } else {
                if( frontier == "both") {
                    points(fullFrontier, col = col[2],
                        xlim = xLim, ylim = yLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(lowerFrontier, col = col[2],
                        xlim = xLim, ylim = yLim, ann = FALSE, ...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1],  ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }
    } else if (xArg != "NULL" & yArg == "NULL") {
        # In this case only xlim is specified in the argument list
        yLim = range(fullFrontier[, 2])
        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], ylim = yLim,
                    ann = FALSE, ...)
            } else {
                if( frontier == "both") {
                    points(fullFrontier, col = col[2], ylim = yLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(fullFrontier, col = col[2], ylim = yLim,
                        ann = FALSE, ...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1], ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }
    } else if(xArg == "NULL" & yArg != "NULL") {
        # In this only ylim is specified in the argument list
        xRange = range(fullFrontier[, 1])
        xDiff = diff(xRange)
        xLim = c(xRange[1] - 2.5*xDiff/10, xRange[2] + xDiff/10)
        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], xlim = xLim,
                    ann = FALSE, ...)
            } else {
                if( frontier == "both") {
                    points(fullFrontier, col = col[2], xlim = xLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(lowerFrontier, col = col[2], xlim = xLim,
                        ann = FALSE,...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1], ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }
    } else if (xArg != "NULL" & yArg != "NULL"){
        #  If both xlim and ylim are not defined in argument list ...
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(fullFrontier, type = "n", ann = FALSE, ...)
                points(upperFrontier, col = col[1], ...)
            }
            if(frontier == "both") {
                points(lowerFrontier, col = col[2], ...)
            }
            if(frontier == "lower") {
                plot(lowerFrontier, col = col[2], ann = FALSE, ...)
            }
        } else{
            if(frontier == "upper" | frontier == "both") {
                points(upperFrontier, col = col[1], ...)
            }
            if(frontier == "lower" | frontier == "both") {
                points(lowerFrontier, col = col[2], ...)
            }
        }
    }

    # Add Info:
    if (labels) {
        mtext(paste(getType(object), "|", getSolver(object)),
            side = 4, adj = 0, col = "grey", cex = 0.7)
    }


    # Add Info:
    if (title) {
        title(
            main = "Efficient Frontier",
            xlab = "Target Risk",
            ylab = "Target Return")
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


minvariancePoints <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the minimum risk point to a MV and CVaR portfolio plot

    # FUNCTION:

    # Get Portfolio Slots:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)

    # Add Minimum Variance Point:
    mvPortfolio = minvariancePortfolio(Data, Spec, Constraints)
    assets = frontierPoints(mvPortfolio)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


cmlPoints <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the capital market line to a portfolio plot

    # FUNCTION:

    # Get Portfolio Statistics:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    Type = getType(object)

    # Add Capital Market Line Tangency Point:
    cmlPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    assets = frontierPoints(cmlPortfolio)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


cmlLines <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the capital market line to a portfolio plot

    # FUNCTION:

    # Get Portfolio Statistics:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    Type = getType(object)

    # Add Capital Market Line:
    cmlPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    riskFreeRate = getRiskFreeRate(Spec)
    slope = ((getTargetReturn(cmlPortfolio)[, "mean"] - riskFreeRate) /
        getTargetRisk(cmlPortfolio)[, "cov"])
    if(slope > 0) abline(b = slope, a = riskFreeRate, ...)

    # Return Value:
    invisible(slope)
}


# ------------------------------------------------------------------------------


tangencyPoints <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds tangency point and line to a MV and CVaR portfolio plot

    # FUNCTION:

    # Get Portfolio Slots:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)

    # Compute Tangency Portfolio:
    tgPortfolio = tangencyPortfolio(Data, Spec, Constraints)

    # Add Tangency Point:
    points(frontierPoints(tgPortfolio), ...)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


tangencyLines <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds tangency point and line to a MV and CVaR portfolio plot

    # FUNCTION:

    # Get Portfolio Slots:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)

    # Compute Tangency Portfolio:
    tgPortfolio = tangencyPortfolio(Data, Spec, Constraints)

    # Add Tangency Line:
    slope = getTargetReturn(tgPortfolio) / getTargetRisk(tgPortfolio)[1]
    abline(0, slope, ...)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


equalWeightsPoints =
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds equal weights portfolio to a portfolio plot

    # FUNCTION:

    # Get Portfolio Statistics:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    Type = getType(object)
    NumberOfAssets = getNAssets(object)

    # Set Equal Weights:
    setWeights(Spec) = rep(1/NumberOfAssets, times = NumberOfAssets)

    # Add Equal Weights Portfolio:
    ewPortfolio = feasiblePortfolio(Data, Spec, Constraints)
    if (Type == "MV") {
        assets = frontierPoints(ewPortfolio)
    } else if (Type == "CVaR") {
        assets = frontierPoints(ewPortfolio) * c(-1, 1)
    }
    points(assets, ...)

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


singleAssetPoints <-
    function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds all single assets returns and risks to a portfolio plot

    # FUNCTION:

    # Add Single Assets:
    Statistics = getStatistics(object)
    Type = getType(object)

    Return = Statistics$mu
    if (Type == "MV") {
        Risk = sqrt(diag(Statistics$Sigma))
    } else if (Type == "CVaR") {
        nAssets = getNAssets(object)
        Data = getSeries(object)
        alpha = getAlpha(object)
        Risk = NULL
        for (i in 1:nAssets) Risk = c(Risk, -.cvarRisk(Data[ ,i], 1, alpha))
    }
    assets = cbind(Risk = Risk, Return = Return)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


twoAssetsLines <-
    function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds efficient long-only frontier of all portfolio pairs

    # Note:
    #   Only supported for "Short" and "LongOnly" Constraints!

    # FUNCTION:

    # Supported ?
    check = rev(attr(object@constraints, "model"))[1]

    # Get Portfolio Statistics:
    Data = getData(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    Type = getType(object)

    # Add Frontiers for all Two-Assets Portfolios:
    N = getNAssets(object)
    Data = getSeries(object)
    setWeights(Spec) = NULL
    for ( i in 1:(N-1) ) {
        for (j in (i+1):N ) {
            index = c(i, j)
            Data2 = Data[, index]
            # Zero-One Constraints2 ?
            ans = portfolioFrontier(data = Data2, spec = Spec)
            lines(frontierPoints(ans), ...)
        }
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


sharpeRatioLines <-
    function(object, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds Sharpe Ratio

    # FUNCTION:

    # Get Portfolio Slots:
    Data = getSeries(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    Type = getType(object)

    # Efficient Frontier:
    x = getTargetRisk(object)[, "cov"]
    y = getTargetReturn(object)[, "mean"]

    # Tangency Portfolio:
    tangencyPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    x.tg = getTargetReturn(tangencyPortfolio)[1, "mean"]

    # Normalization to fit in EF Plot:
    norm = x.tg / max(y/x)
    index = 2:length(x)
    index = index[diff(x) > 0]
    x = x[index]
    y = y[index]
    y.norm = (y/x*norm)
    assets = cbind(x, y.norm)
    lines(x, y.norm, ...)

    # Add Tailored Labels -  2 may be a good Number ...
    x.tg = x.tg[index]
    norm2 = x.tg / max(y)
    Range = range(y/x * norm)

    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    Labels = signif(Range, nPrecision)
    axis(4, at = Range, labels = c(" ", " "), cex.axis = 0.75)
    axis(4, at = mean(Range), labels = paste(Labels[1], "   ", Labels[2]),
        cex.axis = 0.75)

    # Add Axis Labels and Title:
    mtext("Sharpe Ratio", side = 4, line = 2, cex = 0.75)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


monteCarloPoints <-
    function(object, mcSteps = 5000, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds randomly feasible portfolios to a plot

    # FUNCTION:

    # Get Portfolio Statistics:
    Statistics = getStatistics(object)
    Type = getType(object)
    mu = Statistics$mu
    Sigma = Statistics$Sigma
    N = length(mu)

    # Get Specification:
    if (Type == "MV") {
        # Get Constraints Model:
        Model = rev(attr(object@constraints, "model"))[1]
        Model = "LongOnly"
        if (Model == "Short" | any(object@constraints == "Short")) {
            # Monte Carlo Loop - Short:
            for (k in 1:mcSteps) {
                s = sign(rnorm(N, mean = rnorm(1)))
                weights = s * abs(rcauchy(N))
                weights = weights / sum(weights)
                Return = as.numeric(mu %*% weights)
                Risk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
                points(Risk, Return, ...)
            }
        } else if (Model == "LongOnly" | object@constraints == "LongOnly") {
            # Monte Carlo Loop - Long Only:
            for (k in 1:mcSteps) {
                weights = abs(rcauchy(N))
                weights = weights / sum(weights)
                Return = as.numeric(mu %*% weights)
                Risk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
                points(Risk, Return, ...)
            }
        } else {
            cat("\n\tOnly for Short and LongOnly Portfolios\n")
        }
    } else if (Type == "CVaR") {
        # Monte Carlo Loop - Long Only:
        x = getSeries(object)
        alpha = getAlpha(object)
        for (k in 1:mcSteps) {
            weights = abs(rcauchy(N))
            weights = weights / sum(weights)
            Return = as.numeric(mu %*% weights)
            Risk = .cvarRisk(x, weights, alpha)
            points(-Risk, Return, ...)
        }
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.weightsWheel <-
    function(object, piePos = NULL, pieR = NULL, pieOffset = NULL, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of weights for MV and CVaR Portfolios

    # Details:
    #   The default settings are:
    #   piePos - Position of tangency Portfolio
    #   pieR - 10% of the Risk Range: diff(range(targetRisk(object)))/10

    # FUNCTION:

    # Extraction coordinates
    p = par()$usr/15
    dx = p[2]-p[1]
    dy = p[4]-p[3]

    # Pie Position:
    if(is.null(piePos)) {
        Data = getSeries(object)
        Spec = getSpec(object)
        Constraints = getConstraints(object)
        tg = getTargetReturn(tangencyPortfolio(Data, Spec, Constraints))
        ef = as.vector(getTargetReturn(object))
        piePos = which(diff(sign(ef-tg)) > 0)
    }

    # Pie Radius:
    if(is.null(pieR)) {
        pieR = c(1, 1)
    }

    # Pie Offset:
    if(is.null(pieOffset)) {
        pieOffset = c(-2*dx, 0)
    }

    # Plot Circle:
    weights = getWeights(object)[piePos, ]
    nWeights = length(weights)
    Sign = rep("+", nWeights)
    Sign[(1:nWeights)[weights < 0]] = "-"
    x = getTargetRisk(object)[piePos]
    y = getTargetReturn(object)[piePos]
    phi =  seq(0, 2*pi, length = 360)
    X = x + pieOffset[1] + pieR[1] * sin(phi) * dx
    Y = y + pieOffset[2] + pieR[2] * cos(phi) * dy
    lines(X, Y)

    # Add Center Point:
    points(x, y, col = "red", pch = 19, cex = 1.5)

    # Add Arrow:
    lines(c(x, x+pieOffset[1]), c(y, y+pieOffset[2]))

    # Add Color Wheel:
    psi = 2*pi*c(0, cumsum(abs(weights)/sum(abs(weights))))
    for (i in 1 : length(weights) ) {
        # Plotting Only Pie pieces with Weights > 5%
        if(psi[i+1]-psi[i] > 0.05 * 2*pi) {
            Psi = psi[i] + (0:100) * (psi[i+1]-psi[i])/100
            polyX = x + pieOffset[1] + pieR[1]*c(0, sin(Psi), 0) * dx
            polyY = y + pieOffset[2] + pieR[2]*c(0, cos(Psi), 0) * dy
            polygon(polyX, polyY, col = rainbow(nWeights)[i])
            # Adding the Asset Signs:
            text(x + pieOffset[1] + 0.75*pieR[1]* sin(Psi[51]) * dx,
                y + pieOffset[2] + 0.75*pieR[2]* cos(Psi[51]) * dy,
                col = "white", Sign[i])
         }
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.attributesWheel <-
    function(object, piePos = NULL, pieR = NULL, pieOffset = NULL, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of the weights

    # Details:
    #   The default settings are:
    #   piePos - Position of tangency Portfolio
    #   pieR - 10% of the Risk Range: diff(range(targetRisk(object)))/10

    # FUNCTION:

    # Extraction coordinates
    p = par()$usr/15
    dx = p[2]-p[1]
    dy = p[4]-p[3]

    # Pie Position:
    if(is.null(piePos)) {
        Data = getSeries(object)
        Spec = getSpec(object)
        Constraints = getConstraints(object)
        tg = getTargetReturn(tangencyPortfolio(Data, Spec, Constraints))
        ef = as.vector(getTargetReturn(object))
        piePos = which(diff(sign(ef-tg)) > 0)
    }

    # Pie Radius:
    if(is.null(pieR)) {
        pieR = c(1, 1)
    }

    # Pie Offset:
    if(is.null(pieOffset)) {
        pieOffset = c(2*dx, 0)
    }

    # Plot Circle - Get weighted Returns:
    weights = getWeights(object)
    dim = dim(weights)
    returns = getStatistics(object)$mu
    weightedReturns = NULL
    for(i in 1:dim[2]){
        nextWeightedReturns = weights[,i]*returns[i]
        weightedReturns = cbind(weightedReturns, nextWeightedReturns)
    }
    colnames(weightedReturns) = colnames(weights)
    weightedReturns = weightedReturns[piePos, ]
    nWeights = length(weightedReturns)
    Sign = rep("+", times =  nWeights)
    Sign[(1:nWeights)[weightedReturns < 0]] = "-"
    x = getTargetRisk(object)[piePos]
    y = getTargetReturn(object)[piePos]
    phi =  seq(0, 2*pi, length = 360)
    X = x + pieOffset[1] + pieR[1] * sin(phi) * dx
    Y = y + pieOffset[2] + pieR[2] * cos(phi) * dy
    lines(X, Y)

    # Add Center Point:
    points(x, y, col = "red", pch = 19, cex = 1.5)

    # Add Arrow:
    lines(c(x, x+pieOffset[1]), c(y, y+pieOffset[2]))

    # Add Color Wheel:
    psi = 2*pi*c(0, cumsum(abs(weightedReturns)/sum(abs(weightedReturns))))
    for (i in 1 : nWeights) {
        # Plotting Only Pie pieces with Weights > 5%
        if(psi[i+1]-psi[i] > 0.05 * 2*pi) {
            Psi = psi[i] + (0:100) * (psi[i+1]-psi[i])/100
            polyX = x + pieOffset[1] + pieR[1]*c(0, sin(Psi), 0) * dx
            polyY = y + pieOffset[2] + pieR[2]*c(0, cos(Psi), 0) * dy
            polygon(polyX, polyY, col = rainbow(nWeights)[i])
            # Adding the Asset Signs:
            text(x + pieOffset[1] + 0.75*pieR[1]* sin(Psi[51]) * dx,
                y + pieOffset[2] + 0.75*pieR[2]* cos(Psi[51]) * dy,
                col = "white", Sign[i])
         }
    }

    # Return Value:
    invisible()
}


#-------------------------------------------------------------------------------


.notStackedWeightsPlot <-
    function(object, col = NULL)
{
    # A function implemented by Rmetrics

    # Description:

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette

    # FUNCTION:

    # Settings:
    weights = getWeights(object)
    N = ncol(weights)
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)

    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(N)

    # Plot first asset ...
    plot(weights[, 1], col = col[1], type = "l", ylim = c(min(weights),
        max(weights)), xaxt = "n", xlab = "", ylab = "")

    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = min(targetRisk)

    # Big Point at minimum risk for first asset ...
    points(x = minIndex, y = weights[minIndex, 1], col = col[1], pch = 19,
        xaxt = "n", yaxt = "n", cex = 2)

    # ... and all other assets
    for(i in 1:(N-1)){
        points(weights[, i+1], col = col[i+1], type = "l", xaxt = "n",
        yaxt = "n")
        points(x = minIndex, y = weights[minIndex, i+1], col = col[i+1],
            pch = 19, xaxt = "n", yaxt = "n", cex = 2)
    }
    grid()
    abline(h = 0, col = "grey", lty = 3)
    lines(x = c(minIndex, minIndex), y = c(0, 1), col = "black", lwd = 2)

    # Add Tailored Labels -  6 may be a good Number ...
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) * nLabels + 1
    text(minIndex, 1, "Min Risk", pos = 4)
    minRiskValue = as.character(signif(minRisk, 3))
    minReturnValue = as.character(signif(targetReturn[minIndex], 3))
    mtext(minRiskValue, side = 1, at = minIndex, cex = 0.7)
    mtext(minReturnValue, side = 3, line = 0.5, at = minIndex, cex = 0.7)

    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))

    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = 0.7)
    mtext("Target Return", side = 3, line = 2, cex = 0.7)
    mtext("Weight", side = 2, line = 2, cex = 0.7)

    # Add Info:
    mtext(paste(getType(object), "|", getSolver(object)),
        side = 4, adj = 0, col = "grey", cex = 0.7)

    # Add Title:
    mtext("Weights", adj = 0, line = 2.5, font = 2, cex = 0.8)

    # Return Value:
    invisible()
}


#-------------------------------------------------------------------------------


.addlegend <-
    function(object, control = list())
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds a perdefined legend to sliders

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   control - control list for colors and symbols

    # FUNCTION:

    # Settings:
    dim = getNAssets(object)
    namesSingleAsset = names(object@data$statistics$mu)
    # Check if polt is used for forntierSlider...
    if(control$sliderFlag == "frontier"){
        legendtext = c("Efficient Frontier", "Sharpe Ratio", "Minimum Variance",
            "Tangency Portfolio", "Market Portfolio", "Equal Weights",
            namesSingleAsset)
        color = c("black", control$sharpeRatio.col, control$minvariance.col,
            control$tangency.col, control$cml.col, control$equalWeights.col,
            control$singleAsset.col)
        sym = c(19, 19, control$minvariance.pch, control$tangency.pch,
            control$cml.pch, control$equalWeights.pch,
            rep(control$singleAsset.pch, times = dim))
    # ... else is the weightsSlider case
    } else {
            legendtext = c("Efficient Frontier", "Minimum Variance",
            "Tangency Portfolio", namesSingleAsset)
        color = c("black", control$minvariance.col,
            control$tangency.col, control$singleAsset.col)
        sym = c(19, control$minvariance.pch, control$tangency.pch,
            rep(control$singleAsset.pch, times = dim))
    }

    # Adding Legend:
    legend("topleft", legend = legendtext, col = color, pch = sym, cex = .8,
        bty = "n")

    # Return Value:
    invisible()

}


################################################################################

