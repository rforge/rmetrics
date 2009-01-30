
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
#  frontierPlotControl          Sets frontier plot control parameters
# FUNCTION:                    DESCRIPTION:
#   .weightsWheel               Adds a pie of weights to frontier plot
#   .attributesWheel            Adds a pie of attributes to frontier plot
# FUNCTION:                    DESCRIPTION:
#   .notStackedWeightsPlot      Plots the not stacked weights of potfolio
#   .addlegend                  Adds legend to sliders
# FUNCTION:                    DESCRIPTION:
#  tailoredFrontierPlot         Tailored frontier plot wit addons
################################################################################


frontierPlot <-
    function(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, labels = TRUE,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, title = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots the efficient frontier

    # FUNCTION:

    # Check Colors:
    stopifnot(length(col) == 2)

    # Settings:
    frontier = match.arg(frontier)
    fullFrontier = frontierPoints(object, frontier = "both",
        return = return, risk = risk, auto = auto)
    upperFrontier = frontierPoints(object, frontier = "upper",
        return = return, risk = risk, auto = auto)
    lowerFrontier = frontierPoints(object, frontier = "lower",
        return = return, risk = risk, auto = auto)

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
        labs = attr(fullFrontier, "control")
        title(
            main = "Efficient Frontier",
            xlab = paste("Target Risk[", labs[1], "]", sep = ""),
            ylab = paste("Target Return[", labs[2], "]", sep = ""))
    }

    # Return Value:
    invisible(fullFrontier)
}


# ------------------------------------------------------------------------------


minvariancePoints <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the minimum risk point to a MV and CVaR portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Slots:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Add Minimum Variance Point:
    mvPortfolio = minvariancePortfolio(data, spec, constraints)
    assets = frontierPoints(mvPortfolio, return = return, risk = risk,
        auto = auto)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


cmlPoints <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the capital market line to a portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Statistics:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Add Capital Market Line Tangency Point:
    cmlPortfolio = tangencyPortfolio(data, spec, constraints)
    assets = frontierPoints(cmlPortfolio, return = return, risk = risk,
        auto = auto)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


cmlLines <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds the capital market line to a portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Statistics:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Add Capital Market Line:
    cmlPortfolio = tangencyPortfolio(data, spec, constraints)
    riskFreeRate = getRiskFreeRate(spec)
    slope = ((getTargetReturn(cmlPortfolio)[, "mean"] - riskFreeRate) /
        getTargetRisk(cmlPortfolio@portfolio)[, "Cov"])
    if(slope > 0) abline(b = slope, a = riskFreeRate, ...)

    # Return Value:
    invisible(slope)
}


# ------------------------------------------------------------------------------


tangencyPoints <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds tangency point and line to a MV and CVaR portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Slots:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Compute Tangency Portfolio:
    tgPortfolio = tangencyPortfolio(data, spec, constraints)

    # Add Tangency Point:
    assets = frontierPoints(tgPortfolio, return = return, risk = risk,
        auto = auto)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


tangencyLines <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds tangency point and line to a MV and CVaR portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Slots:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Compute Tangency Portfolio:
    tgPortfolio = tangencyPortfolio(data, spec, constraints)

    # Add Tangency Line:
    assets = frontierPoints(tgPortfolio, return = return, risk = risk,
        auto = auto)
    slope = assets[2] / assets[1]
    abline(0, slope, ...)

    # Return Value:
    invisible(list(slope = slope, assets = assets))
}


# ------------------------------------------------------------------------------


equalWeightsPoints =
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds equal weights portfolio to a portfolio plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Statistics:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)
    numberOfAssets = getNAssets(object)

    # Set Equal Weights:
    setWeights(spec) = rep(1/numberOfAssets, times = numberOfAssets)

    # Add Equal Weights Portfolio:
    ewPortfolio = feasiblePortfolio(data, spec, constraints)
    assets = frontierPoints(ewPortfolio, return = return, risk = risk,
        auto = auto)
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


singleAssetPoints <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds all single assets returns and risks to a portfolio plot

    # FUNCTION:

    # Add Single Assets:
    Statistics = getStatistics(object)
    Type = getType(object)

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get auto Risk:
    if (auto) {
        return = "mu"
        Type = getType(object)
        Estimator = getEstimator(object)
        if (Type == "MV") risk = "Cov"
        if (Type == "MV" & Estimator != "covEstimator") risk = "Sigma"
        if (Type == "QLPM") risk = "Sigma"
        if (Type == "CVaR") risk = "CVaR"
    }

    # Return:
    if (return == "mean") {
        Return = Statistics$mean
    } else if (return == "mu") {
        Return = Statistics$mean
    }

    # Risk:
    if (risk == "Cov") {
        Risk = sqrt(diag(Statistics$Cov))
    } else if (risk == "Sigma") {
        Risk = sqrt(diag(Statistics$Sigma))
    } else if (risk == "CVaR") {
        nAssets = getNAssets(object)
        Data = getSeries(object)
        alpha = getAlpha(object)
        Risk = NULL
        for (i in 1:nAssets) Risk = c(Risk, -.cvarRisk(Data[ ,i], 1, alpha))
    } else if (risk == "VaR") {
        nAssets = getNAssets(object)
        Data = getSeries(object)
        alpha = getAlpha(object)
        Risk = NULL
        for (i in 1:nAssets) Risk = c(Risk, -.varRisk(Data[ ,i], 1, alpha))
    }
    Risk = as.vector(Risk)

    # Add Points:
    assets = cbind(targetRisk = Risk, targetReturn = Return)
    attr(assets, "control") <-
        c(targetRisk = risk, targetReturn = return, auto = as.character(auto))
    points(assets, ...)

    # Return Value:
    invisible(assets)
}


# ------------------------------------------------------------------------------


twoAssetsLines <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds efficient long-only frontier of all portfolio pairs

    # Note:
    #   Only supported for "Short" and "LongOnly" Constraints!

    # FUNCTION:

    # Supported ?
    check = rev(attr(object@constraints, "model"))[1]

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Statistics:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)

    # Add Frontiers for all Two-Assets Portfolios:
    N = getNAssets(object)
    setWeights(spec) = NULL
    for ( i in 1:(N-1) ) {
        for (j in (i+1):N ) {
            index = c(i, j)
            data2 = data[, index]
            # Zero-One Constraints2 ?
            ans = portfolioFrontier(data = data2, spec = spec)
            lines(frontierPoints(ans,
                return = return, risk = risk, auto = auto), ...)
        }
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


sharpeRatioLines <-
    function(object,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds Sharpe Ratio

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

    # Get Portfolio Slots:
    data = getSeries(object)
    spec = getSpec(object)
    constraints = getConstraints(object)
    Type = getType(object)

    # Efficient Frontier:
    frontPoints = frontierPoints(object, frontier = "upper",
        return = return, risk = risk, auto = auto)
    x = frontPoints[, 1]
    y = frontPoints[, 2]

    # Tangency Portfolio:
    tangencyPortfolio = tangencyPortfolio(data, spec, constraints)
    # x.tg = getTargetReturn(tangencyPortfolio)[1, "mean"]
    x.tg = frontierPoints(tangencyPortfolio,
        return = return, risk = risk, auto = auto)[, 2]

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
    function(object, mcSteps = 5000,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Adds randomly feasible portfolios to a plot

    # FUNCTION:

    # Match Arguments:
    return = match.arg(return)
    risk = match.arg(risk)

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
        if (Model == "Short" | any(getConstraints(object) == "Short")) {
            # Monte Carlo Loop - Short:
            for (k in 1:mcSteps) {
                s = sign(rnorm(N, mean = rnorm(1)))
                weights = s * abs(rcauchy(N))
                weights = weights / sum(weights)
                Return = as.numeric(mu %*% weights)
                Risk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
                points(Risk, Return, ...)
            }
        } else if (Model == "LongOnly" | any(getConstraints(object) == "LongOnly")) {
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


################################################################################


frontierPlotControl <-
    function(

    # Colors:
    sharpeRatio.col   = "blue",
    minvariance.col   = "red",
    tangency.col      = "steelblue",
    cml.col           = "green",
    equalWeights.col  = "blue",
    singleAsset.col   = "topo.colors",
    twoAssets.col     = "grey",
    monteCarlo.col    = "black",

    # Point Sizes:
    minvariance.cex   = 1.25,
    tangency.cex      = 1.25,
    cml.cex           = 1.25,
    equalWeights.cex  = 1.25,
    singleAsset.cex   = 1.25,
    twoAssets.cex     = 0.01,
    monteCarlo.cex    = 0.01,
    sharpeRatio.cex   = 0.1,

    # Limits:
    xlim              = NULL,
    ylim              = NULL,

    # MC Steps:
    mcSteps           = 5000,

    # Pie Settings:
    pieR              = NULL,
    piePos            = NULL,
    pieOffset         = NULL
    )
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets frontier plot control parameters

    # FUNCTION:

    # Return Value:
    list(

        # Colors:
        sharpeRatio.col  = sharpeRatio.col,
        minvariance.col  = minvariance.col,
        tangency.col     = tangency.col,
        cml.col          = cml.col,
        equalWeights.col = equalWeights.col,
        singleAsset.col  = singleAsset.col,
        twoAssets.col    = twoAssets.col,
        monteCarlo.col   = monteCarlo.col,

        # Point Sizes:
        minvariance.cex  = minvariance.cex,
        tangency.cex     = tangency.cex,
        cml.cex          = cml.cex,
        equalWeights.cex = equalWeights.cex,
        singleAsset.cex  = singleAsset.cex ,
        twoAssets.cex    = twoAssets.cex,
        monteCarlo.cex   = monteCarlo.cex,
        sharpeRatio.cex  = sharpeRatio.cex,

        # Limits:
        xlim             = xlim,
        ylim             = ylim,

        # MC Steps:
        mcSteps          = 5000,

        # Pie Settings:
        pieR             = pieR,
        piePos           = piePos,
        pieOffset        = pieOffset

        )

}


################################################################################


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


tailoredFrontierPlot <-
function(object,
    risk = c("Cov", "Sigma", "CVaR", "VaR"),
    mText = NULL, col = NULL, xlim = NULL, ylim = NULL,
    twoAssets = FALSE)
{

    # 1. Plot the Frontier, add margin text, grid and ablines:
    offset = 0.10
    risk <- match.arg(risk)
    if (is.null(xlim)) {
        if (risk == "Cov") {
            xmax = max(sqrt(diag(getCov(object))))
        }
        if (risk == "Sigma") {
            xmax = max(sqrt(diag(getSigma(object))))
        }
        if (risk == "CVaR") {
            alpha = getAlpha(object)
            quantiles = colQuantiles(getSeries(object), prob = alpha)
            n.max = which.max(-quantiles)
            r = getSeries(object)[, n.max]
            r = r[r < quantiles[n.max]]
            xmax = -mean(r)
        }
        if (risk == "VaR") {
            xmax = max(-colQuantiles(getSeries(object), prob = alpha))
        }
        xlim = c(0, xmax)
        Xlim = c(xlim[1]-diff(xlim)*offset, xlim[2]+diff(xlim)*offset)
    }
    if (is.null(ylim)) {
        ylim = range(getMean(object))
        Ylim = c(ylim[1]-diff(ylim)*offset, ylim[2]+diff(ylim)*offset)
    }
    frontierPlot(object, pch = 19, risk = risk, xlim = Xlim, ylim = Ylim)
    if(is.null(mText)) mText = getTitle(object)
    mtext(mText, side = 3, line = 0.5, font = 2)
    grid()
    abline(h = 0, col = "grey")
    abline(v = 0, col = "grey")

    # 2. Add minimum risk (variance) Portfolio Point:
    data = getData(object)
    spec = getSpec(object)
    constraints = getConstraints(object)
    mvPortfolio = minvariancePortfolio(data, spec, constraints)
    minvariancePoints(object, risk = risk, auto = FALSE,
        pch = 19, col = "red")

    # 3. Add Tangency Portfolio Point and Tangency Line:
    tangencyPoints(object, risk = risk, pch = 19, col = "blue")
    tangencyLines(object, risk = risk, col = "blue")

    # 4. Add Equal Weights Portfolio:
    xy = equalWeightsPoints(object, risk = risk, pch = 15, col = "grey")
    text(xy[, 1]+diff(xlim)/20, xy[, 2]+diff(ylim)/20, "EWP",
        font = 2, cex = 0.7)

    # 5. Add all Assets Points:
    if (is.null(col)) col = rainbow(6)
    xy = singleAssetPoints(object, risk = risk, cex = 1.5,
        col = col, lwd = 2)
    text(xy[, 1]+diff(xlim)/20, xy[, 2]+diff(ylim)/20,
        rownames(xy), font = 2, cex = 0.7)

    # 6. Add optionally all Two Assets  Lines
    if (twoAssets) {
        twoAssetsLines(object, risk = risk, lty = 3, col = "grey")
    }

    # 6. Add Sharpe Ratio Line:
    sharpeRatioLines(object, risk = risk, col = "orange", lwd = 2)

    # Return Value:
    invisible(object)
}


################################################################################

