
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
# FUNCTION:                    FRONTIER BAR PLOTS:
#  weightsPlot                  Plots staggered weights
#  weightedReturnsPlot          Plots staggered weighted returns
#  covRiskBudgetsPlot           Plots covariance risk budgets
#  tailRiskBudgetsPlot          Plots copulae tail risk budgets
################################################################################


weightsPlot <-
    function(object, labels = TRUE, col = NULL, title = TRUE,
    box = TRUE, legend = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of weights

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   labels - should the graph be automatically labeled?
    #   col - a color palette, by default the rainbow palette
    #   title - should the graph get default title and labels?
    #   legend - should a legend be added to the plot?

    # FUNCTION:

    # Use default color if not specified ...
    Title = "Weights"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Compute Weights:
    weights = getWeights(object)
    pos.weights = +0.5 * (abs(weights) + weights)
    neg.weights = -0.5 * (abs(weights) - weights)

    # Define Plot Range:
    ymax = max(rowSums(pos.weights))
    ymin = min(rowSums(neg.weights))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    dim = dim(weights)
    range = dim[1]
    xmin = 0
    xmax = range + 0.2 * range

    # Create Bar Plots:
    if (labels) {
        if(!legend){
        barplot(t(pos.weights), col = col, space = 0, ylab = "",
            ylim = c(ymin, ymax), border = "grey", ...)
        } else {
            barplot(t(pos.weights), col = col, space = 0, ylab = "",
                xlim = c(xmin, xmax), ylim = c(ymin, ymax),
                border = "grey", ...)
            legendtext = names(getStatistics(object)$mu)
            if(is.null(legendtext)){
                for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
            }
            legend("topright", legend = legendtext, bty = "n", cex = 0.7,
                fill = col)
        }
        barplot(t(neg.weights), col = col, space = 0, add = TRUE,
            border = "grey", ...)
    } else {
        barplot(t(pos.weights), col = col, ...)
    }

    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)
    targetReturn = getTargetReturn(object)
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1
    nSignifDigits = 3
    axis(3, at = M, labels = signif(targetRisk[M], nSignifDigits))
    axis(1, at = M, labels = signif(targetReturn[M], nSignifDigits))

    # Add Axis Labels and Title:
    if (title) {
        mtext("Target Risk", side = 3, line = 2, adj = 1, cex = CEX)
        mtext("Target Return", side = 1, line = 2, adj = 1, cex = CEX)
        mtext("Weight", side = 2, line = 2, adj = 1, cex = CEX)
    }

    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3)
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)

    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk[, 1])
    minRisk = signif(min(targetRisk[, 1]), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)

    # Add Info:
    if (title) {
        mtext(paste(getType(object), "|", getSolver(object), "|", "minRisk =",
        minRisk), side = 4, adj = 0, col = "grey", cex = CEX)
    }

    # Add Title:
    if (title) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Complete to draw box ...
    if (box) box()

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


weightedReturnsPlot <-
    function(object, labels = TRUE, col = NULL, title = TRUE,
    box = TRUE, legend = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots weighted returns

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   labels - should the graph be automatically labeled?
    #   col - a color palette, by default the rainbow palette
    #   title - should the graph get default title and labels?
    #   legend - should a legend be added to the plot?

    # FUNCTION:

    # Use default color if not specified ...
    Title = "Weighted Returns"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Compute Weighted Returns:
    weights = getWeights(object)
    dim = dim(weights)
    returns = getStatistics(object)$mu
    weightedReturns = NULL
    for(i in 1:dim[2]){
        nextWeightedReturns = weights[,i]*returns[i]
        weightedReturns = cbind(weightedReturns, nextWeightedReturns)
    }
    colnames(weightedReturns) = colnames(weights)
    pos.weightedReturns = +0.5 * (abs(weightedReturns) + weightedReturns)
    neg.weightedReturns = -0.5 * (abs(weightedReturns) - weightedReturns)

    # Define Plot Range:
    ymax = max(rowSums(pos.weightedReturns))
    ymin = min(rowSums(neg.weightedReturns))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    range = dim[1]
    xmin = 0
    xmax = range + 0.2 * range

    # Create Bar Plots:
    if (labels) {
        if(legend){
            barplot(t(pos.weightedReturns), space = 0, ylab = "",
                xlim = c(xmin, xmax), ylim = c(ymin, ymax), col = col,
                border = "grey", ...)
            legendtext = names(getStatistics(object)$mu)
            if(is.null(legendtext)){
                for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
            }
            legend("topright", legend = legendtext, bty = "n", cex = 0.7,
                fill = col)
        } else {
            barplot(t(pos.weightedReturns), space = 0, ylab = "",
                ylim = c(ymin, ymax), col = col, border = "grey", ...)
        }
        barplot(t(neg.weightedReturns), space = 0, add = TRUE, col = col,
            border = "grey", ...)
    } else {
        barplot(t(pos.weightedReturns), col = col, ...)
    }

    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) *nLabels + 1
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nSignifDigits = 3
    axis(3, at = M, labels = signif(targetRisk[M], nSignifDigits))
    axis(1, at = M, labels = signif(targetReturn[M], nSignifDigits))

    # Add Axis Labels and Title:
    if (title) {
        mtext("Target Risk", side = 3, line = 2, adj = 1, cex = CEX)
        mtext("Target Return", side = 1, line = 2, adj = 1, cex = CEX)
        mtext("Weighted Return", side = 2, line = 2, adj = 1, cex = CEX)
    }

    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3)
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)

    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk))
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)

    # Add Info Text:
    if (title) {
        mtext(paste(getType(object), "|", getSolver(object), "|",
            "minRisk =", minRisk), side = 4, adj = 0, col = "grey",
            cex = 0.7)
    }

    # Add Title:
    if (title) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Complete to draw box ...
    if (box) box()

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


covRiskBudgetsPlot <-
    function(object, labels = TRUE, col = NULL, title = TRUE,
    box = TRUE, legend = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of covariance risk budgets

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   labels - should the graph be automatically labeled?
    #   col - a color palette, by default the rainbow palette
    #   title - should the graph get default title and labels?
    #   legend - should a legend be added to the plot?

    # FUNCTION:

    # Use default color if not specified ...
    Title = "Cov Risk Budgets"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Compute Covariance Risk Budgets:
    budgets = getCovRiskBudgets(object)
    pos.budgets = +0.5 * (abs(budgets) + budgets)
    neg.budgets = -0.5 * (abs(budgets) - budgets)

    # Define Plot Range:
    ymax = max(rowSums(pos.budgets))
    ymin = min(rowSums(neg.budgets))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    dim = dim(budgets)
    range = dim[1]
    xmin = 0
    xmax = range + 0.2 * range

    # Create Bar Plots:
    if (labels) {
        if(!legend){
            barplot(t(pos.budgets), space = 0, ylab = "",
                ylim = c(ymin, ymax), col = col, border = "grey", ...)
        } else {
            legendtext = names(getStatistics(object)$mu)
            if(is.null(legendtext)){
                for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
            }
            barplot(t(pos.budgets), space = 0, ylab = "", xlim = c(xmin, xmax),
                ylim = c(ymin, ymax), col = col, border = "grey", ...)
            legend("topright", legend = legendtext, bty = "n", cex = 0.7,
                fill = col)
        }
        barplot(t(neg.budgets), space = 0, add = TRUE, col = col,
            border = "grey", ...)
    } else {
        barplot(t(pos.budgets), col = col, ...)
    }

    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1

    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nSignifDigits = 3
    axis(3, at = M, labels = signif(targetRisk[M], nSignifDigits))
    axis(1, at = M, labels = signif(targetReturn[M], nSignifDigits))

    # Add Axis Labels and Title:
    if(title) {
        mtext("Target Risk", side = 3, line = 2, adj = 1, cex = CEX)
        mtext("Target Return", side = 1, line = 2, adj = 1, cex = CEX)
        mtext("Cov Risk Budgets", side = 2, line = 2, adj = 1, cex = CEX)
    }

    # Add Budgets 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3)
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)

    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)

    # Add Info:
    if (title) {
        mtext(paste(
            getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
            side = 4, adj = 0, col = "grey", cex = CEX)
    }

    # Add Title:
    if (title) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Complete to draw box ...
    if (box) box()

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


tailRiskBudgetsPlot <-
    function(object, labels = TRUE, col = NULL, title = TRUE,
    box = TRUE, legend = TRUE, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of tail risk budgets

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   labels - should the graph be automatically labeled?
    #   col - a color palette, by default the rainbow palette
    #   title - should the graph get default title and labels?
    #   legend - should a legend be added to the plot?

    # FUNCTION:

    # Use default color if not specified ...
    Title = "Tail Risk Budgets"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Check:
    stop("Not yet implemented")
    tailRiskMatrix = getTailRisk(object)

    # Compute Tail Risk Budgets:
    budgets = getTailRiskBudgets(object)
    budgets[is.na(budgets)] = 0
    pos.budgets = +0.5 * (abs(budgets) + budgets)
    neg.budgets = -0.5 * (abs(budgets) - budgets)

    # Define Plot Range:
    ymax = max(rowSums(pos.budgets))
    ymin = min(rowSums(neg.budgets))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    dim = dim(budgets)
    range = dim[1]
    xmin = 0
    xmax = range + 0.2 * range

    # Create Bar Plots:
    if(!legend){
        barplot(t(pos.budgets), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey", ...)
    } else {
        legendtext = names(getStatistics(object)$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.budgets), space = 0, ylab = "", xlim = c(xmin, xmax),
            ylim = c(ymin, ymax), col = col, border = "grey", ...)
        legend("topright", legend = legendtext, bty = "n", cex = 0.7,
            fill = col)
    }
    barplot(t(neg.budgets), space = 0, add = TRUE, col = col,
            border = "grey", ...)

    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1
    nSignifDigits = 3
    axis(3, at = M, labels = signif(targetRisk[M], nSignifDigits))
    axis(1, at = M, labels = signif(targetReturn[M], nSignifDigits))

    # Add Axis Labels and Title:
    if (title) {
        mtext("Target Risk", side = 3, line = 2, adj = 1, cex = CEX)
        mtext("Target Return", side = 1, line = 2, adj = 1, cex = CEX)
        mtext("Weight", side = 2, line = 2, adj = 1, cex = CEX)
    }

    # Add Budgets 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3)
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)

    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)

    # Add Info:
    if (title) {
        mtext(paste(
            getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
            side = 4, adj = 0, col = "grey", cex = CEX)
    }

    # Add Title:
    if (title) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Complete to draw box ...
    if (box) box()

    # Return Value:
    invisible()
}


################################################################################

