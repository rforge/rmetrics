
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
#  attributesPlot               Plots weighted means
#  covRiskBudgetsPlot           Plots covariance risk budgets
#  tailRiskBudgetsPlot          Plots tail risk budgets
################################################################################


weightsPlot <- 
    function(object, col = NULL, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of weights
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get Type:
    Type = getType(object)
    
    # Get Weights:
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
    if(!legend){
        barplot(t(pos.weights), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(getStatistics(object)$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.weights), space = 0, ylab = "", xlim = c(xmin, xmax),
            ylim = c(ymin, ymax), col = col, border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.weights), space = 0, add = TRUE, col = col, border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object) 
    targetReturn = getTargetReturn(object) 
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1
    
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = 0.7)
    mtext("Target Return", side = 3, line = 2, cex = 0.7)
    mtext("Weight", side = 2, line = 2, cex = 0.7)
      
    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk[, 1])
    minRisk = signif(min(targetRisk[, 1]), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)
    
    # Add Info:
    mtext(paste(
        getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
        side = 4, adj = 0, col = "grey", cex = 0.7)
        
    # Add Title:
    mtext("Weights", adj = 0, line = 2.5, font = 2, cex = 0.8)
    
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


attributesPlot <- 
    function(object, col = NULL, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots ...
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get weighted Returns:
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
    if(!legend){
        barplot(t(pos.weightedReturns), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(getStatistics(object)$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.weightedReturns), space = 0, ylab = "",
            xlim = c(xmin, xmax), ylim = c(ymin, ymax), col = col,
            border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.weightedReturns), space = 0, add = TRUE, col = col,
        border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) *nLabels + 1
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = 0.7)
    mtext("Target Return", side = 3, line = 2, cex = 0.7)
    mtext("Return", side = 2, line = 2, cex = 0.7)
      
    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk))
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)
   
    # Add Info:
    mtext(paste(
        getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Title:
    mtext("Investments", adj = 0, line = 2.5, font = 2, cex = 0.8)
    
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


covRiskBudgetsPlot <- 
    function(object, col = NULL, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of covariance risk budgets
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get Type:
    Type = getType(object)
    
    # Get Budgets:
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
    if(!legend){
        barplot(t(pos.budgets), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(getStatistics(object)$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.budgets), space = 0, ylab = "", xlim = c(xmin, xmax),
            ylim = c(ymin, ymax), col = col, border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.budgets), space = 0, add = TRUE, col = col, border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1
    
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = .7)
    mtext("Target Return", side = 3, line = 2, cex = .7)
    mtext("Weight", side = 2, line = 2, cex = .7)
      
    # Add Budgets 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)
    
    # Add Info:
    mtext(paste(
        getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Title:
    mtext("Cov Budgets", adj = 0, line = 2.5, font = 2, cex = 0.8)
    
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


tailRiskBudgetsPlot <- 
    function(object, col = NULL, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a bar chart of tail risk budgets
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get Type:
    Type = getType(object)
    
    # Get Budgets:
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
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(getStatistics(object)$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.budgets), space = 0, ylab = "", xlim = c(xmin, xmax),
            ylim = c(ymin, ymax), col = col, border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.budgets), space = 0, add = TRUE, col = col, border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)[, 1]
    targetReturn = getTargetReturn(object)[, 1]
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1:(nSigma %/% nLabels) ) ) *nLabels + 1
    
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = .7)
    mtext("Target Return", side = 3, line = 2, cex = .7)
    mtext("Weight", side = 2, line = 2, cex = .7)
      
    # Add Budgets 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    minIndex = which.min(targetRisk)
    minRisk = signif(min(targetRisk), 3)
    abline(v = minIndex, col = "black", lty = 1, lwd = 2)
    
    # Add Info:
    mtext(paste(
        getType(object), "|", getSolver(object), "|", "minRisk =", minRisk),
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Title:
    mtext("Tail Budgets", adj = 0, line = 2.5, font = 2, cex = 0.8)
    
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


################################################################################

