
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
# FUNCTION:                    PORTFOLIO PIE PLOTS:
#  weightsPie                   Pie Plots weights
#  weightedReturnsPie           Pie Plots weighted means
#  covRiskBudgetsPie            Pie Plots covariance risk budgets
#  tailRiskBudgetsPie           Pie Plots copulae tail risk budgets
################################################################################


weightsPie <-
    function(object, pos = NULL, labels = TRUE, col = NULL,
    box = TRUE, legend = TRUE, radius = 0.8, ...)
{
    # A function implemented by Diethelm Wuertz and Oliver Greshake

    # Description:
    #   Plots a Pie Chart of Weigths

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'.
    #   pos - a numeric value, determining the position on the efficient
    #       frontier plotting the pie, by default NULL, i.e. expecting
    #       an object having only one set of weights like the tangency
    #       portfolio.
    #   box - a logical value, determining whether a frame (box) should
    #       be plotted around the pie, by default TRUE.
    #   col - a color palette, by default the rainbow palette.
    #   legend - a logical value, determining whether a legend with
    #       the names of the assets should be plotted, by default TRUE.

    # Example:
    #   weightsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")

    # FUNCTION:

    # Default Settings:
    Title = "Weights"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Extracting weights position on the efficient frontier:
    if(!is.null(pos)){
        object = object
        object@portfolio$weights = getWeights(object)[pos, ]
    }

    # Get Weights:
    X = getWeights(object)

    # Check for Negative Pie Segments:
    nX = getNAssets(object)
    Sign = rep("+", nX)
    Sign[(1:nX)[X < 0]] = "-"
    absX = abs(X)
    Index = (1:nX)[X > 0]

    # Take care of labels, they are also used by the function pie():
    if (!is.logical(labels)) {
        Names = pieLabels = labels
        labels = FALSE
    } else  {
        Names = pieLabels = getNames(object)
    }

    # Pie Chart:
    col = col[Index]
    legendAssets = Names[Index]
    Labels = paste(Names, Sign)
    Labels = Labels[X > 0]
    Y = X[X > 0]

    # Plot:
    if (labels) {
        pie(Y, labels = Labels, col = col, radius = radius, cex = CEX)
    } else {
        pie(Y, labels = pieLabels, col = col, radius = radius, ...)
    }

    # Add Title:
    if (labels) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Add Info:
    if (labels) {
        mtext(paste(getType(object), "|", getSolver(object)),
            side = 4, adj = 0, col = "grey", cex = 0.7)
    }

    # Add Legend:
    if (legend) {
        legend("topleft", legend = legendAssets, bty = "n", cex = CEX,
            fill = col)
        legendY = as.character(round(100*Y, digits = 1))
        legendY = paste(Sign[Index], legendY, sep = "")
        legendY = paste(legendY, "%")
        legend("topright", legend = legendY, bty = "n", cex = CEX,
            fill = col)
    }

    # Add Box:
    if (box) box()

    # Return Value:
    invisible(Y)
}


# ------------------------------------------------------------------------------


weightedReturnsPie <-
    function(object, pos = NULL, labels = TRUE, col = NULL,
    box = TRUE, legend = TRUE, radius = 0.8, ...)
{
    # A function implemented by Diethelm Wuertz and Oliver Greshake

    # Description:
    #   Adds a pie plot of the weights

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'.
    #   pos - a numeric value, determining the position on the efficient
    #       frontier plotting the pie, by default NULL, i.e. expecting
    #       an object having only one set of weights like the tangency
    #       portfolio.
    #   box - a logical value, determining whether a frame (box) should
    #       be plotted around the pie, by default TRUE.
    #   col - a color palette, by default the rainbow palette.
    #   legend - a logical value, determining whether a legend with
    #       the names of the assets should be plotted, by default TRUE.

    # Example:
    #   attributesPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")

    # FUNCTION:

    # Default Settings:
    Title = "Weighted Returns"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Extracting weights position, if specified
    if(!is.null(pos)){
        object = object
        object@portfolio$weights = getWeights(object)[pos, ]
    }

    # Get Weighted Returns:
    weights = getWeights(object)
    returns = getStatistics(object)$mu
    X = weights * returns

    # Check for Negative Pie Segments:
    nX = getNAssets(object)
    Sign = rep("+", nX)
    Sign[(1:nX)[X < 0]] = "-"
    absX = abs(X)
    Index = (1:nX)[X > 0]

    # Take care of labels, they are also used by the function pie():
    if (!is.logical(labels)) {
        Names = pieLabels = labels
        labels = FALSE
    } else  {
        Names = pieLabels = getNames(object)
    }

    # Pie Chart:
    col = col[Index]
    legendAssets = Names[Index]
    Labels = paste(Names, Sign)
    Labels = Labels[X > 0]
    Y = X[X > 0]

    # Plot:
    if (labels) {
        pie(Y, labels = Labels, col = col, radius = radius, cex = CEX)
    } else {
        pie(Y, labels = pieLabels, col = col, radius = radius, ...)
    }

    # Add Title:
    if (labels) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Add Info:
    if (labels) {
        mtext(paste(getType(object), "|", getSolver(object)),
            side = 4, adj = 0, col = "grey", cex = 0.7)
    }

    # Add Legend:
    if (legend) {
        legend("topleft", legend = legendAssets, bty = "n", cex = CEX,
            fill = col)
        legendY = as.character(round(100*Y, digits = 1))
        legendY = paste(Sign[Index], legendY, sep = "")
        legendY = paste(legendY, "%")
        legend("topright", legend = legendY, bty = "n", cex = CEX,
            fill = col)
    }

    # Add Box:
    if (box) box()

    # Return Value:
    invisible(Y)
}


# ------------------------------------------------------------------------------


covRiskBudgetsPie <-
    function(object, pos = NULL, labels = TRUE, col = NULL,
    box = TRUE, legend = TRUE, radius = 0.8, ...)
{
    # A function implemented by Diethelm Wuertz and Oliver Greshake

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'.
    #   pos - a numeric value, determining the position on the efficient
    #       frontier plotting the pie, by default NULL, i.e. expecting
    #       an object having only one set of weights like the tangency
    #       portfolio.
    #   box - a logical value, determining whether a frame (box) should
    #       be plotted around the pie, by default TRUE.
    #   col - a color palette, by default the rainbow palette.
    #   legend - a logical value, determining whether a legend with
    #       the names of the assets should be plotted, by default TRUE.

    # Description:
    #   Plots a Pie Chart of Risk Budgets

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette

    # Example:
    #   riskBudgetsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")

    # FUNCTION:

    # Default Settings:
    Title = "Covariance Risk Budgets"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Extracting weights position, if specified
    if(!is.null(pos)){
        object@portfolio$weights = getWeights(object)[pos, ]
        object@portfolio$covRiskBudgets = getCovRiskBudgets(object)[pos, ]
    }

    # Get Covariance Risk Budgets:
    X = getCovRiskBudgets(object)

    # Check for Negative Pie Segments:
    nX = getNAssets(object)
    Sign = rep("+", nX)
    Sign[(1:nX)[X < 0]] = "-"
    absX = abs(X)
    Index = (1:nX)[X > 0]

    # Take care of labels, they are also used by the function pie():
    if (!is.logical(labels)) {
        Names = pieLabels = labels
        labels = FALSE
    } else  {
        Names = pieLabels = getNames(object)
    }

    # Legend Labels:
    col = col[Index]
    legendAssets = Names[Index]
    Labels = paste(Names, Sign)
    Labels = Labels[X > 0]
    Y = X[X > 0]

    # Plot:
    if (labels) {
        pie(Y, labels = Labels, col = col, radius = radius, cex = CEX)
    } else {
        pie(Y, labels = pieLabels, col = col, radius = radius, ...)
    }

    # Add Title:
    if (labels) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Add Info:
    if (labels) {
        mtext(paste(getType(object), "|", getSolver(object)),
            side = 4, adj = 0, col = "grey", cex = 0.7)
    }

    # Add Legend:
    if (legend) {
        legend("topleft", legend = legendAssets, bty = "n", cex = CEX,
            fill = col)
        legendY = as.character(round(100*Y, digits = 1))
        legendY = paste(Sign[Index], legendY, sep = "")
        legendY = paste(legendY, "%")
        legend("topright", legend = legendY, bty = "n", cex = CEX,
            fill = col)
    }

    # Add Box:
    if (box) box()

    # Return Value:
    invisible(Y)
}


# ------------------------------------------------------------------------------


tailRiskBudgetsPie <-
    function(object, pos = NULL, labels = TRUE, col = NULL,
    box = TRUE, legend = TRUE, radius = 0.8, ...)
{
    # A function implemented by Diethelm Wuertz and Oliver Greshake

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'.
    #   pos - a numeric value, determining the position on the efficient
    #       frontier plotting the pie, by default NULL, i.e. expecting
    #       an object having only one set of weights like the tangency
    #       portfolio.
    #   box - a logical value, determining whether a frame (box) should
    #       be plotted around the pie, by default TRUE.
    #   col - a color palette, by default the rainbow palette.
    #   legend - a logical value, determining whether a legend with
    #       the names of the assets should be plotted, by default TRUE.

    # Description:
    #   Plots a Pie Chart of Tail Risk Budgets

    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette

    # Example:
    #   riskBudgetsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")

    # FUNCTION:

    # Default Settings:
    Title = "Tail Risk Budgets"
    if (is.null(col)) col = seqPalette(getNAssets(object), "Blues")
    if (sum(c(par()$mfrow, par()$mfcol)) == 4) CEX = 0.9 else CEX = 0.7

    # Extracting weights position, if specified
    if(!is.null(pos)){
        object = object
        object@portfolio$weights = getWeights(object)[pos, ]
    }

    # Check:
    stop("Not yet implemented")
    tailRiskMatrix = getTailRisk(object)
    X = getCovRiskBudgets(object)

    # Check for Negative Pie Segments:
    nX = getNAssets(object)
    Sign = rep("+", nX)
    Sign[(1:nX)[X < 0]] = "-"
    absX = abs(X)
    Index = (1:nX)[X > 0]

    # Take care of labels, they are also used by the function pie():
    if (!is.logical(labels)) {
        Names = pieLabels = labels
        labels = FALSE
    } else  {
        Names = pieLabels = getNames(object)
    }

    # Legend Labels:
    col = col[Index]
    legendAssets = Names[Index]
    Labels = paste(Names, Sign)
    Labels = Labels[X > 0]
    Y = X[X > 0]

    # Plot:
    if (labels) {
        pie(Y, labels = Labels, col = col, radius = radius, cex = CEX)
    } else {
        pie(Y, labels = pieLabels, col = col, radius = radius, ...)
    }

    # Add Title:
    if (labels) mtext(Title, adj = 0, line = 2.5, font = 2, cex = CEX+0.1)

    # Add Info:
    if (labels) {
        mtext(paste(getType(object), "|", getSolver(object)),
            side = 4, adj = 0, col = "grey", cex = 0.7)
    }

    # Add Legend:
    if (legend) {
        legend("topleft", legend = legendAssets, bty = "n", cex = CEX,
            fill = col)
        legendY = as.character(round(100*Y, digits = 1))
        legendY = paste(Sign[Index], legendY, sep = "")
        legendY = paste(legendY, "%")
        legend("topright", legend = legendY, bty = "n", cex = CEX,
            fill = col)
    }

    # Add Box:
    if (box) box()

    # Return Value:
    invisible(Y)
}


################################################################################

