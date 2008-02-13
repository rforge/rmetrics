
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
#  weightsPie                   Plots staggered weights
#  attributesPie                Plots weighted means
#  covRiskBudgetsPie            Plots covariance risk budgets
#  tailRiskBudgetsPie           Plots tail risk budgets
################################################################################


weightsPie <- 
    function(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a Pie Chart of Weigths
        
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # Example:
    #   weightsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")
    
    # FUNCTION:
    
    # Extracting weights position, if specified
    if(!is.null(pos)){
        Object = object
        object@portfolio$weights = getWeights(Object)[pos, ]
    }

    # Plot Circle:
    weights = getWeights(object)
    nWeights = length(weights)
    # if(length(weights) != nWeights) stop("Plot position is not specified")
    Sign = rep("+", nWeights)
    Sign[(1:nWeights)[weights < 0]] = "-"
    
    # Color Palette:
    if (is.null(col)) col = rainbow(nWeights)
    
    # Pie Chart:
    Weights = abs(weights)
    Index = (1:nWeights)[Weights > 0]
    col = col[Index]
    names = names(weights)
    legendAssets = names[Index]
    Labels = paste(names, Sign)
    Labels = Labels[Weights > 0]
    Weights = Weights[Weights > 0]
    Radius = 0.8
    if (length(Weights) > 10) Radius = 0.65
    pie(Weights, labels = Labels, col = col, radius = Radius)
    if (box) box()
    
    # Add Title:
    title(main = "Weights")
    
    # Add Info:
    mtext(paste(getType(object), "|", getSolver(object)), 
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Legend:
    if (legend) {
        # Add Legend:
        legend("topleft", legend = legendAssets, bty = "n", cex = 0.8, 
            fill = col)
        
        # Add Legend:
        legendWeights = as.character(round(100*Weights, digits = 1))
        legendWeights = paste(Sign[Index], legendWeights, sep = "")
        legendWeights = paste(legendWeights, "%")
        legend("topright", legend = legendWeights, bty = "n", cex = 0.8, 
            fill = col)
    }
    
    # Return Value:
    invisible()
}



# ------------------------------------------------------------------------------


attributesPie <- 
    function(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of the weights
        
    # Example:
    #   attributesPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")
    
    # FUNCTION:
    
    # Extracting weights position, if specified
    if(!is.null(pos)){
        Object = object
        object@portfolio$weights = getWeights(Object)[pos, ]
    }
    
    # Get weighted Returns:
    weights = getWeights(object)
    names = names(weights)
    nWeights = length(weights)
    # if(length(weights) != nWeights) stop("Plot position is not specified")
    returns = getStatistics(object)$mu
    weightedReturns = weights * returns
    
    # Plot Circle:
    Sign = rep("+", nWeights)
    Sign[(1:nWeights)[weightedReturns < 0]] = "-"
    names = substr(names, 1, 3)
    
    # Color Palette:
    if (is.null(col)) col = rainbow(nWeights)

    # Pie Chart:
    WeightedReturns = abs(weightedReturns)
    Index = (1:nWeights)[WeightedReturns > 0]
    col = col[Index]
    names = names(weights)
    legendAssets = names[Index]
    Labels = paste(names, Sign)
    Labels = Labels[WeightedReturns > 0]
    WeightedReturns = WeightedReturns[WeightedReturns > 0]
    Radius = 0.8
    if (length(WeightedReturns) > 10) Radius = 0.65
    pie(WeightedReturns, labels = Labels, col = col, radius = Radius)
    if (box) box()
    
    # Add Title:
    title(main = "Investments")
    
    # Add Info:
    mtext(paste(getType(object), "|", getSolver(object)), 
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Legend:
    if (legend) {
        # Add Legend:
        legend("topleft", legend = legendAssets, bty = "n", cex = 0.8, 
            fill = col)
        
        # Add Legend:
        sumWeightedReturns = sum(WeightedReturns)
        legendWeights = as.character(round(100*WeightedReturns/
            sumWeightedReturns, digits = 1))
        legendWeights = paste(Sign[Index], legendWeights)
        legendWeights = paste(legendWeights, "%")
        legend("topright", legend = legendWeights, bty = "n", cex = 0.8, 
            fill = col)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


covRiskBudgetsPie <- 
    function(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a Pie Chart of Risk Budgets
        
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # Example:
    #   riskBudgetsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")
    
    # FUNCTION:
    
    # Extracting weights position, if specified
    if(!is.null(pos)){
        Object = object
        object@portfolio$weights = getWeights(Object)[pos, ]
    }

    # Plot Circle:
    riskBudgets = getCovRiskBudgets(object)
    nRiskBudgets = length(riskBudgets)
    if(length(riskBudgets) != nRiskBudgets) 
        stop("Plot position is not specified")
    Sign = rep("+", nRiskBudgets)
    Sign[(1:nRiskBudgets)[riskBudgets < 0]] = "-"
    
    # Color Palette:
    if (is.null(col)) col = rainbow(nRiskBudgets)
    
    # Pie Chart:
    RiskBudgets = abs(riskBudgets)
    Index = (1:nRiskBudgets)[RiskBudgets > 0]
    col = col[Index]
    names = names(RiskBudgets)
    legendAssets = names[Index]
    Labels = paste(names, Sign)
    Labels = Labels[RiskBudgets > 0]
    RiskBudgets = RiskBudgets[RiskBudgets > 0]
    Radius = 0.8
    if (length(RiskBudgets) > 10) Radius = 0.65
    pie(RiskBudgets, labels = Labels, col = col, radius = Radius)
    if (box) box()
    
    # Add Title:
    title(main = "Cov Risk Budgets")
    
    # Add Info:
    mtext(paste(getType(object), "|", getSolver(object)), 
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Legend:
    if (legend) {
        # Add Legend:
        legend("topleft", legend = legendAssets, bty = "n", cex = 0.8, 
            fill = col)
        
        # Add Legend:
        legendRiskBudgets = as.character(round(100*RiskBudgets, digits = 1))
        legendRiskBudgets = paste(Sign[Index], legendRiskBudgets)      
        legendRiskBudgets = paste(legendRiskBudgets, "%")
        legend("topright", legend = legendRiskBudgets, bty = "n", cex = 0.8, 
            fill = col)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


tailRiskBudgetsPie <- 
    function(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Plots a Pie Chart of Tail Risk Budgets
        
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # Example:
    #   riskBudgetsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")
    
    # FUNCTION:
    
    # Extracting weights position, if specified
    if(!is.null(pos)){
        Object = object
        object@portfolio$weights = getWeights(Object)[pos, ]
    }

    # Plot Circle:
    riskBudgets = getTailRiskBudgets(object)
    nRiskBudgets = length(riskBudgets)
    if(length(riskBudgets) != nRiskBudgets) 
        stop("Plot position is not specified")
    Sign = rep("+", nRiskBudgets)
    Sign[(1:nRiskBudgets)[riskBudgets < 0]] = "-"
    
    # Color Palette:
    if (is.null(col)) col = rainbow(nRiskBudgets)
    
    # Pie Chart:
    RiskBudgets = abs(riskBudgets)
    Index = (1:nRiskBudgets)[RiskBudgets > 0]
    col = col[Index]
    names = names(RiskBudgets)
    legendAssets = names[Index]
    Labels = paste(names, Sign)
    Labels = Labels[RiskBudgets > 0]
    RiskBudgets = RiskBudgets[RiskBudgets > 0]
    Radius = 0.8
    if (length(RiskBudgets) > 10) Radius = 0.65
    pie(RiskBudgets, labels = Labels, col = col, radius = Radius)
    if (box) box()
    
    # Add Title:
    title(main = "Tail Risk Budgets")
    
    # Add Info:
    mtext(paste(getType(object), "|", getSolver(object)), 
        side = 4, adj = 0, col = "grey", cex = 0.7)
    
    # Add Legend:
    if (legend) {
        # Add Legend:
        legend("topleft", legend = legendAssets, bty = "n", cex = 0.8, 
            fill = col)
        
        # Add Legend:
        legendRiskBudgets = as.character(round(100*RiskBudgets, digits = 1))
        legendRiskBudgets = paste(Sign[Index], legendRiskBudgets)      
        legendRiskBudgets = paste(legendRiskBudgets, "%")
        legend("topright", legend = legendRiskBudgets, bty = "n", cex = 0.8, 
            fill = col)
    }
    
    # Return Value:
    invisible()
}


################################################################################

