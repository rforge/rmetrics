
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
#   1999 - 2009, Rmetrics Association, Zurich
#   1999 - 2009, Diethelm Wuertz <wuertz@itp.phys.ethz.ch>  
#   www.rmetrics.org
# for code accessed (or partly included) from other R-ports 
#   and other sources see R's copyright and license files


################################################################################
# PLOT FUNCTIONS:       DESCRIPTION:
# backtestStats          Wrapper function for calculating rolling statistics
# rollingSigma           Rolling portfolio Sigma risk
# rollingVaR             Rolling Value at Risk
# rollingCVaR            Rolling Conditional Value at Risk
# rollingDar             Rolling Drawdowns at Risk
# rollingCDaR            Rolling Conditional Drawdowns at Risk
# .rollingRiskBudgets    Rolling portfolio risk budgets
################################################################################


# backtestStats <- function(object, FUN = "rollingSigma", ...) 
#     UseMethod("portfolioBacktesting")
# rollingSigma <- function(object) UseMethod("portfolioBacktesting")
# rollingVaR <- function(object) UseMethod("portfolioBacktesting")
# rollingCVaR <- function(object) UseMethod("portfolioBacktesting")
# rollingDaR <- function(object) UseMethod("portfolioBacktesting")
# rollingCDaR <- function(object) UseMethod("portfolioBacktesting")
# .rollingRiskBudgets <- function(object) UseMethod("portfolioBacktesting")


# ------------------------------------------------------------------------------


backtestStats <- 
function(object, FUN = "rollingSigma", ...)
{
    # A function implemented by William Chen
    
    # Description:
    #   Wrapper function for calculating rolling statistics
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    #   FUN - a character string, the name of the statistics function
        
    # Example:
    #   data = returns(align(SPISECTOR))
    #   formula <- SPI ~ BASI+INDU+CONG+HLTH+CONS+TELE+UTIL+FINA+TECH
    #   backtests <- portfolioBacktesting(formula, data, trace = FALSE) 
    #   portfolios <- portfolioSmoothing(backtests, portfolioBacktest())
         
    # FUNCTION:

    # Perform Statistics:
    statsFun <- match.fun(FUN)
    ans <- statsFun(object, ...)

    # Return Value
    ans
}

# ------------------------------------------------------------------------------


rollingSigma <-
function(object)
{
    # A function implemented by William Chen and Diethelm Wuertz
    
    # Description:
    #   Returns rolling sigmas from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingSigma(object)
    
    # FUNCTION:
    
    # quick fix ... there is some confusion with getTargetRisk of
    # @portfolio and @spec
    portfolios <- object$strategyList
    prtval <- lapply(portfolios, slot, "portfolio")
    ans <- sapply(prtval, function(x) getTargetRisk(x)["Sigma"])
    dates <- sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return Value:
    timeSeries(ans, charvec = dates, units = "Sigma")
}


# ------------------------------------------------------------------------------


rollingVaR <-
function(object)
{
    # A function implemented by William Chen
    
    # Description:
    #   Returns rolling VaR from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingVaR(object)
    
    # FUNCTION:
    
    # calculate VaR for one portfolio:
    .var = function(x) {
        alpha = getAlpha(x)
        R = getSeries(x) %*% getWeights(x)
        quantile.default(R, probs = alpha) }
        
    # Get Portfolios:
    portfolios <- object$strategyList
    
    # Calculates VaR for all portfolios:
    ans = sapply(portfolios, FUN = .var)

    # Extracts the dates:
    dates <- sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return Value:
    alpha = getAlpha(portfolios[[1]])
    timeSeries(ans, charvec = dates, units = paste("VaR", alpha, sep = "."))
}


# ------------------------------------------------------------------------------


rollingCVaR <- 
function(object)
{
    # A function implemented by William Chen
    
    # Description:
    #   Returns rolling DVaR from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingCVaR(object)
    
    # FUNCTION:
    
    # Calculate CVaR for one portfolio:
    .cvar = function(x) {
        alpha = getAlpha(x)
        R = getSeries(x) %*% getWeights(x)
        z = quantile.default(R, probs = alpha)
        mean(as.numeric(R)[R <= z], na.rm = TRUE)}

    # Get Portfolios:
    portfolios <- object$strategyList
    
    # Calculate CVaR for all portfolios:
    ans = sapply(portfolios, FUN = .cvar)

    # Extract the Dates:
    dates = sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return:
    alpha = getAlpha(portfolios[[1]])
    timeSeries(ans, charvec = dates, units = paste("CVaR", alpha, sep = "."))
}


# ------------------------------------------------------------------------------


rollingDaR <-
function(object)
{
    # A function implemented by William Chen
    
    # Description:
    #   Returns rolling DaR from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingDaR(object)
    
    # FUNCTION:
    
    # calculate DaR for one portfolio:
    .dar = function(x) {
        alpha = getAlpha(x)
        R = getSeries(x) %*% getWeights(x)
        dd = 100 * drawdowns(as.timeSeries(R)/100)
        quantile.default(dd, probs = alpha)}

    # Get Portfolios:
    portfolios <- object$strategyList
    
    # Calculates DaR for all portfolios:
    ans = sapply(portfolios, FUN = .dar)

    # Extracts the dates:
    dates = sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return:
    alpha = getAlpha(portfolios[[1]])
    timeSeries(ans, charvec = dates, units = paste("DaR", alpha, sep = "."))
}


# ------------------------------------------------------------------------------


rollingCDaR <- 
function(object)
{
    # A function implemented by William Chen
    
    # Description:
    #   Returns rolling CDaR from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingCDaR(object)
    
    # FUNCTION:
    
    # Calculate CDaR for one portfolio:
    .cdar = function(x){
        alpha = getAlpha(x)
        R = as.numeric(getSeries(x) %*% getWeights(x))
        dd = 100 * drawdowns(as.timeSeries(R)/100)
        z = quantile.default(dd, probs = alpha)
        mean(dd[dd <= z])
        }

    # Get Portfolios:
    portfolios <- object$strategyList
    
    # Calculate CVaR for all portfolios:
    ans = sapply(portfolios, FUN = .cdar)

    # Extract the dates:
    dates = sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return:
    alpha = getAlpha(portfolios[[1]])
    timeSeries(ans, charvec = dates, units = paste("CDaR", alpha, sep = "."))
}


# ------------------------------------------------------------------------------


.rollingRiskBudgets <- 
function(object)
{
    # A function implemented by William Chen
    
    # Description:
    #   Returns rolling RiskBudgets from an object of class fPFOLIOBACKTEST
    
    # Arguments:
    #   object - a list as returned by the function portfolioBacktesting()
    
    # Example:
    #   rollingCDaR(object)
    
    # FUNCTION:
    
    # Portfolios:
    portfolios = object$StrategyList
    nPortfolios = length(portfolios)
    assetNames = colnames(getSeries(portfolios[[1]]))

    ans = NULL
    for (i in 1:nPortfolios) {
        ans = rbind(ans, getCovRiskBudgets(portfolios[[i]])) }

    # Extract the Dates:
    dates = sapply(portfolios, function(x) rev(rownames(getSeries(x)))[1])

    # Return:
    timeSeries(ans, charvec = dates, units = assetNames)
}


################################################################################

