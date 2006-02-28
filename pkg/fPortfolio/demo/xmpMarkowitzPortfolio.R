
#
# Example:
#	Mean variance Portfolio Optimization
#
# Description:
# 	This example shows how to create and optimize a portfolio of simulated 
#	and real world assets by the mean-variance Markowitz approach. 
#
# Content:
#	1. Compute efficient frontier from a simulated set of assets
#	2. Compute efficient frontier from a Berndt's data set of assets
#	3. Optimize the portfolio for "ONE" given target return
#
# References:
#   Elton E.J., Gruber M.J. (1991); 
#       Modern Portfolio Theory and Investment Analysis, 
#       4th Edition, Wiley, NY, pp. 65--93.
#   Huang C.J., Litzenberger R.H. (1988); 
#       Foundations for Financial Economics, 
#       Elsevier, NY, pp. 59--82.
#	 
# Last Check:
#	2006-02-05 ok
#
# Author:
#	(C) 2003-2006, Diethelm Wuertz, GPL
#

################################################################################
# Load Required Package:
	

	# Load:
	require(fPortfolio)
	###

	
# ------------------------------------------------------------------------------
# 1. Compute Efficient frontier from a simulated set of assets
    
    # Composed Model:
    #   n - Number of data records to be simulated
    #   mu - Numeric vector of Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, (measures kurtosis)
    ###
    
    # Set Parameters - Skewed Student-t Portfolio of 10 Assets:
    n = 10
    ###
    
    # Use Random Location Parameters and Covariance Matrix:
    mu = 0.1*runif(n) / 100
    M1 = matrix(runif(n^2), n)
    Omega = (0.5*(M1+t(M1)) + 2 * diag(runif(n, 0.8, 1.2))) / 100^2
    alpha = rep(0, n) 
    df = 4
    ###
    
    # Show the Parameters:
    model = list(mu = mu, Omega = Omega, alpha = alpha, df = df)
    model
    ###
    
    # Simulate Asset Returns and Display a Scatterplot:
    par(mfrow = c(1, 1), cex = 0.5)
    myAssets = round(assetsSim(n = 100, dim = 10, model = model), 4)
    head(myAssets)
    plot(myAssets, cex = 0.5)
	###

    # Plot Simulated Prices:
    par(mfrow = c(1, 1), cex = 0.7)
    plot(ts(100*12*apply(as.matrix(myAssets), 2, cumsum)), main = "")
    title(main = "Assets Prices")
    ###
  
    # Optimize Markowitz Portfolio - Calculate Efficient Frontier:
    par(mfrow = c(1, 1))
    myPF = frontierMarkowitz(x = myAssets)     
    plot(myPF, which = 1)  
    # Add Monte Carlo Points:
    myPF = montecarloMarkowitz(myPF)
    ###
    
    # Print and add further Plots:
    par(mfrow = c(2,2), cex = 0.7)
    print(myPF)  
	plot(myPF, which = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
    ###


# ------------------------------------------------------------------------------
# 2. Compute Efficient frontier from a Berndt's data set of assets

	# The data set "berndtInvest" is from Berndt's textbook 
	# "The Practice of Econometrics". It is a data.frame consisting
	# of 18 columns with the following entries:
	#  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
 	#  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
	# [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
	# The first column holds the date, the 11th the market rate,
	# and the last (the 18th) the risk free rate.
	###
		
	# Load the Data:
	data(berndtInvest)
	berndtInvest = as.timeSeries(berndtInvest, format = "%d-%B-%y")
	class(berndtInvest)
	head(berndtInvest)
	# Exclude the Date, Market Returns and Interest Rate Columns 
	# from the data frame, then multiply by 100 for percentual returns ...
	berndtAssets = 100 * berndtInvest[, -c(10, 17)]
	# Assign Dates as Row Names:
	class(berndtAssets)
	head(berndtAssets)
	###
	
	# Optimize Markowitz Portfolio - Calculate Efficient Frontier:
    par(mfrow = c(1, 1))
    myPF = frontierMarkowitz(x = berndtAssets)     
    plot(myPF, which = 1)  
    title(main = "\n\nBerndt's Data Set")
    # Add Monte Carlo Points:
    myPF = montecarloMarkowitz(myPF)
    ###
    
    # Print and add further Plots:
    par(mfrow = c(2,2), cex = 0.7)
    print(myPF)  
	plot(myPF, which = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
    ###
	
	
# ------------------------------------------------------------------------------
# 3. Optimize the portfolio for "ONE" given target return

	# Load the Data:
	data(berndtInvest)
	berndtInvest = as.timeSeries(berndtInvest, format = "%d-%B-%y")
	# Exclude the Date, Market Returns and Interest Rate Columns 
	# from the data frame, then multiply by 100 for percentual returns ...
	berndtAssets = 100 * berndtInvest[, -c(10, 17)]
	###
	
	# Optimize and Print:
	portfolioMarkowitz(berndtAssets, targetReturn = 2.2)
	###
	
	
################################################################################

