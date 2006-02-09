
#
# Example:
# 	Create and optimize a portfolio of simulated assets by the
#	mean-variance Markowitz approach. 
#
# Details:
#   Use the functions from the "MarkowitzPortfolio" collection.
#   Included are functions which investigate the efficient frontier 
#   for a Markowitz portfolio from a given return series "x" in  
#   the mean-variance sense. Tangency, equal weigths, and Monte 
#   Carlo portfolios can also be evaluated. The functions are:
#   1. frontierMarkowitz        Efficient mean-var frontier
#   2. tangencyMarkowitz        Adds tangency portfolio
#   3. equalweightsMarkowitz    Adds equal weights portfolio
#   4. montecarloMarkowitz      Adds randomly created portfolios
#
# Notes:
#   The functions and code are from examples and assignments implemented by
#     Diethelm Wuertz and used in his Lecture in Econophysics.
#     Diethelm Wuertz  [February, 2003]   
#   Code from the following contributed R packages is included:
#     "mvtnorm", "sn", "quadprog", and "tseries" 
#     The required functions from these packages are part of Rmetric's
#     fPortfolio package as builtin functions.
#
# References:
#   Elton E.J., Gruber M.J. (1991); 
#       Modern Portfolio Theory and Investment Analysis, 
#       4th Edition, Wiley, NY, pp. 65--93.
#   Huang C.J., Litzenberger R.H. (1988); 
#       Foundations for Financial Economics, 
#       Elsevier, NY, pp. 59--82.
#
# Author:
#	(C) 2003, Diethelm Wuertz, GPL
#


################################################################################
# PART I - Simulate set of assets:
    
    require(fExtremes)
    
    # Composed Model:
    #   n - Number of data records to be simulated
    #   mu - Numeric vector of Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, (measures kurtosis)
    
    
    # Set Parameters:
    # Skewed Student-t Portfolio of 10 Assets:
    n = 10
    # Use Random Location Parameters and Covariance Matrix:
    mu = 0.1*runif(n) / 100
    M1 = matrix(runif(n^2), n)
    Omega = (0.5*(M1+t(M1)) + 2 * diag(runif(n, 0.8, 1.2))) / 100^2
    alpha = rep(0, n) 
    df = 4
    # Show the Parameters:
    model = list(mu = mu, Omega = Omega, alpha = alpha, df = df)
    model
    # Simulate Asset Returns and Display a Scatterplot:
    myAssets = round(assetsSim(n = 100, dim = 10, model = model), 4)
    head(myAssets)
	par(mfrow = c(1, 1), cex = 0.7)
    plot(myAssets, cex = 0.5)
	###
	
	
    # Plot Simulated Prices:
    par(mfrow = c(1, 1), cex = 0.7)
    plot(ts(100*12*apply(as.matrix(myAssets), 2, cumsum)), 
    	main = "Assets Prices")
    ###
  

    # Optimize Markowitz Portfolio: 
    # Calculate Efficient Frontier:
    par(mfrow = c(2, 2), cex = 0.5)
    myPF = frontierMarkowitz(x = myAssets)       
    # Add Monte Carlo Points:
    myPF = montecarloMarkowitz(myPF)
    # Summary:
    # summary(myPF)
    print(myPF)
    par(mfrow=c(2,2))
	plot(myPF, which = c(F,T,T,T,T))
    ###


################################################################################
# Use Berndt's data set:


	require(fBasics)
	require(fExtremes)

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
	
	# Optimize Markowitz Portfolio: 
    # Calculate Efficient Frontier:
    par(mfrow = c(2, 2), cex = 0.5)
    myPF = frontierMarkowitz(x = berndtAssets)        
    # Add Monte Carlo Points:
    myPF = montecarloMarkowitz(myPF)
    # Summary:
    print(myPF)
    par(mfrow=c(2,2))
	plot(myPF, which = c(F,T,T,T,T))
    ###
	
	
################################################################################
# Optimize the portfolio for a given target return:


	portfolioMarkowitz(berndtAssets@Data, targetReturn = 2.2)
	
	
################################################################################

