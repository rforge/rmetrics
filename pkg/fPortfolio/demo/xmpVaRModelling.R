
#
# Example:
#	Value at Risk Modelling
#
# Description:
#	These examples show how to select the four most dissimilar assets 
#	from Berndt's data set and how to compute risk measures for this
#	data set. In addition the same investigation is done for all assets
#	included in Berndt's data set.
#
# Content:
#	1. Select the 4 most dissimilar assets from Berndt's data set
#	2. Compute risk measures for assets from Berndt's data set
#	3. Repeat Example 2 for all assets
#
# Last Check:
#	2006-02-05 ok
#
# Author:
#	(C) 2003, Diethelm Wuertz, GPL
#

################################################################################
# Load Required Package:
	
	# Load:
	require(fPortfolio)
	###


# ------------------------------------------------------------------------------
# 1. Select the 4 most dissimilar assets from Berndt's data set

	# The data set "berndtInvest" is from Berndt's textbook 
	# "The Practice of Econometrics". It is a data.frame consisting
	# of 18 columns with the following entries:
	#  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"      
 	#  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"    
	# [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"  
	# The first column holds the date, the 11th the market rate,
	# and the last (the 18th) the risk free rate.
	###
	
	# Load Berndt's Monthly Investment Data:
	data(berndtInvest)
	berndtInvest = as.timeSeries(berndtInvest, format = "%d-%B-%y")
	# Exclude Market Returns (10) and Interest Rate Columns (17)
	allAssets = 100 * berndtInvest[, -c(10, 17)]
	head(allAssets)
	###
	
	# Select the "n" Most Dissimilar Assets from Hierarchical Clustering:
	par(mfrow = c(2, 1), cex = 0.7)
	clustered = assetsSelect(allAssets, doplot = TRUE)
	myAssets = allAssets[, c(clustered$order[1:4])]
	###
	
	# Plot Cumulated Returns of the Assets:
	ts.plot(colCumsums(myAssets@Data), col = 1:4)
	legend(0, 300, legend = colnames(myAssets@Data), pch = "----", col = 1:4)
	title(main = "Cumulated Returns", ylab = "Cumulated Returns")
	abline(h = 0, lty = 3)
	###


# ------------------------------------------------------------------------------
# 2. Compute risk measures for assets from Berndt's data set

    # Data:
    # ... use myAssets from previous example
    ###
    
    # Portfolio Weights and Confidence level:
	dim = 4
	equalWeights = rep(1, dim)/dim
	alpha = 0.05
	###
		
	# Risk Measures:
	VaR(myAssets, equalWeights, alpha)
	CVaRplus(myAssets, equalWeights, alpha)
	CVaR(myAssets, equalWeights, alpha)
	lambdaCVaR(n = 120, alpha) 
	pfolioMaxLoss(myAssets, equalWeights)
	###
		
	# Portfolio:
	# Returns:
	r = pfolioReturn(myAssets, equalWeights) 
	head(r)
	# Plot Histogram and Risk Values:
	pfolioHist(myAssets, equalWeights, alpha, breaks = "fd")
	###

	
# ------------------------------------------------------------------------------
# 3. Repeat Example 2 for all assets


	# Data:
    # ... use 'allAssets and myAssets from first example:
    ###
    
    # Plot Histogram and Risk Values:
	par(mfrow = c(2, 1), cex = 0.7)
	pfolioHist(allAssets, n = 20, range = c(-35, 35), 
		main = "allAssets")
	###
	
	# Plot the four assets, for comparison:
	pfolioHist(myAssets, n = 30, range = c(-35, 35),
		main = "myAssets")
	###


################################################################################

