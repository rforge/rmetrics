
#
# Description:
#	Portfolio Assets Modelling
#
# Description:
#	This example shows how to select the four most dissimilar assets 
#	from Berndt's data set and how to fit the distributional paramaters 
#	for these assets. Finally we show how to simulate an artificial 
#	data set of assets with distributions from fitted parameters.
#
# Content:
#	1. Select the 4 most dissimilar assets from Berndt's data set
#	2. Fit the distributional parameters for Berndt's data set
#	3. Simulate an artificial data set of assets
#
# Last Check:
#	2006-02-05 ok
#
# Author:
#	(C) 2003-2006, Diethelm Wuertz, GPL
#


################################################################################
# Requirements:

	
	# Load Packages:
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
		
	# Load the Data and Create an Object of Class 'timeSeries':
	data(berndtInvest)
	berndtInvest = as.timeSeries(berndtInvest, format = "%d-%B-%y")
	class(berndtInvest)
	head(berndtInvest)
	# Exclude the Date, Market Returns and Interest Rate Columns 
	# from the data frame, then multiply by 100 for percentual returns ...
	allAssets = 100 * berndtInvest[, -c(1, 10, 17)]
	# Assign Dates as Row Names:
	class(allAssets)
	head(allAssets)
	###
		
	# Graph Frame:
	par(mfrow = c(2, 1), cex = 0.7)
	###
	
	# Select the "n" Most Dissimilar Assets from 
	# Hierarchical Clustering:
	n = 4
	args(assetsSelect)
	clustered = assetsSelect(allAssets, doplot = TRUE)
	# Create my Assets Set from the "n" selected Symbols:
	myAssets = allAssets[, c(clustered$order[1:n])]
	colnames(myAssets)
	# Print the Column Return:
	mu.vec = colAvgs(myAssets)
	mu.vec
	# or ...
	mu.vec = colMeans(myAssets)
	mu.vec
	# Print the Covariance Matrix:
	cov.mat = cov(myAssets)
	cov.mat
	###
			
	# Plot Cumulated Returns of the Assets:
	ts.plot(colCumsums(myAssets), col = 1:4)
	grid()
	legend(0, 300, legend = colnames(myAssets), pch = "----", col = 1:4)
	title(main = "Cumulated Returns", ylab = "Cumulated Returns")
	abline(h = 0, lty = 3)
	###


# ------------------------------------------------------------------------------
# 2. Fit the distributional parameters for Berndt's data set


	# How do we classify a Portfolio ?
	# We classify a portfolio by three parameters:
	# 1)  the location - mean vector, named "alpha"
	# 2)  the scale    - covariance matrix, named "Omega"
	# 3)  the shape    - skewness vector, named "beta" 
	# 4)               - degrees of freedom (kurtosis)
	# The function "assetsFit" fits the parameters of the distribution
	# by maximum log-likelihood estimation.
	###
	
	# Fit myAsssets Data Set:	
	# Fit a multivariate normal distribution:
	fit.norm  = assetsFit(x = myAssets, method = "norm")
	fit.norm
	# Fit a multivariate skewed normal distribution:
	fit.snorm = assetsFit(x = myAssets, method = "snorm") 
	fit.snorm
	# Fit a multivariate skewed Student-t distribution:
	fit.st = assetsFit(x = myAssets, method = "st")
	fit.st
	# Show Class and Slot Names:
	class(fit.st)
	slotNames(fit.st)
	print(fit.st@model)
	class(fit.st@model)
	###
		
	# Investigate the fitted Parameters:
	par(mfrow = c(1, 1), cex = 0.7)
	# Extract mu, Omega, beta and df:
	alpha = fit.st@model$alpha
	Omega = fit.st@model$Omega
	mu = c(fit.st@model$mu, fit.st@model$df)
	# Create a matrix "z" where the last row holds the returns,
	# the most right column the skewness values, and the last
	# element the number of degrees of freedom:
	z = cbind(rbind(sqrt(Omega), alpha), mu)
	# Create entries x, y, z for Plotting ...
	m = 4
	n = m + 1
	x = rep(1:n, times = n)
	y = rep(1:n, each = n)
	z = as.vector(z)
	# Plot:
	plot(x, y, type = "n", xlim = c(-0.5, n+0.5), ylim = c(0, n+0.5),
		xlab = "", ylab = "")
	abline(h = n - 0.5, lty = 3)
	abline(v = n - 0.5, lty = 3)
	abline(h = 0.5, lty = 3)
	abline(v = 0.5, lty = 3)
	symbols(x, y, squares = abs(z), inches = 0.25, add = TRUE, 
		bg = "steelblue4" )
	text(1:n, rep(0, n), c(colnames(myAssets), "skew") )
	text(rep(0, n), 1:n, c(colnames(myAssets), "mean") )
	title(main = "Parameter Plot")
	text(n+0.5, n+0.5, "df")
	text(1:n, rep(n, n)-0.25, as.character(round(mu, 3)), col = "orange")
	###
		
	# Use methods:
	print(fit.st)
	plot(fit.st, which = 1)
	###
	

# ------------------------------------------------------------------------------
# 3. Simulate an artificial data set of assets


	# Two ways for Simulation:
	# 1 We classify a market Portfolio using the fitted
	#	parameters as innput for the simulation	
	# 2 We start from scratch, define location, scale and shape 
	#	parameters, and generate rvs from the skewed Normal or 
	#   Student-t (fat tailed) multivariate distribution.	
	# For details we refer to Rmetric's 'Multivariate Distribution'
	###

	# Method 1:
	simulatedAssets = assetsSim(
		n = length(myAssets[,1]), 
		model = fit.st@model)
	simulatedAssets
	###
	
	# Method 2:
	# fit.st@model
	# alpha = fit.st@model$alpha
	# Omega = fit.st@model$Omega
	# beta = fit.st@model$beta
	# simulatedAssets = assetsSim(n = 120, 
	#	model = list(beta = beta, Omega = Omega, alpha = alpha, df = Inf))
	# Plot Cumulated Returns of the Assets:
	par(mfrow = c(1, 1))
	ts.plot(colCumsums(simulatedAssets), col = 1:4)
	legend(0, 300, legend = colnames(simulatedAssets), col = 1:4, lty = 1)
	title(main = "Cumulated Returns", ylab = "Cumulated Returns")
	abline(h = 0, lty = 3)
	###
	
	
################################################################################

	