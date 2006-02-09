#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 4.2
#   Modelling and Forecasting with Neural Networks:
#
# List of Examples, Exercises and Code Snippets:
# 
#	Example: 1 Build Data Set for 6 Months US Regression Modelling  
#   Example: 2 Fit Regression Models
#
#   *** This list is not yet complete ***
#
# Author:
#	(C) 2002-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################

		
### Example: 1 Build Data Set for 6 Months US Regression Modelling
	
	# Read the Dataset from File:
	# Column 1: Date - CCYYMM
	# Column 2: Recession 0 | 1
	# Column 3: 3 Month Treasury Bills
	# Column 4: 10 Years Treasury Bonds
	data(recession)
	###
	
	horizon = 6
	n = length(recession[, 1])
	# Response and Predictors:
	response = recession[,2][(1+horizon):n]
	predictor1 = recession[,3][1:(n-horizon)]
	predictor2 = recession[,4][1:(n-horizon)]
	StockWatson = recession[,5][(1+horizon):n]
	# Date/Time:
	ccyy = floor(recession[,1]/100)
	mm = recession[, 1] - 100 * ccyy
	time = ccyy + (mm-0.5)/12  # mid-month
	time = time[1:(n-horizon)]
	# Data Frame:
	data = data.frame(cbind(response, predictor1, predictor2))	
	###

	
# ------------------------------------------------------------------------------


### Example: 2 Fit Regression Models

	family = binomial(link = probit)
	lm.fit = regFit(response ~ predictor1 + predictor2, 
	  data = data, method = "LM")
	  print(glm.fit)
	glm.fit = regFit(response ~ predictor1 + predictor2, 
	  family = family, data = data, method = "GLM")
	  print(glm.fit)
	gam.fit = regFit(response ~ predictor1 + predictor2, 
	  family = family, data = data, method = "GAM")
	  print(gam.fit)
	ppr.fit = regFit(response ~ predictor1 + predictor2, 
	  data = data, method = "PPR", nterms = 2)
	  print(ppr.fit)
	mars.fit = regFit(response ~ predictor1 + predictor2, 
	  data = data, method = "MARS")
	  print(mars.fit)
	polymars.fit = regFit(response ~ predictor1 + predictor2, 
	  data = data, method = "POLYMARS")
	  print(polymars.fit)
	nnet.fit = regFit(response ~ predictor1 + predictor2, data = data, 
	  method = "NNET", linout = TRUE, trace = FALSE, maxit = 1000, size = 6)
	  print(nnet.fit)
	###
	
	# Write Plot Function:
	myPlot = function(time, response, in.sample, StockWatson, title) {
	  plot(time, response, type = "n", main = title, col = "steelblue")
	  grid()
	  lines(time, response, type = "h", col = "steelblue")
	  # The Benchmark - Compare with StockWatson:
	  lines(time, StockWatson, col = "red", lty = 3)
	  # Set Negative Values to Zero:
	  in.sample = (in.sample+abs(in.sample))/2
	  # Set Values Larger than One to One:
	  in.sample = -(1-in.sample+abs(1-in.sample))/2 + 1
	  lines(time, in.sample) 
	}
	###
		
	# Compare with Fitted Values:
	par(mfrow = c(3, 2), cex = 0.7)
	myPlot(time, response, 
	  glm.fit@fit$fitted.values, StockWatson, "GLM")
	myPlot(time, response, 
	  gam.fit@fit$fitted.values, StockWatson, "GAM")
	myPlot(time, response, 
	  ppr.fit@fit$fitted.values, StockWatson, "PPR")
	myPlot(time, response, 
	  mars.fit@fit$fitted.values, StockWatson, "MARS")
	myPlot(time, response, 
	  polymars.fit@fit$fitted.values, StockWatson, "POLYMARS")
	myPlot(time, response, 
	  nnet.fit@fit$fitted.values, StockWatson, "NNET")
	###
		
		
################################################################################

