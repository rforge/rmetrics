#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 7.4
#   Heston-Nandi Option Pricing
#
# List of Examples, Exercises and Code Snippets
#
#   * Example: Computing the Price of Heston-Nandi Options
#   * Example: Computing the Greeks of Heston-Nandi Options
#   * Example: HN Options Pricing - GARCH(1,1) Simulation 
#   * Example: HN Options Pricing - Max LogLikelihood Estimation
#   * Example: HN Options Pricing - Scaling of Prices   
#   * Example: Investigate the Smile Effect
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


### Example: Computing the Price of Heston-Nandi Options

	# Calculate the price of Heston-Nandi Garch(1,1)
	# call and put options. 	
	###

	# Set the Model Parameters for the HN Garch(1,1) Option:
	model = list(lambda = -0.5, omega = 5e-6, alpha = 1e-8, 
		beta = 0.7, gamma = 10)
	S = X = 100
   	Time.inDays = 250
   	r.daily = 0.05/250
   	###
   	
	# List the R Function:
	HNGOption
	###
	
	# Compute the Option Price:
	# Call:
	HNGOption(TypeFlag = "c", model, S, X, Time.inDays, r.daily)
	# Put:
	HNGOption(TypeFlag = "p", model, S, X, Time.inDays, r.daily)
	###
	
	# Compare with the Black-Scholes Formula:
	# Unconditional Variance:
	sigma2.daily = (model$alpha + model$omega) /
		(1-model$beta-model$alpha*model$gamma^2)	
	# Call:
	GBSOption("c", S = 100, X = 100, Time = 1, r = 0.05,
		b = 0.05, sigma = sqrt(sigma2.daily*250))	
	# Put:
	GBSOption("p", S = 100, X = 100, Time = 1, r = 0.05,
		b = 0.05, sigma = sqrt(sigma2.daily*250))
	###
		
# ------------------------------------------------------------------------------


### Example: Computing the Greeks of Heston-Nandi Options
	
	# Calculate the sensitiviteis delta and gamma of 
	# Heston-Nandi Garch(1,1) call and put options. 	
	###
	
	# Option Parameters:
	S = X = 100
	Time.inDays = 125
	r.daily = 0.05/250
	model = list(lambda = -0.5, omega = 2.3e-6, alpha = 2.9e-6, 
		beta = 0.85, gamma = 184.25)
	sigma.daily = sqrt((model$alpha + model$omega) /
		(1 - model$beta - model$alpha*model$gamma^2))		
	# Print:
	S; X; Time.inDays; r.daily; sigma.daily
	model
	###

	# Compute the Delta 
	# and compare the result with the finite dfference approaximation:
	Summary = NULL
	# Call:
	Greek = HNGGreeks("Delta", "c", model, S, X, Time.inDays, r.daily)
	C1 = HNGOption("c", model, S*0.9999, X, Time.inDays, r.daily)$price
	C2 = HNGOption("c", model, S*1.0001, X, Time.inDays, r.daily)$price
	numGreek = (C2-C1)/(0.0002*S)
	Diff = 100*(Greek-numGreek)/Greek
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Put:
	Greek = HNGGreeks("Delta", "p", model, S, X, Time.inDays, r.daily)
	C1 = HNGOption("p", model, S*0.9999, X, Time.inDays, r.daily)$price
	C2 = HNGOption("p", model, S*1.0001, X, Time.inDays, r.daily)$price
	numGreek = (C2-C1)/(0.0002*S)
	Diff = 100*(Greek-numGreek)/Greek
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Print:
	dimnames(Summary) <- 
		list(c("Call", "Put"), c("Greek", "numGreek", "Diff"))
	Summary
	###
	
	# Do the same for the Gamma Sensitivity ...
	Summary = NULL
	# Call:
	Greek = HNGGreeks("Gamma", "c", model, S, X, Time.inDays, r.daily)
	C0 = HNGOption("c", model, S, X, Time.inDays, r.daily)$price
	C1 = HNGOption("c", model, S*0.9999, X, Time.inDays, r.daily)$price 
	C2 = HNGOption("c", model, S*1.0001, X, Time.inDays, r.daily)$price 	
	numGreek = (C2 - 2*C0 + C1)/(0.0002*S/2)^2
	Diff = 100*(Greek-numGreek)/Greek	
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Put:
	Greek = HNGGreeks("Gamma", "p", model, S, X, Time.inDays, r.daily)
	C0 = HNGOption("p", model, S, X, Time.inDays, r.daily)$price
	C1 = HNGOption("p", model, S*0.9999, X, Time.inDays, r.daily)$price 
	C2 = HNGOption("p", model, S*1.0001, X, Time.inDays, r.daily)$price 
	numGreek = (C2 - 2*C0 + C1)/(0.0002*S/2)^2
	Diff = 100*(Greek-numGreek)/Greek	
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Print:
	dimnames(Summary) <- 
		list(c("Call", "Put"), c("Greek", "numGreek", "Diff"))
	Summary
	###
	
	
# ------------------------------------------------------------------------------


### Example: HN Options Pricing - GARCH(1,1) Simulation

	# Description:
	#	Simulate time series with the same parameters as those
	#	fitted by Heston and nandi to the SP500 data ranging
	#	the three years from 01/08/1992 to 12/30/1994.
	#   DATA:
	#     Index Value at 2:30 PM
	#     No. of observations 755
	#     r - TBill rate (3.7%)
	#   RESULT:
	#           lambda   omega   alpha  beta  gamma  THETA   PERS  MLLH
	#     Sym:     0.7  1.6e-6  1.0e-6  0.92    ---   9.2%   0.92  3492
	#     Asym:    0.2  5.0e-6  1.0e-6  0.59    421   8.0%   0.77  3504 
	# Reference:
	# 	S. Heston and S. Nandi, 1997
	# 	A Closed-Form GARCH Option Pricing Model
	###
	
	# Fit a Symmetric HN-GARCH(1,1) Process:
 	set.seed(4711)
 	model = list(lambda = 0.7, omega = 1.6e-6, alpha = 1e-6, 
 		beta = 0.92, gamma = 0, rf = 0.037/252)
 	ts.sym = hngarchSim(model, n = 755, n.start = 100)
 	par(mfcol = c(3, 2), cex = 0.5)
 	ts.plot(ts.sym, main = "Symmetric Data")
	###
		
	# Fit an Asymmetric HN-GARCH(1,1) Process:
 	set.seed(4711)
 	model = list(lambda = 0.2, omega = 5.0e-6, alpha = 1e-6, 
 		beta = 0.59, gamma = 421, rf = 0.037/252)
 	ts.asym = hngarchSim(model, n = 755, n.start = 100)
 	ts.plot(ts.asym, main = "Asymmetric Data")
 	# Plot Both:
 	ts.plot(ts.asym, main = "Both Data Sets")
 	lines(ts.sym, col = "red")
	###
	
	# ACF Plots:
	result = acf(abs(ts.sym), main = "ACF: Symmetric Data")
	result = acf(abs(ts.asym), main = "ACF: Asymmetric Data")
	###
	
	
# ------------------------------------------------------------------------------


### Example: HN Options Pricing - Max LogLikelihood Estimation

	# Simulate time series with the same parameters as those
	# fitted by Heston and nandi to the SP500 data ranging
	# from 2/8/1992 to 30/12/1994, finally re-fit the parameters.
	#   DATA:
	#     Index Value at 2:30 PM
	#     No. of observations 755
	#     r - TBill rate (3.5%)
	#   RESULT:
	#           lambda   omega   alpha  beta  gamma  THETA   PERS  MLLH
	#     Sym:     0.7  1.6e-6  1.0e-6  0.92    ---   9.2%   0.92  3492
	#     Asym:    0.2  5.0e-6  1.0e-6  0.59    421   8.0%   0.77  3504 
	# Reference:
	# 	S. Heston and S. Nandi, 1997
	# 	A Closed-Form GARCH Option Pricing Model
	###

	# SP500 Data from MASS Package:
	require(MASS)
	data(SP500)
	returns = SP500[505:(505+755)]/100
	model.sym = list(lambda = 0.2, omega = 1.6e-6, alpha = 1e-6, 
		beta = 0.92, gamma = 0, rf = 0.035/252)
	model.asym = list(lambda = 0.7, omega = 5e-6, alpha = 1e-6,
		beta = 0.59, gamma = 421, rf = 0.035/252)
	###
	
	# Estimate the Symmetric HN-GARCH(1,1) Parameters: 
  	fit.sym = hngarchFit(x = returns, model = model.sym, 
  		symmetric = TRUE)
  	fit.sym
  	###	
  
	# Estimate the Asymmetric HN-GARCH(1,1):
  	fit.asym = hngarchFit(x = returns, model = model.asym, 
  		symmetric = FALSE)
	fit.asym
	###
	
	# Summarize Results:	
	data.frame(cbind(
		SYM = unlist(model.sym), 
		SYM.FIT = unlist(fit.sym$model),
		ASYM = unlist(model.asym),
		ASYM.FIT = unlist(fit.asym$model)
		))
	###
	
	# Diagnostic Analysis:
	par(mfcol = c(3,2), cex=0.5)
	summary(fit.sym)
	summary(fit.asym)
	###
	
	# Moment Statistics:
	unlist(hngarchStats(model.sym))
	unlist(hngarchStats(model.asym))
	unlist(hngarchStats(fit.sym$model))
	unlist(hngarchStats(fit.asym$model))
	###
	
	
# ------------------------------------------------------------------------------


### Example: HN Options Pricing - Scaling of Prices

	# The example shows for the HN Garch(1,1) model 
	# the scaling in price/strike and volatility*time.
	###
	
	# Initial Parameter Setting:
   	scales = c(1/2, 1, 2)
   	Model = list(lambda=-0.5, omega=1e-6, alpha=1e-6, beta=0.5, gamma=0)
 	par (mfrow = c(2, 2), cex = 0.7)
	###
	
	# Price/Strike Variation:
	S  = seq(75, 125, by = 5); X = 100
	Time = 126 
	###
	
 	# HNG:	
	plot(x = c(0.75, 1.25), y = c(0, 28), type = "n", 
		xlab = "S/X", ylab = "Call Price", 
		main = "HN: Volatility*Time fixed")
 	for (j in 1:length(scales)) {
	 	model = Model
		model$omega = Model$omega*scales[j]
		model$alpha = Model$alpha*scales[j]
      	for (i in 1:length(S)) {
	      	x = S[i]/X
   			y = HNGOption("c", model, S[i], X, Time/scales[j], 0)$price
        	points(x, y, col = j, pch = (j+2)) } }
    legend(0.8, 26, legend = c("scale = 1/2", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
    ###
     	
	# BS:
	plot(x = c(0.75, 1.25), y = c(0, 28), type = "n", 
		xlab = "S/X", ylab = "Call Price", 
		main = "BS: Volatility*Time fixed")
 	for (j in 1:length(scales)) {
	 	model = Model
		model$omega = Model$omega/scales[j]
		model$alpha = Model$alpha/scales[j]
		sigma = sqrt((model$alpha+model$omega)/(1-model$beta))
		print(sigma^2 * Time*scales[j])
      	for (i in 1:length(S)) {
	      	x = S[i]/X
   			y = GBSOption("c", S[i], X, Time*scales[j], r=0, b=0, 
   				sigma=sigma)$price
        	points(x, y, col = j, pch = (j+2)) } }	
	legend(0.8, 26, legend = c("scale = 1/2", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
	###	

	# Volatility*Time Variation:
	S = 100; X = 100
	Time = c((12:1)*21, 5, 1) 
	sigma = sqrt((Model$alpha+Model$omega)/(1-Model$beta)); 252*sigma
	###
	
 	# HNG:		
	plot(x = c(-252, 0) , y = c(1.5, 0), type = "n", xlab = "Days", 
		ylab = "Put Price / scale", main = "HN: S/X fixed")
 	for ( j in 1:length(scales) ) {
	 	S.scaled = S*scales[j]; X.scaled = X*scales[j]	
   		for (i in 1:length(Time)) {
   			x = -Time[i]
   			y = HNGOption("c", Model, S.scaled, X.scaled, Time[i], 0)$price /
   				scales[j]
   			points(x, y, col = j, pch = (j+2)) } }
	legend(-240, 0.6, legend = c("scale = 0.5", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
	###	
			
	# BS:
	plot(x = c(-252, 0) , y = c(1.5, 0), type = "n", xlab = "Days", 
		ylab = "Put Price / scale ", main = "BS: S/X fixed")
	for (j in 1:length(scales) ) {
	 	S.scaled = S*scales[j]; X.scaled = X*scales[j]	
   		for (i in 1:length(Time)) {
   			x = -Time[i]
   			y = GBSOption("c", S.scaled, X.scaled, Time[i], 0, 0, sigma)$price /
   				scales[j]
   			points(x, y, col = j, pch = (j+2)) } }
	legend(-240, 0.6, legend = c("scale = 0.5", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
	###
			
		
# ------------------------------------------------------------------------------


### Example: Investigate the Smile Effect

	# This program calculates for the estimated symmetric and asymmetric
	# Garch(1,1) the implied volatility from the Black and Scholes Option
	# Pricing formula assuming that the real market follows ideally the
	# Heston Nandi Option Garch(1,1) model
	###	
	 	
	# Show the GBSVolatility function:
    GBSVolatility
    ###

	# Compute Smile - Parameters:
	S  = 85:115; X = 100
	Time.inDays = 126 
	model = list(lambda=-0.5, omega=6e-6, alpha=4e-6, beta=0.8, gamma=0)
	sigma = sqrt(252*(model$alpha+model$omega)/(1-model$beta))
	sigma	
	###
	
	# Pricing the Put:
	Put <- impVola <- NULL
	for (i in 1:length(S)) {
		Price = HNGOption("p", model, S[i], X, Time.inDays, 0)$price
		Put = c(Put, Price) 
		Volatility = GBSVolatility(Price, "p", S[i], X, Time.inDays/252, 0, 0)
		impVola = c(impVola, Volatility)
		cat("\n\t", i, "\t", S[i], "\t", Price, "\t", Volatility) }
	###
		
	# Plot:
	par(mfrow = c(2, 2), cex = 0.7)
	plot(S/X, Put, 
		xlab = "S/X", ylab = "Price", main = "HN Put Price")
	plot(S/X, impVola, 
		xlab = "S/X", ylab = "Volatility", main = "BS Implied Volatility")
	###	
	
		
################################################################################


