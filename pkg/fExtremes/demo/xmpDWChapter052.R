#
# Examples from the Monograph:
# 	"Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 5.2
#	Fluctuations of Maxima and GEV Distribution
#
# List of Examples, Exercises and Code Snippets:
#
#   5.2.1 Example: Gumbel, Frechet, Weibull - Create Figure 5.2.1
#       * Example: Tables of Gumbel, Frechet and Weibull Distribution  
#   5.2.2 Example: GEV Density - Create Figure 5.2.2  
#   5.2.3 Example: Return Levels - Create Figure 5.2.3
#   5.2.4 Example: Convergence of Exponential Distribution - Figue 5.2.4
#   5.2.5 Example: Convergence of Normal Distribution - Figure 5.2.5
#   5.2.6 Example: GEV Probability Weighted Moments Fit - Figure 5.2.6
#   5.2.8 Example: BMW Probability Weighted Moments Fit	- Figure 5.2.7	
#   5.2.9 Example: Hill's Estimator
#
#   *** This list is not yet complete ***
#
# Author:
#	(C) 2002-2004, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


### Load Library:

	# Load:
	require(fExtremes)
	###
	
	
# ------------------------------------------------------------------------------


### 5.2.1 Example: Gumbel, Frechet, Weibull - Create Figure 5.2.1
	
	# Weibull Distribution:
	dweibl = function (x, alpha) { # x < 0, alpha > 0		
		alpha*((-x)^(alpha-1))*exp(-(-x)^alpha) }
	pweibl = function (q, alpha) { # q < 0, alpha > 0
		exp(-(-q)^alpha) }
	qweibl = function (p, alpha) { # alpha > 0
		-(-log(p))^(1/alpha) }
	rweibl = function (n, alpha) { # alpha > 0
		-(-log(runif(n)))^(1/alpha) }
	###

	# Gumbel Distribution:
	dgumbel = function (x) {# x real
	 	exp(-exp(-x))*exp(-x) }
	pgumbel = function (q) {# q real
	   	exp(-exp(-q)) }
	qgumbel = function (p) {	
		-log(-log(p)) }
	rgumbel = function (n) {	
		-log(-log(runif(n))) }
	###

	# Frechet Distribution:
	dfrechet = function (x, alpha) {# x > 0, alpha > 0
	  	alpha*(x^(-alpha-1))*exp(-x^(-alpha)) }
	pfrechet = function (q, alpha) {# x  >0, alpha > 0  		
		exp(-q^(-alpha))}
	qfrechet = function (p, alpha) {# abs() handles Inf from q=1
		abs((-log(p))^(-1/alpha)) }
	rfrechet = function (n, alpha) {
		(-log(runif(n)))^(-1/alpha)	}
	###
		
	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Settings:
    s = seq(1.e-5, +6, length = 100)
    ###
    
    # Plot Probability - Create Figure 5.2.1:	
	plot(x = c(-6, 6), y = c(0, 1), type = "n", 
	  	xlab = "x", ylab = "probability", main = "Probability")
	lines(x = c(-rev(s), 6), y = c(pweibl(-rev(s), alpha = 1), 1), 	
		col = 3, lty = 2)
	lines(x = c(-rev(s), s), y = c(pgumbel(-rev(s)), pgumbel(s)), 
		col = 4, lty = 1)
	lines(x = c(-6, s), y = c(0, pfrechet(s, alpha = 1)), 
		col = 2, lty = 4)
	grid()
	###
	
	# Plot Density - Create Figure 5.2.1:
	plot(x = c(-6, 6), y = c(0, 1), type = "n", 
	  	xlab = "x", ylab = "density", main = "Density")
	lines(x = c(-rev(s), 0, 6), y = c(dweibl(-rev(s), alpha = 1), 0, 0), 
		col = 3, lty = 2)
	lines(x = c(-rev(s), s), y=dgumbel(c(-rev(s), s)), 
		col = 4, lty = 1)
	lines(x = c(-6, s), y = c(0, dfrechet(s, alpha = 1)), 
		col = 2, lty = 4)
	grid()
	###
	
	
# ------------------------------------------------------------------------------
		

### Example: Tables of Gumbel, Frechet and Weibull Distribution

    # Frechet Distribution:
    x = q = c(0:5, Inf)
    cbind(x, P = pfrechet(q, alpha = 1), D = dfrechet(x, alpha = 1))
    ###
    
    # Weibull Distribution:
    x = q = c(-Inf, -5:0)
    cbind(x, P = pweibl(q, alpha = 1), D = dweibl(x, alpha = 1))
    ###
    
    # Gumbel Distribution rounded to 3 digits:
    x = q = -5:5
    round(cbind(x, P = pgumbel(q), D = dgumbel(x)), 3) 
    ###
    
    
# ------------------------------------------------------------------------------
    
    
### 5.2.2 Example: GEV Density - Create Figure 5.2.2

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Generate Random Series:
    set.seed(1953)
    r = rgev(1000, xi = 1, mu = 0, sigma = 1)
    plot(r, type = "l", main = "GEV(1|0|1) RV Series", col = "steelblue")
    grid()
	###
	
    # Create Density Plot:
    x = seq(-2, 6, length = 81)
    d = dgev(x, xi = 1, mu = 0, sigma = 1)
	###
	
    # Plot True Density:
    plot(x, d, type = "l", main = "GEV(1|0|1) Density", col = "steelblue")
    grid()
	###
	
    # Kernel Density Estimate - Adjust Bandwidth:
    Density = density(r, from = -2, to = 6, n = 41, adjust = 0.3)
    points(Density$x, Density$y, pch = 19, cex = 0.5)
    ###
    
    
# ------------------------------------------------------------------------------

    
### 5.2.3 Example: Return Levels - Create Figure 5.2.3

    # Graph Frame:
    par(mfrow = c(1, 1))
    ###
    
    # Create p and x Vectors:
	p = seq(0.001, 0.999, length = 500)
	x = -1/log(1-p)
	###
		
	# Plot Return Levels:
	plot (x, qgev(1-p, xi = 0) , type = "l", log = "x", 
		xlab = "-1/log(1-p)", ylab = "Return level", 
		ylim = c(-2, 23), main = "Return Levels", lwd = 2)
	for (xi in c(-0.30, -0.15, 0.15, 0.30)) 
		lines(x, qgev(1-p, xi = xi), col = "steelblue")
    grid()
	###
	
	# Add Labels:	
	text(x = rep(450, 5), y = c(1.9, 5.0, 7.3, 12, 21),
		labels = c("-0.30", "-0.15", "0", "0.15", "0.30"))
    ###
    
    
# ------------------------------------------------------------------------------
	

### 5.2.4 Example: Convergence of Exponential Distribution - Figue 5.2.4

	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Functions:
	an = function(n) {1}
	bn = function(n) {log(n)}
	###
	
	# Plot Convergence:
	x = seq(-2, 4, length = 200)
	plot(x, pgev(x, xi = 0), lwd = 2, type = "l",
	  	main = "Convergence of Exp Maxima")
	grid()
	for ( n in c(10, 5, 3, 2, 1) ) 
	  	lines(x, y = (pexp(an(n)*x+bn(n)))^n, col = "steelblue") 
	###
	
	# Alternative Plot:
	plot(-log(-log(pgev(x, xi = 0))), x, lwd = 2, type = "l",
	  	main = "Convergence of Exp Maxima")
	grid()
	for ( n in c(10, 5, 3, 2, 1) ) {
	  	y = ( pexp( an(n)*x+bn(n) ) )^n; s = -log(-log(y))
	  	lines(s[s > -2], x[s > -2], col = "steelblue") 
	}
	###
	
	
# ------------------------------------------------------------------------------	

	
### 5.2.5 Example: Convergence of Normal Distribution - Figure 5.2.5	
	
	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Functions:	
	an = function(n) { 1/sqrt(2*log(n)) }
	bn = function(n) { sqrt(2*log(n)) - 
		( log(log(n))+log(4*pi) ) / sqrt(2*log(n)) /2 }
	###
	
	# Plot Convergence:	
	x = seq(-2, 5, length = 500)
	plot(x, pgev(x, xi = 0), lwd = 2, type = "l",
	  	main = "Convergence of Gaussian Maxima")
	grid()
	for ( n in c(100, 50, 10, 5, 2) ) 
	  	lines(x, y = (pnorm(an(n)*x+bn(n)))^n, col = "steelblue") 
	###
	  		
	# Alternative Plot:
	plot(-log(-log(pgev(x, xi = 0))), x, xlim = c(-2, 12), lwd = 2, 
		type = "l", main = "Convergence of Gaussian Maxima")
	grid()
	x = seq(-2, 12, length = 500)
	for ( n in c(100, 50, 10, 5, 2) ) {
	  	y = (pnorm(an(n)*x+bn(n)))^n; s = -log(-log(y))
	  	lines(s[x < 5], x[x < 5], col = "steelblue") 
	}	
	###		
	
	
# ------------------------------------------------------------------------------

	
### 5.2.6 Example: GEV Probability Weighted Moments Fit - Figure 5.2.6

    # Graph Frame and Settings:
    par(mfrow = c(2, 2), cex = 0.7)
    set.seed(4711)
	###
	
	# Create and Plot the Random Variables:
	x = rgev(n = 8000, xi = 0.3, mu = 0, sigma = 1)
	plot(x, type = "h", main = "Random Variables", col = "steelblue")
	mtext("Simulated GEV Data", line = 0.5, cex = 0.5)
	lines(x = c(0, length(x)), y = c(0, 0), col = "grey", lty = 3)
	###
	
	# PWM Estimate:  	
  	parm.fit = gevFit(x, type = "pwm")
  	###
	
  	# Print Estimated Results:
  	print(parm.fit)
  	###
	
	# Generated Output:
	#	Call:
	#		gevFit(x = x, type = "pwm")
	#	Estimation Type: 
	#		gev pwm 	
	#	Estimated Parameters:
	#	        xi      sigma         mu 
	#	0.28422082 1.01273658 0.00366702
	###

	# Plot Density:
    d = density(x, n = 200)   
	plot(d$x, d$y, xlim = c(-5, 15), ylim = c(0, 0.4), pch = 19,
		xlab = "x", ylab = "density", main = "GEV Density", col = "steelblue")		
	grid()
	mtext("Simulated GEV Data: PWM Estimate", line = 0.5, cex = 0.5)
	s = seq(-5, 15, length = 200)
	lines(s, dgev(s, xi = xi))
	###
	
	
# ------------------------------------------------------------------------------
	

### 5.2.8 Example: BMW Probability Weighted Moments Fit	- Figure 5.2.7	

	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Load Data and Convert to Numeric Vector:
	BMW.RET = as.timeSeries(data(bmw.ret))
	bmwres = as.vector(BMW.RET)
	blocklength = 63
	###

	# Plot Time Series Data:
	plot(bmwres, type = "h", main = "Daily log Returns", col = "steelblue")
	grid()
	abline(h = mean(bmwres), lty = 3, col = "grey")
	###
	
	# Create Block Maxima of Lower Tail:
	x = blockMaxima(-bmwres, block = blocklength, col = "steelblue", lwd = 1.5)
	mtext("Block Maxima - Lower Tail", line = 0.5, cex = 0.5)
	###
	
	# PWM Estimate:	
  	fit = gevFit(x, type = "pwm")
  	xi = fit$par.ests[1]
  	sigma = fit$par.ests[2]
  	mu = fit$par.ests[3]
  	###
  	
	# Histogram Plot and GEV Density:
	hist(x, nclass = 20, probability = TRUE, col = "steelblue", 
		border = "white", main = "Block Maxima - Histogram")
	s = seq(0, max(x), length = 500)
	lines(s, dgev(s, xi, mu, sigma), lwd = 2, col = "brown")
	mtext("Line: GEV Fit", line = 0.5, cex = 0.5)
	###
		
	# QQ-Plot:
	plot(sort(x), qgev(ppoints(x), xi, mu, sigma), pch = 19, 
	  	col = "steelblue", main="QQ-Plot: Empirical / GEV", 
	  	xlab = "empirical rvs", ylab = "GEV df")
	lines(c(min(x), max(x)), c(min(x), max(x)))	
	grid()
	###
	
	
# ------------------------------------------------------------------------------


### 5.2.8 Example: BMW Maximum Log Likelihood Fit - Figure 5.2.8	

	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Load Data, Take Loss Tail:
	BMW.RET = -as.timeSeries(data(bmw.ret))
	###	
	
	# PWM Estimate:	
  	fit = gevFit(BMW.RET, block = 63, type = "mle") 
    summary(fit)
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Alternatives to Print, Plot and Summarize 'gevFit' Objects


	# Load Data, Take Loss Tail and Fit:
	DAX.RET = -as.timeSeries(data(dax.ret))	
  	fit = gevFit(DAX.RET, block = 63, type = "mle") 
  	###
  	
  	# Print fitted Parameters:
  	print(fit)
  	###
  	
  	# Create Summary Report Including Figures:
  	par(mfrow = c(2, 2), cex = 0.7)
  	summary(fit)
  	###
  	
  	# Create Summary Report With Interactive Plots:
  	par(mfrow = c(1, 1))
	summary(fit, which = "ask")
  	###
  	
  	# Just Show the Quantile-Quantile Plot:
  	par(mfrow = c(1, 1))
  	plot(fit, which = 4)
  	###

    
# ------------------------------------------------------------------------------
	

### Example: Use Calendar Blocks

	# Load Data, Take Loss Tail and Fit:
	DAX.RET = -as.timeSeries(data(dax.ret))	
  	# fit = gevFit(DAX.RET, block = "quarter", type = "mle") 
  	# check does not yet work ...
  	###


# ------------------------------------------------------------------------------

    
### Example: gevglmFit  
    
    # Load Data and Convert to Numeric Vector:
    BMW.RET = as.timeSeries(data(bmw.ret))
    bmwres = as.vector(BMW.RET)
    blocklength = 63
    ###
    
    # Graph Frame:
    par(mfrow = c(3, 2), 0.7)
    ###

    # Fit GEV Data by Max Log Likelihood Method a la ISMEV:
    fit = gevglmFit(x)
    summary(fit)
    ###

    # Profile Likelihood:
    gevglmprofxiPlot(fit, xlow = 0.15, xup = 0.60)
    title(main = "Profile Likelihood for Xi")
    grid()
    gevglmprofPlot(fit, m = 100, xlow = 0.05, xup = 0.15)
    title(main = "Profile Likelihood for Quantile 0.01")
    grid()
    ###
    
	
# ------------------------------------------------------------------------------

	
### 5.2.8 Example: Hill's Estimator

	# Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Simulate and Load Data:
	set.seed(4771)
	x1 = rsymstb(10000, alpha = 1.8)
	data(nyseres)	
	x2 = nyseres[, 1]
  	###
  	
	# Hill Plot with Errors for Upper and Lower Tails:
	result = hillPlot( x1, autoscale = FALSE, ylim = c(0, 3.5)) 
	result = hillPlot( x2, autoscale = FALSE, ylim = c(0, 6.0)) 
	result = hillPlot(-x1, autoscale = FALSE, ylim = c(0, 3.5)) 
	result = hillPlot(-x2, autoscale = FALSE, ylim = c(0, 6.0))
	###
	

# ------------------------------------------------------------------------------
	
	
### 5.2.9 Example: Shape Parameter Summary Plots

	# Graph Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    ###
    
    # Load Data:
	data(nyseres)
	###
		 
	# Chart Parameters:
  	tails = c(  0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.10)
	doplot = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
	###
	
	# Calculate and Plot Shape Parameters:
	s = shaparmPlot(x = nyseres, tails = tails, 
		doplot = doplot, doprint = TRUE, xi.range = c(-1, 3), 
		alpha.range = c(0, 8))
	###
	
	
# ------------------------------------------------------------------------------

		
### 5.2.10 Example: GEV Maximum Likelihood Fitting

	# Graph Frame:
    par(mfrow = c(4, 2), cex = 0.5)
    ###
    
    # Load Data:
    data(nyseres)
    nyseres = nyseres[, 1]
    ts.plot(nyseres, main = "Log Returns")
    mtext("NYSE Index Residuals", line = 0.5, cex = 0.5)
    x = blockMaxima(-nyseres, block = 63)
    mtext("Lower Tail: Quarterly Data", line = 0.5, cex = 0.5)
    ###
      
	# Fit GEV Data by Max Log Likelihood Method a la ISMEV:
   	fit = gevglmFit(x)
    summary(fit)
    ###
    
    # Generated Output:
    #	Call:
	#		gevglmFit(x = x)
	#	Estimation Type: 
	#		gevglm mle 
	#	Estimated Parameters:
	#	         xi       sigma          mu 
	#	0.317408547 0.005641637 0.014932832 
	#	Standard Deviations:
	#	          xi        sigma           mu 
	#	0.0723993029 0.0003170321 0.0005216084 
	#	Log-Likelihood Value:  
	#		-457.9648
	#	Type of Convergence:   
	#		0 
	###

	# Profile Likelihood: 
	gevglmprofxiPlot(fit, xlow = 0.15, xup = 0.60)
	title(main = "Profile Likelihood for Xi")
    gevglmprofPlot(fit, m = 100, xlow = 0.05, xup = 0.15)
    title(main = "Profile Likelihood for Quantile 0.01")
    ###
    
 
# ------------------------------------------------------------------------------


### ADDON:


### Example: GEV Maximum Likelihood Estimation Fit

	#  	Fit the GEV via the Maximum likelihood approach. 
	#	Investigate the data from BMW Stocks
	
	# Settings:
  	par(mfrow = c(2, 2), cex = 0.7)
    data(nyseres)
    nyseres = nyseres[, 1]
    ts.plot(nyseres, main = "Log Returns")
    mtext("NYSE Index Residuals", line = 0.5, cex = 0.5)
    x = blockMaxima(-nyseres, block = 63)
    mtext("Lower Tail: Quarterly Data", line = 0.5, cex = 0.5)
    ###
      
	# Fit GEV Data by Max Log Likelihood Method a la ISMEV:
   	fit = gevglmFit(x)
    print(fit)
    summary(fit)
    ###

	# Profile Likelihood: 
	gevglmprofxiPlot(fit, xlow = 0.15, xup = 0.60)
	title(main = "Profile Likelihood for Xi")
    gevglmprofPlot(fit, m = 100, xlow = 0.05, xup = 0.15)
    title(main = "Profile Likelihood for Quantile 0.01")
    ###
        
	# Fit GEV Data by Max Log Likelihood Method a la EVIS:   
    fit = gevFit(x, type = "mle")
    print(fit)
    summary(fit)
    ###
  
  
# ------------------------------------------------------------------------------
 

### Example: GEV Probability Weighted Moments Fit

	# 	Estimate the parameters of a simulated GEV 
	#	with the method of probability weighted 
	#	moments. 

	# Settings:
	par(mfrow = c(2, 2), cex = 0.6)
  	n = 8000
	xmax.density = 15
	parm0 = list(xi = 1/4, mu = 0.0, sigma = 1.0)
	###
	
	# Create and Plot the Random Variables:
	x = rgev(n, xi = parm0$xi, mu = parm0$mu, sigma = parm0$sigma)
	plot(x, type = "h", main = "Random Variables")
	mtext("Simulated GEV Data", line = 0.5, cex = 0.5)
	lines(x = c(0, length(x)), y = c(0, 0), col = "steelblue")
	lines(x = c(0, length(x)), y = c(-1/0.3, -1/0.3), col = "steelblue")
	###
	
	# PWM Estimate:  
  	parm = gevFit(x, type = "pwm")
	parm
	###
	
	# Plot Empirical and Estimated Densities:
    d = density(x, n = 200)   
	plot(d$x, d$y, xlim = c(-5, 15), ylim=c(0, 0.4),
		xlab = "x", ylab = "density", main = "GEV Density")		
	mtext("Simulated GEV Data", line = 0.5, cex = 0.5)
	s = seq(-5, 15, length = 200)
	lines(s, dgev(s, xi = parm0$xi, mu = parm0$mu, 
		sigma = parm0$sigma), col = "steelblue")
	###


# ------------------------------------------------------------------------------
	  

### Example: Shape Parameter Plot 

	#  Plot the shape parameter obtained from MDA estimators
	#   for the NYSE Composite Index log returns. 

	# Settings:
	par(mfcol = c(3, 2), err = -1, cex = 0.6)
	data(nyseres)
	###
		 
	# Chart Parameters = Plot for 'tail=0.05':
  	tails = c(  0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10)
	doplot = c(FALSE,FALSE,FALSE,FALSE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
	###

	# Calculate and Plot Shape Parameters:
	s = shaparmPlot(x = nyseres, tails = tails, 
		doplot = doplot, doprint = TRUE, xi.range = c(-1, 3), 
		alpha.range = c(0, 8))
	###


################################################################################

	 