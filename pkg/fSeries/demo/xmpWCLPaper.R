#
# Examples from the Paper
#   Parameter Estimation of ARMA Models with GARCH/APARCH Errors
#     An R and SPlus Software Implementation
#     written by Diethelm Wuertz, Yohan Chalabi, and Ladislav Luksan
#   submitted to Journal of Statistical Software
#
# Details:
#   1. Introduction
#   2. Mean and Variance Equation
#   3. The Standard GARCH(1,1) Model
#   4. Alternative Conditional Distributions
#   5. ARMA(m,n) Models with GARCH(p,q) Errors
#   6. APARCH(p,q) - Asymmetric Power Arch Models
#   7. An Unique GARCH Modelling Approach
#   8. Summary and Outlook
#
# List of Code Snippet and Figures:                
#   
#	CHAPTER 3:
#	Code Snippet  1: Implemting garch11Fit() frm Scratch
#	Code Snippet  2: Benchmarking the GARCH(1,1) for DEMGBP Exchange Rates
#   Figure 1: DEMGBP Series, Histogram, QQ, and ACF Plot
#
#   CHAPTER 4:
#	Figure 2: Student-t Density and Distribution
#   Figure 3: GED Density and Distribution
# 	Figure 4: Skew Sudent-t and Skew GED Distributions
#   Code Snippet  3: Fitting Bollerslev's t-GARCH(1,1) Model
#	Code Snippet  4: Comparing "mci" and "uev" Recursion Initialization
#
#   CHAPTER 5:
#	Code Snippet  5: Comparing "mci" and "uev" Recursion Initialization
#	Code Snippet  6: APARCH - Computing Conditional Variances Effectively
#   Code Snippet  7: Using R's Filter Representation
#   Code Snippet  8: Tracing the "t-MA(1)-GARCH(1,2)" Model Fit
#
#   CHAPTER 6:
#   Code Snippet  9: Fitting the TS-GARCH(1,1) Model
# 	Code Snippet 10: Fitting the GJR-GARCH(1,1) Model
#   Code Snippet 11: Fitting the DGE-GARCH(1,1) Model
#
#   CHAPTER 7:
#   Code Snippet 12: Specifying a t-MA(1)-GARCH(1,1) Model
#   Code Snippet 13: Simulating Bollerslev's GARCH(1,1) Model
#   Code Snippet 14: Simulating more complex ARMA-GARCH Models [Figure 5]
#   Code Snippet 15: Summarizing the Results from Parameter Estimates                                      
#   Code Snippet 16: Diagnostic Plots from Parameter Estimates [Figure 6]
#   Code Snippet 17: Forecasting Mean and Variance
#   Code Snippet   : Index, Returns, and Volatility Plots for SP500 [Figure 7]
#   Code Snippet 18: Estimating the Parameters for DGE's SP500 Model
#
# Author:
#   Rmetrics (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# CHAPTER 3: The Standard GARCH(1,1) Model

#	Code Snippet  1: Implemting garch11Fit() frm Scratch
#	Code Snippet  2: Benchmarking the GARCH(1,1) for DEMGBP Exchange Rates
#   Figure 1: DEMGBP Series, Histogram, QQ, and ACF Plot


# ------------------------------------------------------------------------------
# Code Snippet 1: garch11Fit()

	# Load Data:
	require(fSeries)
	data(dem2gbp)
	x = dem2gbp[,1]
	
	# Parameter Estimation:
	garch11Fit = function(x)
	{
	    # Step 1: Initialize Time Series Globally:
	      x <<- x
	   	# Step 2: Initialize Model Parameters and Bounds:
	   	  Mean = mean(x); Var = var(x); S = 1e-6
	   	  params = c(mu = Mean, omega = 0.1*Var, alpha = 0.1, beta = 0.8)
	   	  lowerBounds = c(mu = -10*abs(Mean), omega = S^2, alpha = S, beta = S)
	      upperBounds = c(mu =  10*abs(Mean), omega = 100*Var, alpha = 1-S, 
	      	beta = 1-S)
	    # Step 3: Set Conditional Distribution Function:
	      garchDist = function(z, hh) {
			dnorm(x = z/hh)/hh }
		# Step 4: Compose log-Likelihood Function:
	      garchLLH = function(parm) {   
			mu = parm[1]; omega = parm[2]; alpha = parm[3]; beta = parm[4]
			z = (x-mu)
			Mean = mean(z^2)
			# Use Filter Representation:
			e = omega + alpha * c(Mean, z[-length(x)]^2)
			h = filter(e, beta, "r", init = Mean)
			hh = sqrt(abs(h))
			llh = -sum(log(garchDist(z, hh)))   
			llh } 
		  print(garchLLH(params))    
	    # Step 4: Estimate Parameters and Compute Numerically Hessian:
	      fit = nlminb(start = params, objective = garchLLH,
	      	  lower = lowerBounds, upper = upperBounds, control = list(trace=3))
	      epsilon = 0.0001 * fit$par
	      Hessian = matrix(0, ncol = 4, nrow = 4)
	      for (i in 1:4) {
	        for (j in 1:4) {
	          x1 = x2 = x3 = x4 = fit$par
	          x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
	          x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
	          x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
	          x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
	          Hessian[i, j] = 
	            (garchLLH(x1)-garchLLH(x2)-garchLLH(x3)+garchLLH(x4)) /
	          	(4*epsilon[i]*epsilon[j]) } }
	    # Step 5: Create and Print Summary Report: 
	      se.coef = sqrt(diag(solve(Hessian)))
	      tval = fit$par/se.coef
	      matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
	      dimnames(matcoef) = list(names(tval), c(" Estimate",
	        " Std. Error", " t value", "Pr(>|t|)"))
	      cat("\nCoefficient(s):\n")
	      printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
	}
	
	# Try it:
	garch11Fit(x)
	# 	[1] 1133.253
	#   0      1133.25: -0.0164268 0.0221130 0.100000 0.800000
	#   3      1111.77: -0.0120583 0.0128702 0.120629 0.807733
	#   6      1107.36: -0.0122857 0.0102052 0.132150 0.825474
	#   9      1107.04: -0.00463769 0.0100908 0.133319 0.824104
	#  12      1106.88: -0.00497939 0.00998547 0.136549 0.821671
	#  15      1106.71: -0.00534939 0.00977796 0.146626 0.817967
	#  18      1106.64: -0.00586696 0.0101970 0.151814 0.810639
	#  21      1106.61: -0.00630191 0.0108863 0.155101 0.803834
	#  24      1106.61: -0.00621222 0.0107931 0.153469 0.805558
	#  27      1106.61: -0.00619040 0.0107614 0.153134 0.805974
	# 
	# Coefficient(s):
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# [1,] -0.00619040  0.00846211 -0.73154 0.46444724    
	# [2,]  0.01076140  0.00285270  3.77236 0.00016171 ***
	# [3,]  0.15313411  0.02652273  5.77369 7.7552e-09 ***
	# [4,]  0.80597365  0.03355251 24.02126 < 2.22e-16 ***
	# ---
	# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
	###

	# An AddOn - The Minimalist's Bollerslev GARCH(1,1) Model:
	data(dem2gbp)
	x <<- dem2gbp[,1]
	# Log-Likelihood Function:
    LLH = function(p) {   
		X = (x-p[1])
		M = mean(X^2)
		h = rep(p[2] + p[3]*M + p[4]*M, length = length(X))
		for (i in 2:length(X)) h[i] = p[2] + p[3]*X[i-1]^2 + p[4]*h[i-1]
		s = sqrt(abs(h))
		-sum(log(dnorm(X/s)/s)) 
	}
	# Parameter Estimation:    
	nlminb(c(0, var(x)/10, 0.1, 0.8), LLH, control = list(trace = 1))$par
	# Result:
	# [1] -0.006190413  0.010761397  0.153134067  0.805973671
	###
	
	
# ------------------------------------------------------------------------------
# Code Snippet 2: GARCH(1,1) Benchmark for the DEM/GBP Exchange Rates

	# Fitting Bollerslev's GARCH(1,1) with 'nlminb'
	data(dem2gbp)
	x = dem2gbp[, 1]
	garch11Fit(x = x)
	# 	        Estimate  Std. Error   t value   Pr(>|t|)    
	# mu     -0.00619040  0.00846211  -0.73154 0.46444724    
	# omega   0.01076140  0.00285270   3.77236 0.00016171 ***
	# alpha   0.15313411  0.02652273   5.77369 7.7552e-09 ***
	# beta    0.80597365  0.03355251  24.02126 < 2.22e-16 ***
	###
	
	# R/Rmetrics:
	garchFit()
	#           Estimate  Std. Error   t value   Pr(>|t|)    
	# mu       -0.006190    0.008460    -0.732   0.464311    
	# omega     0.010761    0.002853     3.772   0.000162 ***
	# alpha1    0.153134    0.026522     5.774   7.75e-09 ***
	#beta1      0.805974    0.033552    24.022    < 2e-16 ***
	###
	
	# Ox/G@RCH:
	garchOxFit()
	#            Coefficient  Std.Error  t-value  t-prob
	# Cst(M)       -0.006183  0.0084616  -0.7307  0.4650
	# Cst(V)        0.010761  0.0028506    3.775  0.0002
	# ARCH(Alpha1)  0.153406   0.026568    5.774  0.0000
	# GARCH(Beta1)  0.805876   0.033542    24.03  0.0000
	###
	
	# SPlus/Finmetrics:
	if (FALSE) { 
		# For use under S-Plus, only
		module(finmetrics)
		x = read.table(file = "dem2gbp.csv", header = TRUE, sep = ";")[, 1]
		fit = garch(formula.var = ~garch(1,1), series = x, trace = TRUE,
		  control = bhhh.control(tol=1e-6, delta=1e-6, n.iter=1000))
		summary(fit)
		coef = fit$coef
		se.coef = sqrt(diag(solve(-fit$cov$A)))
		t.value = coef/se.coef
		data.frame(Estimate = coef, StdError = se.coef, "t value" = t.value)    
	} 
	#               Estimate     StdError     t.value 
	#        C  -0.006053483  0.008470953  -0.7146165
	#        A   0.010896279  0.002910292   3.7440499
	#  ARCH(1)   0.154211478  0.026829521   5.7478282
	# GARCH(1)   0.804447554  0.034036971  23.6345221 
	###
	
	
# ------------------------------------------------------------------------------
# Figure 1: DEM/GBP Exchange Rates
	
	# Load Data:
	data(dem2gbp)
	x = dem2gbp[, 1]
	###
	
	# Basic Statistics:
	length(x)
	mean(x)
	var(x)
	sd(x)
	skewness(x)
	kurtosis(x)
	###
	
	# Figure 1:
	par(mfrow = c(2, 2), cex = 0.7)
	plot(x, type = "l", col = "steelblue", ylab = "log Return",
		main = "DEMGBP FX Rates")
	abline(h = 0, lty = 3, col = "grey")
	grid()
	hist(x, n = 30, main = "Volatility Histogram", probability = TRUE,
		col = "steelblue", border = "white", xlim = c(-2, 2))
	s = seq(-2, 2, length = 401)
	lines(s, dnorm(s, mean(x), sd(x)))
	qqnorm(x, col = "steelblue", cex = 0.7)
	qqline(x)
	grid()
	acf(abs(x), main = "ACF |DEMGBP|")
	###
	
	
################################################################################
# CHAPTER 4: Alternative Conditional Distributions

#	Figure 2: Student-t Density and Distribution
#   Figure 3: GED Density and Distribution
# 	Figure 4: Skew Sudent-t and Skew GED Distributions
#   Code Snippet  3: Fitting Bollerslev's t-GARCH(1,1) Model
#	Code Snippet  4: Comparing "mci" and "uev" Recursion Initialization

	
# ------------------------------------------------------------------------------
# Figure 2: Standardized Symmetric Student-t Distribution  
 
	# Standardized Symmetric Student-t Distribution:
	# 	nu =  2.5 Fat tailed Distribution
    # 	nu =  5.0 Less fat tailed
    # 	nu = 10.0 Almost Gaussian Distribution
    ###
    
	# Settings:
	mean = 0; sd = 1; dx = 0.1
	x = seq(-10+mean, 10+mean, by = dx)
	###
	
	# 1st Graph - Density Function:
	par(mfrow = c(2, 2), cex = 0.7)
	d1 = dstd(x, mean = mean, sd = sd, nu = 2.5)
	d2 = dstd(x, mean = mean, sd = sd, nu = 5)
	d3 = dstd(x, mean = mean, sd = sd, nu = 10)
	plot(x, d1, type = "n", xlim = c(-5, 5), ylim = c(0, 0.85), 
		xlab = "z", ylab = "f(z)", main = "Student-t: Density")
	grid()
	lines(x, d1, col = "blue")
	lines(x, d2, col = "red")
	lines(x, d3, col = "green")
	text(3.6, 0.15, "nu=10", col = "green")
	text(2.4, 0.40, "nu=2",  col = "red")
	text(1.7, 0.64, "nu=1",  col = "blue")
	###
	
	# 2nd Graph - Distribution Function:
	p1 = pstd(x, mean = mean, sd = sd, nu = 2.5)
	p2 = pstd(x, mean = mean, sd = sd, nu = 5)
	p3 = pstd(x, mean = mean, sd = sd, nu = 10)
	plot(x, p1, type = "n", xlim = c(-5, 5),
		xlab = "z", ylab = "F(z)", main = "Student-t: Distribution")
	grid()
	lines(x, p1, col = "blue")
	lines(x, p2, col = "red")
	lines(x, p3, col = "green")
	text(1.2, 0.20, "nu=1",  col = "blue")
	text(1.7, 0.50, "nu=2",  col = "red")
	text(2.6, 0.80, "nu=10", col = "green")
	###
	
	
# ------------------------------------------------------------------------------
# Figure 3: Standardized Symmetric GED Distribution
    
	# Standardized Symmetric GED Distribution:
	# 	nu =  1 Laplace Distribution
    # 	nu =  2 Normal Distribution
    # 	nu = 10 Almost Uniform Distribution
    ###
    
	# Settings:
	mean = 0; sd = 1; dx = 0.1
	x = seq(-10+mean, 10+mean, by = dx)
	###
	
	# 1st Graph - Density Function:
	par(mfrow = c(2, 2), cex = 0.7)
	d1 = dged(x, mean = mean, sd = sd, nu = 1)
	d2 = dged(x, mean = mean, sd = sd, nu = 2)
	d3 = dged(x, mean = mean, sd = sd, nu = 10)
	plot(x, d1, type = "n", xlim = c(-5, 5), ylim = c(0, 0.7), 
		xlab = "z", ylab = "f(z)", main = "GED: Density")
	grid()
	lines(x, d1, col = "blue")
	lines(x, d2, col = "red")
	lines(x, d3, col = "green")
	text(3.6, 0.15, "nu=10", col = "green")
	text(2.4, 0.40, "nu=2",  col = "red")
	text(1.7, 0.64, "nu=1",  col = "blue")
	###
	
	# 2nd Graph - Distribution Function:
	p1 = pged(x, mean = mean, sd = sd, nu = 1)
	p2 = pged(x, mean = mean, sd = sd, nu = 2)
	p3 = pged(x, mean = mean, sd = sd, nu = 10)
	plot(x, p1, type = "n", xlim = c(-5, 5),
		xlab = "z", ylab = "F(z)", main = "SGED: Distribution")
	grid()
	lines(x, p1, col = "blue")
	lines(x, p2, col = "red")
	lines(x, p3, col = "green")
	text(1.2, 0.20, "nu=1",  col = "blue")
	text(1.7, 0.50, "nu=2",  col = "red")
	text(2.6, 0.80, "nu=10", col = "green")
	###
		
	
# ------------------------------------------------------------------------------
# Figure 4: Standardized Skew Student-t and GED Distributions
   
	# Skew Student-t and GED Distributions:
    ###
    
	# Settings:
	mean = 0; sd = 1; dx = 0.1
	x = seq(-10+mean, 10+mean, by = dx)
	###
	
	# 1st Graph - Skew Student-t:
	par(mfrow = c(2, 2), cex = 0.7)
	d1 = dsstd(x, mean = mean, sd = sd, nu = 5, xi = 1.0)
	d2 = dsstd(x, mean = mean, sd = sd, nu = 5, xi = 0.8)
	d3 = dsstd(x, mean = mean, sd = sd, nu = 5, xi = 0.6)
	plot(x, d1, type = "n", xlim = c(-5, 5), ylim = c(0, 0.55), 
		xlab = "z", ylab = "f(z)", main = "Skew Student-t")
	grid()
	lines(x, d1, col = "blue")
	lines(x, d2, col = "red")
	lines(x, d3, col = "green")
	text(2.7, 0.15, "xi=1.0", col = "blue")
	text(3.0, 0.30, "xi=0.8", col = "red")
	text(3.3, 0.45, "xi=0.6", col = "green")
	###
	
	# 2nd Graph - Skew GED:
	d1 = dsged(x, mean = mean, sd = sd, nu = 2, xi = 1.0)
	d2 = dsged(x, mean = mean, sd = sd, nu = 2, xi = 0.8)
	d3 = dsged(x, mean = mean, sd = sd, nu = 2, xi = 0.6)
	plot(x, d1, type = "n", xlim = c(-5, 5), ylim = c(0, 0.55), 
		xlab = "z", ylab = "f(z)", main = "Skew GED")
	grid()
	lines(x, d1, col = "blue")
	lines(x, d2, col = "red")
	lines(x, d3, col = "green")
	text(2.7, 0.15, "xi=1.0", col = "blue")
	text(3.0, 0.30, "xi=0.8", col = "red")
	text(3.3, 0.45, "xi=0.6", col = "green")
	###


# ------------------------------------------------------------------------------
# Code Snippet 3: Fitting Bollerslev's t-GARCH Model

	# The relevant arguments of garchFit() are:
	# garchFit(cond.dist = c("dnorm", "dsnorm", "dstd", "dsstd", "dged", "dsged"),
	# 	skew = 0.9, shape = 4, include.skew = NULL, include.shape = NULL, ...)
	###
	
	# Fitting t-GARCH(1,1) Model (Student-t) with 'sqp'
	# Tailor Control List ...
	garchFit(series = "dem2gbp", cond.dist = "dstd", include.shape = TRUE)
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu      0.002249    0.006954    0.323   0.7464    
	# omega   0.002319    0.001167    1.987   0.0469 *  
	# alpha1  0.124438    0.026958    4.616 3.91e-06 ***
	# beta1   0.884653    0.023517   37.617  < 2e-16 ***
	# shape   4.118427    0.401185   10.266  < 2e-16 ***
	###										
	
	# Try all the other conditional distributions:
	data(dem2gbp)
	x = dem2gbp[, 1]
	garchFit()                      # Normal         ok  
	garchFit(cond.dist = "dstd")    # Student-t      ok
	garchFit(cond.dist = "dged")    # GED            ok
	garchFit(cond.dist = "dsnorm")  # Skew Norm      ok
	garchFit(cond.dist = "dsstd", algorithm = "nlminb")   # Skew Student  ok  
	garchFit(cond.dist = "dsged", algorithm = "lbfgsb")   # Skew GED      ok
	###
	
	# Tailor Estimation:
	x = dem2gbp[, 1]
	garchFit(control = list(xscale = TRUE, fscale = TRUE))   
	garchFit(cond.dist = "dsged", skew = 0.8, shape = 1.2, 
	  	control =list(XMAX = 0.1, RPF = 1, xscale = TRUE, fscale = TRUE))
	###
	
	
# ------------------------------------------------------------------------------
# Code Snippet 4: Fitting Laplace (1,1) with Different Solvers

	
	# Compare all three Optimization Methods:
			
	# NLMINB:
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE, algorithm = "nlminb")
	#         Estimate  Std. Error  t value  Pr(>|t|)      
	# mu     0.0030970   0.0002248   13.779  < 2e-16 ***
	# omega  0.0040773   0.0018142    2.247   0.0246 *  
	# alpha1 0.1360962   0.0321700    4.231 2.33e-05 ***
	# beta1  0.8661693   0.0304593   28.437  < 2e-16 ***
	                                            ### Log Likelihood not printed !
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE, algorithm = "nlminb+nm")
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu     0.0030970   0.0002159   14.346  < 2e-16 ***
	# omega  0.0040774   0.0018143    2.247   0.0246 *  
	# alpha1 0.1360974   0.0321703    4.231 2.33e-05 ***
	# beta1  0.8661677   0.0304597   28.436  < 2e-16 ***
	###
	
	# BFGS:
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE, algorithm = "lbfgsb")
	#         Estimate  Std. Error  t value  Pr(>|t|)      
	# mu      0.003060    0.051727    0.059   0.9528    
	# omega   0.004098    0.001832    2.237   0.0253 *  
	# alpha1  0.136381    0.033219    4.106 4.03e-05 ***
	# beta1   0.865843    0.031366   27.604  < 2e-16 ***
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE, algorithm = "lbfgsb+nm")
	#         Estimate  Std. Error  t value  Pr(>|t|)    
	# mu     0.0030970   0.0002159   14.346  < 2e-16 ***
	# omega  0.0040774   0.0018143    2.247   0.0246 *  
	# alpha1 0.1360980   0.0321704    4.231 2.33e-05 ***
	# beta1  0.8661671   0.0304599   28.436  < 2e-16 ***
	###
	
	# SQP:
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE)
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu      0.003098    0.050169    0.062    0.951    
	# omega   0.004077    0.001831    2.226    0.026 *  
	# alpha1  0.136075    0.033163    4.103 4.07e-05 ***
	# beta1   0.866182    0.031381   27.602  < 2e-16 ***
	garchFit(series = "dem2gbp", cond.dist = "dged", shape = 1, 
		include.shape = FALSE, control = list(fscale = TRUE))
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu      0.003099    0.049298    0.063   0.9499    
	# omega   0.004077    0.001831    2.227   0.0260 *  
	# alpha1  0.136089    0.033139    4.107 4.01e-05 ***
	# beta1   0.866176    0.031356   27.624  < 2e-16 ***
	###

	
################################################################################
# CHAPTER 5: ARMA(m,n) with GARCH(p,q) Errors

#	Code Snippet  5: Comparing "mci" and "uev" Recursion Initialization
#	Code Snippet  6: APARCH - Computing Conditional Variances Effectively
#   Code Snippet  7: Using R's Filter Representation
#   Code Snippet  8: Tracing the "t-MA(1)-GARCH(1,2)" Model Fit


# ------------------------------------------------------------------------------
# Code Snippet  5: Comparing "mci" and "uev" Recursion Initialization

	# "mci"
	garchFit(series = "dem2gbp")@fit$coef
	#           mu        omega       alpha1        beta1 
	# -0.006190408  0.010761398  0.153134060  0.805973672
	###
	
	# "uev"
	garchFit(series = "dem2gbp", init.rec = "uev")@fit$coef
	#           mu        omega       alpha1        beta1 
	# -0.006269318  0.010983393  0.148699664  0.805808563
	###
	
	
# ------------------------------------------------------------------------------
# Code Snippet 6: APARCH - Computing Conditional Variances Effectively

	# APARCH - How to Compute Conditional Variances Effectively?
	N = 10000
	eps = round(rnorm(N), digits = 2) / 10
    omega = 0.1
    alpha = c(0.1, 0.05)
    gamma = c(0, 0)
    beta = c(0.4, 0.3)
    delta = 2
    u = length(alpha)
    v = length(beta)
    uv = max(u,v)
    h = rep(0.1, uv)
    ###
  
    # Case I: Conditional Variances as Double for-Loop:
    for (i in(uv+1):N ) {
      ed = 0
      for (j in 1:u) {
        ed = ed+alpha[j]*(abs(eps[i-j])-gamma[j]*eps[i-j])^delta
      }
      h[i] = omega + ed + sum(beta*h[i-(1:v)])
    }
    print(h[1:10])
    ###
    
    
# ------------------------------------------------------------------------------
# Code Snippet  7: Using R's Filter Representation

	# Case II: Conditional Variances in Filter Representation - Loopless:
    edelta = (abs(eps)-gamma*eps)^delta
    edeltat = filter(edelta, filter = c(0, alpha), sides = 1)
    c = omega/(1-sum(beta))
    h = c( h[1:uv], c + filter(edeltat[-(1:uv)], filter = beta,
      method = "recursive", init = h[uv:1]-c))
    print(h[1:10])
    ###
    
    
# ------------------------------------------------------------------------------
# Code Snippet 8: Tracing the Student-t MA(1)-GARCH(1,2) Model Fit

	# Fitting Student t-MA(1)-GARCH(1, 2) Model:
	require(fSeries)
	data(dem2gbp)
	x = dem2gbp[, 1]
	###
	
	# Default SQP Solver:
	garchFit(~arma(0,1), ~garch(1,2), cond.dist = "dstd")
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu      0.003120    0.007177    0.435   0.663797    
	# ma1     0.033416    0.023945    1.396   0.162864    
	# omega   0.002848    0.001490    1.911   0.056046 .  
	# alpha1  0.172111    0.033789    5.094   3.51e-07 ***
	# beta1   0.299823    0.147459    2.033   0.042026 *  
	# beta2   0.540753    0.144052    3.754   0.000174 ***
	# shape   4.139274    0.404663   10.229   < 2e-16 ***
	###



################################################################################
# CHAPTER 6: APARCH(p,q) - Assymmetric Power ARCH Model


#   Code Snippet  9: Fitting the TS-GARCH(1,1) Model
# 	Code Snippet 10: Fitting the GJR-GARCH(1,1) Model
#   Code Snippet 11: Fitting the DGE-GARCH(1,1) Model
	
	
# ------------------------------------------------------------------------------
# Code Snippet 9: Fitting Taylor-Schwert GARCH Model

	# Fitting Taylor/Schwert TS-GARCH(1,1) Model:
	require(fSeries)
	data(dem2gbp)
	x = dem2gbp[, 1]
	###
	
	# Default SQP Algorithm:
	garchFit(formula.var = ~aparch(1,1), delta = 1, leverage = FALSE, 
		include.delta = FALSE)
	# Error Analysis:
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu     -0.005210    0.008202   -0.635      0.525    
	# omega   0.030959    0.006135    5.046   4.51e-07 ***
	# alpha1  0.166849    0.021055    7.924   2.22e-15 ***
	# beta1   0.808431    0.026182   30.878    < 2e-16 ***
	###
	
	# Use NLMINB+NM Algorithm:
	garchFit(formula.var = ~aparch(1,1), delta = 1, leverage = FALSE, 
		include.delta = FALSE, algorithm = "nlminb+nm")
	# Error Analysis:
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu     -0.005193    0.008202   -0.633      0.527    
	# omega   0.030967    0.006134    5.049   4.45e-07 ***
	# alpha1  0.166941    0.021058    7.928   2.22e-15 ***
	# beta1   0.808358    0.026177   30.881    < 2e-16 ***
	###
																
	
# ------------------------------------------------------------------------------
# Code Snippet 10: Fitting GJR GARCH Model

	# Fitting GJR-GARCH(1,1) Model:
	require(fSeries)
	data(dem2gbp)
	x = dem2gbp[, 1]
	###
	
	# Use Default SQP Algorithm:
	garchFit(formula.var = ~aparch(1,1), delta = 2, include.delta = FALSE)
	# 	Error Analysis:
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu     -0.007907    0.008625   -0.917   0.359253    
	# omega   0.011234    0.003019    3.721   0.000198 ***
	# alpha1  0.154348    0.026984    5.720   1.07e-08 ***
	# gamma1  0.046000    0.046075    0.998   0.318099    
	# beta1   0.801434    0.034860   22.990    < 2e-16 ***
	###
	
	# Use NLMINB+NM Algorithm:
	garchFit(formula.var = ~aparch(1,1), delta = 2, include.delta = FALSE,
		algorithm = "nlminb+nm")	
	# 	Error Analysis:
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu     -0.007905    0.008625   -0.916 0.359450    
	# omega   0.011233    0.003019    3.721 0.000198 ***
	# alpha1  0.154346    0.026984    5.720 1.07e-08 ***
	# gamma1  0.045921    0.046035    0.998 0.318509    
	# beta1   0.801441    0.034858   22.991  < 2e-16 ***
	###
	
	
# ------------------------------------------------------------------------------
# Code Snippet 11: Fitting the DGE-GARCH(1,1) Model

	# Fitting GJR-GARCH(1,1) Model:
	require(fSeries)
	data(dem2gbp)
	x = dem2gbp[, 1]
	###
	
	# Use Default SQP Algorithm:
	garchFit(formula.var = ~aparch(1,1))
	# 	Error Analysis:
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu     -0.007907    0.008625   -0.917   0.359253    
	# omega   0.011234    0.003019    3.721   0.000198 ***
	# alpha1  0.154348    0.026984    5.720   1.07e-08 ***
	# gamma1  0.046000    0.046075    0.998   0.318099    
	# beta1   0.801434    0.034860   22.990    < 2e-16 ***
	###
	
													# ERROR!
	
	# Use NLMINB+NM Algorithm:
	garchFit(formula.var = ~aparch(1,1), algorithm = "nlminb+nm")	
	# 	Error Analysis:
	#         Estimate  Std. Error  t value   Pr(>|t|)    
	# mu     -0.009771    0.008641   -1.131    0.25813    
	# omega   0.025410    0.007738    3.284    0.00102 ** 
	# alpha1  0.170626    0.023182    7.360   1.84e-13 ***
	# gamma1  0.106758    0.060566    1.763    0.07796 .  
	# beta1   0.803157    0.028322   28.358    < 2e-16 ***
	# delta   1.231987    0.210132    5.863   4.55e-09 ***
	###
	
	
################################################################################
# CHAPTER 7: An Unique GARCH Modelling Approach

#   Code Snippet 12: Specifying a t-MA(1)-GARCH(1,1) Model
#   Code Snippet 13: Simulating Bollerslev's GARCH(1,1) Model
#   Code Snippet 14: Simulating more complex ARMA-GARCH Models [Figure 5]
#   Code Snippet 15: Summarizing the Results from Parameter Estimates                                      
#   Code Snippet 16: Diagnostic Plots from Parameter Estimates [Figure 6]
#   Code Snippet 17: Forecasting Mean and Variance
#   Code Snippet   : Index, Returns, and Volatility Plots for SP500 [Figure 7]
#   Code Snippet 18: Estimating the Parameters for DGE's SP500 Model


# ------------------------------------------------------------------------------
# Code Snippet 12: Specifying a t-MA(1)-GARCH(1,1) Model

	# Argument List:
	args(garchSpec)
	# function (model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
	#	presample = NULL, cond.dist = c("rnorm", "rged", "rstd", "rsnorm", 
	#   "rsged", "rsstd"), rseed = NULL)

	# Specify Model:
	garchSpec(model = list(mean = 0.1, ma = 0.3, omega = 2.0e-6, alpha = 0.12,
     	beta = 0.84, skew = 1.1, shape = 4), cond.dist = "rsstd", rseed = 4711)
	###
	
	
# ------------------------------------------------------------------------------
# Code Snippet 13: Simulating Bollerslev's GARCH(1,1) Model

	# Specify Model:
	model = list(omega = 1.0e-6, alpha = 0.13, beta = 0.81)
	x = garchSim(model, n = 100)
	x
	###
	
	# Add Plot:
	par(mfrow = c(2, 1), cex = 0.7)
	plot(x, type = "b", col = "steelblue")
	title(main = "Bollerslev's GARCH(1,1) Model")
	grid()
	###
	

# ------------------------------------------------------------------------------
# Code Snippet 14: Simulating More Complex ARMA-GARCH Models [Figure 5]


    # ARMA-APARCH Simulation - Show Argument List:
    args(garchSim)
	###
	
    # Specify ARCH(2) Model:
 	model = list(omega = 1e-6, alpha = c(0.1, 0.3), beta = 0)
 	garchSim(model)
 	###

 	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Specify Bollerslev t[4]-GARCH(1,1) Model:
 	model = list(omega = 1e-6, alpha = 0.15, beta = 0.75, shape = 4)
 	x = garchSim(model, n = 500, cond.dist = "rstd", rseed = 4711)
 	x
 	ts.plot(x, main = "t[4]-GARCH(1,1) Simulated Returns", col = "steelblue", 
 		ylab = "Returns")
	grid()
	abline(h = 0, col = "grey")
	ts.plot(cumsum(x), main = "t[4]-GARCH(1,1) Prices", col = "steelblue", 
		ylab = "Prices")
	grid()
 	###

    # Specify Ding-Engle-Granger MA(1)-APARCH(1,1) Model:
	model = list(ma = 0.1, omega = 1e-6, alpha = 0.15, gamma = 0.3, 
		beta = 0.75, delta = 1.3))
	x = garchSim(model, n = 500, rseed = 4711)	
	x
	ts.plot(x, main = "MA(1)-APARCH(1,1) Simulated Returns", col = "steelblue", 
		ylab = "Returns")
	grid()
	abline(h = 0, col = "grey")
	ts.plot(cumsum(x), main = "MA(1)-APARCH(1,1) Prices", col = "steelblue", 
		ylab = "Prices")
	grid()
	###


# ------------------------------------------------------------------------------
# Code Snippet 15: Summarizing the Results from Parameter Estimates

	# Estimate Parameters:
	fit = garchFit(series = "dem2gbp")
	###
	
	# Summary Report:
	summary(fit)
	###
	
	
# ------------------------------------------------------------------------------
# code Snippet 16: Conditional Variance and Quantile-Quantile Plots [Figure6]

	# Parameter Estimates:
	fit1 = garchFit(series = "dem2gbp")
	fit2 = garchFit(series = "dem2gbp", cond.dist = "dstd")
	###
	
	# Plot:
	par(mfrow = c(2,2), cex = 0.7)
	plot(fit1, which = 3)
	title(main = "\n\nDEMGBP | GARCH(1,1)")
	plot(fit1, which = 13)
	title(main = "\n\nDEMGBP | GARCH(1,1)")
	plot(fit2, which = 3)
	title(main = "\n\nDEMGBP | t-GARCH(1,1)")
	plot(fit2, which = 13)
	title(main = "\n\nDEMGBP | t-GARCH(1,1)")
	###
	
		
# ------------------------------------------------------------------------------
# Code Snippet 17: Forecasting Mean and Variance

	# Forecasting Mean and Variance
	###
	
	# Fit:
	fit.norm = garchFit(series = "dem2gbp")
	fit.std = garchFit(series = "dem2gbp", cond.dist = "dstd")
	###
	
	# Forecasting:
	predict(fit.norm, 10)
	###

	# R/Rmetrics Forecasts:
	predict(garchFit(series = "dem2gbp"), 15)	
	#      meanForecast   meanError standardDeviation
	# 	1  -0.006190408   0.4702368        0.3833 961
	#	2  -0.006190408   0.4702368        0.3895 422
	#	3  -0.006190408   0.4702368        0.3953 472
	#	4  -0.006190408   0.4702368        0.4008 358
	#	5  -0.006190408   0.4702368        0.4060 303
	#	6  -0.006190408   0.4702368        0.4109 507
	#	7  -0.006190408   0.4702368        0.4156 152
	#	8  -0.006190408   0.4702368        0.4200 402
	#	9  -0.006190408   0.4702368        0.4242 410
	#	10 -0.006190408   0.4702368        0.4282 313
	#	11 -0.006190408   0.4702368        0.4320 237
	#	12 -0.006190408   0.4702368        0.4356 301
	#	13 -0.006190408   0.4702368        0.4390 612
	#	14 -0.006190408   0.4702368        0.4423 269
	#   15 -0.006190408   0.4702368        0.4454 367
	###
	
	# Ox/G@RCH Forecasts:
	data(dem2gbp)
	x = dem2gbp[,1]
	garchOxFit()
	# Horizon      Mean   Variance
	#      1  -0.006183     0.1470
	#      2  -0.006183     0.1517
	#      3  -0.006183     0.1563
	#      4  -0.006183     0.1606
	#      5  -0.006183     0.1648
	#      6  -0.006183     0.1688
	#      7  -0.006183     0.1727
	#      8  -0.006183     0.1764
	#      9  -0.006183     0.1799
	#     10  -0.006183     0.1833
	#     11  -0.006183     0.1866
	#     12  -0.006183     0.1897
	#     13  -0.006183     0.1927
	#     14  -0.006183     0.1956
	#     15  -0.006183     0.1983
	variance = c( 0.1470, 0.1517, 0.1563, 0.1606, 0.1648, 0.1688, 0.1727, 
	      0.1764, 0.1799, 0.1833, 0.1866, 0.1897, 0.1927, 0.1956, 0.1983)
	round(data.frame(sd=sqrt(variance)), 4)
	#               sd
	#	1   0.3834 058
	#	2   0.3894 868
	#	3   0.3953 479
	#	4   0.4007 493
	#	5   0.4059 557
	#	6   0.4108 528
	#	7   0.4155 719
	#	8   0.4200 000
	#	9   0.4241 462
	#	10  0.4281 355
	#	11  0.4319 722
	#	12  0.4355 456
	#	13  0.4389 761
	#	14  0.4422 669
	#	15  0.4453 089
	###
	
	# SPlus/Finmetrics Forecasts:
	if (FALSE) { 
		# For use under S-Plus, only
		module(finmetrics)
		x = read.table(file = "dem2gbp.csv", header = TRUE, sep = ";")[, 1]
		fit = garch(formula.var = ~garch(1,1), series = x, trace = TRUE,
		  control = bhhh.control(tol = 1e-6, delta = 1e-6, n.iter = 1000))
		ans = predict(fit, 15)
		data.frame(pred = ans$series.pred, sigma = ans$sigma.pred)
	} 
	#              pred       sigma 
	#	 1 -0.006053483  0.3838 101
	#	 2 -0.006053483  0.3900 212
	#	 3 -0.006053483  0.3958 840
	#	 4 -0.006053483  0.4014 241
	#	 5 -0.006053483  0.4066 643
	#	 6 -0.006053483  0.4116 253
	#	 7 -0.006053483  0.4163 256
	#	 8 -0.006053483  0.4207 823
	#	 9 -0.006053483  0.4250 110
	#	10 -0.006053483  0.4290 256
	#	11 -0.006053483  0.4328 394
	#	12 -0.006053483  0.4364 642
	#	13 -0.006053483  0.4399 111
	#	14 -0.006053483  0.4431 903
	#	15 -0.006053483  0.4463 114
	###

	
# ------------------------------------------------------------------------------	
# Figure 7 - Price Index, Returns, and Absolute Returns Plots for SP500

	# Load Data:
	require(fSeries)
	data(sp500dge)
	x = sp500dge[, 1]
	###
	
	# Plot:
	par(mfrow = c(3, 1), cex =0.7)
	# Returns:
	plot(x, type = "l", col = "steelblue", ylab = "Returns")
	title(main = "SP500 Daily Returns")
	grid()
	abline(h = 0, col = "grey")
	# Cumulated Returns:
	plot(x = exp(cumsum(x)), type = "l", col = "steelblue", 
		ylab = "exp(Cumulated Returns)")
	title(main = "SP500 Exponential Cumulated Returns")
	grid()
	abline(h = 0, col = "grey")
	# Abs Returns:
	plot(x = abs(x), type = "l", col = "steelblue", ylab = "abs(Returns)")
	title(main = "SP500 Volatilities")
	grid()
	abline(h = 0, col = "grey")
	###
	

# ------------------------------------------------------------------------------
# Code Snippet 18: Estimating the Parameters for DGE's SP500 Model

	# Load Data:
	require(fSeries)
	data(sp500dge)
	x = 100 * sp500dge[, 1]
	###
	
	# Results from DGE Paper:
	#         Estimate  t.value
	# mu      0.00021       3.2
	# ma      0.145        19
	# omega   0.000014      4.5
	# alpha   0.083        32.4
	# gamma   0.373        20.7
	# beta    0.920       474
	# delta   1.43         33.7
	#
	parm = c(0.00021, 0.145, 0.000014, 0.083, 0.373, 0.920, 1.43)                 
	tval = c(3.2, 19, 4.5, 32.4, 20.7, 474, 33.7)
	###
	   	   
	# R/Rmetrics - SQP Scaled: [41 sec]
	garchFit(~arma(0,1), ~aparch(1,1)) 	   
	#         Estimate  Std. Error  t value Pr(>|t|)    
	# mu      0.020646    0.006346    3.253  0.00114 ** 
	# ma1     0.144745    0.008357   17.319  < 2e-16 ***
	# omega   0.009988    0.001085    9.203  < 2e-16 ***
	# alpha1  0.083803    0.004471   18.742  < 2e-16 ***
	# gamma1  0.373092    0.027995   13.327  < 2e-16 ***
	# beta1   0.919401    0.004093  224.622  < 2e-16 ***
	# delta   1.435124    0.067200   21.356  < 2e-16 ***
	#	Log Likelihood:   21563.34    normalized:  1.264341 
	# Rescale:
	scale = 1/100
	mu = 0.020646; mu*scale
	# [1] 0.00020646
	omega = 0.009988; delta = 1.435124; omega / (1/scale)^(2/delta)
	# [1] 1.630283e-05
	###
	
	# Ox/G@RCH - Scaled: [47 sec]
	garchOxFit(~arma(0,1), ~aparch(1,1))
	# 	                Coefficient  Std.Error  t-value  t-prob
	# Cst(M)               0.020375  0.0063657    3.201  0.0014
	# MA(1)                0.144631  0.0083808    17.26  0.0000
	# Cst(V)               0.009991  0.0010827    9.228  0.0000
	# ARCH(Alpha1)         0.083769  0.0044350    18.89  0.0000
	# APARCH(Gamma1)       0.376495   0.028137    13.38  0.0000
	# GARCH(Beta1)         0.919863  0.0040708    226.0  0.0000
	# APARCH(Delta)        1.416169   0.066176    21.40  0.0000
	# Rescale:
	mu = 0.020375; mu*scale
	# [1] 0.00020375
	omega = 0.009991; delta = 1.416169; omega / (1/scale)^(2/delta)
	# [1] 1.496536e-05
	###
	
	# SPlus/Finmetrics - BHHH with Tailored Control Scaled:  
	if (FALSE) { 
		# For use under S-Plus, only
		module(finmetrics)
		x = 100*as.vector(sp500)
		date()
		fit = garch(~arma(0,1), ~pgarch(1,1), series = x, leverage = TRUE, 
		  control = bhhh.control(tol=1e-6, delta=1e-6, n.iter=10000), 
		  trace = TRUE)
		date()
		# Use Hessian Matrix ...
		coef = fit$coef
		se.coef = sqrt(diag(solve(-fit$cov$A)))
		t.value = coef/se.coef
		data.frame(Estimate = coef, StdError = se.coef, "t value" = t.value)   
	}   
	#    	      Estimate     StdError     t.value 
	#        C  0.02084031  0.006330720    3.291934
	#    MA(1)  0.14470177  0.008294756   17.444971
	#        A  0.01002876  0.001091768    9.185798
	#  ARCH(1)  0.08374599  0.004448664   18.824976
	#   LEV(1) -0.37098826  0.027775705  -13.356574
	# GARCH(1)  0.91954293  0.004078342  225.469798
	#    POWER  1.42901650  0.067071355   21.305914          
	# Rescale:
	mu = 0.02084; mu*scale
	# [1] 0.0002084
	omega = 0.01003; delta = 1.42902; omega / (1/scale)^(2/delta)
	# [1] 1.592868e-05
	###

	
################################################################################

