#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 3.4
#   Modelling Volatility by Heteroskedastic Processes
#
# List of Examples, Exercises and Code Snippets:
#
#   3.4.1 Exercise: GARCH(1,1) - Main Fit Function
#   3.4.2 Exercise: Garch(1,1) - Series Initialization
#   3.4.3 Exercise: GARCH(1,1) - Parameter Initialization
#   3.4.4 Exercise: GARCH(1,1) - Parameter Optimization
#   3.4.5 Exercise: GARCH(1,1) - logLikelihood Function
#
#   3.4.6 Example: Stylized Facts of  'dem2gbp' Data Set
#       * Example: Investigate the 'dem2gbp' Data Set
#       * Example: Skew Normal Distribution Specification Tests
#       * Example: Symmetric Student-t Distribution Specification Tests
#       * Example: Skew Student-t Distribution Specification Tests
#       * Example: Symmetric GED Specification Tests
#       * Example: Skew GED Specification Tests
#
#       * Example: Ox - garchOX Interface Description
#       * Example: Ox - ARMA/GARCH Models with Gaussian Distribution
#       * Example: Ox - Other than Gaussian Distributions
#       * Example: Ox - Extended GARCH Models: 
#
# Author:
#	(C) 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################
# GARCH(1,1) Parameter Estimation:


### 3.4.1 Exercise: GARCH(1,1) - Fit Function

    garch11Fit = function(x)
    {
        # Initialization:
        .series <<- .garch11InitSeries(x)
        .params <<- .garch11InitParameters(x)
        .LLH <<- 1e99
        
        # Optimize LLH:
        fit = .garch11OptimizeLLH()
        
        # Final Coefficients and Standard Errors:
        coef = fit$par
        se.coef = sqrt(diag(solve(fit$hessian)))
        tval = coef/se.coef
        matcoef = cbind(coef, se.coef, tval, 2*(1-pnorm(abs(tval))))
        dimnames(matcoef) = list(names(tval), c(" Estimate",
        	" Std. Error", " t value", "Pr(>|t|)"))
        cat("\nCoefficient(s):\n")
        printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
        cat("\n")
        
        # Return Value:
        ans = list(fit = fit, residuals = x - fit$par["mu"],
            fitted.values = fit$par["mu"], variances = .series$h)
        invisible(ans)
    }
    
    
# ------------------------------------------------------------------------------


### 3.4.2 Exercise: Garch(1,1) - Series Initialization

    .garch11InitSeries = function(x)
    {	
		# Add Startup Values:
        h = rep( mean((x-mean(x))^2), times = 1 + length(x) )
        x = c(sqrt(h[1]), x)
        
        # Trace:
        cat("\nInitialize Series:\n")
        print(cbind.data.frame(t = 0, h1 = h[1], x1 = x[1]))
        cat("\n")
              
        # Return Value:
        list(h = h, x = x, N = length(h))
    }
       
    
# ------------------------------------------------------------------------------
    
    
### 3.4.3 Exercise: GARCH(1,1) - Parameter Initialization

    .garch11InitParameters = function(x)
    {
        # Set Bounds:
        U = c(mu = -10*abs(mean(x)), omega = 1e-12, alpha = 1e-6,
            beta = 1e-6)
        V = c(mu =  10*abs(mean(x)), omega = 100*var(x),
            alpha = 1-1e-6, beta = 1-1e-6)
            
        # Initialize Parameters:
        params = c(mu = mean(x), omega = 0.10*var(x),
            alpha = 0.1, beta = 0.8)
            
        # Trace:
        cat("\nInitial Parameters:\n")
        print(params)
        cat("\nBounds:\n")
        print(rbind(U, V))
        cat("\n")
        
        # Return Value:
        list(params = params, U = U, V = V)
    }


# ------------------------------------------------------------------------------


### 3.4.4 Exercise: GARCH(1,1) - Parameter Optimization

    .garch11OptimizeLLH = function()
    {
        # Estimate Parameters:
        cat("\nIteration Path:\n")
        fit = nlminb(start = .params$params, objective = .garch11LLH,
        	lower = .params$U, upper = .params$V)
        
        # Compute Hessian Matrix Numerically:
		eps = 0.0001 * fit$par
		n = length(fit$par)
		H = matrix(0, ncol = n, nrow = n)
		f = .garch11LLH
		for (i in 1:n) {
			for (j in 1:n) {
				x1 = x2 = x3 = x4 = fit$par
				x1[i] = x1[i] + eps[i]
				x1[j] = x1[j] + eps[j]
				x2[i] = x2[i] + eps[i]
				x2[j] = x2[j] - eps[j]
				x3[i] = x3[i] - eps[i]
				x3[j] = x3[j] + eps[j]
				x4[i] = x4[i] - eps[i]
				x4[j] = x4[j] - eps[j]
				H[i, j] = (f(x1)-f(x2)-f(x3)+f(x4))/(4*eps[i]*eps[j])
			}
		}
		colnames(H) = rownames(H) = names(.params$params)
		fit$hessian = H
		
        # Print Hessian Matrix:
        cat("\nHessian Matrix:\n")
        print(fit$hessian)
        
        # Return Value:
        fit
    }
    
    
# ------------------------------------------------------------------------------


### 3.4.5 Exercise: GARCH(1,1) - logLikelihood Function

    .garch11LLH = function(parm)
    {
        # Conditional Distribution Function:
        .garchDist = function(z, hh) {
            dnorm(x = z/hh, mean = 0, sd = 1) / hh 
        }
            
        # Extract Parameters:
		mu = parm[1]
		omega = parm[2]
		alpha = parm[3]
		beta = parm[4]		
		
		# Iterate and Save Conditional Variances:
		N = .series$N
		h = .series$h
		z = .series$x - mu
		h[1] = mean( (.series$x[-1] - mu)^2 )
		z[1] = sqrt(h[1])
		for ( i in 2:N )
		h[i] = omega + alpha*z[i-1]^2 + beta*h[i-1]
		.series$h <<- h
		
		# Calculate log-Likelihood:
		llh = -sum(log(.garchDist(z = z[-1], hh = sqrt(abs(h[-1])))))
			
		# Trace:
		if (llh < .LLH) {
			.LLH <<- llh
			cat("\n LLH: ", llh, "\n")
			print(parm)
	}
		# Return Value:
		llh
    }
    
    
# ------------------------------------------------------------------------------


### 3.4.6 Example: Stylized Facts of  'dem2gbp' Data Set

	# Settings:
	par (mfrow = c(3, 2), cex = 0.7)
	###
	
	# Load Data Set:
	data(dem2gbp)
	dem2gbp = dem2gbp[, 1]/100
	###
	
	# Plot Log-Return Series:
	ts.plot(dem2gbp, xlab = "index", ylab = "log Return", col = "steelblue3",
		main = "DEMGBP FX Rate")
	grid()
	abline (h = 0, col = "grey")
	###

	# Histogram Plot:
	hist(dem2gbp, col = "steelblue", probability = TRUE, 
		breaks = "FD", border = "white", xlim = c(-0.02, 0.02))
	abline (h = 0)
	x = seq(-0.02, 0.02, length = 201)
	lines(x, dnorm(x = x, mean = mean(dem2gbp), sd = sqrt(var(dem2gbp))))
	###
	
	# Quantile - Quantile Plot:
    qqnorm(dem2gbp, xlab = "Normal Quantiles", ylab = "Empirical Quantiles",
    	col = "steelblue3", main = "Normal QQ-Plot", cex = 0.5) 
    qqline(dem2gbp)
    grid()
    ###
    
	# Partial Autocorrelation Plot:  
    pacf(dem2gbp)
  	###
    
    # ACF Volatilities Plot:
    acf(abs(dem2gbp))
   	###
   	

# ------------------------------------------------------------------------------


### 3.4.7 Example: GARCH(1,1) Benchmark Fit

    # Estimate the Parameters:
    data(dem2gbp)
	dem2gbp = dem2gbp[, 1]
	###
	
	# Fit:
    garch11Fit(x = dem2gbp)
    ###
    
    # Result:
	#     Coefficient(s):
	#          Estimate   Std. Error  t value   Pr(>|t|)    
	# mu    -0.00619041  0.00846260 -0.73150 0.46447258    
	# omega  0.01076140  0.00285270  3.77236 0.00016171 ***
	# alpha  0.15313407  0.02652271  5.77370 7.7552e-09 ***
	# beta   0.80597366  0.03355251 24.02126 < 2.22e-16 ***

	# Benchmark:
	# mu: -0.006190, omega = 0.01076, alpha = 0.1531, beta = 0.8060
	###
   

################################################################################
# Alternative Conditional Distributions:


### Example: Skew Normal Distribution Specification Tests:

	# Test if the Normal Distribution has the desired properties:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	# 2. Is the density normalized?
	# 3. Is the first moment equal to "mean"?
	# 4. Is the sqrt(variance) equal to "sd"?
	# 5. Is the computed distribution the same as the integrated?
	# 6. Is the quantile function the inverse of the distribution function?
	# 7. Have the random deviates proper mean and variance?
	# 8. Fits the true density to the histogram?
	###
		
	# Test snorm Settings:
	par(mfrow = c(1, 1))
	mean = 0.5; sd = 1.5; xi = 1.5
	x = -4:4; q = x; n = 100000
	###
			
	# Test snorm Density Function:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	Density1 = dsnorm( x, mean = 0, sd = 1, xi = xi)
	Density2 = dsnorm(-x, mean = 0, sd = 1, xi = 1/xi)
	c(mean = mean, sd = sd, xi = xi)
	cbind(x, Density1, Density2)
	# 2. Is the density normalized?
	f = function(x, mean, sd, nu, xi) { 
	  dsnorm(x, mean = mean, sd = sd, xi = xi) }
	Norm = integrate(f, -Inf, Inf, mean = mean, sd = sd, xi = xi)$value
	c(mean = mean, sd = sd, xi = xi)
	c(norm = 1, Norm = Norm)	
	# 3. Is the first moment equal to "mean"?
	f = function(x, mean, sd, nu, xi) { 
	  x * dsnorm(x, mean = mean, sd = sd, xi = xi) }
	mu = integrate(f, -Inf, Inf, mean = mean, sd = sd, xi = xi)$value
	c(mean = mean, sd = sd, xi = xi)
	c(mean = mean, mu = mu)	
	# 4. Is the variance equal to "sd"?
	f = function(x, mean, sd, nu, xi) { 
		(x-mean)^2 * dsnorm(x, mean = mean, sd = sd, xi = xi) }
	sigma = sqrt(integrate(f, -Inf, Inf, mean = mean, sd = sd, xi = xi)$value)
	c(mean = mean, sd = sd, xi = xi)
	c(sd = sd, sigma = sigma)
	###
		
	# Test snorm Distribution Function:
	# 5. Is the computed distribution the same as the integrated?	
	Probability1 = psnorm(q = q, mean = mean, sd = sd, xi = xi)
	Probability2 = NULL
	for (i in 1:length(q)) Probability2 = c(Probability2, integrate(dsnorm, 
		-Inf, q[i], mean = mean, sd = sd, xi = xi)$value)
	c(mean = mean, sd = sd, xi = xi)
	cbind(q, Probability1, Probability2)
	###
		
	# Test snorm Quantile Function:	
	# 6. Is the quantile function the inverse of the distribution function?
	Probability = psnorm(q = q, mean = mean, sd = sd, xi = xi)
	Quantile = qsnorm(p = Probability, mean = mean, sd = sd, xi = xi)
	c(mean = mean, sd = sd, xi = xi)
	cbind(q, Probability, Quantile)
	###
		
	# Test snorm Random Variates:
	# 7. Have the random deviates proper mean and variance?
	r = rsnorm(n = n, mean = mean, sd = sd, xi = xi)
	c(mean = mean, sd = sd, xi = xi)
	c(mu = mean(r), sigma =  sqrt(var(r)))	
	# 8. Fits the true density to the histogram?
	x1 = x[1] * sd + mean
	x2 = x[length(x)] * sd + mean
	hist(r, n = 100, probability = TRUE, xlim = c(x1, x2))
	z = seq(x1, x2, length = 801)
	lines(z, dsnorm(z, mean = mean, sd = sd, xi = xi), col = "red")
	###

	
# ------------------------------------------------------------------------------


### Example: Symmetric Student-t Distribution Specification Tests:

	# Test if the Symmetric Student-t has the desired properties:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	# 2. Is the density normalized?
	# 3. Is the first moment equal to "mean"?
	# 4. Is the sqrt(variance) equal to "sd"?
	# 5. Is the computed distribution the same as the integrated?
	# 6. Is the quantile function the inverse of the distribution function?
	# 7. Have the random deviates proper mean and variance?
	# 8. Fits the true density to the histogram?
	###
	
	# Test STD Settings:
	par(mfrow = c(1, 1))
	mean = 0.5; sd = 1.5; nu = 2.5
	x = -4:4; q = x; n = 100000
	###
			
	# Test STD density function:
	# 1. Fullfills the density the relation f(x) = f(-x)?
	Density1 = dstd( x, mean = 0, sd = 1, nu = nu)
	Density2 = dstd(-x, mean = 0, sd = 1, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	cbind(x, Density1, Density2)
	# 2. Is the density normalized?
	f = function(x, mean, sd, nu) { 
	  dstd(x, mean = mean, sd = sd, nu = nu) }
	Norm = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value
	c(mean = mean, sd = sd, nu = nu)
	c(norm = 1, Norm = Norm)
	# 3. Is the first moment equal to "mean"?
	f = function(x, mean, sd, nu) { 
	  x * dstd(x, mean = mean, sd = sd, nu = nu) }
	mu = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value
	c(mean = mean, sd = sd, nu = nu)
	c(mean = mean, mu = mu)
	# 4. Is the variance equal to "sd"?
	f = function(x, mean, sd, nu) { 
	  (x-mean)^2 * dstd(x, mean = mean, sd = sd, nu = nu) }
	sigma = sqrt(integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value)
	c(mean = mean, sd = sd, nu = nu)
	c(sd = sd, sigma = sigma)
	###
	
	# Test STD distribution function:
	# 5. Is the computed distribution the same as the integrated?	
	Probability1 = pstd(q = q, mean = mean, sd = sd, nu = nu)
	Probability2 = NULL
	for (i in 1:length(q)) Probability2 = c(Probability2, integrate(dstd, 
	  -Inf, q[i], mean = mean, sd = sd, nu = nu)$value)
	c(mean = mean, sd = sd, nu = nu)
	cbind(q, Probability1, Probability2)
	###	
	
	# Test STD quantile function:	
	# 6. Is the quantile function the inverse of the distribution function?
	Probability = pstd(q = q, mean = mean, sd = sd, nu = nu)
	Quantile = qstd(p = Probability, mean = mean, sd = sd, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	cbind(q, Probability, Quantile)
	###
		
	# Test STD random variates:
	# 7. Have the random deviates proper mean and variance?
	r = rstd(n = n, mean = mean, sd = sd, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	c(mu = mean(r), sigma =  sqrt(var(r)))	
	# 8. Fits the true density to the histogram?
	x1 = x[1]*sd + mean
	x2 = x[length(x)]*sd + mean
	hist(r, n = 500, probability = TRUE, xlim = c(x1, x2))
	z = seq(x1, x2, length = 801)
	lines(z, dstd(z, mean = mean, sd = sd, nu = nu), col = "red")
	###
	
	
# ------------------------------------------------------------------------------


### Example: Skew Student-t Distribution Specification Tests:

	# Test if the Skew Student-t has the desired properties:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	# 2. Is the density normalized?
	# 3. Is the first moment equal to "mean"?
	# 4. Is the sqrt(variance) equal to "sd"?
	# 5. Is the computed distribution the same as the integrated?
	# 6. Is the quantile function the inverse of the distribution function?
	# 7. Have the random deviates proper mean and variance?
	# 8. Fits the true density to the histogram?
	###
	
	# Test SSTD Settings:
	par(mfrow = c(1, 1))
	mean = 0.5; sd = 1.5; nu = 2.5; xi = 1.5
	x = -4:4; q = x; n = 100000
	###
		
	# Test SSTD density function:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	Density1 = dsstd( x, mean = 0, sd = 1, nu = nu, xi = xi)
	Density2 = dsstd(-x, mean = 0, sd = 1, nu = nu, xi = 1/xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(x, Density1, Density2)		
	# 2. Is the density normalized?
	f = function(x, mean, sd, nu, xi) { 
		dsstd(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	Norm = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu, 
		xi = xi)$value
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(norm = 1, Norm = Norm)		
	# 3. Is the first moment equal to "mean"?
	f = function(x, mean, sd, nu, xi) { 
		x * dsstd(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	mu = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu, 
		xi = xi)$value
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, mu = mu)	
	# 4. Is the variance equal to "sd"?
	f = function(x, mean, sd, nu, xi) { 
		(x-mean)^2 * dsstd(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	sigma = sqrt(integrate(f, -Inf, Inf, mean = mean, sd = sd, 
		nu = nu, xi = xi)$value)
	c(sd = sd, sigma = sigma)
	###
		
	# Test SSTD distribution function:
	# 5. Is the computed distribution the same as the integrated?	
	Probability1 = psstd(q = q, mean = mean, sd = sd, nu = nu, xi = xi)
	Probability2 = NULL
	for (i in 1:length(q)) Probability2 = c(Probability2, integrate(dsstd, 
		-Inf, q[i], mean = mean, sd = sd, nu = nu, xi = xi)$value)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(q, Probability1, Probability2)
	###	

	# Test SSTD quantile function:	
	# 6. Is the quantile function the inverse of the distribution function?
	Probability = psstd(q = q, mean = mean, sd = sd, nu = nu, xi = xi)
	Quantile = qsstd(p = Probability, mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(q, Probability, Quantile)
	###
	
	# Test SSTD random variates:
	# 7. Have the random deviates proper mean and variance?
	r = rsstd(n = n, mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(mu = mean(r), sigma =  sqrt(var(r)))	
	# 8. Fits the true density to the histogram?
	x1 = x[1] * sd + mean
	x2 = x[length(x)] * sd + mean
	hist(r, n = 500, probability = TRUE, xlim = c(x1, x2))
	z = seq(x1, x2, length = 801)
	lines(z, dsstd(z, mean = mean, sd = sd, nu = nu, xi = xi), col = "red")
	###
	

# ------------------------------------------------------------------------------


### Example: Symmetric GED Specification Tests:

	# Test if the Symmetric GED has the desired properties:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	# 2. Is the density normalized?
	# 3. Is the first moment equal to "mean"?
	# 4. Is the sqrt(variance) equal to "sd"?
	# 5. Is the computed distribution the same as the integrated?
	# 6. Is the quantile function the inverse of the distribution function?
	# 7. Have the random deviates proper mean and variance?
	# 8. Fits the true density to the histogram?

	# Test GED Settings:
	par(mfrow = c(1, 1))
	mean = 0.5; sd = 1/2; nu = 2.5
	x = -4:4; q = x; n = 100000
	###	
	
	# Test GED density function:
	# 1. Fullfills the density the relation f(x) = f(-x)?
	Density1 = dged( x, mean = 0, sd = 1, nu = nu)
	Density2 = dged(-x, mean = 0, sd = 1, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	cbind(x, Density1, Density2)
	# 2. Is the density normalized?
	f = function(x, mean, sd, nu) { 
	  dged(x, mean = mean, sd = sd, nu = nu) }
	Norm = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value
	c(mean = mean, sd = sd, nu = nu)
	c(norm = 1, Norm = Norm)
	# 3. Is the first moment equal to "mean"?
	f = function(x, mean, sd, nu) { 
	  x * dged(x, mean = mean, sd = sd, nu = nu) }
	mu = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value
	c(mean = mean, sd = sd, nu = nu)
	c(mean = mean, mu = mu)
	# 4. Is the variance equal to "sd"?
	f = function(x, mean, sd, nu) { 
	  (x-mean)^2 * dged(x, mean = mean, sd = sd, nu = nu) }
	sigma = sqrt(integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu)$value)
	c(mean = mean, sd = sd, nu = nu)
	c(sd = sd, sigma = sigma)
	###
		
	# Test GED distribution function:
	# 5. Is the computed distribution the same as the integrated?	
	Probability1 = pged(q = q, mean = mean, sd = sd, nu = nu)
	Probability2 = NULL
	for (i in 1:length(q)) Probability2 = c(Probability2, integrate(dged, 
	  -Inf, q[i], mean = mean, sd = sd, nu = nu)$value)
	c(mean = mean, sd = sd, nu = nu)
	cbind(q, Probability1, Probability2)
	###
	
	# Test GED quantile function:	
	# 6. Is the quantile function the inverse of the distribution function?
	Probability = pged(q = q, mean = mean, sd = sd, nu = nu)
	Quantile = qged(p = Probability, mean = mean, sd = sd, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	cbind(q, Probability, Quantile)
	###	
	
	# Test GED random variates:
	# 7. Have the random deviates proper mean and variance?
	r = rged(n = n, mean = mean, sd = sd, nu = nu)
	c(mean = mean, sd = sd, nu = nu)
	c(mu = mean(r), sigma =  sqrt(var(r)))
	# 8. Fits the true density to the histogram?
	x1 = 3 * x[1] * sd + mean
	x2 = 3 * x[length(x)] * sd + mean
	hist(r, n = 100, probability = TRUE, xlim = c(x1, x2))
	z = seq(x1, x2, length = 801)
	lines(z, dged(z, mean = mean, sd = sd, nu = nu), col = "red")
	###


# ------------------------------------------------------------------------------


### Example: Skew GED Specification Tests:

	# Test if the Skew GED has the desired properties:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	# 2. Is the density normalized?
	# 3. Is the first moment equal to "mean"?
	# 4. Is the sqrt(variance) equal to "sd"?
	# 5. Is the computed distribution the same as the integrated?
	# 6. Is the quantile function the inverse of the distribution function?
	# 7. Have the random deviates proper mean and variance?
	# 8. Fits the true density to the histogram?
	###
	
	# Test SGED Settings
	par(mfrow = c(1, 1))
	mean = 0.5; sd = 1/2; nu = 2.5; xi = 1.5
	x = -4:4; q = x; n = 100000
	###
			
	# Test SGED density function:
	# 1. Fullfills the density the relation f(x|xi) = f(-x|1/xi)?
	Density1 = dsged( x, mean = 0, sd = 1, nu = nu, xi = xi)
	Density2 = dsged(-x, mean = 0, sd = 1, nu = nu, xi = 1/xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(x, Density1, Density2)
	# 2. Is the density normalized?
	f = function(x, mean, sd, nu, xi) { 
	  dsged(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	Norm = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu, 
	  xi = xi)$value
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(norm = 1, Norm = Norm)
	###
		
	# 3. Is the first moment equal to "mean"?
	f = function(x, mean, sd, nu, xi) { 
	  x * dsged(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	mu = integrate(f, -Inf, Inf, mean = mean, sd = sd, nu = nu, xi = xi)$value
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, mu = mu)	
	# 4. Is the sqrt(variance) equal to "sd"?
	f = function(x, mean, sd, nu, xi) { 
	  (x-mean)^2 * dsged(x, mean = mean, sd = sd, nu = nu, xi = xi) }
	sigma = sqrt(integrate(f, -Inf, Inf, mean = mean, sd = sd, 
	  nu = nu, xi = xi)$value)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(sd = sd, sigma = sigma)
	###
		
	# Test SGED distribution function:
	# 5. Is the computed distribution the same as the integrated?	
	Probability1 = psged(q = q, mean = mean, sd = sd, nu = nu, xi = xi)
	Probability2 = NULL
	for (i in 1:length(q)) Probability2 = c(Probability2, integrate(dsged, 
	  -Inf, q[i], mean = mean, sd = sd, nu = nu, xi = xi)$value)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(q, Probability1, Probability2)
	###	

	# Test SGED quantile function:	
	# 6. Is the quantile function the inverse of the distribution function?
	Probability = psged(q = q, mean = mean, sd = sd, nu = nu, xi = xi)
	Quantile = qsged(p = Probability, mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	cbind(q, Probability, Quantile)
	###
		
	# Test SGED random variates:
	# 7. Have the random deviates proper mean and variance?
	r = rsged(n = n, mean = mean, sd = sd, nu = nu, xi = xi)
	c(mean = mean, sd = sd, nu = nu, xi = xi)
	c(mu = mean(r), sigma =  sqrt(var(r)))	
	# 8. Fits the true density to the histogram?
	x1 = 3 * x[1] * sd + mean
	x2 = 3 * x[length(x)] * sd + mean
	hist(r, n = 100, probability = TRUE, xlim = c(x1, x2))
	z = seq(x1, x2, length = 801)
	lines(z, dsged(z, mean = mean, sd = sd, nu = nu, xi = xi), col = "red")
	###

	
################################################################################


### Example: garchOX Interface Description


	# Description:
	#   Write an interface to the Garch Ox Software Package.
	#
	# Details:
	#   Currently, as of May 2004, I am writing a new R package for 
	#   the analysis of GARCH processes. As tests I use the benchmarks
	#   described and discussed by Brooks, Burke and Persand, and by
	#   McCullough and Renfro. As a reference for my R implementation 
	#   I used among other software products mainly the GARCH Ox Package 
	#   of Laurent and Peters [2002].
	#   To make the comparisons more easier for me I have implementd
	#   an Interface for R to GARCH Ox. Here you will find it. The 
	#   concept is quite simple, the Garch function writes all 
	#   parameters to an ASCII file, then the Ox engine is started, 
	#   reads the parameters from the file and returns the results 
	#   to other ASCII files. These again are read by R and can be 
	#   used for the diagnostic and graphical analysis. Only a print 
	#   and a plot method are available.
	#   
	# Notes:
	#   The function "garchOxFit" interfaces a subset of the functionality 
	#   of the G@ARCH 4.0 Package written in Ox. G@RCH 4.0 is to my 
	#   opinion one of the most sophisticated packages for modelling 
	#   univariate GARCH processes including GARCH, EGARCH, GJR, APARCH, 
	#   IGARCH, FIGARCH, FIEGARCH, FIAPARCH and HYGARCH models. Parameters
	#   can be estimated by Approximate (Quasi-) Maximum Likelihood Methods
	#   under four assumptions: normal, Student-t, GED or skewed Student-t .
	#
	#   "Ox" is an object-oriented matrix language with a comprehensive 
	#   mathematical and statistical function library. Many packages were 
	#   written for Ox including software mainly for econometric modelling. 
	#   The Ox packages for time series analysis and forecasting, Arfima,
	#   Garch and State Space Modelling are especially worth to note. 
	#
	#   Before you can use Ox, you have to check the "Ox citation and 
	#   "copyright" rules and if you agree and fullfill the conditions, 
	#   then download the OxConsole Software together with the "OxGarch" 
	#   Package, currently G@RCH 4.0. If you are not qualified for a free 
	#   license, order your copy from Timberlake Consultants. 
	#
	#   Windows: I recommend to install the "Setup.exe" under the path 
	#   "C:\\Ox\\" and to unzip the OxGarch Package in the directory 
	#   "C:\\Ox\\Packages". Please copy the file "GarchOxModelling.ox"
	#	from the data directory of this package into the directory
	#	"C:\Ox\lib".
	#   Linux: not used so far ...
	#
	#   Ox Citation and Copyright Rules: "Ox" and all its components 
	#   are copyright of Jurgen A. Doornik. The Console (command line) 
	#   versions may be used freely for academic research and teaching 
	#   purposes only. Commercial users and others who do not qualify 
	#   for the free version must purchase the Windows version of Ox 
	#   and GiveWin with documentation, regardless of which version 
	#   they use (so even when only using "Ox" on Linux or Unix). 
	#   Ox must be cited whenever it is used. Refer to the references 
	#   given below. Note, failure to cite the use of "Ox" in published 
	#   work may result in loss of the right to use the free version, 
	#   and an invoice at the full commercial price. The "Ox" syntax is 
	#   public, and you may do with your own "Ox" code whatever you wish.
	#   
	#   Remember, only a small part of the functionalities are interfaced 
	#   to R. But, principally it would be possible to interface also other
	#   functionalities offered by the "Ox" Garch Package. Feel free to 
	#	modify and to add additional functionalities to the file 
	#	"GarchOxModelling.ox".
	#
	# Notes:
	#   OX and GARCH@OX ARE NOT PART OF THIS DISTRIBUTION!
	#
	# References:
	#   Brooks C., Burke S.P, Persand G. (2001);
	#       \emph{Benchmarks and the Accuracy of GARCH Model Estimation},
	#       International Journal of Forecasting 17, 45--56.    
	#   Doornik, J.A. (2002), 
	#       Object-Oriented Matrix Programming Using Ox, 
	#       London, 3rd ed.: Timberlake Consultants Press and Oxford: 
	#       www.nuff.ox.ac.uk/Users/Doornik.    
	#   Doornik, J.A. and Ooms, M. (1999), 
	#       A Package for Estimating, Forecasting and Simulating Arfima Models, 
	#       Oxford: www.nuff.ox.ac.uk/Users/Doornik.        
	#   Laurent S., Peters J.P. (2002);
	#       \emph{G@RCH 2.2: an Ox Package for Estimating and Forecasting 
	#       Various ARCH Models}, 
	#       Journal of Economic Surveys, 16, 447--485.      
	#   McCullough B.D., Renfro C.G. (1998);
	#       \emph{Benchmarks and Software Standards: A Case Study of GARCH 
	#       Procedures},
	#       Journal of Economic and Social Measurement 25, 59--71. 
	#
	# Author:
	#	(C) 2002, Diethelm Wuertz, GPL
	#
	
	# Not yet implemented and tested are the following models:
	# "igarch", "figarch.bbm", "figarch.chung", "fiegarch", 
	# "fiaparch.bbm", "fiaparch.chung", "hygarch"


# ------------------------------------------------------------------------------


# Example:  Load Data

    #   The file "dem2gbp" contains daily observations of the 
    #   Deutschmark / British Pound foreign exchange log returns. 
    #   This data set has been promoted as an informal benchmark 
    #   for GARCH time-series software validation. See McCullough and 
    #   Renfro [1991], and Brooks, Burke, and Persand [2001] for details.
    #   The nominal returns are expressed in percent, as published in 
    #   Bollerslev and Ghysels [2001]. The data set is available from 
    #   the \emph{Journal of Business and Economic Statistics}, (JBES), 
    #   \emph{ftp://www.amstat.org}. A text file has one column of 
    #   data listing the percentual log-returns of the DEM/GBP exchange 
    #   rates. The sample period is from January 3, 1984, to December 
    #   31, 1991, for a total of 1975 daily observations of FX exchange 
    #   rates.
	###
	
	
    # Load Benchmark Data Set:
    data(dem2gbp)
    x = dem2gbp[, 1]
    ###

    
# ------------------------------------------------------------------------------


### Example: ARMA/GARCH Models - Gaussian Distribution
   
    # ARCH(2):
    arch2 = garchOxFit(
      formula.mean = ~ arma(0,0), formula.var = ~ garch(0,2))
    arch2
    arch2 = garchOxFit(
      formula.mean = ~ arma(0,0), formula.var = ~ arch(2))
    arch2
    ###
    
    # GARCH(1,1):
    garch11 = garchOxFit(
      formula.mean = ~arma(0,0), formula.var = ~garch(1,1))
      garch11
    ###
    
    # GARCH(1,2):    
    garch21 = garchOxFit(
      formula.mean = ~ arma(0,0), formula.var = ~garch(1,2))
    garch21
    ###
    
    # GARCH(2,1):    
    garch21 = garchOxFit(
      formula.mean = ~ arma(0,0), formula.var = ~garch(2,1))
    garch21
    ###
     
    # ARMA(1,0)-GARCH(1,1):   
    ar1.garch11 = garchOxFit(
      formula.mean = ~arma(1,0), formula.var = ~garch(1,1))   
      ar1.garch11     
    ###
      
    # ARMA(0,1)-GARCH(1,1):   
    ma1.garch11 = garchOxFit(
      formula.mean = ~arma(0,1), formula.var = ~garch(1,1))    
      ma1.garch11
    ###
      
    # ARMA(1,1)-GARCH(1,1): 
    arma11.garch11 = garchOxFit(
      formula.mean = ~arma(1,1), formula.var = ~garch(1,1))
      arma11.garch11  
    ###
  
        
# ------------------------------------------------------------------------------


### Example: Other than Gaussian Distributions
      
    # t-GARCH(1,1):
    garch11.t = garchOxFit(
      formula.var = ~garch(1,1), cond.dist = "t")  
    garch11.t 
    ###
    
    # ged-GARCH(1,1):     
    garch11.ged = garchOxFit(
      formula.var = ~garch(1,1), cond.dist = "ged")   
    garch11.ged   
    ###
    
    # skewt-GARCH(1,1):      
    garch11.st = garchOxFit(
      formula.var = ~garch(1,1), cond.dist = "skewed-t")  
      garch11.st    
    ###

  
# ------------------------------------------------------------------------------

  
### Example: Extended GARCH Models
      
    # Fit EGARCH(1,1):
    egarch11 = garchOxFit(
      formula.var = ~ egarch(1,1))  
    egarch11
    ###
    
    # Fit GJR(1,1):   
    gjr11 = garchOxFit(
      formula.var = ~ gjr(1,1)) 
    gjr11
    ###
    
    # Fit APARCH(1,1):  
    aparch11 = garchOxFit(
      formula.var = ~ aparch(1,1))  
    aparch11       
    ###
    
        
################################################################################
# Modelling Heteroskedastic Processes - GARCH(p,q) Processes:


### Example: Write functions for the modelling of GARCH(p,q) processes
	   
	# Details:
	#   The orders p and q must both be greater or equal 1. 
	#
	# Description: 
	#   This is an exercise which shows how to write functions for the
	#   modelling of GARCH(p,q) processes. The functions can be devided
	#   into the following classes including
	#   PART I:     Model Specification
	#   PART II:    Time Series Simulation
	#   PART III:   Parameter Estimation
	#   PATY IV:    Forecasting Volatility  
	# 
	# Examples:
	#   DEMGBP Benchmark Data
	#   Garch(p,q) Specification
	#   Garch(p,q) Simulation
	#   Garch(1,1) Parameter Estimation - With Mean
	#   Garch(1,1) Parameter Estimation - Zero Mean
	#   GARCH(1,2) AND GARCH(2,1) Processes
	#   Garch(1,1) Forecasting
	#
	# Author:
	#   (C) 2002, Diethelm Wuertz, GPL
	#
 	###
 	

# ------------------------------------------------------------------------------


### Exercise: Write tseries - Wrapper

	# His script uses the contributed tseries package from Adrian Tapletti:
    garchTsFit = function(x, order) 
    {
        require(tseries)
        fit = garch(x, order)
        llh = -(fit$n.likeli + length(x)*log(2*pi)/2)
        ans = cbind(Estimate = fit$coef, "Std. Error" = fit$asy.se.coef, 
            "t value" = fit$coef/fit$asy.se.coef)
        rownames(ans) = names(fit$coef)
        round(ans, digits = 6)
    }
    ###
    
    
# ------------------------------------------------------------------------------


### Exercise: Write Splus - Wrapper

	# This program runs under SPlus only!
    garchSplusFit = function(...)
    {
        fit = garch(..., control = bhhh.control(tol = 1.0e-12, n.iter = 10000))
        ans = cbind(fit$coef, sqrt(diag(solve(-fit$cov$A))), 
        fit$coef/sqrt(diag(solve(-fit$cov$A))))
        colIds(ans) = c("Estimate", "Std.Error", "t.value")
        round(ans, digits = 6)
    }
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Garch(p,q) Specification:
    
	# Load Bollerslev and Ghysels DEMGBP Rates:
    require(fSeries)
    data(dem2gbp)
    x = dem2gbp[, 1]
    ###
    
    # Default GARCH(1,1) Model:
    garchpqSpec()
    
    # GARCH(2,1) Model - Omega Missing:
    garchpqSpec(model = list(alpha = c(0.15, 0.05), beta = 0.75))
    
    # GARCH(1,1) with User Defined Presample:
    garchpqSpec(model = list(omega = 2e-6, alpha = 0.12, beta = 0.85),
        presample = data.frame(cbind(z = 0.12, h = 0.05, y = -0.02)))
        

# ------------------------------------------------------------------------------
# Garch(p,q) Simulation:


    # Default GARCH(1,1) Model:
    garchpqSim()
    
    # GARCH(1,1) with User Defined Presample:
    garchpqSim(model = list(omega = 2e-6, alpha = 0.12, beta = 0.85),
        presample = data.frame(cbind(z = 0.12, h = 0.05, y = -0.02)))
        
    # GARCH(1,1) with User Defined Presample - no warm up:
    garchpqSim(presample = data.frame(cbind(z = 0.12, h = 0.05, y = -0.02)),
        n.start = 0)


# ------------------------------------------------------------------------------
# Garch(1,1) Parameter Estimation - With Mean:
    

    # Data:
    require(fSeries)
    data(dem2gbp)
    x = dem2gbp[, 1]
   
    # -0.006190  0.01076   0.1531   0.8060
    #  0.008462  0.002852  0.02652  0.03355
        
    # Rmetrics:
    fit1 = garchpqFit(formula.var = ~ garch(1,1))
    fit1
    #              Estimate  Std. Error   t value    
    #   mu        -0.006209    0.008469    -0.733   
    #   omega      0.010760    0.002853     3.772    
    #   alpha1     0.153403    0.026579     5.771    
    #   beta1      0.805885    0.033564    24.010     

    # Ox - R/Interface:
    fit2 = garchOxFit(formula.var = ~ garch(1, 1))
    fit2
    #                 Value  Std. Error   t value
    #   Cst(M)    -0.006183   0.0084616    -0.731
    #   Cst(V)     0.010761   0.0028506     3.775
    #   ARCH(1)    0.153410   0.0265680     5.774
    #   GARCH(1)   0.805880   0.0335420    24.026
    
    # S-Plus:
    x = scan("dem2gbp.csv", skip = 1)
    garchSplusFit(formula.var =  ~ garch(1, 1), series = x)
    #              Estimate  Std. Error   t value 
    #        C    -0.006091   0.0084693    -0.719
    #        A     0.010848   0.0028900     3.754
    #  ARCH(1)     0.153982   0.0267438     5.758
    # GARCH(1)     0.804816   0.0338972    23.743
    

# ------------------------------------------------------------------------------


### Example: Parameter Estimation - Zero Mean

    
    # Rmetrics:
    fit1 = garchpqFit(formula.var = ~ garch(1,1),  
        init = c(0, 0.01, 0.10, 0.80), fixed = c(T, F, F, F))
    fit1  
    #              Estimate  Std. Error   t value
    #   omega      0.010866    0.002888     3.763   
    #   alpha1     0.154596    0.026784     5.772    
    #   beta1      0.804432    0.033857    23.760   
    
    # Ox - R/Interface:
    fit2 = garchOxFit(formula.var = ~ garch(1, 1), include.mean = FALSE)
    fit2
    #              Estimate  Std. Error   t value
    #   Cst(V)     0.010873    0.002887     3.766
    #   ARCH(1)    0.154640    0.026778     5.775
    #   GARCH(1)   0.804350    0.033847    23.765
    
    # R tseries:
    fit3 = garchTsFit(x, order = c(1, 1))
    fit3
    #              Estimate  Std. Error   t value
    #   omega      0.010784    0.001288     8.372
    #   alpha      0.154074    0.013823    11.147
    #   beta       0.805295    0.015966    50.439
    
 
    
# ------------------------------------------------------------------------------


### Example: GARCH(1,2) AND GARCH(2,1) Processes:


    # Note: Garch(p,q) 
    #   in Rmetrics / S-Plus  p is ARCH-order
    #   in Ox / tseries       q is ARCH-order !

    # Rmetrics:
    garchpqFit(formula.var = ~ garch(2, 1))
    #              Estimate  Std. Error   t value
    # mu          -0.006279    0.008477    -0.741  
    # omega        0.010786    0.002857     3.775  
    # alpha1       0.153372    0.002655     5.776  
    # alpha2       3.17e-10          NA        NA    
    # beta1        0.805781    0.003356    24.012 
    garchpqFit(formula.var = ~ garch(1, 2))  
    #              Estimate  Std. Error   t value
    # mu          -0.005062    0.008524    -0.594  
    # omega        0.011249    0.002983     3.771  
    # alpha1       0.168611    0.027673     6.093  
    # beta1        0.489901    0.130747     3.747  
    # beta2        0.297293    0.12587      2.362  

    # Ox:
    garchOxFit(formula.var = ~ garch(1, 2), series = dem2gbp[, 1])
    #              Estimate  Std. Error   t value
    # Cst(M)      -0.006218    0.008484    -0.733  
    # Cst(V)       0.010759    0.007692     1.399   
    # ARCH(Alpha1) 0.153439    0.026693     5.748  
    # ARCH(Alpha2) 0.000000    0.083615     0.000   
    # GARCH(Beta1) 0.805868    0.11124      7.245   
    garchOxFit(formula.var = ~ garch(2, 1), series = dem2gbp[, 1])
    #              Estimate  Std. Error   t value
    # Cst(M)      -0.005035    0.008510    -0.592   
    # Cst(V)       0.011247    0.002980     3.774  
    # ARCH(Alpha1) 0.168616    0.027663     6.095  
    # GARCH(Beta1) 0.489921    0.13074      3.747   
    # GARCH(Beta2) 0.297278    0.12586      2.362   
            
    # S-Plus:
    x = scan("dem2gbp.csv", skip = 1)
    garchSplusFit(formula.var = ~ garch(2, 1), series = x)
	#              Estimate  Std. Error   t value
	#        C    -0.002247    0.008514    -0.264
	#        A     0.001875    0.000840     2.231
	#  ARCH(1)     0.221632    0.033738     6.569
	#  ARCH(2)    -0.176475    0.035500    -4.971
	# GARCH(1)     0.947134    0.014626    64.757
    garchSplusFit(formula.var = ~ garch(1, 2), series = x)
	#              Estimate  Std. Error   t value
	#        C    -0.004895    0.008515    -0.575
	#        A     0.011198    0.002976     3.763
	#  ARCH(1)     0.168278    0.027592     6.099
	# GARCH(1)     0.486852    0.130312     3.736
	# GARCH(2)     0.300699    0.125557     2.395
    
    # R tseries:
    garchTsFit(x, order = c(1, 2))
    #              Estimate  Std. Error   t value
    # a0           0.011361    0.001568     7.245
    # a1           0.163420    0.018222     8.968
    # a2           0.000000    0.025379     0.000
    # b1           0.795568    0.022755    34.963
    garchTsFit(x, order = c(2, 1))      
    #              Estimate  Std. Error   t value
    # a0           0.011237    0.001497     7.508
    # a1           0.169283    0.016457    10.286
    # b1           0.484615    0.110617     4.381
    # b2           0.302105    0.100985     2.992
    
    
# ------------------------------------------------------------------------------       
# Garch(1,1) Forecasting:

    
    # Data:
    require(fSeries)
    data(dem2gbp)
    x = dem2gbp[, 1]
    
    # Rmetrics:
    fit = garchpqFit(formula.var = ~ garch(1, 1))   
    predict(fit, 10)
    #            Mean   Variance
    # 1975  -0.006216     0.1470
    # 1976  -0.006216     0.1518
    # 1977  -0.006216     0.1564
    # 1978  -0.006216     0.1607
    # 1979  -0.006216     0.1650
    # 1980  -0.006216     0.1690
    # 1981  -0.006216     0.1729
    # 1982  -0.006216     0.1766
    # 1983  -0.006216     0.1802
    # 1984  -0.006216     0.1836
        
    # Ox - R/Interface:
    fit = garchOxFit(formula.var = ~ garch(1, 1))  
    # Horizon   Mean    Variance
    #   1   -0.006185     0.1470
    #   2   -0.006185     0.1517
    #   3   -0.006185     0.1563
    #   4   -0.006185     0.1607
    #   5   -0.006185     0.1648
    #   6   -0.006185     0.1689
    #   7   -0.006185     0.1727
    #   8   -0.006185     0.1764
    #   9   -0.006185     0.1800
    #  10   -0.006185     0.1834
    
    # Splus:
    fit = garch(formula.var = ~ garch(1, 1), series = x,
    	control = bhhh.control(tol = 1.0e-12, n.iter = 10000))
    predict(fit. 10)
    $series.pred:
	 [1] -0.00609 -0.00609  -0.00609  -0.00609  -0.00609
 	 [6] -0.00609 -0.00609  -0.00609  -0.00609  -0.00609
	$sigma.pred:
	 [1] 0.3836317 0.3898176 0.3956579 0.4011777 0.4063997 
	 [6] 0.4113442 0.4160299 0.4204735 0.4246903 0.4286945
	   
    
################################################################################   
        
 