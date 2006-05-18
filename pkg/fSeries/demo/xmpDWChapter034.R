#
# Examples from the forthcoming Monograph:
#   Rmetrics - Financial Engineering and Computational Finance
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
#   BOLLERSLEV GARCH(1,1) DEM2GBP BENCHMARK:
#   3.4.1 Example:   GARCH(1,1) - Write Main Fit Function
#   3.4.2 Example:   Garch(1,1) - Time Series Initialization
#   3.4.3 Example:   GARCH(1,1) - Parameter Initialization
#   3.4.4 Example:   GARCH(1,1) - Parameter Optimization
#   3.4.5 Example:   GARCH(1,1) - logLikelihood Function
#   3.4.6 Example:   Stylized Facts of 'dem2gbp' Data Set
#   3.4.7 Example:   GARCH(1,1) Modelling of 'dem2gbp' Data Set
#   
#   CONDITIONAL DISTRIBUTIONS:
#   3.4.8  Snippet:  *ged Functions
#   3.4.9  Exercise: Implementation Tests for the Symmetric GED
#   3.4.10 Example:  Generate Figure 3.4.3 Symmetric GED 
#   3.4.11 Snippet:  *std Functions
#   3.4.12 Exercise: Implementation Tests for the Symmetric Student-t
#   3.4.13 Exercise: Generate Figure 3.4.4 Symmetric Student-t
#   3.4.14 Exercise: Skewed Distributions
#   
#   HETEROSKEDASTIC MODELLING:
#   3.4.15 Example:  GARCH/APARCH Modelling Functions
#   3.4.16 Snippet:  garchSpec Class Representation
#   3.4.17 Example:  GARCH Specification - Argument List
# 
#   DGE SP500 MA-APARCH MODELLING:
#       * Example:   DGE Bollerslev GARCH Model - Formula (15)
#       * Example:   DGE Taylor Schwert MA-TSGARCH Model - Formula (17)
#       * Example:   DGE MA-APARCH Model - Formula (19)
#
#   OX INTERFACE:
#       * Example: Ox - G@RCH Ox Interface 
#       * Example: Ox - ARMA/GARCH Models with Gaussian Distribution
#       * Example: Ox - Other than Gaussian Distributions
#       * Example: Ox - Extended GARCH Models: 
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# GARCH(1,1) Parameter Estimation:


### 3.4.1 Exercise: GARCH(1,1) - Write Main Fit Function

    # Write the Main garch11Fit() Function:
    garch11Fit = function(x)
    {
        # Initialize:
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
    ###
    
    
# ------------------------------------------------------------------------------


### 3.4.2 Exercise: Garch(1,1) - Time Series Initialization

    # Initialize the Time Series
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
    ###
       
    
# ------------------------------------------------------------------------------
    
    
### 3.4.3 Exercise: GARCH(1,1) - Parameter Initialization

    # Initialize Model Parameters and Bounds:
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
    ###


# ------------------------------------------------------------------------------


### 3.4.4 Exercise: GARCH(1,1) - Parameter Optimization

    # Estimate Parameters and Compute Hessian:
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
    ###
    
    
# ------------------------------------------------------------------------------


### 3.4.5 Exercise: GARCH(1,1) - logLikelihood Function

    # Compute Value of Log-Likelihood Function:
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
    ###
    
    
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


### 3.4.7 Example: GARCH(1,1) Modelling of 'dem2gbp' Data Set

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
# CONDITIONAL DISTRIBUTIONS:

    
### 3.4.8 Code Snippet: *ged Functions

    # Symmetric Generalized Error Distribution:
    #   Write functions to compute density, probability and quantile
    #   functions for the generalized error distribution and to generate
    #   random numbers.
    ###
    
    # Density Function:
    .dged = function(x, mean = 0, sd = 1, nu = 2)
    {
        # x - Vector of quantiles
        x = (x - mean ) / sd
        lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
        g = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
        Density = g * exp (-0.5*(abs(x/lambda))^nu) / sd
        return(Density)
    }
    ###

    # Distribution Function:
    .pged = function(q, mean = 0, sd = 1, nu = 2)
    {
        # q - Vector of quantiles
        q = (q - mean ) / sd
        lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
        g = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
        h = 2^(1/nu) * lambda * g * gamma(1/nu) / nu
        s = 0.5 * ( abs(q) / lambda )^nu
        Distribution = 0.5 + sign(q) * h * pgamma(s, 1/nu)
        return(Distribution)
    }
    ###

    # Quantile Function:
    .pged = function(p, mean = 0, sd = 1, nu = 2)
    {
        # p - Vector of probabilities
        lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
        g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
        h = 2^(1/nu) * lambda * g * gamma(1/nu) / nu
        s = 0.5 * ( abs(p) / lambda )^nu
        Quantiles = qgamma(s, 1/nu)
        return(Quantiles)
    }
    ###

    # Random Variates:
    .rged = function(n, mean = 0, sd = 1, nu = 2)
    {
        # n - Number of random deviates to be generated
        r = rgamma(n, shape = 1/nu, scale = nu)
        Deviates = r^(1/nu) * sign(runif(n)-0.5)
        return(Deviates)
    }
    ###

    # Try -
    hist(.rged(1000), probability = TRUE)
    x = seq(-4, 4, length = 251)
    lines(x, .dged(x))
    ###
    
    
# ------------------------------------------------------------------------------


### 3.4.9 Exercise: Implementation Tests for the Symmetric GED

    # Test if the Symmetric GED has the desired properties:
    # 1. Fullfills the density the relation f(x) = f(-x)?
    # 2. Is the density normalized?
    # 3. Is the first moment equal to "mean"?
    # 4. Is the sqrt(variance) equal to "sd"?
    # 5. Is the computed distribution the same as the integrated?
    # 6. Is the quantile function the inverse of the distribution function?
    # 7. Have the random deviates proper mean and variance?
    # 8. Fits the true density to the histogram of the rvs?

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


### 3.4.10 Example: Generate Figure 3.4.
    
    # Symmetric GED Function - Limiting Cases:
    #   nu = 1   : Laplace Distribution
    #   nu = 2   : Gaussian Distribution
    #   nu = Inf : Uniform Distribution
    ###
    
    # Settings:
    par(mfrow = c(2, 2), cex = 0.7)
    mean = 0; sd = 1; dx = 0.001
    x = seq(-5+mean, 5+mean, by = dx)
    ###

    # Figure - GED Density Function:
    d1 = dged(x, mean = mean, sd = sd, nu = 1)
    d2 = dged(x, mean = mean, sd = sd, nu = 2)
    d3 = dged(x, mean = mean, sd = sd, nu = 10)
    plot(x, d1, type = "n", xlim = c(-5, 5), ylim = c(0, 0.7), 
        xlab = "z", ylab = "f(z)", main = "GED: Density")
    lines(x, d1, col = "blue")
    lines(x, d2, col = "red")
    lines(x, d3, col = "green")
    text(3.6, 0.15, "nu=10", col = "green")
    text(2.4, 0.40, "nu=2",  col = "red")
    text(1.7, 0.64, "nu=1",  col = "blue")
    ###

    # Figure 2 - GED Distribution Function:
    p1 = pged(x, mean = mean, sd = sd, nu = 1)
    p2 = pged(x, mean = mean, sd = sd, nu = 2)
    p3 = pged(x, mean = mean, sd = sd, nu = 10)
    plot(x, p1, type = "n", xlim = c(-5, 5),
        xlab = "z", ylab = "F(z)", main = "GED: Distribution")
    lines(x, p1, col = "blue")
    lines(x, p2, col = "red")
    lines(x, p3, col = "green")
    text(2.0, 0.20, "nu=1", col = "blue")
    text(2.7, 0.50, "nu=2", col = "red")
    text(3.8, 0.80, "nu=10", col = "green")
    ###
    
    # Figure 3 - GED Absolute Moment Ratios
    plot(c(0, 11), c(0, 15), type = "n", 
        xlab = "n", ylab = "M(n)/M(n-1)", main = "absMoment Ratio")
    for (nu in c(3/4, 1, 2)) {
        # True Values:
        m1 = absMoments(n = 1:10, density = "dged", nu = nu)
        m2 = absMoments(n = 2:11, density = "dged", nu = nu)
        cat("\n nu: ", nu, "\n")
        print(diff(m2/m1))
        points(1:10, m2/m1)
        lines (1:10, m2/m1) 
        text(10,  2.4, "nu=2")
        text(10,  6.6, "nu=1")
        text(10, 11.6, "nu=3/4") 
    }
    ###

    # Figure 4: The Kurtosis M4/M2^2 - 3
    nu = seq(0.1, 10, length = 100)
    M4 = absMoments(n = 4, density = "dged", nu = nu)
    # Plot:
    plot(nu, log(M4), type = "l", ylim = c(0,10), 
        xlab = "nu", ylab = "ln M(4)", main = "GED: 4th Moment")
    abline(v = 0, lty = 3)
    abline(h = log(3), lty = 3)
    points(2, log(3))
    points(1, log(6))
    text(3.1, log(4.8), "Normal")
    text(2.1, log(9.3), "Laplace")
    text(9.1, log(1.2), "Uniform")
    ###
    
    
# ------------------------------------------------------------------------------


### 3.4.11 Code Snippet: *std Functions

    # Standardized Symmetric Student-t Distribution:
    #   Write functions to compute density, probability and quantile
    #   functions for the standardized Student-t distribution and to 
    #   generate random numbers.
    ###

    # Density Function:
    .dstd = function(x, mean = 0, sd = 1, nu = 5)
    {
        # x - Vector of quantiles
        s = sqrt(nu/(nu-2))
        z = (x-mean) / sd
        Density = dt(x = z * s, df = nu) * s/sd
        return(Density)
    }
    ###

    # Distribution Function:
    .pstd = function (q, mean = 0, sd = 1, nu = 5)
    {
        # q - Vector of quantiles
        s = sqrt(nu/(nu-2))
        z = (q-mean) / sd
        Distribution = pt(q = z*s, df = nu)
        return(Distribution)
    }
    ###

    # Quantile Function:
    .qstd = function (p, mean = 0, sd = 1, nu = 5)
    {
        # p - vector of probabilities
        s = sqrt(nu/(nu-2))
        Quantiles = qt(p = p, df = nu) * sd / s + mean
        return(Quantiles)
    }
    ###

    # Random Deviates:
    .rstd = function(n, mean = 0, sd = 1, nu = 5)
    {
        # n - Number of random deviates to be generated
        s = sqrt(nu/(nu-2))
        Deviates = rt(n = n, df = nu) * sd / s + mean
        return(Deviates)
    }
    ###

    # Try -
    set.seed(4711)
    hist(.rstd(1000), breaks = "FD", probability = TRUE, ylim = c(0, 0.6))
    x = seq(-4, 4, length = 251)
    lines(x, .dstd(x))
    ###

 
# ------------------------------------------------------------------------------


### 3.4.12 Exercise: Implementation Tests for the Symmetric Student-t

    # Test if the Symmetric Student-t has the desired properties:
    # 1. Fullfills the density the relation f(x) = f(-x)?
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


### 3.4.13 Exercise: Generate Figure 3.4 Symmetric Student-t
    
    # Standardized Symmetric Student-t Distribution:
    #   nu =  2.5 Fat tailed Distribution
    #   nu =  5.0 Less fat tailed
    #   nu = 10.0 Almost Gaussian Distribution
    ###
    
    # Settings:
    mean = 0; sd = 1; dx = 0.001
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
        xlab = "z", ylab = "F(z)", main = "Student-t Distribution")
    grid()
    lines(x, p1, col = "blue")
    lines(x, p2, col = "red")
    lines(x, p3, col = "green")
    text(1.2, 0.20, "nu=1",  col = "blue")
    text(1.7, 0.50, "nu=2",  col = "red")
    text(2.6, 0.80, "nu=10", col = "green")
    ###
    
    # 3rd Graph - Histogram Plot:
    set.seed(0.4711)
    r = rstd(n = 10000, mean = 1, sd = 2, nu = 2.5)
    mean(r)
    sqrt(var(r))
    hist(r, n = 500, probability = TRUE, xlim = c(-5, 7), 
        main = "10000 Random Deviates")
    x = seq(-5, 7, length = 201)
    lines(x, dstd (x, mean = 1, sd = 2, nu = 2.5), col = "red")
    lines(x, dnorm(x, mean = 1, sd = 1.90), col = "blue")

    # 4th Graph - Kurtosis:
    nu = seq(8.1, 16, by = 0.1)
    M4 = M2 = rep(NA, times = length(nu))
    for (i in 1:length(nu)) {
        M4[i] = absMoments(n = 4, density = "dstd", nu[i])
        M2[i] = absMoments(n = 2, density = "dstd", nu[i]) }
    kurtosis = M4/M2 - 3
    plot(nu, kurtosis)
    ###

        
# ------------------------------------------------------------------------------


### 3.4.14 Exercise: Skewed Distributions

    # Skewed Student-t Density Function:
    .dsstd = function(x, nu, xi)
    {
        # Standardize:
        m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
        mu = m1*(xi-1/xi)
        sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
        z = x*sigma + mu
        # Compute:
        Xi = xi^sign(z)
        g = 2 / (xi + 1/xi)
        Density = g * dstd(x = z/Xi, nu = nu) * sigma
        return(Density)
    }
    ###

    # Skewed Student-t Distribution Function:
    .psstd = function(q, nu, xi)
    {
        # Standardize:
        m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
        mu = m1*(xi-1/xi)
        sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
        z = q*sigma + mu
        # Compute:
        Xi = xi^sign(z); g = 2 / (xi + 1/xi)
        Distribution = H(z) -
            sign(z) * g * Xi * pstd(q = -abs(z)/Xi, nu = nu)
        return(Distribution)
    }
    ###

    # Skewed Student-t Quantile Function:
    .qsstd = function(p, nu, xi)
    {
        # Standardize:
        m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
        mu = m1*(xi-1/xi)
        sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
        # Compute:
        g = 2  / (xi + 1/xi); sig = sign(p-1/2)
        Xi = xi^sig; p = (H(p-1/2)-sig*p) / (g*Xi)
        Quantiles = (-sig*qstd(p=p, sd=Xi, nu=nu) - mu ) / sigma
        return(Quantiles)
    }
    ###

    # Skewed Student-t Random Deviates:
    .rsstd = function(n, nu, xi)
    {
        # Generate Random Deviates:
        weight = xi / (xi + 1/xi)
        z = runif(n, -weight, 1-weight)
        Xi = xi^sign(z)
        Random = -abs(rstd(n, nu=nu))/Xi * sign(z)
        # Scale:
        m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
        mu = m1*(xi-1/xi)
        sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
        Deviates = (Random - mu ) / sigma
        return(Deviates)
    }
    ###

    # For skewed distributions only change the function names
    # and exchange the expression for the 1st abs moment "m1".
    # For skewed distributions with arbitrary mean and standard
    # deviation, we have only to add a shift and scale parameter:
    .dsstd = function (x, mean = 0, sd = 1, nu = 5, xi = 1.5)
    {
        .dsstd(x = (x - mean)/sd, nu = nu, xi = xi)/sd
    }
    ###

    # For details of the implementation inspect the functions:
    # .[dpqr]snorm(x, mean, sd, xi)
    # .[dpqr]sged(x, mean, sd, nu, xi)
    # .[dpqr]sstd(x, mean, sd, nu, xi)
    ###

    # TRY -
    #   Write Implementation Tests for the Skewed Distributions
    #   1. Skew Normal Distribution
    #   2. Skew Generalized Error Distribution
    #   3. Skew Standardized Student-t Distribution
    ###

    # Exercise: 1. Skew Normal Distribution Tests
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

    
    # Example: 2. Skew Student-t Distribution Specification Tests:
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

    
    # Example: 3. Skew GED Specification Tests:
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
# HETEROSKEDASTIC MODELLING
    
    
### 3.4.15 Example: GARCH/APARCH Modelling Functions

    # List Arguments:
    args(garchSpec)
    args(garchSim)
    args(garchFit)
    args(predict.fGARCH)
    ###
    
    
# ------------------------------------------------------------------------------

  
### 3.4.16 Code Snippet: garchSpec Class Representation

    # Class Representation:
    setClass("garchSpec",
      representation(
        call = "call",
        formula = "formula",
        model = "list",
        presample = "matrix",
        distribution = "character")
      )
    )
    
    
# ------------------------------------------------------------------------------


### 3.4.17 Example: GARCH Specification - Argument List

    # List Arguments:
    > args(garchSpec)
    # function (model = list(omega = 1e-06, alpha = 0.1, beta = 0.8),
    #    presample = NULL, cond.dist = c("rnorm", "rged", "rstd",
    #    "rsnorm", "rsged", "rsstd"))
    ###
    
    
# ------------------------------------------------------------------------------


### 3.4.18 Example: Specify ARMA-GARCH/APARCH Models

    # Create the default GARCH(1,1) specifiaction:
    garchSpec()
    ###
    
    # Try the following specifications:
    ###
    
    # ARCH(1)
    garchSpec(model = list())
    garchSpec(model = list(alpha = 0.1))
    garchSpec(model = list(omega = 1e-6, alpha = 0.1))
    ###
    
    # AR(1)-ARCH(1)
    garchSpec(model = list(ar = 0.5))
    ###
    
    # AR(subset 5)-ARCH(1)
    garchSpec(model = list(ar = c(0.5, 0, 0, 0, 0.1)))
    ###
    
    # ARMA(1,2)-ARCH(1)
    garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3)))
    ###
    
    # ARCH(2)
    garchSpec(model = list(alpha = c(0.12, 0.04)))
    ###

    # GARCH(1,1)
    garchSpec()
    garchSpec(model = list(alpha = 0.12, beta = 0.81))
    garchSpec(model = list(omega = 2.2e-6, alpha = 0.05, beta = 0.83))
    ###

    # GARCH(1,2) and GARCH(2,1)
    garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4))
    garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
    ###

    # r[s]norm-GARCH(1,1)
    garchSpec(model = list(), cond.dist = "rnorm")
    garchSpec(model = list(dist = 2), cond.dist = "rsnorm")
    ###

    # r[s]ged-GARCH(1,1)
    garchSpec(model = list(dist = 4), cond.dist = "rged")
    garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsged")
    ###

    # r[s]std-GARCH(1,1)
    garchSpec(model = list(dist = 4), cond.dist = "rstd")
    garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsstd")
    ###

    # Taylor- Schwert GARCH(1,1)
    garchSpec(list(delta = 1)
    ###

    # AR(1)-t-APARCH(2, 1)
    garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6,
       alpha = c(0.1, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8,
        dist = c(nu = 4, xi = 0.5)), cond.dist = "rsstd")
    ###
    
    
################################################################################
# Ding, Granger Engle [1993]


### Example: DGE SP500 - Investigate Data

    # Load Data -
    # Note, this may be not exactly the same data set ...
    data(sp500dge)
    # Get Index:
    x.ts = as.timeSeries(sp500dge)
    x = as.vector(x.ts)
    # Create Return Series:
    r.ts = returnSeries(x.ts)
    r = as.vector(r.ts)
    # Compute Abs Return Series:
    rabs = abs(r)
    rabs.ts = r.ts; rabs.ts@Data = matrix(rabs, ncol = 1)
    ###
        
    # Create Table 2.1:
    table21 = c(
        "sample size" = length(x),
        mean = mean(r),
        std = sd(r),
        skewness = skewness(r),
        kurtosis = kurtosis(r),
        min = min(r),
        max = max(r),
        "studentized range" = (max(r)-min(r))/sd(r),
        "normality test" =  jarqueberaTest(r)@test$statistic )
    table21
    ###
    
    # Plot Figures 2.1 - 2.3:
    par(mfrow = c(3,1), cex = 0.7)
    plot(x.ts, ylab = "", col = "steelblue", 
        main = "SP500 Daily Price Index")
    plot(r.ts, ylab = "", col = "steelblue", 
        main = "SP500 Daily Returns")
    plot(rabs.ts, ylab = "", col = "steelblue", 
        main = "SP500 Daily Absolute Returns")
    ###
    
    # Create Table 3.1:
    lags = c(1:5, 10, 20, 40, 70, 100)
    ACF = NULL
    ACF = rbind(ACF, acf(r, lag.max = 100, plot = FALSE)$acf[lags+1])
    ACF = rbind(ACF, acf(abs(r), lag.max = 100, plot = FALSE)$acf[lags+1])
    ACF = rbind(ACF, acf(r*r, lag.max = 100, plot = FALSE)$acf[lags+1])
    rownames(ACF) = c("r", "|r|", "r^2")
    colnames(ACF) = c("lag 1", as.character(lags[-1]))
    table31 = round(ACF, digits = 3)
    table31
    ###
            
    # Create Table 3.2:
    par(mfrow = c(1,1))
    d = c(0.125, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3)
    ACFD = NULL
    for (D in d) {
        ACFD = rbind(ACFD, 
            acf(abs(r)^D, lag.max = 100, plot = FALSE)$acf[lags+1])
    }
    rownames(ACFD) = as.character(d)
    colnames(ACFD) = c("lag 1", as.character(lags[-1]))
    table32 = round(ACFD, digits = 3)
    table32
    ###
    
    # Taylor Plot:
    teffectPlot(abs(r), deltas = seq(1/8, 3, length = 50), lag.max = 10) 
    ###
    

# ------------------------------------------------------------------------------


### Example: DGE MA-GARCH Model - Formula (15)
    
    # Load Time Series:
    data(sp500dge)
    # Vector of Compounded Returns:
    x = diff(log(sp500dge[, 2]))
    # Vector of Percentage Compounded Returns:
    X = 100*x
    ###
        
    # Use Rmetrics:
    fit = garchFit(~arma(0,1), ~garch(1,1), series = X)
    summary(fit)
    # Coefficient(s):
    #         Estimate  Std. Error  t value Pr(>|t|)    
    # mu     0.0436913   0.0063223    6.911 4.82e-12 ***
    # ma1    0.1439907   0.0084690   17.002  < 2e-16 ***
    # omega  0.0077464   0.0009489    8.164 2.22e-16 ***
    # alpha1 0.0914195   0.0045493   20.095  < 2e-16 ***
    # beta1  0.9061627   0.0045087  200.981  < 2e-16 ***
    # ---
    # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    #
    # Recalculate with x - Why do we scaling of variables?
    ###

    # Use Ox: Estimate MA(1)- GARCH(1,1) With garchOxFit()
    garchOxFit(formula.mean = ~arma(0,1), formula.var = ~garch(1,1))
    #   
    # Maximum Likelihood Estimation (Std.Errors based on Numerical OPG matrix)
    #                   Coefficient    Std.Error  t-value  t-prob
    # Cst(M)               0.000440  6.0956e-005    7.212  0.0000
    # MA(1)                0.143539    0.0078410    18.31  0.0000
    # Cst(V)               0.007741   0.00062376    12.41  0.0000
    # ARCH(Alpha1)         0.091464    0.0017888    51.13  0.0000
    # GARCH(Beta1)         0.906265    0.0020807    435.6  0.0000
    # 
    # No. Observations :      17053   No. Parameters  :         5
    # Mean (Y)         :    0.00018   Variance (Y)    :   0.00013
    # Skewness (Y)     :   -0.49340   Kurtosis (Y)    :  25.58650
    # Log Likelihood   :  56826.973   Alpha[1]+Beta[1]:   0.99773
    #
    # Warning : To avoid numerical problems, the estimated parameter
    # Cst(V), and its std.Error have been multiplied by 10^4.
    ###
        
    # Use SPlus: Estimate MA(1)- GARCH(1,1) With garch():
    x = read.table("sp500dge.csv", header = TRUE, sep = ";")[, 1]
    x = diff(log(x)); X = 100*x
    Q = 1000 # Improve Precision
    fit = garch(formula.mean = ~ ma(1),formula.var =  ~garch(1, 1), series = x,
      control = bhhh.control(tol = 0.0001/Q, delta = 0.0001/Q, n.iter = 100*Q))
    summary(fit)
    # 
    # Estimated Coefficients:
    #               Value  Std.Error t value   Pr(>|t|) 
    #        C 4.376e-004 6.086e-005   7.191 3.348e-013
    #    MA(1) 1.441e-001 7.832e-003  18.403 0.000e+000
    #        A 7.778e-007 6.245e-008  12.456 0.000e+000
    #  ARCH(1) 9.184e-002 1.797e-003  51.100 0.000e+000
    # GARCH(1) 9.058e-001 2.090e-003 433.393 0.000e+000
    #
    # Compare with Q=1 - Note, we run in convergence problems.
    ### 
          
    # Summary MA(1) - GARCH(1,1)
    #
    #           DGE       RMETRICS   SPLUS     OX
    #       mu  0.000438  0.000437   0.000438  0.000440
    #    MA(1)  0.144     0.1440     0.1441    0.1435
    #    omega  0.008     0.00775    0.00778   0.00774    /10^4
    #  ARCH(1)  0.091     0.0914     0.0918    0.0915
    # GARCH(1)  0.906     0.906      0.906     0.906
    ###
    
    
# ------------------------------------------------------------------------------         


### Example: DGE Taylor Schwert MA-TSGARCH Model - Formula (17)
    
    # Load Time Series:
    data(sp500dge)
    # Vector of Compounded Returns:
    x = diff(log(sp500dge[, 2]))
    # Vector of Percentage Compounded Returns:
    X = 100*x
    ###
    

# ------------------------------------------------------------------------------         


### Example: DGE MA-APARCH Model - Formula (19)
    
    # Load Time Series:
    data(sp500dge)
    # Vector of Compounded Returns:
    x = diff(log(sp500dge[, 2]))
    # Vector of Percentage Compounded Returns:
    X = 100*x
    ###
    
    # Rmetrics:
    fit = garchFit(~arma(0,1), ~aparch(1,1), symmetric = FALSE, series = x)
    summary(fit)
    ###
    
    # nlminb / 100*x:
    
    # optim / 100*x: - 21550.48
         mu    ma1    omega  alpha1 gamma1  beta1 delta 
    0.021   0.145  0.014    0.083   0.383  0.920  1.43
    0.02003 0.1447 0.009893 0.08355 0.3804 0.9204 1.406 

    
    # Ox: Estimate MA(1)- APGARCH(1,1) With garchOxFit()
    garchOxFit(formula.mean = ~ arma(0,1), formula.var = ~ aparch(1,1))     
    #
    # Maximum Likelihood Estimation (Std.Errors based on Numerical OPG matrix)
    #                   Coefficient   Std.Error  t-value  t-prob
    # Cst(M)               0.000204 6.5105e-005    3.126  0.0018
    # MA(1)                0.144792   0.0076502    18.93  0.0000
    # Cst(V)               0.144021    0.031802    4.529  0.0000
    # ARCH(Alpha1)         0.083615   0.0025485    32.81  0.0000
    # GARCH(Beta1)         0.920108   0.0019257    477.8  0.0000
    # APARCH(Gamma1)       0.376925    0.017956    20.99  0.0000
    # APARCH(Delta)        1.417771    0.042038    33.73  0.0000
    #
    # No. Observations :     17053   No. Parameters  :         7
    # Mean (Y)         :   0.00018   Variance (Y)    :   0.00013
    # Log Likelihood   : 56981.565
    # 
    # Warning : To avoid numerical problems, the estimated parameter
    # Cst(V), and its std.Error have been multiplied by 10^4.
    ###
    
    # SPlus: Estimate MA(1)- GARCH(1,1) With pgarch():
    Q = 1000
    fit = garch(formula.mean =  ~ ma(1), formula.var =  ~ pgarch(1, 1), 
      series = x, leverage = T,
      control = bhhh.control(tol = 0.0001/Q, delta = 0.0001/Q, n.iter = 100*Q))
    # 
    # Estimated Coefficients:
    #               Value  Std.Error  t value   Pr(>|t|) 
    #        C  0.00020599 6.511e-005    3.164 7.794e-004
    #    MA(1)  0.14485265 7.654e-003   18.925 0.000e+000
    #        A  0.00001374 3.052e-006    4.502 3.391e-006
    #  ARCH(1)  0.08357483 2.560e-003   32.651 0.000e+000
    #   LEV(1) -0.37505310 1.792e-002  -20.927 0.000e+000
    # GARCH(1)  0.91994221 1.929e-003  476.838 0.000e+000
    #    POWER  1.42737881 4.232e-002   33.726 0.000e+000

      
    # Summary MA(1) - APGARCH(1,1)
    #   
    #                       R          SPLUS       G@RCH OX
    #            DGE        RMETRICS   FINMETRICS  OXMETICS
    #       mu   0.00021               0.00021     0.00020
    #       ma   0.145                 0.145       0.145
    #    omega   0.000014              0.000014    0.000014
    #    alpha   0.083                 0.084       0.084
    #    gamma  -0.373                -0.375      -0.377
    #     beta   0.920                 0.920       0.920
    #    delta   1.43                  1.43        1.42
    ###
     
    
################################################################################
# RMETRICS INERFACE TO G@ARCH OX


    # Description:
    #   Rmetrics Interface to the Garch Ox Software Package.
    # Notes:    
    #   Not yet implemented and tested are the following models:
    #   "igarch", "figarch.bbm", "figarch.chung", "fiegarch", 
    #   "fiaparch.bbm", "fiaparch.chung", "hygarch"
    ###


# ------------------------------------------------------------------------------


### Example: Load Data
    
    # Load DEMGBP Benchmark Data Set:
    data(dem2gbp)
    x = diff(log(dem2gbp[, 1]))
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
        
 