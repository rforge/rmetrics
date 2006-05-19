#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 1.3
#   Distribution Functions in Finance
#
# List of Examples, Exercises and Code Snippets:
#
#   1.3.1 Example: Plot NYSE Histogram and Density
#       * Example: Plot USDCHF Histogram and Density
#   1.3.2 Example: Show Methods of a Generic Function
#       * Exercise: Write a Histogram Plot Function
#       * Exercise: Write a Density Plot Function
#   1.3.3 Example: Create Log Density and Quantile Plot
#       * Example: Compute and Plot Empirical Cumulative Distribution 
#       * Example: Estimate Distributions Using Smooth Splines
#   1.3.4 Example: The Central Limit Theorem
#       * Example: Estimate Parameters of the Student-t Distribution 
#       * Code Snippet: 'rsymstb' Function
#       * Exercise: Plot Stable Return Series
#       * Code Snippet: 'psymstb' Function
#       * Exercise: Plot Cumulated Stable Series
#       * Code Snippet: 'dstable' Function
#   1.3.5 Example: Plot Stable Distribution
#       * Code Snippet: '.integrate' Function
#       * Example: Symmetric Stable Distribution
#       * Example: Limit Behaviour of the Symmetric Stable Distribution
#   1.3.6 Example: Generalized Hyperbolic Distribution
#       * Example: Hyperbolic Distribution
#   1.3.7 Example: Normal Inverse Gaussian Distribution
#       * Code Snippet: 'nigFit' Function
#   1.3.8 Example: Fit NYSE Index and USDCHF Rates
#       * Example: MLE Fit to Hyperbolic Density
#       * Example: MLE Fit to Normal Inverse Gaussian Density
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Packages:

    require(fBasics)
    require(fCalendar)
    require(fMultivar)
    ###
    

# ------------------------------------------------------------------------------


### 1.3.1  Example: Plot NYSE Histogram and Density

    # Creates histogram and density plot for the NYSE composite index
    ###
    
    # Load NYSE Composite Index Values:
    data(nyse)
    NYSE.IDX = as.timeSeries(nyse)
    # Compute Log Returns:
    NYSE.RET = returnSeries(NYSE.IDX)
    NYSE.RET@units = "NYSE Returns"
    # Note, that the definition of the Index has changed
    # end of 2003. To remove the returns from the change,
    # we remove returns greater than 50%:
    NYSE.RET = NYSE.RET[abs(NYSE.RET@Data) < 0.5]
    ###
   
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    # Show Histogram:
    x = as.vector(NYSE.RET)
    x = x[abs(x) < 0.5]
    hist(x = x, probability = TRUE, nclass = 100,
        xlim = c(-0.05, 0.05), col = "steelblue",
        border = "white", xlab = "Log Returns",
        main = "NYSE Composite Index Returns")
    # Add Fit:
    s = seq(-0.05, 0.05, length = 500)
    lines(s, dnorm(s, mean = mean(x), sd = sd(x)), 
        lwd = 2, col = "orange")
    ###
    
    # Show Density Estimate:
    den = density(x)
    plot(den, xlab = "Log Returns", ylab = "Density",
        xlim = c(-0.05, 0.05), lty = 3, lwd = 2,
        main = "NYSE Composite Index")
    # Add Normal Fit:
    lines(s, dnorm(s, mean(x), sd(x)), col = "red")
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Plot USDCHF Histogram and Density

    # Creates histogram and density plot for the 30m USDCHF rates
    ###
    
    # Load USDCHF Data:
    data(usdchf)
    usdchf.ts = as.timeSeries(usdchf, format = "%Y%m%d%H%M")
    USDCHF.RET = returnSeries(usdchf.ts)
    ###
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Log Density Plot:
    x = as.vector(USDCHF.RET)
    hist(x = x, probability = TRUE, nclass = 100,
        xlim = c(-0.005, 0.005), col = "steelblue",
        border = "white", xlab = "Log Returns",
        main = "USDCHF FX Returns")
    # Add Fit:
    s = seq(-0.005, 0.005, length = 201)
    lines(s, dnorm(s, mean = mean(x), sd = sd(x)), lwd = 2,
        col = "orange")
    ###
        
    # Show Density Estimate:
    den = density(x)
    plot(den, xlab = "Log Returns", ylab = "Density",
        xlim = c(-0.005, 0.005), lty = 3, lwd = 2,
        main = "USDCHF FX Returns")
    # Add Normal Fit:
    lines(s, dnorm(s, mean(x), sd(x)), col = "red")
    ###
    
    
# ------------------------------------------------------------------------------


### 1.3.2 Example: Show Methods

    # Print availalble Methods:
    methods(as.vector)
    # [1] as.vector.factor     as.vector.timeSeries
    ###
  
    
# ------------------------------------------------------------------------------  
    

### Exercise: Write a Histogram Plot Function
    
    # Function:
    .histPlot = function(x, main = NULL, fit = TRUE, ...)
    {
      # Convert Data:
      x.vec  = as.vector(x)
      mean = mean(x.vec); sd = sd(x.vec)
      xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd))
      # Histogram Plot:
      hist(x = x.vec, breaks = "FD", probability = TRUE,
        xlim = xlim, col = "steelblue", border = "white",
        xlab = "Log Returns", main = "", ...)
      if (is.null(main)) title(main = x@units)
      # Fit Normal Density:
      if (fit) {
        s = seq(xlim[1], xlim[2], length = 500)
        lines(s, dnorm(s, mean, sd), col = "orange", ...)
      }
    }
    ###
   
    # Try:
    .histPlot(x = NYSE.RET)
    ###
        
    # Use the Rmetrics functions:
    histPlot(NYSE.RET)
    ###
    

# ------------------------------------------------------------------------------


### Exercise: Write a Density Plot Function

    # Function:
    .densityPlot = function(x, xlim = NULL, main = NULL, fit = TRUE, ...)
    {
      # Convert Data:
      x.vec  = as.vector(x)
      mean = mean(x.vec); sd = sd(x.vec)
        xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd))
      # Density Plot:
      den = density(x.vec)
      plot(den, xlab = "Log Returns", ylab = "Density",
        xlim = xlim, lty = 3, lwd = 2,  main = "", ...)
      if (is.null(main)) title(main = x@units)
      # Fit Normal Density:
      if (fit) {
        s = seq(xlim[1], xlim[2], length = 500)
        lines(s, dnorm(s, mean, sd), col = "orange", ...)
      }
     invisible()
    }
    ###
    
    # Try:
    .densityPlot(x = NYSE.RET)
    ###
    
    # Use the Rmetrics functions:
    densityPlot(NYSE.RET)
    ###
   

# ------------------------------------------------------------------------------


### 1.3.3 Example: Create Log Density and Quantile Plot

    # Use Data from Example 1.3.1:
    x = as.vector(NYSE.RET)
    ###
    
    # Graph Frame:
    par(mfrow = c(1, 1))
    ###
    

    # Log Density Plot:
    den = density(x)
    plot(den$x, log10(den$y), type = "l", lty = 3, lwd =2,
        xlim = c(-0.05, 0.05), ylim = c(-7.5, 2.5),
        xlab = "log10 Density", ylab = "log Return",
        main = "NYSE Composite Index")
    s = seq(from = -0.5, to = 0.5, length = 500)
    lines(s, log10(dnorm(s, mean(x), sd(x))), col = "red")
    # Normal Quantile-Quantile Plot:
    qqnorm(x, ylim = c(-0.1, 0.1), cex = 0.4)
    qqline(x, col = "red")
    ###
    
    # Do the same plots for the USDCHF FX returns ...
    # Use alternatively the Rmetrics functions:
    logpdfPlot(NYSE.RET)
    require(fExtremes)
    qqPlot(NYSE.RET)
    qqbayesPlot(NYSE.RET)
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Compute and plot an empirical cumulative distribution 

    # Simple didactical  ecdf  example:
    Fn <- ecdf(rnorm(12))
    print(Fn)
    summary(Fn)
    ### 
    
    # == 1:12  if and only if there are no ties !
    12 * Fn(knots(Fn)) == 1:12 
    y <- round(rnorm(12), 1); y[3] <- y[1]
    Fn12 <- ecdf(y)
    Fn12
    print(knots(Fn12), dig = 2)
    ###
        
    # ~= 1:12  if there where no ties
    12 * Fn12(knots(Fn12))  
    summary(Fn12)
    summary.stepfun(Fn12)
    print(ls.Fn12 <- ls(env = environment(Fn12)))
    ###
        
    #[1] "f"  "method"  "n"  "ties"   "x"  "y"  "yleft"  "yright"
    12 * Fn12((-20:20)/10)
    ###
    
    # Plotting:
    op <- par(mfrow = c(3, 2), mgp = c(1.5, 0.8, 0), 
     mar = 0.1 + c(3, 3, 2, 1))
    F10 <- ecdf(rnorm(10))
    summary(F10)
    plot(F10)
    plot(F10, verticals = TRUE, do.p = FALSE)
    plot(Fn12)  
    xx <- unique(sort(c(seq(-3, 2, length = 201), knots(Fn12))))
    lines(xx, Fn12(xx), col = 'blue')
    abline(v = knots(Fn12), lty = 2,col = 'gray70')
    plot(xx, Fn12(xx), type='b', cex=.1)#- plot.default
    plot(Fn12, col.h='red', add= TRUE)  #- plot method
    abline(v=knots(Fn12),lty=2,col='gray70')
    plot(Fn12, verticals=TRUE, col.p='blue', col.h='red',col.v='bisque')
    ###
        
    # This works too (automatic call to  ecdf(.)):
    plot.ecdf(rnorm(24))
    par(op)
    ###

   
# ------------------------------------------------------------------------------


### Example: Estimate Distributions Using Smooth Splines:

    # Estimate probability densities using smoothing spline ANOVA models 
    ###
    
    # Fit: Estimate from Empirical Distribution  
    data(nyseres)
    par(mfcol = c(3, 2), cex = 0.7, err = -1)
    # Take subset at random ...
    nyseres = sample(nyseres[, 1])[1:1000]
    e = (nyseres - mean(nyseres))/sqrt(var(nyseres))
    # This will take some time ...
    fit = ssdFit(e)
    ###
         
    # SSD - Random Variates:    
    r = rssd(100, fit)
    plot(r, type = "l", main = "SSD Series")
    mean(r)
    var(r)
    ###
    
    # SSD - Density:
    x = seq(min(r), max(r), length = 256)   
    d = dssd(x, fit)
    plot(x, log(d), type = "l", main = "SSD Density")
    density = density(r, from = -6, to = 6, n = 128)
    points(density$x, log(density$y), col = 4)
    ###
    
    # SSD - Probability:    
    p = pssd(x, fit)
    plot(x, p, type = "l", main = "SSD Probability")
    points(sort(r), (1:length(r))/length(r), col = 4)
    ###
    
    
# ------------------------------------------------------------------------------


### 1.3.4  Example: The Central Limit Theorem

    # Create an artificial time series with t-distributed
    # innovations: You can think of this as an artificial daily
    # series of return values. Then aggregate the series on an
    # aggregation level of 20, this will coincide approximately 
    # with monthly return values:
    set.seed(1985)
    df = 4
    r = rt(2500, df)
    ###
    
    # Standardize the series:
    r = r/sqrt(df/(df-2))
    main1 = "\n\n Artificial Student-t Series"
    main20 = "\n\n Aggregated Student-t Series"
    ### 

    # Consider an aggregation level of 20:
    # Write series row-by-row as matrix with 20 columns:
    X = matrix(r, byrow = TRUE, ncol = 20)
    # Sum row-by-row and scale by the size:
    r20 = apply(X, MARGIN = 1, FUN = sum)/sqrt(20)
    ###   

    # Show Quantile-Quantile Plots:
    # The plots are displayed in figure 1.3.3.
    par(mfrow = c(2, 2), cex = 0.7)
    range = c(-4, 4)
    qqnorm(r, cex = 0.5, col = "steelblue", pch = 19,
        xlim = range, ylim = range)
    title(main = main1)
    qqline(r)
    grid()
    qqnorm(r20, cex = 0.5, col = "steelblue", pch = 19,
        xlim = range, ylim = range)
    title(main = main20)
    qqline(r20)
    grid()
    ###
    
    # Print Kurtosis Values:
    kurtosis(r)
    kurtosis(r20)
    ###
    
    # Now we repeat the investigation for IBM log-Returns:
    data(DowJones30)
    IBM.RET = returnSeries(as.timeSeries(DowJones30)[, "IBM"])
    r = as.vector(IBM.RET)
    # Standardize the Series:
    r = (r - mean(r))/sd(r)
    main1 = "\n\n Standardized IBM log-Returns"
    main20 = "\n\n Aggregated IBM log-Returns"
    ###
       
    # Now proceed as above ...
    X = matrix(r, byrow = TRUE, ncol = 20)
    r20 = apply(X, MARGIN = 1, FUN = sum)/sqrt(20)
    qqnorm(r, cex = 0.5, col = "steelblue", pch = 19,
        xlim = range, ylim = range)
    title(main = main1)
    qqline(r)
    grid()
    qqnorm(r20, cex = 0.5, col = "steelblue", pch = 19,
        xlim = range, ylim = range)
    title(main = main20)
    qqline(r20)
    grid()
    kurtosis(r)
    kurtosis(r20)
    ###
    
    
# ------------------------------------------------------------------------------


### Exercise: Estimate Parameters of the Student-t Distribution

    # Write the log-Likelihood Function:
    llh = function(x, returns) {
        if (x[1] <= 2) x[1] = x.save
        f = -sum(log(dt(returns, df = x[1])))
        # Save Globally:
        x.save <<- x[1]
        # Return Value:
        f
    }
    ###

    # Create Simulated t-Series:
    set.seed(4711)
    returns = rt(n = 2500, df = 4)
    ###
    
    # Start Value for the Optimization:
    sd = sd(returns)
    df = 2*sd/(sd-1)
    print(df)
    ###
    
    # MLE Estimation, use R Function 'nlm':
    r = nlm(f = llh, p = df, returns = returns)
    print(r$estimate)
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snippet: 'rsymstb' Function

    # Function:
    .rsymstb = function(n, alpha)
    {
        # Uniform and exponential distributed random numbers:
        theta = pi * (runif(n)-1/2)
        w = -log(runif(n))
        # Calculate Random Deviates for alpha >1:
        ans = (sin(alpha*theta) / ((cos(theta))^(1/alpha))) *
            (cos((1-alpha)*theta)/w)^((1-alpha)/alpha)
        ans
    }
    ###
    
    # Try:
    par(mfrow = c(2, 2), cex = 0.7)
    set.seed(4711)
    rvs = .rsymstb(1000, 1.8)
    ts.plot(rvs, main = "rsymstb Series")
    hist(rvs, n = 50, xlim = c(-10, 10))
    ###
    
    
# ------------------------------------------------------------------------------


### Exercise: Plot Stable Series 

    # Set of six Parameters:
    par(mfrow = c(3, 2), cex = 0.7)
    for (alpha in c(2, 1.9, 1.7, 1.4, 1, 0.6) ) {                              
        seed = 4711
        set.seed(seed)
        r1 = rstable(1000, alpha, beta = 0)
        set.seed(seed)
        r2 = rstable(1000, alpha, beta = 0.2)
        set.seed(seed)
        r3 = rstable(1000, alpha, beta = -0.2)
        Title = paste("alpha =", as.character(alpha))
        plot(c(0, 1000), range(c(r1, r2, r3)), type = "n",
            ylab = "r", main = Title)
        lines(r1, col = "black") 
        lines(r2, col = "brown", lty = 3) 
        lines(r3, col = "steelblue", lty = 3) 
    } 
    ###

    
# ------------------------------------------------------------------------------


### Code Snippet: 'psymstb' Function

    # Shows how to call a Fortran subroutine from R.
    .symstb = function (x, alpha)
    {
        # Use McCullochs Fortran Routine:
        result = .Fortran("symstb",
            as.double(x),               # x-values
            as.double(1:length(x)),     # probability
            as.double(1:length(x)),     # density
            as.integer(length(x)),      # number of x-values
            as.double(alpha),           # index alpha
            package = "fBasics")
        list(p = result[[2]], d = result[[3]])
    }
    ###
    
    # To be conform with R add the functions .psymstb()
    # and .dsymstb() to compute the CDF and PDF of the
    # symmetric stable distribution:
    .dsymstb = function (x, alpha) .symstb(x, alpha)$d
    .psymstb = function (x, alpha) .symstb(x, alpha)$p
    ###
       
    # Try:
    .psymstb(0.5, 1.7)
    .dsymstb(0.5, 1.7)
    ###
 
    
# ------------------------------------------------------------------------------


### Exercise: Plot Cumulated Stable Series 

    # Set of six Parameters:        
    for (alpha in c(2, 1.9, 1.7, 1.4, 1, 0.6) ) {                              
        seed = 4711
        set.seed(seed)
        r1 = cumsum(rstable(1000, alpha, beta = 0))
        set.seed(seed)
        r2 = cumsum(rstable(1000, alpha, beta = 0.2))
        set.seed(seed)
        r3 = cumsum(rstable(1000, alpha, beta = -0.2))
        Title = paste("alpha =", as.character(alpha))
        plot(c(0, 1000), range(c(r1, r2, r3)), type = "n",
            ylab = "Cumulated r", main = Title)
        lines(r1, col = "black") 
        lines(r2, col = "brown", lty = 3) 
        lines(r3, col = "steelblue", lty = 3) 
    }
    ###
       
    
# ------------------------------------------------------------------------------


### Code Snippet: 'dstable' Function

    # Function:
    .dstable = function(x, alpha, beta = 0)
    {
      # Internal Function:
      fct = function(x, xarg, alpha, beta, varzeta, theta0, c2)
      {
        v = (cos(alpha*theta0))^(1/(alpha-1)) *
            (cos(x)/sin(alpha*(theta0+x)))^(alpha/(alpha-1)) *
            cos(alpha*theta0+(alpha-1)*x)/cos(x)
        g = (xarg-varzeta)^(alpha/(alpha-1)) * v
        f = c2 * g * exp(-g)
        # Return Value:
        f
      }
      # Precision:
      subdivisions = 1000
      rel.tol = .Machine$double.eps^0.5    
      # Start Calculation:
      result = rep(0, times = length(x))
      varzeta = -beta * tan(pi*alpha/2)
      theta0 = -(1/alpha) * atan(varzeta)
      for ( i in 1:length(result) ) {
        if (x[i] == varzeta){
          result[i] = gamma(1+1/alpha)*cos(theta0) /
            (pi*(1+varzeta^2)^(1/(2*alpha)))
        }
        if (x[i] > varzeta) {
          c2 = alpha/(pi*abs(alpha-1)*(x[i]-varzeta))
          result[i] = integrate(fct, lower = -theta0, upper = pi/2,
            subdivisions = subdivisions, rel.tol = rel.tol,
            xarg = x[i], alpha = alpha, beta = beta,
            varzeta = varzeta, theta0 = theta0, c2 = c2)$value
        }
        if (x[i] < varzeta) {
          c2 = -alpha/(pi*abs(alpha-1)*(x[i]-varzeta))
          result[i] = integrate(fct, lower = theta0, upper = pi/2,
            subdivisions = subdivisions, rel.tol = rel.tol,
            xarg = -x[i], alpha = alpha, beta = -beta,
            varzeta = -varzeta, theta0 = -theta0, c2 = c2)$value
        }
      }
      # Return Value:
      result
    }
    ###
  
    # Here comes an example with a nice printing of the
    # result in form of a data frame:
    x = seq(-3, 3, length = 7)
    d = .dstable(x, 1.8, 0.2)
    data.frame(x = x, density = d)
    #     x     density
    # 1  -3  0.02780538
    # 2  -2  0.09488465
    # 3  -1  0.21465968
    # 4   0  0.28300101
    # 5   1  0.21374465
    # 6   2  0.09854270
    # 7   3  0.03264026
    ###
    
   
# ------------------------------------------------------------------------------


### 1.3.5  Example: Stable Mode and Other Parameterizations

    # Graph Settings:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Stable Mode:
    alpha = seq(0, 2, by = 0.05)
    beta = c(1.00, 0.75, 0.50, 0.25, 0.00)
    for ( i in 1:length(beta) ) {
        y = NULL
        for ( j in 1:length(alpha) ) { 
            y = c(y, stableMode(alpha = alpha[j], beta = beta[i]))
        }
        if ( i == 1 ) { 
            plot(x = alpha,y = y, type = "l", ylab = "m")
        } else {
            lines(x = alpha, y = y, lty = i+1)
        }
    }
    title(main = "Stable Mode")
    legend(1.5, -0.25, legend = paste("beta=", beta), lty = 1:5)
    ###
  
    # First Parameterization: S0
    x = seq(-5.001, 5.001, by = 0.05)
    alpha = c(0.50, 0.75, 1.00, 1.25, 1.50)
    beta  = 0.50
    gamma = 1.00
    delta = 0.00
    for ( i in 1:length(alpha) ) {
        y = dstable(x, alpha[i], beta, gamma, delta, pm = 0) 
        if (i == 1) {
            plot(x, y, type = "l")
        } else {
            lines(x, y,  lty = i+1)
        }
    }
    ###

    # Second Parameterization: S1
    x = seq(-5.001, 5.001, by = 0.05)
    alpha = c(0.50, 0.75, 1.00, 1.25, 1.50)
    beta  = 0.50
    gamma = 1.00
    delta = 0.00
    for ( i in 1:length(alpha) ) {
        y = dstable(x, alpha[i], beta, gamma, delta, pm = 1) 
        if (i == 1) {
            plot(x, y, type = "l", ylab = "pdf")
        } else {
            lines(x, y,  lty = i+1)
        }
    }
    title(main = "S1 Parameterization")
    legend(-4, 0.55, legend = paste("alpha=", alpha), lty = 1:5)
    ###

    # Third Parameterization: S2
    x = seq(-5.001, 5.001, by = 0.05)
    alpha = c(0.50, 0.75, 1.00, 1.25, 1.50)
    beta  = 0.50
    gamma = 1.00
    delta = 0.00
    for ( i in 1:length(alpha) ) {
        y = dstable(x, alpha[i], beta, gamma, delta, pm = 2) 
        if (i == 1) {
            plot(x, y, type = "l", ylim = c(0, 0.5), ylab = "pdf")
        } else {
            lines(x, y,  lty = i+1)
        }
    }
    title(main = "S2 Parameterization")
    legend(-4, 0.45, legend = paste("alpha=", alpha), lty = 1:5)
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snippet: '.integrate' Function

    # Function:
    .integrate = function (f, 
        lower, upper, subdivisions, rel.tol, abs.tol, ...)
    {
        # Integrate:
        if (class(version) != "Sversion") {
            # We are under the R Environment:
            f = match.fun(f)
            ff = function(x) f(x, ...)
            wk = .External("call_dqags", ff, rho = environment(),
                as.double(lower), as.double(upper), as.double(abs.tol),
                as.double(rel.tol), limit = as.integer(subdivisions),
                PACKAGE = "base")
            ans = wk[c("value", "abs.error", "subdivisions")]
        } else {
            # We are under the SPlus Environment:
            ans = integrate(f, lower, upper, subdivisions, rel.tol,
                abs.tol, ...)
        }
        # Return Value:
        ans
    }
    ###
    
   
# ------------------------------------------------------------------------------


### Example: Symmetric Stable Distribution

    # Settings:
    par(mfcol = c(3, 2), err = -1)
    ###
        
    # RSYMSTB(1.01) - Symmetric Stable Distribution - Close Cauchy: 
    x = seq(-7.5, 7.5, length = 256)
    r = rsymstb(4096, alpha = 1.01)
    plot(r, type = "l", main = "RSYMSTB(1.01) Series")
    d = dsymstb(x, alpha = 1.01)
    d.cauchy = dcauchy(x)
    plot(x, log(d), type ="l", main="DSYMSTB(1.01) - Close Cauchy")
    density = density(r, from = -10, to = 10, n = 256)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    lines(x, log(d.cauchy), col = "orange")
    p = psymstb(x, alpha = 1.01)
    p.cauchy = pcauchy(x)
    plot(x, p, type = "l", main = "PSYMSTB(1.01) - Close Cauchy")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    lines(x, p.cauchy, col = "orange")
    ###
        
    # RSYMSTB(1.99) - Symmetric Stable Distribution - Close Cauchy: 
    x = seq(-5, 5, length = 256)
    r = rsymstb(4096, alpha = 1.99)
    plot(r, type = "l", main = "RSYMSTB(1.99) Series")
    d = dsymstb(x, alpha = 1.99)
    d.norm = dnorm(x, sd = sqrt(2))
    plot(x, log(d), type = "l", main = "DSYMSTB(1.99) - Close Normal")
    density = density(r, from = -5, to = 5, n = 256)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    lines(x, log(d.norm), col = "orange")
    p = psymstb(x, alpha = 1.99)
    p.norm = pnorm(x, sd = sqrt(2))
    plot(x, p, type = "l", main = "PSYMSTB(1.99) - Close Normal")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    lines(x, p.norm, col = "orange")
    ###
    
    
# ------------------------------------------------------------------------------
    

### Example: Limit Behaviour of the Symmetric Stable Distribution

    # Settings:
    par(mfrow = c(3, 2), cex = 0.7, err = -1)
    ###
    
    # Histogram of Symmetric Stable Deviates with alpha=1.001
    # Comparison with Cauchy Density
    # Figure 1 - Histogram Close to Cauchy / alpha=1.001
    x = rsymstb(n = 5000, alpha = 1.001)
    hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
        ylim = c(0.0,0.5), main = "Close Cauchy")
    x = seq(from = -10, to = 10, by = 0.10)
    lines(x, dcauchy(x))
    ###
    
    # Histogram of Symmetric Stable Deviates with alpha=1.999
    # Comparison with Gaussian Density, Note: sd=sqrt(2)!
    # Figure 2 - Histogram Close to Gaussian / alpha=1.999
    x = rsymstb(n = 5000, alpha = 1.999)
    hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
        ylim=c(0.0,0.5), main = "Close Gaussian")
    x = seq(from = -10, to = 10, by = 0.10)
    lines(x, dnorm(x, sd = sqrt(2)))
    ###
    
    # Histogram of Symmetric Stable Deviates with alpha=0.500
    # Comparison with Stable Density "dstable"
    # Figure 3 - Symmetric Stable Density / alpha=0.500
    x = rsymstb(n = 5000, alpha = 0.500)
    hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
        ylim = c(0.0, 0.6), main = "alpha=0.500")
    x = seq(from = -10, to = 10, by = 0.05)
    lines(x, dsymstb(x, alpha = 0.500))
    ###
    
    # Histogram of Symmetric Stable Deviates with alpha=1.500
    # Comparison with Stable Density "dsymstb"
    # Figure 4 - Symmetric Stable Density / alpha=1.500
    x = rsymstb(n = 5000, alpha = 1.500)
    hist(x[abs(x)<10], probability = TRUE, nclass = 40, xlab = "x", 
        ylim = c(0.0, 0.6), main = "alpha=1.500")
    x = seq(from = -10, to = 10, by = 0.25)
    lines(x, dsymstb(x, alpha = 1.500))
    ### 
    
    # CDF of Symmetric Stable Deviates with alpha=1.001
    # Comparison with Cauchy Probability Function
    # Figure 5 - CDF Close to Cauchy / alpha=1.001
    n = 500
    x = sort(rsymstb(n, alpha = 1.001))
    y = (1:n)/n
    plot(x, y, xlim = c(-120, 120), main = "Close Cauchy")
    x = seq(from = -100, to = 100, by = 1)
    lines(x, pcauchy(x), col = 5)
    lines(x, psymstb(x, alpha = 1.001), col = "steelblue")
    ###
    
    # CDF of Symmetric Stable Deviates with alpha=1.999
    # Comparison with Gaussian Probability Function, Note: sd=sqrt(2)!
    # Figure 6 - CDF Close to Gaussian / alpha=1.999
    n = 500
    x = sort(rsymstb(n, alpha = 1.999))
    y = (1:n)/n
    plot(x, y, xlim = c(-10, 10), main = "Close Gaussian")
    x = seq(from = -10, to = 10, by = 0.25)
    lines(x, pnorm(x, sd = sqrt(2)), col = "steelblue")
    lines(x, psymstb(x, alpha=1.999), col = "steelblue")
    ###
    
    
# ------------------------------------------------------------------------------
 

### Example: 'symstb' Precision of the DF in the Tail:

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Show the precision of the DF in the Tail:
    # There remains somw further work to do ... 
    x = seq(-20, 0, 1)
    alpha = 1.9 # worst in the limit alpha -> 2.0
    plot(x, log(dsymstb(x, alpha)), type = "b", main = "dsymstb")
    plot(x, log(psymstb(x, alpha)), type = "b", main = "psymstb")
    x = seq(-12, -3, 0.25)
    plot(x, log(dsymstb(x, alpha)), type = "b", main = "dsymstb")
    plot(x, log(psymstb(x, alpha)), type = "b", main = "psymstb")
    ###
   
   
# ------------------------------------------------------------------------------


### Example: Plot General (Skew) Stable Distribution

    # Graph Frame:
    par(mfcol = c(3, 3), cex = 0.7)
    ### 
    
    # RSTABLE(1.1, 0.5) - Stable Distribution:    
    x = seq(from = -50, to = 50, length = 100) 
    r = rstable(4096, alpha = 0.4, beta = 0.5)
    plot(r, type = "l", main = "RSTABLE (0.4,0.5)")    
    d = dstable(x, alpha = 0.4, beta = 0.5)
    plot(x, log(d), type = "l", main = "DSTABLE (0.4,0.5)")
    density = density(r[abs(r)<10], n = 100)
    points(density$x, log(density$y), col = "steelblue")    
    p = pstable(x, alpha=0.4, beta = 0.5)
    plot(x, p, type = "l", ylim = c(0, 1), main = "PSTABLE (0.4,0.5)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue")
    ###
    
    # RSTABLE(1.1, 0.5) - Stable Distribution:   
    x = seq(from = -10, to = 10, length = 100) 
    r = rstable(4096, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
    plot(r, type = "l", main = "RSTABLE (1.1,0.5,1.5,-1.0)")    
    d = dstable(x, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
    plot(x, log(d), type = "l", main = "DSTABLE (1.1,0.5,1.5,-1.0)")
    density = density(r[abs(r)<10], n = 100)
    points(density$x, log(density$y), col = "steelblue")    
    p = pstable(x, alpha = 1.1, beta = 0.5, gamma = 1.5, delta = -1.0)
    plot(x, p, type = "l", ylim = c(0, 1), main = "PSTABLE (1.1,0.5,1.5,-1.0)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue")
    ###
        
    # RSTABLE(1.9, 0.5) - Stable Distribution: 
    x = seq(from = -5, to = 5, length = 100)   
    r = rstable(4096, alpha = 1.9, beta = 0.5)
    plot(r, type = "l", main = "RSTABLE (1.9,0.5)")    
    d = dstable(x, alpha = 1.9, beta = 0.5)
    plot(x, log(d), type = "l", main = "DSTABLE (1.9,0.5)")
    density = density(r, n = 200)
    points(density$x, log(density$y), col = "steelblue")    
    p = pstable(x, alpha = 1.9, beta = 0.5)
    plot(x, p, type = "l", main = "PSTABLE (1.9,0.5)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue")
    ###
  
  
# ------------------------------------------------------------------------------


### 1.3.6 Example: Generalized Hyperbolic Distribution

    # Graph Frame:
    par(mfcol = c(3, 2), cex = 0.7)
    ###
    
    # GH(1, 0, 1, 0, 1) - Generalized Hyperbolic Distribution:  
    x = seq(-5, 5, length = 256)
    r = rgh(5000, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
    plot(r, type = "l", main = "RGH(1, 0, 1, 0, 1) Series", col = "steelblue")
    grid()
    d = dgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
    plot(x, log(d), type = "l", main = "DGH(1, 0, 1, 0, 1)")
    density = density(r, from = -5, to = 5, n = 128)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    grid()
    p = pgh(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
    plot(x, p, type = "l", main = "PGH(1, 0, 1, 0, 1)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    grid()
    ###
    
    # GH(1, 0.3, 1, 1, 1) - Generalized Hyperbolic Distribution:    
    x = seq(-3, 7, length = 256)
    r = rgh(5000, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
    plot(r, type = "l", main = "RNIG(1, 0.3, 1, 1) Series", col = "steelblue")
    grid()
    d = dgh(x, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
    plot(x, log(d), type = "l", main = "DGH(1, 0.3, 1, 1)")
    density = density(r, from = -5, to = 5, n = 128)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    grid()
    p = pgh(x, alpha = 1, beta = 0.3, delta = 1, mu = 1, lambda = 1)
    plot(x, p, type = "l", main = "PGH(1, 0.3, 1, 1, 1)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    grid()
    ###
  
    
# ------------------------------------------------------------------------------


### Example: Hyperbolic Distribution:

    # Graph Frame:
    par(mfcol = c(3, 2), cex = 0.7, err = -1)
    ### 
    
    # HYP(1, 0, 1, 0) - Hyperbolic Distribution:    
    x = seq(-6, 6, length = 256)
    r = rhyp(4096, alpha = 1, beta = 0, delta = 1, mu = 0)
    plot(r, type = "l", main = "RHYP(1, 0, 1, 0) Series", col = "steelblue")
    mean(r); var(r) 
    d = dhyp(x, alpha = 1, beta = 0, delta = 1, mu = 0)
    plot(x, log(d), type = "l", main = "DHYP(1, 0, 1, 0)")
    density = density(r, from = -6, to = 6, n = 128)
    points(density$x, log(density$y), col = "steelblue")    
    p = phyp(x, alpha = 1, beta = 0, delta = 1, mu = 0)
    plot(x, p, type = "l", main = "PHYP(1, 0, 1, 0)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue")
    ### 
    
    # HYP(1, 0.3, 1, 0.0) - Hyperbolic Distribution:        
    x = seq(-7, 15, length = 256)   
    r = rhyp(4096, alpha = 1, beta = 0.3, delta = 2, mu = 1)
    plot(r, type = "l", main = "RHYP(1, 0.3, 2, 1) Series", col = "steelblue")
    mean(r); var(r)
    d = dhyp(x, alpha = 1, beta = 0.3, delta = 2, mu = 1)
    plot(x, log(d), type="l", main="DHYP(1, 0.3, 2, 1)")
    density = density(r, from = -7, to = 15, n = 128)
    points(density$x, log(density$y), col = "steelblue")
    p = phyp(x, alpha = 1, beta = 0.3, delta = 2, mu = 1)
    plot(x, p, ylim = c(0,1), type = "l", main = "PHYP(1, 0.3, 2, 1)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue")
    ###


# ------------------------------------------------------------------------------


### 1.3.7 Example: Normal Inverse Gaussian Distribution:
 
    # Graph Frame:
    par(mfcol = c(3, 2), cex = 0.7, err = -1)
    ###
    
    # NIG(1, 0, 1, 0) - Hyperbolic Distribution:    
    x = seq(-5, 5, length = 256)
    r = rnig(5000, alpha = 1, beta = 0, delta = 1, mu = 0)
    plot(r, type = "l", main = "RNIG(1, 0, 1, 0) Series", col = "steelblue")
    d = dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0)
    plot(x, log(d), type="l", main="DNIG(1, 0, 1, 0)")
    density = density(r, from = -5, to = 5, n = 128)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    grid()
    p = pnig(x, alpha=1, beta=0, delta=1, mu=0)
    plot(x, p, type = "l", main = "PNIG(1, 0, 1, 0)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    grid()
    ###
    
    # NIG(1, 0.3, 1, 1) - Hyperbolic Distribution:  
    x = seq(-5, 5, length = 256)
    r = rnig(5000, alpha = 1, beta = 0.3, delta = 1, mu = 1)
    plot(r, type = "l", main = "RNIG(1, 0.3, 1, 1) Series", col = "steelblue")
    d = dnig(x, alpha = 1, beta = 0.3, delta = 1, mu = 1)
    plot(x, log(d), type = "l", main = "DNIG(1, 0.3, 1, 1)")
    density = density(r, from = -5, to = 5, n = 128)
    points(density$x, log(density$y), col = "steelblue", cex = 0.7)
    grid()
    p = pnig(x, alpha = 1, beta = 0.3, delta = 1, mu = 1)
    plot(x, p, type = "l", main = "PNIG(1, 0.3, 1, 1)")
    points(sort(r), (1:length(r))/length(r), col = "steelblue", cex = 0.7)
    grid()
    ###

    
# ------------------------------------------------------------------------------


### Code Snippet: 'nigFit' Function
    
    # Function:
    .nigFit = function(x)
    {
        # Log-likelihood Function:
        LLH = function(x, y = x) {
            # Take care of parameter restrictions:
            alpha = exp(-x[1]); beta = alpha * tanh(x[2])
            if (abs(beta) == alpha) return(Inf)
            delta = exp(-x[3]); mu = x[4]
            # Compute Likelihood:
            llh = -sum(log(dnig(y, alpha, beta, delta, mu)))
            # Print Iteration Path:
            cat("\n LLH: ", -llh)
            cat(" Parameters: ", alpha, beta, delta, mu, "\n")
            llh
        }

        # Start Settings:
        alpha = 1; beta = 0; delta = 1; mu = 0
        # Variable Transformation and Minimization:
        fit = nlm(f = LLH, p = c(-log(alpha), atanh(beta/alpha),
            -log(delta), mu), y = x)
        # Return Value:
        invisible(fit)
    }
    ###

    # Try:
    set.seed(4711)
    x = rnig(5000, alpha = 10.00, beta = -1.0, delta = 2.00, mu = 1.00)
    .nigFit(x)
    # Final Estimate:
    #              alpha = 8.175  beta = -1.3  delta = 1.66  mu = 1.07
    ###
    
    
# ------------------------------------------------------------------------------


### 1.3.8  Example: Fit NYSE Index and USDCHF Rates

    ### NOTE - CHECK gh
    
    # Load Data:
    data(nyse)
    x = as.vector(returnSeries(as.timeSeries(nyse)))
    # Take Care of Change of Index Definition and Plot:
    x = x[abs(x) < 0.5]
    ###

    # Estimate Parameters:
    hyp = hypFit(x)
    hyp.est = hyp@fit$estimate
    nig = nigFit(x)
    nig.est = nig@fit$estimate
    gh = ghFit(x)
    gh.est = gh@fit$estimate
    LLH = c(hyp = hyp@fit$minimum,
        nig = nig@fit$minimum, gh = gh@fit$minimum)
    ###
       
    # Print Parameters and LHH and compare them ...
    ###

    # Histogram Plot:
    hist(x, probability = TRUE, n = 200, col = "steelblue",
        border = "white", xlim = c(-0.04, 0.04), ylim =
        c(0, 70), main = "NYSE Composite Index")
    s = seq(-0.04, 0.04, length = 301)
    hyp.y = dhyp(s, hyp.est[1], hyp.est[2], hyp.est[3], hyp.est[4])
    lines(s, hyp.y, col = "red", lwd = 2)
    nig.y = dnig(s, nig.est[1], nig.est[2], nig.est[3], nig.est[4])
    lines(s, nig.y, col = "green", lwd = 2)
    gh.y = dgh(s, gh.est[1], gh.est[2], gh.est[3], gh.est[4],
        gh.est[5])
    lines(s, gh.y, col = "orange", lwd = 2)
    norm.y = dnorm(s, mean(x), sd(x))
    lines(s, norm.y, lty = 3)
    text(x = rep(0.015, 4), y = c(40, 58, 61, 65),
        c("norm", "gh", "nig", "hyp"))
    ###
    
    # Kernel Density Estimate - Log Plot:
    kde = density(x)
    plot(kde$x, log(kde$y), type = "l", lwd = 2,
        xlim = c(-0.04, 0.04), ylim = c(-5, 5),
        main = "NYSE Composite Index")
    lines(s, log(hyp.y), col = "red")
    lines(s, log(nig.y), col = "green")
    lines(s, log(gh.y), col = "orange")
    lines(s, log(norm.y), lty = 3)
    text(x = rep(0.04, 4), y = c(-3.5, -2.9, -1.4, -1.0),
        c("norm", "hyp", "nig", "gh"))
    grid()
    ###
    

# ------------------------------------------------------------------------------    


### Example: MLE Fit to Hyperbolic Density
    
    # Fit the Parameters and Plot:
    # Data: NYSE Composite Index Returns
    x = as.vector(returnSeries(as.timeSeries(nyse)))
    # Standardize time series
    s = (x-mean(x))/sqrt(var(x)) 
    ###
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # Fit the Parameters and Plot:
    fit = hypFit(x = s, alpha = 1, beta = 0, delta = 1, mu = 0, 
        doplot = TRUE, width = 1)
    fit
    ### 
    
    # Fit the Parameters and Plot:
    # Data: Simulated Random Variates HYP(1,0,1,0)
    s = rhyp(length(x), 1, 0, 1, 0) 
    # Note, this may take some time:
    fit = hypFit(s, alpha = 1, beta = 0, delta = 1, mu = 0, 
        doplot = TRUE)
    fit 
    ###
    

# ------------------------------------------------------------------------------    


### Example: MLE Fit to Normal Inverse Gaussian Density
    
    # Fit the Parameters and Plot:
    # Data: NYSE Composite Index Returns
    x = as.vector(returnSeries(as.timeSeries(nyse)))
    # Standardize time series
    s = (x-mean(x))/sqrt(var(x))
    ###
    
    
    # Note, this may take some time:
    fit = nigFit(x = s, alpha = 1, beta = 0, delta = 1, mu = 0, 
        doplot = TRUE)
    fit
    ###
    
    # Fit the Parameters and Plot:
    # Data: Simulated Random Variates NIG(1,0,1,0) \n")
    s = rnig(length(x), 1, 0, 1, 0) 
    # Note, this may take some time:
    fit = nigFit(s, alpha = 1, beta = 0, delta = 1, mu = 0, 
        doplot = TRUE)
    fit
    ###

        
################################################################################

