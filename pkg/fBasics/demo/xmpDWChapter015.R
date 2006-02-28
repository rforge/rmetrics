#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 1.5
#   Probability Theory and Hypothesis Tests
#
# List of Examples, Exercises and Code Snippets:
#
#       * Example: Compute Summary of Random Vector x   
#       * Example: Basic Statistics of a Vector x       
#       * Example: Generate Portable Random Numbers  
#       * Code Snippet: runif.lcg 
#       * Code Snippet: fHTEST
#   1.5.1 Example: The Normal Limit of Student-t Variates 
#   1.5.2 Example: Student-t Variates under Aggregation
#   1.5.3 Example:
#   1.5.4 Example: 
#       * Example: Jarque Bera Test
#       * Example: Correlation Tests
#       * Example: Goodness-Of-Fit Tests for Testing Normality
#       * Example: Aggregation of NYSE Residuals to the Gaussian Limit
#       * Example: Aggregation of Student's-t rvs to the Gaussian Limit
#       * Example: Compare Subsamples of Time Series by the KS Test
#       * Example: Runs Test 
#       * Example: Bootstrapped Mean
#
# Author:
#   (C) 1997-2004, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Example: Compute Summary of a Random Vector x

	# Generate random vector an compute summary
	set.seed(1953)
    x = rnorm(1000)
    summary(x)
    #            Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
    # -4.0310000 -0.6681000 -0.0002383 -0.0023840  0.6741000  3.9720000
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Basic Statistics of a Vector x
    
	# Generate random vector and compute basic statistics
    set.seed(1953)
    x = rnorm(100)
    basicStats(x)
    #                    Value
	# nobs        100.00000000
	# NAs           0.00000000
	# Minimum      -2.54920919
	# Maximum       2.50495921
	# 1. Quartile  -0.55454175
	# 3. Quartile   0.49634607
	# Mean         -0.08870588
	# Median        0.15574755
	# Sum          -8.87058793
	# SE Mean       0.09622708
	# LCL Mean     -0.27964129
	# UCL Mean      0.10222953
	# Variance      0.92596517
	# Stdev         0.96227084
	# Skewness     -0.33775532
	# Kurtosis      0.02421936
	###

# ------------------------------------------------------------------------------
   

### Example: Generate Portable Random Numbers
    
	# Set the Seed in R environment:
    set.lcgseed(seed = 65890)
    ###
    
    # Generate Uniform, Normal and Student-t rvs:
    cbind(runif.lcg(10), rnorm.lcg(10), rt.lcg(10, df = 4))
    #            [,1]       [,2]        [,3]
    #  [1,] 0.4125252  0.1343111  2.04846215
    #  [2,] 0.4008696  0.6894022 -0.51108298
    # ...
    ###

    # Return the Seed:
    get.lcgseed()
    # [1] 1743389204
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snippet: 'runif.lcg' Function
    
	# Function:
    .runif.lcg = function(n)
    {
        # Generate:
        if(!exists("lcg.seed")) lcg.seed <<- 4711
        r.lcg = numeric()
        a = 13445; c = 0; m = 2^31-1
        for (i in 1:n) {
            lcg.seed <<- (a*lcg.seed + c) %% m
            r.lcg[i] = lcg.seed/m
        }
        r.lcg
    }
    ###

    # Try:
    set.lcgseed(seed = 65890)
    .runif.lcg(100)
    ###
    
    # R:
    #   [1] 0.412525167 0.400869636 0.692256716 0.391545769 0.332864333
    #   [6] 0.360958518 0.087276699 0.435218890 0.517975170 0.176166115
    #  ...
	#  [91] 0.034661501 0.023883587 0.114830256 0.892798551 0.676522341
    #  [96] 0.842873946 0.440200731 0.498826918 0.727911956 0.776254895
	###
	
	# SPlus 2000:
    #   [1] 0.412525167 0.400869636 0.692256716 0.391545769 0.332864333
    #   [6] 0.360958518 0.087276699 0.435218890 0.517975170 0.176166115
    #   ...
    #  [91] 0.034661501 0.023883587 0.114830256 0.892798551 0.676522341
    #  [96] 0.842873946 0.440200731 0.498826918 0.727911956 0.776254895
    ###

    
    
# ------------------------------------------------------------------------------
 

### Code Snippet: 'fHTEST' Class Representation

    # Class Representation:
    # setClass("fHTEST",
    #   representation(
    #     call = "call",
    #     data = "list",
    #     test = "list",
    #     title = "character",
    #     description = "character") 
    #   )
    ###
    
    # Get Class:
    getClass("fHTEST")
    ###
    
    
# ------------------------------------------------------------------------------


### 1.5.1 Example: The Normal Limit of Student-t Variates  
 
    # Set Graphics Frame:   
    par(mfrow = c(2,2), cex = 0.7)
    ###
    
    # Tests:
    Tests = c("ks", "ad", "sw", "jb")
    Titles = c("Kolmogorov-Smirnov", "Anderson-Darling",
        "Shapiro-Wilks", "Jarque-Bera")
    # Number of Samples and Sample Length:
    N = 100
    L = 1000
    # Max Number of Degrees of Freedom:
    DF = 20
    ###
    
    # Tests - Create Figure 1.5.1:
    for ( i in 1:length(Tests) ) {
        plot(c(2, DF), c(0, 1), type = "n", xlab = "df",
            ylab = "p Value            Rejectance Rate")
        grid()
        title(main = paste(Titles[i], "Test"))
        text(17.7, 0.95, paste("L =", as.character(L)))
        text(17.7, 0.87, paste("N =", as.character(N)))
        abline(h = 0.05, col = "blue", lty = 3)
        abline(h = 0.50, col = "red", lty = 3)
        for ( nu in 2:DF ) {
            PVAL = NULL
            COUNT = 0
            set.seed(1128)
            for (j in 1:N) {
                x = rt(n = L, df = nu)
                p.val = normalTest(x, method = Tests[i])@test$p.value[1]
                PVAL = c(PVAL, p.val)
                if (p.val <= 0.05) COUNT = COUNT+1
            } 
            mean = mean(PVAL)
            count = COUNT/100
            points(nu, mean, col = "blue")
            points(nu, count, pch = 19, col = "red")
            print(round(c(nu, mean, count), digits = 4))
        }
    }
    ###
    
    
# ------------------------------------------------------------------------------


### 1.5.2 Example: Student-t Variates under Aggregation  
    
    # Set Graphics Frame:   
    par(mfrow = c(2,2), cex = 0.7)
    ###
    
    # Tests:
    Tests = c("ad", "jb")
    Titles = c("Anderson-Darling", "Jarque-Bera")
    # Number of Samples and Sample Length:
    N = 100
    # Max Number of Degrees of Freedom:
    L = 32768
    A = 11
    ###
    
    # Tests - Create Figure 1.5.2:
    for ( i in 1:length(Tests) ) {
        plot(c(0, A-1), c(0, 1), type = "n", xlab = "Aggregation Level",
            ylab = "p Value            Rejectance Rate")
        grid()
        title(main = paste(Titles[i], "Test"))
        abline(h = 0.05, col = "blue", lty = 3)
        abline(h = 0.50, col = "red", lty = 3)
        for ( j in 1:A ) {
            PVAL = NULL
            COUNT = 0
            set.seed(4711)
            for (k in 1:N) {
                X = matrix( rt(n = L, df = 5), byrow = TRUE, ncol = 2^(j-1) )
                x = apply(X, 1, sum)
                p.val = normalTest(x, method = Tests[i])@test$p.value[1]
                PVAL = c(PVAL, p.val)
                if (p.val <= 0.05) COUNT = COUNT + 1
            } 
            mean = mean(PVAL)
            count = COUNT/100
            points(j-1, mean, col = "blue")
            points(j-1, count, pch = 19, col = "red")
            print(round(c(j-1, mean, count), digits = 4))
        }
    }
    ###
    
    
# ------------------------------------------------------------------------------


### 1.5.3 Example: 

    # Load the Series:
    NYSE.RET = outlierSeries(returnSeries(as.timeSeries(data(nyse))))
    nyse.ret = as.vector(NYSE.RET) 
    L = length(nyse.ret)
    ###
    
    # Perform the Tests:
    ans = NULL
    levelNames = c("2Days", "Week", "2Weeks", "Month", "2Months", 
        "Quarter", "HalfYear")
    for ( i in c(2, 5, 10, 21, 42, 63, 125) ) {
        len = floor(L/i)
        X = matrix(nyse.ret[1:(i*len)], byrow = TRUE, ncol = i)
        x = apply(X, 1, sum)
        p.cm = format(cvmTest(x)@test$p.value[[1]], digits = 3)
        p.ad = format(adTest(x)@test$p.value[[1]], digits = 3)
        p.sw = format(shapiroTest(x)@test$p.value[[1]], digits = 3)
        p.da = format(dagoTest(x)@test$p.value[[1]], digits = 3)
        p.jb = format(jarqueberaTest(x)@test$p.value[[1]], digits = 3)
        ans = rbind( ans, c(len, round(kurtosis(x), 3), 
            round(skewness(x), 3), p.cm, p.ad, p.sw, p.da, p.jb) )  
    }
    ###
    
    # Print the Results:
    ans = data.frame(ans)
    colnames(ans) = c("Length", "Kurtosis", "Skewness", 
        "p CvM", "p AD", "p SW", "p dAgo", "p JB")
    rownames(ans) = levelNames
    print(ans)
    ###
      
#          Length Kurtosis Skewness    p CvM     p AD     p SW   p dAgo     p JB
# 2Days      4906   30.655   -1.403       NA 3.05e-84 6.56e-45        0        0
# Week       1962   15.437   -1.178 4.43e-10 4.23e-22 1.12e-27        0        0
# 2Weeks      981   10.331   -1.125 8.54e-09 1.41e-12 2.33e-19        0        0
# Month       467    6.119   -1.192 2.85e-07 1.96e-09 4.56e-13        0        0
# 2Months     233    3.488   -1.070 0.000372 1.13e-05 2.29e-08 1.93e-13        0
# Quarter     155    2.352   -0.768  0.00183  0.00121 7.69e-05 1.56e-06 2.51e-12
# HalfYear     78    0.052   -0.190     0.15    0.249     0.51    0.629     0.76

    
# ------------------------------------------------------------------------------


### 1.5.4 Example: 
    
    # Load the Series:
    USDCHF.RET = returnSeries(as.timeSeries(data(usdchf)))
    usdchf.ret = as.vector(USDCHF.RET) 
    L = length(usdchf.ret)
    ###
    
    # Perform the Tests:
    ans = NULL
    levelNames = c("Hour", "4Hours", "BusinessDay", "Day", "2Days", "Week")
    for ( i in c(2, 8, 16, 48, 96, 240) ) {
        len = floor(L/i)
        X = matrix(usdchf.ret[1:(i*len)], byrow = TRUE, ncol = i)
        x = apply(X, 1, sum)
        p.cm = format(cvmTest(x)@test$p.value[[1]], digits = 3)
        p.ad = format(adTest(x)@test$p.value[[1]], digits = 3)
        p.sw = format(shapiroTest(x)@test$p.value[[1]], digits = 3)
        p.da = format(dagoTest(x)@test$p.value[[1]], digits = 3)
        p.jb = format(jarqueberaTest(x)@test$p.value[[1]], digits = 3)
        ans = rbind( ans, c(len, round(kurtosis(x), 3), 
            round(skewness(x), 3), p.cm, p.ad, p.sw, p.da, p.jb) )  
    }
    ###
    
    # Print the Results:
    ans = data.frame(ans)
    colnames(ans) = c("Length", "Kurtosis", "Skewness", 
        "p CvM", "p AD", "p SW", "p dAgo", "p JB")
    rownames(ans) = levelNames
    print(ans)
	###
    
#         Length Kurtosis Skewness    p CvM      p AD     p SW   p dAgo     p JB
# Hour     31247    9.622   -0.138       NA        NA       NA        0        0
# 4Hours    7811    5.549   -0.204       NA 1.82e-171       NA        0        0
# Bus..Day  3905    3.006   -0.164       NA  1.53e-73 5.12e-31        0        0
# Day       1301    1.359   -0.337 1.05e-07  1.07e-09  9.3e-10 1.15e-14        0
# 2Days      650    0.442   -0.304  0.00235   0.00107  0.00140 0.000769 0.000405
# Week       260    0.186   -0.355    0.110    0.0604   0.0646   0.0433   0.0494
        

# ------------------------------------------------------------------------------


### Example: Jarque Bera Test 

	#	Precise finite sample quantiles of the Jarque-Bera adjusted
	#	Lagrange multiplier test
	#
	# Description:
	#	This is a collection of functions used to produce tables and
	#	fugures presented in the paper "Precise finite sample quantiles 
	#	of the Jarque-Bera adjusted Lagrange multiplier test" written
	#	by Diethelm Wuertz and Helmut G. Katzgraber (2005).
	#
	# Details:
	#   1 Table 1 for the JB and adjusted JB Lagrange multiplier test
	#   2 Figure 1 showing finite sample probability vs. statistic values 
	#   3 Figure 2 showing 1/N Expansion in comparison with Lawford's results
	#	4 Figure 3 Response Surface	
	#   5 Jarque Bera Test Example
	###
	
	#   Load Data:
    require(fBasics)
    ###	
		
	# 1 
	# Create Table 1 for the JB and adjusted JB Lagrange multiplier test
	# p Values for Excel Table:
	P = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 15, 20, 30, 40)
	P = c(P, 50, 100-rev(P))/100
	# LM Data:
	jbLM = jbTable("LM")
	p = as.numeric(rownames(jbLM))
	N = as.integer(colnames(jbLM))	
	# Select for Excel Table:
	select = NULL
	for ( i in 1:length(P) ) select = c(select, which(abs(p-P[i]) < 1e-15))
	p[select]
	ans = round(jbLM[rev(select), -c(1:3, 16) ], digits = 4)
	Names = as.character(P+0.00001)
	rownames(ans) = paste(substr(Names, 3, 4), ".", 
		substr(Names, 5, 6), "%", sep = "")
	ans	
	# ALM Data:
	jbALM = jbTable("ALM")
	p = as.numeric(rownames(jbALM))
	N = as.integer(colnames(jbALM))
	# Select for Excel Table:
	select = NULL
	for ( i in 1:length(P) ) select = c(select, which(abs(p-P[i]) < 1e-15))
	p[select]
	ans = round(jbALM[rev(select), -c(1:3, 16) ], digits = 4)
	Names = as.character(P+0.00001)
	rownames(ans) = paste(substr(Names, 3, 4), ".", 
		substr(Names, 5, 6), "%", sep = "")
	ans
	###
	
	# 2 
	# Create Figure 1 showing finite sample probability vs. statistic values 
	# Graph Settings:
	par(mfrow = c(2, 2), cex = 0.7)
	xmax = 20	
	# Plot LM - Start with N = 10:
	jbLM = jbTable("LM")[, -(1:3)]
	p = as.numeric(rownames(jbLM))
	N = as.integer(colnames(jbLM))
	L = length(N)
	X = as.matrix(jbLM)
	plot(rev(X[, L]), p, type = "n", xlim = c(0, xmax),
		xlab = "LM Statistic", 
		ylab = "Asymptotic Difference                p Value             ")
	for (i in 2:L) lines(X[, i], p)
	lines(qchisq(p, 2), p, lwd = 2)	
	for (i in 2:L) lines(x = qchisq(p, 2), y = -(pchisq(X[, i], 2) - p))	
	abline(v = qchisq(0.99, 2), lty = 3)
	abline(v = qchisq(0.95, 2), lty = 3)
	abline(v = qchisq(0.90, 2), lty = 3)
	abline(h = 0)
	title(main = "Jarque Bera LM Statistic")	
	# Plot Adjusted LM - Start with N = 10::
	jbALM = jbTable("ALM")[, -(1:3)]
	p = as.numeric(rownames(jbALM))
	N = as.integer(colnames(jbALM))
	L = length(N)
	Y = as.matrix(jbALM)
	plot(rev(Y[, L]), p, type = "n", , xlim = c(0, xmax),
		xlab = "ALM Statistic", 
		ylab = "Asymptotic Difference                p Value             ")
	for (i in 2:L) lines(Y[, i], p)
	lines(qchisq(p, 2), p, lwd = 2)	
	for (i in 2:L) lines(x = qchisq(p, 2), y = -(pchisq(Y[, i], 2) - p))	
	abline(v = qchisq(0.99, 2), lty = 3)
	abline(v = qchisq(0.95, 2), lty = 3)
	abline(v = qchisq(0.90, 2), lty = 3)
	abline(h = 0)
	title(main = "Jarque Bera ALM Statistic")
	###
	
	# 3 
	# Create Figure 2 showing 1/N Expansion in comparison with Lawford's results
	# Write Function:
	jbLawford = function(n) 
	{   
		# Computes the quantile response surface for alpha = 90% 
	    # 	(10%) and 95% (5%) as parameterized by Steve Lawford.	   
	    # 90% Confidence Level:
	    qinf90 = 4.605049
	    beta90 = c(-145.1860602, 7276.233799, -275153.9753, 6437304.253, 
	        -92456006.82, 814503598.1, -4276230401, 12243649840, -14677406860)	        
	    # 95% Confidence Level:
	    qinf95 = 5.9913104
	    beta95 = c(-67.00449919, 1719.108744, -74443.10488, 1962801.944, 
	        -30095541.45, 275285058.6, -1479198621, 4299485882, -5206421393)	        
	    # Compute:
	    ans90 = ans95 = NULL
	    for (N in n) {
	        sampleSize = N^(-(1:9))
	        ans90 = c(ans90, qinf90 + sum(beta90*sampleSize))
	        ans95 = c(ans95, qinf95 + sum(beta95*sampleSize)) }	    
	    # Return Value:
	    ans = t(cbind(ans95, ans90))
	    colnames(ans) = as.character(n)
	    rownames(ans) = c("L-05", "L-10")
	    ans
	}
	# Write Function:
	statisticsPlot = function(X, degree = 6, col = "steelblue4")
	{
		# Settings:
		p = as.numeric(rownames(X))
		N = as.integer(colnames(X))
		X = as.matrix(X)
		rownames(X) = as.character(as.integer(p*10^7))
		# Indexes and Quantiles:
		selectedIndexes = sort(c(10^(3:5), 5*10^(3:5), (1:5)*10^6))
		selectedIndexes = c(selectedIndexes, 10^7-rev(selectedIndexes)[-1])
		selectedIndexes = as.character(as.integer(selectedIndexes))
		X = X[selectedIndexes, ]		
		# Plot:
		for ( k in selectedIndexes ) {
			i = as.integer(k)
			statistic = X[k, ]
			if (k == selectedIndexes[1]) 
				plot(log10(N), log10(statistic), type = "n", 
				xlab = "log10 N", ylab = "log10 Statistic", 
				xlim = c(0.5, 4.5), ylim = c(-4, 2) )
			points(log10(N), log10(statistic), pch = 19, col = col)	
			# Fitting:
			x = rep(1, times = length(statistic))
			for ( j in 1:degree ) x = cbind(x, log10(N)^j)
			coef = lm.fit(x, y = log10(statistic))$coef
			print(coef, 4)			
			# Add LM Fit:
			n = 1:2800
			s = rep(0, times = length(n))
			for ( l in n )
				for ( j in 0:degree ) 
					s[l] = s[l] + coef[j+1]*log10(l)^j
			lines(log10(n[n>4]), s[n>4], col = col, lty = 3) }		
		# Add Level Legend:
		levels = paste(as.character(as.numeric(selectedIndexes)/10^5), 
			"%", sep = "")
		y = 2.13 - 0.28*(1:length(levels))
		for ( i in 1:length(levels) ) text(x = 4.3, y = y[i], levels[i]) 		
		# Return Value:
		invisible()
	}
	# Figure 2 - Graph Settings:
	par(mfrow = c(2, 2), cex = 0.7)
	p = sort(c(10^(3:5), 5*10^(3:5), (1:5)*10^6))
	p = c(p, 10^7-rev(p)[-1])/10^7	
	# Plot LM Data - Neglect N = 6, 8, and 10'000
	X = jbTable("LM")[, -c(2, 3, 19)] 
	statisticsPlot(X = X, degree = 6, col = 1)
	title(main = "Jarque Bera LM Test")
	# Add Lawford's Series Expansion:	
	n = 1:2400
	lines(log10(n[n > 4]), log10((jbLawford(n)[2,])[n > 4]), lwd = 2) 
    lines(log10(n[n > 4]), log10((jbLawford(n)[1,])[n > 4]), lwd = 2)   
    # Add Asymptotic Values:
	points(rep(3.7, length(p)), log10(qchisq(1-p, 2)), cex = 0.7)	
    # Plot ALM Data - neglect N = 6, 8, and 10'000
	X = jbTable("ALM")[, -c(2, 3, 19)]
	# Plot:
	statisticsPlot(X = X, degree = 6, col = 1)	
	# Add Title:
	title(main = "Jarque Bera ALM Test")
	# Add Asymptotic Value:
	points(rep(3.7, length(p)), log10(qchisq(1-p, 2)), cex = 0.7)
	###   
	
	# 4 
	# Create Figure 3 Response Surface
	# The small table plot, fast ...
	par(mfrow = c(1, 1), cex = 0.5)
	# LM:
	pPlot(X = jbTable(type = "LM", size = "small"), nN = 100, nStat = 100, 
		logN = TRUE, logStat = TRUE, fill = TRUE, linear = FALSE, digits = 8, 
		doplot = TRUE) 
	# ALM:
	pPlot(X = jbTable(type = "ALM", size = "small"), nN = 100, nStat = 100, 
		logN = TRUE, logStat = TRUE, fill = TRUE, linear = FALSE, digits = 8, 
		doplot = TRUE) 
	# If you have some more time, run the "all" data sized version ...
	###

	# 5 
	# Test Example
	# Normal Distribution:
	set.seed(1953)
	x = rnorm(125)
	jbTest(x)
	# Student-t Distribution:
	set.seed(1953)
	x = rt(125, 3)
	jbTest(x)
	###
	
	
# ------------------------------------------------------------------------------


###	Example: Correlation Tests
 
	#	This example investigates correlations between yesterdays and
	#  	todays volatilities of the NYSE stock market index. It compares 
	#	the results with those obtained from a resampled time series.

	# Investigate Correlations in Time Series
	# Data: NYSE Composite Index log Returns
	data(nyseres)
	###
	
	# Correlation Tests:
	x = nyseres[6000:7000, 1]
	# Standardize:
	x = (x-mean(x))/sqrt(var(x)) 
	lag = 1
	print(cor.test(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative = "two.sided", method = "spearman"))
	print(cor.test(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative = "two.sided", method = "kendall"))
	###
	
	# Compare with Resampled Series:	
	# Resampled Empirical Time Series
	x = sample(x)
	print(cor.test(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative = "two.sided", method = "spearman"))
	print(cor.test(x[1:(length(x)-lag)], x[(1+lag):length(x)], 
		alternative = "two.sided", method = "kendall"))
	###
	

# ------------------------------------------------------------------------------

		 
###	Example: Goodness-Of-Fit Tests for Testing Normality

	#	Test random deviates if they ar normally
	#  	distributed. This testsuite includes the foloowing 
	#  	test procedures:
	#	  01  Omnibus Moments Test for Normality
	#	  02  Geary's Test of Normality
	#	  03  Studentized Range for Testing Normality
	#	  04  D'Agostino's D-Statistic Test of Normality
	#	  05  Kuiper V-Statistic Modified to Test Normality
	#	  06  Watson U^2-Statistic Modified to Test Normality
	#	  07  Durbin's Exact Test (Normal Distribution
	#	  08  Anderson-Darling Statistic Modified to Test Normality
	#	  09  Cramer-Von Mises W^2-Statistic to Test Normality
	#	  10  Kolmogorov-Smirnov D-Statistic to Test Normality 
	#	  11  Kolmogorov-Smirnov D-Statistic (Lilliefors Critical Values)
	#	  12  Chi-Square Test of Normality (Equal Probability Classes)
	#	  13  Shapiro-Francia W-Test of Normality for Large Samples
	###
	
	# Goodness-Of-Fit Suite for Testing Normality
	data(nyseres)
	###
	
	# GoF Testing financial market log-returns:
	x = nyseres[4001:5000, 1]
	x = (x-mean(x))/sqrt(var(x))
  	r = gofnorm(x, doprint = TRUE)
	###

	# GoF Testing normal innovations:
  	x = rnorm(length(x))
  	r = gofnorm(x, doprint = TRUE)
  	###
  	
	# GoF Testing t-distributed innovations:
	x = rt(length(x), df = 2)
  	r = gofnorm(x, doprint = TRUE)
	###
	
 
# ------------------------------------------------------------------------------

 	
###	Example: Aggregation of NYSE Residuals to the Gaussian Limit
	
	#  	This example investigated by calculation of the value of the kurtosis 
	#  	and the KS statistic together with its p.value how the distribution 
	#  	functions of the NYSE residuals converge to a normal distribution 
	#  	by aggregating the original times series in powers of two.


	# Investigate the Convergence to a Gaussian by Aggregation
	# Data: NYSE Composite Index log Returns
	data(nyseres)
	###
	
	# Aggregate in powers of two:
	x = nyseres[, 1]
	AggregationLevels = 8
	x = x[1:2^13]
	###
	
	# Further Settings:
	x.length = x.kurtosis = x.statistic = x.pvalue = rep(NA, times = 8)	
	statistic.gof = function(x, ...) ksgof.test(x, ...)$statistic	
	pvalue.gof = function(x, ...) ksgof.test(x, ...)$p.value
	###
		
	# Investigate for all data subsets:		
	for ( i in 1:AggregationLevels){ 
		cat("  ",i)
	  	ncol = 2^(i-1)
		x.length[i] = length(x)/ncol
		if(i == 1) {
			x.aggregated = x
		} else {
			x.aggregated = apply(matrix(x, byrow = TRUE, ncol = ncol), 
				MARGIN = 1, FUN = sum)
		}
		# Remove Ties:
		x.aggregated = unique(x.aggregated)
		x.kurtosis[i] = kurtosis(x.aggregated)
		x.mean = mean(x.aggregated)
		x.sdev = sqrt(var(x.aggregated))
		ksgof = ks.test(x = x.aggregated, y = "pnorm", x.mean, 
			x.sdev, alternative = "two.sided")
		x.statistic[i] = ksgof$statistic
		x.pvalue[i] = ksgof$p.value 
	}
	###
	
	# Output as data.frame:
	cbind.data.frame(x.length, x.kurtosis, x.statistic, x.pvalue)
	###
	
		
# ------------------------------------------------------------------------------


###	Example: Aggregation of Student's-t rvs to the Gaussian Limit

	# This example investigates by the Kolmogrov-Smirnov Test
	# how random variables from Student's t distribution approach 
	# the Gaussian distribution with increasing degree of freedom df.
	###
	
	# How approach t-distributed random variables the Gaussian limit?"
	# Data: Sequence of 1000 t-distributed Random Variates
  	options(warn = -1)
  	par(mfrow = c(3, 2), cex = 0.7)
  	###

	# Execute Tests:
	x = seq(-4, 4, length = 1000)
   	DegreesOfFreedom = c(2, 4, 8, 16, 32, 64)
	statistic = p.value = c(0, 0, 0, 0, 0, 0)
  	for (i in 1:length(DegreesOfFreedom)) { 
	 	plot(dnorm(x), type = "l")
    		lines(dt(x, df = DegreesOfFreedom[i]), col = 6)
    		result = ks.test(x = rt(10000, df = DegreesOfFreedom[i]), 
    			y = "pnorm", 0, 1, alternative = "two.sided")
		statistic[i] = result$statistic
		p.value[i] = result$p.value}	
	cbind.data.frame(DegreesOfFreedom, statistic, p.value)
	###
	

# ------------------------------------------------------------------------------


###	Example: Compare Subsamples of Time Series by the KS Test

	# This example cuts the NYSE log-Return time series into 4 parts and 
	# compare the distributons of the individual time series by applying 
	# the two-sample Kolmogoroff-Smirnov Test.
	###
	
  	# Cut a log-Return Series into Parts and Compare the Distributions
	# Data: NYSE Composite Index log Returns
	options(digits = 5)
	data(nyseres)
	###

	# Data Manipulations:
	x = unique(nyseres[, 1])
	x = matrix(x[1:(4*trunc(length(x)/4))], ncol = 4)
	x.resampled = matrix(sample(x)[1:(4*trunc(length(x)/4))], 
		ncol = 4) # resampled
	NumberOfSubsets = 4
	###
	
	# Write a "compare" function:
	compare = function(x, n) 
	{  
		k = 0
		i = j = statistic = p.value = rep(NA,time=n*(n-1)/2)
  		for ( ii in 1:(n-1) ) {  
    		for ( jj in (ii+1):n ) { 
			k = k + 1 
        		ksgof = ks.test(x=x[,ii], y=x[,jj])
        		i[k] = ii
			j[k] = jj
			statistic[k] = ksgof$statistic
			p.value[k] = ksgof$p.value }}
   		cbind.data.frame(i, j, statistic, p.value)
	}
	###
   
	# Compare subsets of the NYSE time series:
	compare(x = x, NumberOfSubsets)
	###
	
	# Compare subsets of the resampled NYSE time series:
  	compare(x = x.resampled, NumberOfSubsets)
  	###
  	
  
# ------------------------------------------------------------------------------


###	Example: Runs Test 

	# This example shows how to perform a Runs Test using Traplettis
	# function implemented from his 'tseries' R-package.
	###

	# Perform a Runs Test
	# Data: NYSE Composite Index log Returns
	data(nyseres)
	###
	
	# Runs Test:
	runsTest(nyseres[, 1])
	###
	
	# Re-sampled NYSE log-Returns:
	runsTest(sample(nyseres[, 1]))
	###

	#	This example shows how to perform an extended Runs Test using 
	#	Fillibens runs test procedure.
	data(nyseres)
	###
	
	# Runs Test:
	runsTest(nyseres[, 1])
	###
	
	
# ------------------------------------------------------------------------------


### Example: Bootstrapped Mean

   	# Write Function:
   	bootMean =
	function(x, B = 1000, ci = 0.95, na.rm = TRUE, reps = FALSE)
	{    # A function implemented by Diethelm Wuertz
		# Description:
	    #   A very fast implementation of the basic nonparametric 
	    #   bootstrap for obtaining confidence limits for the population 
	    #   mean without assuming normality.       
	    # Arguments:
	    #   B - number of bootstrap resamples, by default 1000.
	    #   ci - specifies the confidence level (0-1) for interval 
	    #       estimation of the population mean. 
	    #   na.rm - a logical flag, should NAs be removed?
	    #   reps - set to TRUE to have bootMean return the vector 
	    #       of bootstrapped means as the reps attribute of 
	    #       the returned object .
	    # Notes:
	    #   The function calls "smean.cl.boot" from the "Hisc" package
	    #   Requirements: require(Hmisc)       
	    # FUNCTION:       
	    # Requirements:
	    sink("@sink@") # Don't print loading ...
	    library(Design, warn.conflicts = FALSE)
	    library(Hmisc, warn.conflicts = FALSE)
	    sink()
	    unlink("@sink@")        
	    # Return Value:
	    ans = smean.cl.boot(x = x, conf.int = ci, B = B, na.rm = na.rm, 
	    	reps = reps)
	    ans
	}
	###
	
    # Bootstrapped Mean:
    data(nyseres)
    print(bootMean(nyseres[, 1]))
    ###

    
################################################################################

	