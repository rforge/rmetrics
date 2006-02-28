#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 3.2
#   Time Series With Trends: Unit Root Tests 
#
# List of Examples, Exercises and Code Snippets
#
#   3.2.1 Example: Simulate I(0) and I(1) Series
#   3.2.2 Example: Print Dickey-Fuller Distribution
#   3.2.3 Example: Write Dickey-Fuller Test Function
#   3.2.4 Example: TBILLS Dickey-Fuller OLS t-Test
#   3.2.5 Example: True FARIMA Covariances and Fourier Transform
#       * Example: True FGN Covariances and Fourier Transform
#   3.2.6 Example:  
#   3.2.7 Example:
#   3.2.8 Example: 
#
#   Alternative Unit Root Tests 
#   3.2.9 Example:  
#
# Author:
#	(C) 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


### 3.2.1 Example: Simulate I(0) and I(1) Series

    # Simulate I(1) and I(0) Data:
    set.seed(201) # try other seeds ...
    e = rnorm(250)
    z.I1 = cumsum(e)
    z.I0 = armaSim(model = list(ar = 0.75, d = 0, ma = 0), n = 250, innov = e)
    x.I1 = 5.0 + 0.1*seq(250) + z.I1
    x.I0 = 5.0 + 0.1*seq(250) + z.I0
    ###

    # Reproduce Figure 4.1 in Zivot and Wang:
    par(mfrow = c(2, 1), cex = 0.7
    ts.plot(x.I1, x.I0, lty = c(1, 2), ylab = "x")
    title(main = "Simulated I(0) and I(1) Processes")
    legend(25, max(x.I1), legend = c("I(1)", "I(0)"), lty = c(1, 2))
   	###

   	
# ------------------------------------------------------------------------------


### 3.2.2 Example: Simulate the DF Distribution

    # Simulate Functions of Wiener Processes
    # According to Zivot and Wang [2003], Chapter 4
    wiener = function(nobs) {
        e = rnorm(nobs)
        y = cumsum(e)
        ym1 = y[1:(nobs-1)]
        intW2 = nobs^(-2) * sum(ym1^2)
        intWdW = nobs^(-1) * sum(ym1*e[2:nobs])
        ans = list(intW2 = intW2, intWdW = intWdW)
        ans
    }
    ###

    # Simulate DF and NB Distributions:
    nobs = nsim = 1000
    DF = NB = rep(0, nsim)
    for (i in 1:nsim) {
        BN.moments = wiener(nobs)
        NB[i] = BN.moments$intWdW/BN.moments$intW2
        DF[i] = BN.moments$intWdW/sqrt(BN.moments$intW2)
    }
    ###

    # Plot Histograms of Simulated DF and NB:
    # Distribution - Plot Figure 4.2 in Zivot and Wang:
    par(mfrow = c(2, 2), cex = 0.7)
    hist(DF, main = "Simulated DF Distribution",
        col = "steelblue", border = "white")
    hist(NB, main = "Simulated Normalized Bias",
        col = "steelblue", border = "white")
    ###


# ------------------------------------------------------------------------------


### 3.2.3 Example: Print Dickey-Fuller Distribution

    # Print t-Statistic Table for Case I: no-constant "nc"
    dfTable(trend = "nc", statistic = "t")
    #     0.010 0.025 0.050 0.100 0.900 0.950 0.975 0.990
    #  25 -2.66 -2.26 -1.95 -1.60  0.92  1.33  1.70  2.16
    #  50 -2.62 -2.25 -1.95 -1.61  0.91  1.31  1.66  2.08
    # 100 -2.60 -2.24 -1.95 -1.61  0.90  1.29  1.64  2.03
    # 250 -2.58 -2.23 -1.95 -1.62  0.89  1.29  1.63  2.01
    # 500 -2.58 -2.23 -1.95 -1.62  0.89  1.28  1.62  2.00
    # Inf -2.58 -2.23 -1.95 -1.62  0.89  1.28  1.62  2.00
	###
	
    # Interpolated Probabilities - t Statistic:
    round(pdftest(c(-2.24, -1.50, -1.61, 0), 100, "nc"), digits = 3)
    # [1] 0.025 0.135 0.100 0.613
    pdftest(-2.80, 100)
    # [1] 0.01
    # Warning message: p-value smaller/greater than printed p-value in:
    # .pTable(X, STAT, N, digits)
    ###

    # Print all Tables:
    for (trend in c("nc", "c", "ct")) {
    	for (statistic in c("t", "n")) {
    		cat ("\nTrend:", trend, "Statistic:", statistic, "\n")
    		print(dfTable(trend, statistic))
		}
	}
	###
    
    # Interpolated Quantiles:
    for (trend in c("nc", "c", "ct")) {
    	for (statistic in c("t", "n")) {
    		cat ("\nTrend:", trend, "Statistic:", statistic, "\n")
    		print(dfTable(trend, statistic))
		}
	}
	###
    
	# Interpolated Quantiles - t Statistic:
    round(qdftest(c(0.01, 0.10, 0.50), 100, "nc"), digits = 3)
    # [1] 0.025 0.135 0.100 0.613
    qdftest(0.005, 100)
    # NA
    ###
    
    
# ------------------------------------------------------------------------------


### 3.2.3 Example: Write Dickey-Fuller Test Function

    # Write the Function:
    .dfTest = function(x)
    {
        # Regression for Case I "nc" and OLS t-Statistic:
        x.diff = diff(x)[-1]
        x.lag.1 = x[2:(length(x)-1)]
        LM = summary(lm(x.diff ~ x.lag.1 - 1))

        # OLS t-Statistic:
        STAT = LM$coefficients[1]/LM$coefficients[2]
        names(STAT) = "Dickey-Fuller"

        # P Value for OLS t-Test:
        PVAL = pdftest(q = STAT, n.sample = length(x), trend = "nc")
        names(PVAL) = ""

        # Return Value - Use fHTEST Class:
        new("fHTEST", call = match.call(), data = list(x = x),
            test = list(lm = LM, statistic = STAT, p.value = PVAL),
            title = "Dickey Fuller Test", description = " Test Type: nc")
    }
    ###

    # Try -
    # A Series that has no Unit-Root:
    set.seed(4711)
    x = rnorm(500)
    .dfTest(x)
    # A Series that contains a Unit-Root:
    .dfTest(diffinv(x))
    ###
    
    
# ------------------------------------------------------------------------------


### 3.2.4 Example: TBILLS Dickey-Fuller OLS t-Test

    # Example 17.3 Hamilton - Load GNP:
    data(gnptbill)
    x = gnptbill[, "TBILL"]

    # DF Test:
    .dfTest(x)

    
# ##############################################################################
# Serial Correlation - Augmented Dickey-Fuller Test


	# ADF Test:
	adfTest()
	unitrootTest()
	urdfTest()
	
	# lags = 0 : DF Test
	

# ------------------------------------------------------------------------------


### Example: Plot Asymptotic Distribution for t and z Statistic

	# This example deals with the numerical distribution from
	#  McKinnon's response surface.
	###
	
	# Graph Frame:
	par(mfrow = c(2,2), cex = 0.7)
	###
		
	# Asymptotic Probability for t Statistic:
	plot(c(-6, 4), c(0, 1), type = "n", main = "Probability | tau Statistic", 
		xlab = "q", ylab = "punitroot(q)")
	Trends = c("nc", "c", "ct", "ctt")
	x = (-60:40)/10
	for (i in 1:4) {
		y = punitroot(q = x, n.sample = 0, trend = Trends[i], statistic = "t")
		lines(x, y, col = "steelblue")
	}
	lines(x, pnorm(x), lty = 3)
	text(seq(-2.4, 0.2, length = 5), seq(0.65, 0.45, length = 5), 
		labels = c("ctt", "ct", "c", "nc", "N"))
	grid()
	###
		
	# Asymptotic Density for t Statistic:
	plot(c(-6, 4), c(0, 0.65), type = "n", main = "Density | tau Statistic", 
		xlab = "x", ylab = "diff(punitroot(q))")
	Trends = c("nc", "c", "ct", "ctt")
	x = (-60:40)/10
	X = ((-60:40)/10+0.05)[-1]
	for (i in 1:4) {
		y = punitroot(q = x, n.sample = 0, trend = Trends[i], statistic = "t")
		Y = diff(y)/0.1
		lines(X, Y, col = "steelblue")
	}
	lines(X, dnorm(X), lty = 3)
	text(seq(-2.2, 0.8, length = 5), seq(0.6, 0.4, length = 5), 
		labels = c("ctt", "ct", "c", "nc", "N"))
	grid()
	###
		
	# Asymptotic Probability for z Statistic:
	plot(c(-40, 4), c(0, 1), type = "n", main = "Probability | z Statistic", 
		xlab = "q", ylab = "punitroot(q)")
	Trends = c("nc", "c", "ct", "ctt")
	x = (-400:40)/10
	for (i in 1:4) {
		y = punitroot(q = x, n.sample = 0, trend = Trends[i], statistic = "n")
		lines(x, y, col = "steelblue")
	}
	text(seq(-10, 0, length = 4), seq(0.65, 0.45, length = 4), 
		labels = c("ctt", "ct", "c", "nc"))
	grid()
	###
		
	# Asymptotic Density  for z Statistic:
	plot(c(-40, 4), c(0, 0.3), type = "n", main = "Density | z Statistic", 
		xlab = "x", ylab = "diff(punitroot(q))")
	Trends = c("nc", "c", "ct", "ctt")
	x = (-400:40)/10
	X = ((-400:40)/10+0.05)[-1]
	for (i in 1:4) {
		y = punitroot(q = x, n.sample = 0, trend = Trends[i], statistic = "n")
		Y = diff(y)/0.1
		lines(X, Y, col = "steelblue")
	}
	text(seq(-14.5, 0.5, length = 4), seq(0.07, 0.27, length = 4), 
		labels = c("ctt", "ct", "c", "nc"))
	grid()
	###
	
	
# ------------------------------------------------------------------------------

	
### Example: Tabulated OLS t-Statistic vs Response Surface
	
	# This example compares the tabulated OLS t-Statistic for the 
	# DF test with those computed from McKinnons Response Surface:
	
	# Cases:
	trend = c("nc", "c", "ct")
	###
	
	# Loop over all three Cases:
	for ( i in 1:3 ) {
		# Tabulated Values:
		tTable = as.matrix(dfTable(trend = trend[i], statistic = "t"))
        # Response Surface Values:
	    nobs.vec = c(25, 50, 100, 250, 500, 0)  
	    x = c(0.01, 0.025, 0.05, 0.10, 0.90, 0.95, 0.975, 0.99)
	    urTable = NULL
	    for ( nobs in nobs.vec ) {
	    	ans = qunitroot(p = x, nobs, trend[i], statistic = "t")
	        urTable = rbind(urTable, ans) 
    	}
    	cat("\nTrend = ", trend[i], "\n")
    	print(round(tTable - urTable, digits = 3))
	}
	###
	
	
# ------------------------------------------------------------------------------


### Example: Compare punitroot() with SPlus' Finmetrics :

    # This example tests and compares the results obtained from Rmetrics' 
    # "punitroot" with those obtained from Splus 6.1:
    ###

    # Statistic: "t"
    round(punitroot(q=-6:2, n.sample = 100, trend = "nc", statistic = "t"), 4)
    # R:      0.0000 0.0000 0.0001 0.0030 0.0441 0.2829 0.6803 0.9155 0.9889
    # Splus:  0.0000 0.0000 0.0001 0.0030 0.0441 0.2829 0.6803 0.9155 0.9889
    round(punitroot(q=-6:2, n.sample = 100, trend = "c", statistic = "t"), 4)
    # R:      0.0000 0.0001 0.0021 0.0383 0.2865 0.7510 0.9558 0.9964 0.9999
    # Splus:  0.0000 0.0001 0.0021 0.0383 0.2865 0.7510 0.9558 0.9964 0.9999    
    round(punitroot(q=-6:2, n.sample = 100, trend = "ct", statistic = "t"), 4)
    # R:      0.0000 0.0004 0.0117 0.1375 0.5940 0.9387 0.9958 0.9999 1.0000
    # Splus:  0.0000 0.0004 0.0117 0.1375 0.5940 0.9387 0.9958 0.9999 1.0000    
    round(punitroot(q=-6:2, n.sample = 100, trend = "ctt", statistic = "t"), 4)
    # R:      0.0001 0.0020 0.0384 0.2979 0.8077 0.9847 0.9995 1.0000 1.0000
    # Splus:  0.0001 0.0020 0.0384 0.2979 0.8077 0.9847 0.9995 1.0000 1.0000    
    ###
    
    # Statistic: "n"
    round(punitroot(q=-6:2, n.sample = 100, trend = "nc", statistic = "n"), 4)
    # R:      0.0872 0.1197 0.1654 0.2307 0.3262 0.4700 0.6803 0.9105 0.9884
    # Splus:  0.0872 0.1197 0.1654 0.2307 0.3262 0.4700 0.6803 0.9105 0.9884    
    round(punitroot(q=-6:2, n.sample = 100, trend = "c", statistic = "n"), 4)
    # R:      0.3375 0.4261 0.5318 0.6519 0.7759 0.8837 0.9558 0.9881 0.9975
    # Splus:  0.3375 0.4261 0.5318 0.6519 0.7759 0.8837 0.9558 0.9881 0.9975    
    round(punitroot(q=-6:2, n.sample = 100, trend = "ct", statistic = "n"), 4)
    # R:      0.7353 0.8144 0.8828 0.9349 0.9688 0.9874 0.9958 0.9988 0.9997
    # Splus:  0.7353 0.8144 0.8828 0.9349 0.9688 0.9874 0.9958 0.9988 0.9997    
    round(punitroot(q=-6:2, n.sample = 100, trend = "ctt", statistic = "n"), 4)
    # R:      0.9213 0.9542 0.9761 0.9889 0.9954 0.9984 0.9995 0.9998 1.0000
    # Splus:  0.9213 0.9542 0.9761 0.9889 0.9954 0.9984 0.9995 0.9998 1.0000
    ###
  
    
# ------------------------------------------------------------------------------


### Example: Compare qunitroot() with SPlus' Finmetrics:

    # This example tests and compares the results obtained from Rmetrics' 
    # "qunitroot" with those obtained from Splus 6.1:
    ###

    # q-Values:
    q = c(0.15, 0.30, 0.45, 0.60, 0.75, 0.90)
    ###
     
    # Statistic: "t"
    round(qunitroot(q, n.sample = 100, trend = "nc", statistic = "t"), 4)
    # R:      -1.3979 -0.9576 -0.6118 -0.2328  0.2265  0.8967
    # Splus:  -1.3979 -0.9576 -0.6118 -0.2328  0.2265  0.8967   
    round(qunitroot(q, n.sample = 100, trend = "c", statistic = "t"), 4)
    # R:      -2.3799 -1.9690 -1.6568 -1.3577 -1.0029 -0.4232    
    # Splus:  -2.3799 -1.9690 -1.6568 -1.3577 -1.0029 -0.4232   
    round(qunitroot(q, n.sample = 100, trend = "ct", statistic = "t"), 4)
    # R:      -2.9559 -2.5588 -2.2625 -1.9889 -1.6869 -1.2227     
    # Splus:  -2.9559 -2.5588 -2.2625 -1.9889 -1.6869 -1.2227   
    round(qunitroot(q, n.sample = 100, trend = "ctt", statistic = "t"), 4)
    # R:      -3.3927 -2.9955 -2.7002 -2.4292 -2.1354 -1.7073    
    # Splus:  -3.3928 -2.9955 -2.7002 -2.4291 -2.1354 -1.7073
    ###
    
    # Statistic: "n"
    round(qunitroot(q, n.sample = 100, trend = "nc", statistic = "n"), 4)
    # R:      -4.2999 -2.2371 -1.1160 -0.3463  0.2795  0.9382    
    # Splus:  -4.2999 -2.2372 -1.1160 -0.3463  0.2795  0.9382   
    round(qunitroot(q, n.sample = 100, trend = "c", statistic = "n"), 4)
    # R:      -9.2935 -6.4935 -4.7588 -3.4215 -2.2137 -0.8166   
    # Splus:  -9.2934 -6.4936 -4.7588 -3.4215 -2.2137 -0.8165   
    round(qunitroot(q, n.sample = 100, trend = "ct", statistic = "n"), 4)
    # R:     -15.4278 -11.8711  -9.5254  -7.6163  -5.8205  -3.7052   
    # Splus: -15.4277 -11.8710  -9.5254  -7.6163  -5.8206  -3.7050  
    round(qunitroot(q, n.sample = 100, trend = "ctt", statistic = "n"), 4)
    # R:     -20.5330 -16.4756 -13.7175 -11.4106  -9.1839  -6.5128     
    # Splus: -20.5330 -16.4755 -13.7174 -11.4107  -9.1839  -6.5126
    ###


# ------------------------------------------------------------------------------


### Example: Test Internal Function ".urcval"

	# Reproduce Figure 1 to 3 as plotted in the paper of J.G. MacKinnon
	###

    # JGM: Figure 1 to 3
    figure1to3 = function(figure) {
        x = (-80:20)/10
        plot (x = c(-8, 2), y = c(0, 1), type = "n")
        for (i in 1:12) {
            y = .urcval(x, nobs = 0, niv = i, itt = 1, itv = figure, nc = 2)
            lines(x, y) 
        }
        if (figure == 3) {
            for (i in 1:12) {
                y = .urcval(x, nobs = 0, niv = i, itt = 1, itv = 4, nc = 2)
                lines(x, y, lty = 3) 
            } 
        }
        invisible() 
    }
    ###
            
    # Plot:
    par(mfcol = c(3, 2), cex = 0.5)
    figure1to3(1)
    figure1to3(2)
    figure1to3(3)
    ###

    
# ##############################################################################
# Nelson Plosser Data
    

### Example: Plot the Nelson-Plosser US Economy Data 1909 - 1970

	# Nelson Plosser Data - Description of Columns
	# %Y%m%d - Date index from 18601231 until 19701231 
	# gnp.r  - Real GNP, [Billions of 1958 Dollars] starting 1909 
	# gnp.n  - Nominal GNP, [Millions of Current Dollars] starting 1909
	# gnp.pc - Real Per Capita GNP, [1958 Dollars], starting 1909  
	# ip 	 - Industrial Production Index, [1967 = 100], starting 1860  
	# emp 	 - Total Employment, [Thousands], starting 1890 
	# ur 	 - Total Unemployment Rate, [Percent], starting 1890
	# gnp.p  - GNP Deflator, [1958 = 100], starting 1889
	# cpi 	 - Consumer Price Index, [1967 = 100], starting 1860
	# wg.n 	 - Nominal Wages, [current Dollars], starting 1900
	# wg.r 	 - Real Wages, [Nominal wages/CPI], starting 1900
	# M 	 - Money Stock (M2), [Billions of USD, annual avgs], starting 1889
	# vel 	 - Velocity of Money, starting 1869
	# bnd 	 - Bond Yields of 30-year Corporate Bonds, [% pa], starting 1900
	# sp 	 - Stock Prices, [Index; 1941 - 43 = 100], starting 1871
	###
	
	
# ------------------------------------------------------------------------------

	
### Example: Nelson Plosser Paper - Plot Data:

	# Load Data:
    data(nelsonplosser)
   	Instrument = c("Real GnP", "Nominal GNP", "Per Capita GNP", "CPI",
		"Employment", "Unemployment", "GNP deflator", "CPI", "Nominal Wages",
		"Real Wages", "M2", "Velocity M2", "BondYields", "Stock Prices") 
    Years = 1909:1970
    nelsonplosser = nelsonplosser[50:111, -1]
    Dim = dim(nelsonplosser)[2]
    colName = colnames(nelsonplosser)
    
    # Plot:
    par(mfrow = c(3, 3), cex = 0.5)
    for (i in 1:9)
    	plot(Years, nelsonplosser[, i], xlab = "Year", ylab = colName[i],
    		type = "b", pch = 19, col = "steelblue4", main = Instrument[i])
    ###
    
    # Continue:
    for (i in 10:Dim)
    	plot(Years, nelsonplosser[, i], xlab = "Year", ylab = colName[i],
    		type = "b", pch = 19, col = "steelblue4", main = Instrument[i])		
    ###
 
    
# ------------------------------------------------------------------------------
   
    
### Example: Reproduce Nelson Plosser Paper - Table 2


	# Create Table 2:
    data(nelsonplosser)
    Instrument = c("Real GnP", "Nominal GNP", "Per Capita GNP", "CPI",
		"Employment", "Unemployment", "GNP deflator", "CPI", "Nominal Wages",
		"Real Wages", "M2", "Velocity M2", "BondYields", "Stock Prices") 
    Table.2 = NULL
    for ( i in 1:14 ) {
	    x = na.omit(nelsonplosser[, i+1])
	    N = length(x)
	    To = 1970
	    From = To - N + 1
	    if (i != 13) x = log(x)
	    r = round(acf(x, plot = FALSE)$acf[2:7], digits = 2)
	    Table.2 = rbind(Table.2, c(N, From, To, r))    	
    }
    colnames(Table.2) = c("T", "From", "To", paste("r", 1:6, sep = ""))
    rownames(Table.2) = Instrument
    Table.2
    ###

    
# ------------------------------------------------------------------------------


### Nelson Plotter Paper - Create Table 5
    

    # Create Table 5:
    data(nelsonplosser)
    Instrument = c("Real GnP", "Nominal GNP", "Per Capita GNP", "CPI",
		"Employment", "Unemployment", "GNP deflator", "CPI", "Nominal Wages",
		"Real Wages", "M2", "Velocity M2", "BondYields", "Stock Prices") 
	k = c(2, 2, 2, 6, 3, 4, 2, 4, 3, 2, 2, 1, 3, 3)
    Table.5 = NULL
    for ( i in 1:14 ) {
	    x = na.omit(nelsonplosser[, i+1])
	    T = length(x)
	    if (i != 13) x = log(x)
	    ans = adfTest(x, type = "ct", lags = k[i])
	    lm = summary(ans@test$lm)
	    mu.hat = round(lm$coefficients[1, 1], digits = 3)
	    t.mu = round(lm$coefficients[1, 3], digits = 2)
	    gamma.hat = round(lm$coefficients[3, 1], digits = 3)
	    t.gamma = round(lm$coefficients[3, 3], digits = 2)
	    rho1.hat = round(lm$coefficients[2, 1], digits = 3)+1
	    t.rho1 = round(lm$coefficients[2, 3], digits = 2)
	    seps = round(lm$sigma, digits = 3)
	    r1 = round(acf(lm$residuals)$acf[2], digits = 3)
	    vec = c(T, k[i], mu.hat, t.mu, gamma.hat, t.gamma, rho1.hat, 
	    	t.rho1, seps, r1)
	    Table.5 = rbind(Table.5, vec)    	
    }
    colnames(Table.5) = c("T", "lag", "mu", "t.mu", "gam", "t.gam", "rho1",
	    	"t.rho1)", "s.eps", "r.1")
    rownames(Table.5) = Instrument
    Table.5
    ###
    
    
# ******************************************************************************
# ERS Unit Root Test:


ptCvals = 
function(type = c("constant", "trend"))
{
    # Description:
    #   Returns Pt level values for the ERS unit root test.
    
    # Details:
    #   These are the numbers given in Table 1 of the paper
    #   of Elliott, Rothenberg and Stock.
    
    # TABLE:
    
    # Level Values:
    if (type[1] ==  "constant") {
        # Constant:
        ans = matrix(c(
            # 50   100   200   Inf
            1.87, 1.95, 1.91, 1.99,    # constant  # 0.010   # c =  -7.0
            2.39, 2.47, 2.47, 2.55,                # 0.025
            2.97, 3.11, 3.17, 3.26,                # 0.050
            3.91, 4.17, 4.33, 4.48                 # 0.100
        ), 4, 4) 
        colnames(ans) = c("1%", "2.5%", "5%", "10%")
        rownames(ans) = c("50", "100", "200", "Inf")
    } else if (type[1] == "trend") {
        # Trend:
        ans = matrix(c(
            # 50   100   200   Inf
            4.22, 4.26, 4.05, 3.96,    # trend     # 0.010   # c = -13.5
            4.94, 4.90, 4.83, 4.78,                # 0.025
            5.72, 5.64, 5.66, 5.62,                # 0.050
            6.77, 6.79, 6.86, 6.89                 # 0.100
         ), 4, 4)
         colnames(ans) = c("1%", "2.5%", "5%", "10%")
         rownames(ans) = c("50", "100", "200", "Inf")
    } else {
        stop("type must be either constant or trend")
    }
    
    # Return Value:
    ans
}   


	ptCvals("constant")
	ptCvals("trend")


# ------------------------------------------------------------------------------
   
 
### Example: complement the ERS level values by your own MC values
   
	ptCvalsMC = 
	function(N = Inf, mcSteps = 1000, mcLoops = 20, dcSteps = 1000,
	c = -7, method = 1:2, trace = TRUE)
	{   
	    # Description:
	    #   Try to replicate the results for P_t listed in Table 1 
	    #   of the paper of Elliott, Rothenberg, and Stock for finite
	    #   sample lengths N.
	    
	    # Arguments:
	    #   N - an integer value, the sample size. If N is infinite
	    #       the asymptotic limit is evaluated using "dcSteps"
	    #       discretization steps.   
	    #   mcSteps - the number of Monte Carlo steps per MC loop for
	    #       a finite sample.
	    #   mcLoops - the number of Monte Carlo loops, only used for 
	    #       finite samples.
	    #   dcSteps - if "N" is infinite, then "dcSteps"
	    #       determines the number of discretization steps.
	    #   c - constant value, by default c=-7 as used by ERS.
	    #   method - selects one from two alternative methods to
	    #       compute omega, for details see ERS.
	    #   trace - a logical flag, should the simulation be traced?
	    
	    # Details:
	    #   The numbers in Table 1 of ERS are:
	    #             1%    2.5%      5%     10%
	    #    50     1.87    2.39    2.97    3.91
	    #   100     1.95    2.47    3.11    4.17
	    #   200     1.91    2.47    3.17    4.33
	    #   Inf     1.99    2.55    3.26    4.48
	    
	    # Notes:
	    #   The function is slow, but sufficient enough for educational 
	    #   purposes.
	    
	    # FUNCTION:
	    
	    # Settings:
	    statistic = rep(0, times = mcSteps * mcLoops)
	    ahat = 1 + c/N
	    za1 = c(1, rep(1 - ahat, N - 1))
	    index = c("1%" = 0.01, "2.5%" = 0.025, "5%" = 0.05, "10%" = 0.1) 
	    
	    # Simulate:
	    if (N == Inf) {
	        # Brownian Motion Approximation:
	        # c^2 * INTRGRAL(W^2) - c * W^2(1) ] =
	        # c^2 * INTRGRAL(W^2) - c * (2 * INTEGRAL(WdW) + 1) ]
	        statistic = rep(0, times = mcSteps)
	        n = dcSteps
	        i = 0
	        for (i in 1:mcSteps) {
	            i = i + 1
	            r = rnorm(n)
	            y = cumsum(r)
	            intWdW = sum(y[-n]*r[-1])/n
	            intW2 = sum(y[-n]^2)/n^2
	            statistic[i] = c^2 * intW2 - c * ( 2*intWdW + 1 )
	        }
	        if (trace) {
	            # Print Results after each Loop:
	            P = matrix(sort(statistic)[index*mcSteps], ncol = length(index))
	            colnames(P) = names(index)
	            rownames(P) = as.character(mcSteps)
	            print(as.data.frame(P))
	        }
	    } else {
	        # Finite Sample Regression:   
	        i = 0
	        for (m in 1:mcLoops) {
	            for (n in 1:mcSteps) {
	                i = i + 1
	                y = as.vector(filter(rnorm(N), filter = 1, "recursive"))    
	                ya = c(y[1], y[-1] - ahat * y[-N])
	                yd.reg = lm(ya ~ -1 + za1)    
	                sig.res = sum(residuals(yd.reg)^2)
	                sig.null = sum(diff(y)^2)  
	                if (method[1] == 1) {
	                    omega2 = sig.null/N    
	                } else if (method[1] == 2) {               
	                    omega2 = (summary(lm(diff(y) ~ y[-N]))$sigma)^2
	                }
	                statistic[i] = (sig.res - ahat * sig.null)/omega2
	            }                       
	            if (trace) {
	                # Print Results after each Loop:
	                p = sort(statistic[1:(m*mcSteps)])
	                P = t(p[round(index * m * mcSteps, digits = 0)])
	                colnames(P) = names(index)
	                rownames(P) = as.character(m*mcSteps)
	                print(as.data.frame(P))
	            }
	        }   
	    }
	            
	    # Return Value:
	    invisible(statistic)
	}


# ------------------------------------------------------------------------------
  
    # Monte Carlo Results using method 1 for omega:
    # omega2 = sig.null/N
              
    
    # N=20:           1%    2.5%      5%     10%
    #    20000      1.96    2.34    2.81    3.59    # zeroth run
    #    20000      1.98    2.41    2.85    3.61    # first run
    #    20000      1.97    2.44    2.91    3.65    # second run
    #    20000      1.98    2.40    2.83    3.53    # third run 
    #    20000      1.92    2.33    2.81    3.56    # fourth run
    p.0020 = c(     1.96,   2.38,   2.84,   3.59 )  # MEAN  
                    
    # N=25:           1%    2.5%      5%     10%
    #    20000      1.95    2.42    2.88    3.67    # zeroth run
    #    20000      1.91    2.39    2.92    3.73    # first run
    #    20000      1.90    2.38    2.87    3.66    # second run
    #    20000                                      # third run 
    #    20000                                      # fourth run
    p.0025 = c(     1.92,   2.40,   2.89,   3.69 )  # MEAN
    
    # N=33:           1%    2.5%      5%     10%
    #    20000      1.91    2.38    2.91    3.73    # zeroth run
    #    20000      1.90    2.38    2.88    3.74    # first run
    #    20000      1.94    2.40    2.90    3.75    # second run
    #    20000                                      # third run 
    #    20000                                      # fourth run 
    p.0033 = c(     1.92,   2.39,   2.90,   3.74 )  # MEAN
       
    # N=50:           1%    2.5%      5%     10%
    #    20000      1.87    2.39    2.97    3.91    # ERS
    #    20000      1.88    2.37    2.98    3.95    # first run
    #    20000      1.90    2.41    2.96    3.97    # second run
    #    20000      1.93    2.36    2.98    3.95    # third run
    #    20000      1.92    2.41    2.98    3.95    # fourth run
    p.0050 = c(     1.90,   2.39,   2.97,   3.95 )  # MEAN
                
    # N=100:          1%    2.5%      5%     10%
    #    20000      1.95    2.47    3.11    4.17    # ERS
    #    20000      1.91    2.41    3.07    4.14    # first run
    #    20000      1.93    2.50    3.08    4.15    # second run
    #    20000      1.96    2.44    3.05    4.15    # third run
    #    20000      1.90    2.49    3.12    4.13    # fourth run
     p.0100 = c(    1.93,   2.46,   3.09,   4.15 )  # MEAN
         
    # N=200:          1%    2.5%      5%     10%
    #    20000      1.91    2.47    3.17    4.33    # ERS
    #    20000      1.90    2.49    3.16    4.35    # first run
    #    20000      1.88    2.44    3.16    4.33    # second run
    #    20000      1.91    2.48    3.14    4.24    # third run
    #    20000      1.85    2.47    3.16    4.31    # fourth run
    p.0200 = c(     1.89,   2.47,   3.16,   4.31 )  # MEAN
         
    # N=1000:         1%    2.5%      5%     10%
    #    20000      1.92    2.57    3.30    4.46    # zeroth run
    #    20000      1.93    2.51    3.26    4.43    # first run
    #    20000      1.93    2.55    3.27    4.52    # second run
    #    20000      1.98    2.56    3.28    4.47    # third run
    #    20000      1.94    2.49    3.20    4.42    # fourth run
    p.1000 = c(     1.94,   2.54,   3.26,   4.46 )  # MEAN
         
    # N=Inf           1%    2.5%      5%     10%
    #    20000      1.99    2.55    3.26    4.48    # ERS
    #                 1%    2.5%      5%     10%
    #    20000      1.94    2.56    3.34    4.51    # 1000 dcSteps
    #    20000      1.98    2.58    3.35    4.60    # 1000
    #    20000      1.94    2.60    3.33    4.54    # 1000
    #    20000      1.93    2.57    3.29    4.54    # 1000
    #    20000      1.99    2.59    3.32    4.60    # 1000
    #               1.96    2.58    3.33    4.56    # MEAN
    #                 1%    2.5%      5%     10%
    #    20000      2.02    2.64    3.38    4.60    # 2000 dcSteps
    #    20000      2.00    2.60    3.28    4.54    # 2000
    #    20000      1.93    2.60    3.41    4.66    # 2000
    #    20000      1.99    2.60    3.35    4.54    # 2000
    #    20000      1.94    2.52    3.33    4.54    # 2000
    p.Inf = c(      1.98,   2.59,   3.35,   4.58 )  # MEAN
 

# ------------------------------------------------------------------------------   


    # Monte Carlo Results using method 2 for omega:
    # omega2 = (summary(lm(diff(y) ~ y[-N]))$sigma)^2
    
    # N= 50:      
    #    20000      1.87    2.39    2.97    3.91    # ERS
    #    20000      1.92    2.38    2.93    3.83    # first run
    #    20000      1.88    2.37    3.00    3.96    # second run
    #    20000      1.95    2.40    2.96    3.95    # third run
    #    20000 
                    
    # N=100:
    #    20000      1.95    2.47    3.11    4.17    # ERS
    #    20000      1.89    2.43    3.13    4.21    # first run
    #    20000 
    #    20000 
    #    20000 
         
    # N=200:
    #    20000      1.91    2.47    3.17    4.33    # ERS
    #    20000      1.96    2.52    3.15    4.30    # first run
    #    20000 
    #    20000 
    #    20000 
        
    # ... yields the same MC simulation results 
  

# ------------------------------------------------------------------------------


### Plot the Response Surface:

    # Bind MC Simulation Results:
    p = rbind(p.0020, p.0025, p.0033, p.0050, p.0100, p.0200, p.1000, p.Inf)   
    colnames(p) = c("1%", "2.5%", "5%", "10%") 
    x = c(20, 25, 33, 50, 100, 200, 1000, Inf)
    rownames(p) = as.character(x)
    p
    
    # Settings:
    par(mfrow = c(2, 2), cex = 0.7)
    ymin =     c(1.85, 2.30, 2.80, 3.50)
    ymax =     c(2.02, 2.64, 3.45, 4.80)
    ESR050 =   c(1.87, 2.39, 2.97, 3.91)
    ESR100 =   c(1.95, 2.47, 3.11, 4.17)
    ESR200 =   c(1.91, 2.47, 3.17, 4.33)
    asymp =    c(1.98, 2.58, 3.33, 4.56)
    ESRasymp = c(1.99, 2.55, 3.26, 4.48)
    index = c("1%", "2.5%", "5%", "10%")
    
    # Plot:
    for ( k in 1:4) {
        data = as.data.frame(cbind(x = p[,k], 
            y1 = 1/x, y2 = 1/x^2, y3 = 1/x^3, y4 = 1/x^4))
        fit = lm(x ~ y1 + y2 + y3 + y4, data)
        N = 20:1200
        y = NULL
        for (i in N) y = c(y, sum(fit$coef*c(1, 1/i, 1/i^2, 1/i^3, 1/i^4)))
        plot(N, y, type = "l", col = "red", ylim = c(ymin[k], ymax[k]))
        points(x, p[, k])
        points(c(50, 100, 200), c(ESR050[k], ESR100[k], ESR200[k]), 
            col = "blue") 
        abline(h = asymp[k], lty = 3)
        abline(h = ESRasymp[k], lty = 3, col = "blue")
        title(main = paste(index[k], "Response Surface"))
        text(x = max(N)/2, y = ymax[k] - 0.07*(ymax[k] - ymin[k]),
            "mean from 5 samples with 20'000 replications")
        text(x = 0.8*max(N), y = ymax[k] - 0.93*(ymax[k] - ymin[k]),
            "4-th order 1/N Fit")
    }


# ------------------------------------------------------------------------------


Pt.trend = 
function(N, mcSteps = 100, mcLoops = 100, c = -13.5)
{   
    statistic = rep(0, times = mcSteps*mcLoops)
    ahat = 1 + c/N
    za1 = c(1, rep(1 - ahat, N - 1))
    za2 = c(1, 2:N - ahat * (1:(N - 1)))
    index = c("1%" = 0.01, "2.5%" = 0.025, "5%" = 0.05, "10%" = 0.1) 
    
    i = 0
    for (m in 1:mcLoops) {
        for (n in 1:mcSteps) {
            i = i + 1
            y = filter(rnorm(N), filter = 1, "recursive")     
            ya = c(y[1], y[2:N] - ahat * y[1:(N - 1)])
            yd.reg = summary(lm(ya ~ -1 + za1 + za2))          
            sig.res = sum(residuals(yd.reg)^2)
            sig.null = sum(diff(y)^2)    
            y.l = y[-N]
            y.diff = diff(y)  
            omega = summary(lm(y.diff ~ y.l))$sigma
            statistic[i] = (sig.res - ahat * sig.null)/omega^2
        }               
        # Printing:
        p = sort(statistic[1:(m*mcSteps)])
        P = t(p[round(index * m * mcSteps, digits = 0)])
        colnames(P) = names(index)
        rownames(P) = as.character(m*mcSteps)
        print(as.data.frame(P))
    }           
    # Return Value:
    statistic
}


# ------------------------------------------------------------------------------


lm.fast = 
function(x, y)
{
    # Description:
    #	A fast lm function, everything what is not really need
    #	for the MC Simulation is rempved from the function 'lm'
    
    # Access the Fortran routine:
    z = .Fortran("dqrls", qr = x, n = nrow(x), p = ncol(x), 
        y = y, ny = NCOL(y), tol = as.double(1.0e-7), 
        coefficients = mat.or.vec(ncol(x), NCOL(y)), 
        residuals = y, effects = y, rank = integer(1), 
        pivot = 1:ncol(x), qraux = double(ncol(x)), 
        work = double(2 * ncol(x)), PACKAGE = "base") 
        
    # Compute Omega:      
    omega = sum(z$residuals^2)/(nrow(x) - z$rank)
    
    # Return Value:
    omega
}


################################################################################
# PART V: Alternative Unit Root Tests (adapted from 'urca')

    # Description:
    #	Examples how to use Unit Root Tests from 'Rmetrics':
    # 		urerTest()
    # 		urkpssTest()
    # 		urppTest()
    #		urspTest()
    # 		urzaTest()
    

# ------------------------------------------------------------------------------


### Example: ERS Test


    urersTest(GNP, type = "DF-GLS", model = "const", lag.max = 4) 
    urersTest(GNP, type = "DF-GLS", model = "trend", lag.max = 4) 
    ###
    
    urersTest(GNP, type = "P-test", model = "const", lag.max = 4)   
    urersTest(GNP, type = "P-test", model = "trend", lag.max = 4)   
    ###
    
 
# ------------------------------------------------------------------------------

   
### Example: KPSS Test


    urkpssTest(log(GNP), type = "tau", lags = "nil")  
    urkpssTest(log(GNP), type = "mu", lags = "nil") 
    ###
    
    urkpssTest(log(GNP), type = "tau", lags = "short")  
    urkpssTest(log(GNP), type = "mu", lags = "short") 
    ###
       
    urkpssTest(log(GNP), type = "tau", lags = "long")  
    urkpssTest(log(GNP), type = "mu", lags = "long") 
    ###
          
    urkpssTest(log(GNP), type = "tau", use.lag = 5)  
    urkpssTest(log(GNP), type = "mu", use.lag = 5)   
    ###
    
    
# ------------------------------------------------------------------------------

   
### Example: PP Test


    urppTest(GNP, type = "Z-alpha", model = "constant", lags = "short")
    urppTest(GNP, type = "Z-tau", model = "constant", lags = "short")
    urppTest(GNP, type = "Z-alpha", model = "trend", lags = "short")
    urppTest(GNP, type = "Z-tau", model = "trend", lags = "short")
    ###
       
    urppTest(GNP, type = "Z-alpha", model = "constant", lags = "long")
    urppTest(GNP, type = "Z-tau", model = "constant", lags = "long")
    urppTest(GNP, type = "Z-alpha", model = "trend", lags = "long")
    urppTest(GNP, type = "Z-tau", model = "trend", lags = "long")
    ###
    
    urppTest(GNP, type = "Z-alpha", model = "constant", use.lag = 5)
    urppTest(GNP, type = "Z-tau", model = "constant", use.lag = 5)
    urppTest(GNP, type = "Z-alpha", model = "trend", use.lag = 5)
    urppTest(GNP, type = "Z-tau", model = "trend", use.lag = 5)
    ###
    

# ------------------------------------------------------------------------------

   
### Example: SP Test


    urspTest(GNP, type = "tau", pol.deg = 1, signif = 0.01)
    urspTest(GNP, type = "tau", pol.deg = 2, signif = 0.01)
    urspTest(GNP, type = "tau", pol.deg = 3, signif = 0.01)
    urspTest(GNP, type = "tau", pol.deg = 4, signif = 0.01)
    ###
    
    urspTest(GNP, type = "rho", pol.deg = 1, signif = 0.01)
    urspTest(GNP, type = "rho", pol.deg = 2, signif = 0.01)
    urspTest(GNP, type = "rho", pol.deg = 3, signif = 0.01)
    urspTest(GNP, type = "rho", pol.deg = 4, signif = 0.01)
    ###
    
    
# ------------------------------------------------------------------------------

   
### Example: ZA Test


    urzaTest(GNP, model = "intercept", lag = 2)
    urzaTest(GNP, model = "trend", lag = 2)
    urzaTest(GNP, model = "both", lag = 2)
    ###
    
    
################################################################################
# PART VI: Use Bernhard Pfaff's contributed 'urca' package


    # Description:
    #   This part shows the usage of unit root tests from Bernhard Pfaff's
    #   contributed R package 'urca'. Note, you have to install the 'urca'
    #   package and to load it calling "library(urca)" or "require(urca)".
   
    # Arguments:
    #   Unit Root Tests from 'urca' Package:
    # 	ur.df()
    #		...
    # 	ur.ers(y, type = c("DF-GLS", "P-test"), model = c("constant", "trend"), 
    #		lag.max = 4)
    # 	ur.kpss(y, type = c("mu", "tau"), lags = c("short", "long", "nil"), 
    #		use.lag = NULL)
    # 	ur.pp(x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"), 
    #		lags = c("short", "long"), use.lag = NULL)
    # 	ur.sp(y, type = c("tau", "rho"), pol.deg = c(1, 2, 3, 4), signif = 
    #		c(0.01, 0.05, 0.1))
    # 	ur.za(y, model = c("intercept", "trend", "both"), lag)

    # Details:
    # 	Function Overview:
    # 	TABLE:
    #             ur.df  ur.ers    ur.kpss  ur.pp     ur.sp          ur.za
    # 	------------------------------------------------------------------------
    #  	type             DF-GLS    mu       Z-alpha   tau            -
    #  	                 P-test    tau      Z-tau     rho            -
    # 	pol.deg          -         -        -         1:4            -
    #  	signif                                        0.01,0.05,0.1
    #  	model            constant  -        constant  -              intercept
    #   	             trend     -        trend     -              trend
    #       	         -         -        -         -              both
    #  	lag.max          4         -        -         -              -
    #  	lags             -         short    short     -              - 
    #  	                 -         long     long      -              -
    #   	             -         nil      -         -              -
    #  	use.lags         -         NULL     NULL      -              -
    # ------------------------------------------------------------------------

    
# ------------------------------------------------------------------------------


### Example: Load and Plot Real GPD from Nelson-Plotter Data:


    # Requirements:
    require(urca)
    ###
    
    # GNP Data:
    data(nporg)
    gnp = na.omit(nporg[, "gnp.r"])
    dgnp = diff(gnp)
    lgnp = log(gnp)
    dlgnp = diff(lgnp)
    par(mfrow = c(2, 2), cex = 0.7)
    gnp.log = log(gnp)
    ###
       
    # Plot:
    plot(gnp, type = "b", main = "Nelson-Plosser Data")
    plot(diff(gnp), type = "b", main = "Differenced")
    abline(h = mean(diff(gnp)), lty = 3)
    # Plot Log Data:
    plot(gnp.log, type = "b", main = "Log: Nelson-Plosser Data")
    plot(diff(gnp.log), type = "b", main = "Differenced")
    abline(h = mean(diff(gnp.log)), lty = 3)
    ###
    

# ------------------------------------------------------------------------------


### Example: DF and ADF Test


    # ur.df(y, type = c("none", "drift", "trend"), lags = 1)


# ------------------------------------------------------------------------------


### Example: ERS - ERS Test


    args(ur.ers)
    # function (y, type = c("DF-GLS", "P-test"), model = c("constant", 
    #   "trend"), lag.max = 4) 
    ###

    # DF-GLS | constant:
    ers.gnp = ur.ers(dgnp, type = "DF-GLS", model = "constant", lag.max = 4)
    print(ers.gnp)
    plot(ers.gnp)
    summary(ers.gnp)
    ###
    
    # P-test | constant:
    ers.gnp = ur.ers(dgnp, type = "P-test", model = "constant", lag.max = 4)
    print(ers.gnp)
    summary(ers.gnp)
    ###
        
    # DF-GLS | trend:
    ers.gnp = ur.ers(gnp, type = "DF-GLS", model = "trend", lag.max = 4)
    print(ers.gnp)
    plot(ers.gnp)
    summary(ers.gnp)
    ###
        
    # P-test | trend:
    ers.gnp = ur.ers(gnp, type = "P-test", model = "trend", lag.max = 4)
    print(ers.gnp)
    summary(ers.gnp)
    ###


# ------------------------------------------------------------------------------


### Example: KSPP - KSPP Test
 

    args(ur.kpss)
    #   function (y, type = c("mu", "tau"), lags = c("short", "long", 
    #       "nil"), use.lag = NULL) 
    # Arguments:
    #   type - Type of deterministic part,
    #       mu:     constant
    #       tau:    constant with linear trend
    #   lags - Maximum number of lags used for error term correction, or
    #       short:  trunc(4 * (n/100)^0.25)
    #       long:   trunc(12 * (n/100)^0.25)
    #       nil:    0
    #   use.lag - User specified number of lags. 
    ###

    # KPSS | tau | short:
    kpss.gnp = ur.kpss(lgnp, type = "tau", lags = "short")
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ###

    # KPSS | tau | long:
    kpss.gnp = ur.kpss(lgnp, type = "tau", lags = "long")
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ###
        
    # KPSS | tau | 6 lags:
    kpss.gnp = ur.kpss(lgnp, type = "tau", use.lag = 6)
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ####
    
    # KPSS | mu | short:
    kpss.gnp = ur.kpss(dlgnp, type = "mu", lags = "short")
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ###
        
    # KPSS | mu | long:
    kpss.gnp = ur.kpss(dlgnp, type = "mu", lags = "long")
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ###
        
    # KPSS | mu | 6 lags:
    kpss.gnp = ur.kpss(dlgnp, type = "mu", use.lag = 6)
    print(kpss.gnp)
    plot(kpss.gnp)
    summary(kpss.gnp)
    ###


# ------------------------------------------------------------------------------


### Example: PP - Phillips & Perron Unit Root Test


    args(ur.pp)
    # function (x, type = c("Z-alpha", "Z-tau"), model = c("constant", 
    #   "trend"), lags = c("short", "long"), use.lag = NULL) 
    # Arguments:
    #   type
    #   model
    #   lags
    #   use.lag
    ###
    
    # PP | Ztau | trend | short:
    pp.gnp = ur.pp(gnp, type = "Z-tau", model = "trend", lags = "short")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Ztau | trend | long:
    pp.gnp = ur.pp(gnp, type = "Z-tau", model = "trend", lags = "long")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Ztau | trend | 6 lags:
    pp.gnp = ur.pp(gnp, type = "Z-tau", model = "trend", use.lag = 6)
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
        
    # PP | Ztau | constant | short:
    pp.gnp = ur.pp(diff(gnp), type = "Z-tau", model = "constant", 
        lags = "short")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Ztau | trend | long:
    pp.gnp = ur.pp(diff(gnp), type = "Z-tau", model = "constant", 
        lags = "long")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Ztau | constant | 6 lags:
    pp.gnp = ur.pp(diff(gnp), type = "Z-tau", model = "constant", 
        use.lags = 6)
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Zalpha | trend:
    pp.gnp = ur.pp(gnp, type = "Z-alpha", model = "trend")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###
    
    # PP | Zalpha | constant:
    pp.gnp = ur.pp(diff(gnp), type = "Z-alpha", model = "constant")
    print(pp.gnp)
    plot(pp.gnp)
    summary(pp.gnp)
    ###


# ------------------------------------------------------------------------------


### Example: SP - Schmidt & Phillips Unit Root Test


    args(ur.sp)
    # function (y, type = c("tau", "rho"), pol.deg = c(1, 2, 3, 4), 
    #   signif = c(0.01, 0.05, 0.1)) 
    ###

    # SP:
    sp.gnp = ur.sp(gnp, type = "tau", pol.deg = 1, signif = 0.05) 
    print(sp.gnp)
    plot(sp.gnp)
    summary(sp.gnp)
    ###
    
    # SP:
    ur.sp(gnp, type = c("tau", "rho"), pol.deg = c(1, 2, 3, 4), 
      signif = c(0.01, 0.05, 0.1)) 
    ###
    

# ------------------------------------------------------------------------------


### Example: ZA - Zivot and Andrews Unit Root Test


    args(ur.za)
    # function (y, model = c("intercept", "trend", "both"), lag) 
    ###

    # ZA | intercept | 2 lags:
    za.gnp = ur.za(gnp, model = "intercept", lag = 2)
    plot(za.gnp)
    summary(za.gnp)
    ###
        
    # ZA | trend | 2 lags:
    za.gnp = ur.za(gnp, model = "trend", lag = 2)
    plot(za.gnp)
    summary(za.gnp)
    ###
            
    # ZA | both | 2 lags:
    za.gnp = ur.za(gnp, model = "both", lag = 2)
    plot(za.gnp)
    summary(za.gnp)
    ###
        
    # ZA:
    za.gnp = urzaTest(gnp, model = "intercept", lag = 2)
    summary(za.gnp)
    ###
    
    # ZA:
    za.gnp = urzaTest(gnp, model = "trend", lag = 2)
    summary(za.gnp)
    ###
    
    # ZA:
    za.gnp = urzaTest(gnp, model = "both", lag = 2)
    summary(za.gnp)
    ###


################################################################################


#
# Example: 
#	Stationarit Tests
#


################################################################################
# Tests:
	
		
	# KPSS Test for Stationarity:
	# A time series which is level stationary:
	x = rnorm(1000)
	tskpssTest(x)
	# A time series which contains a unit-root:
	y = cumsum(x)
	tskpssTest(y)
	# A time series which is trend stationary:
	x = 0.3*(1:1000)+rnorm(1000)
	tskpssTest(x, null = "Trend")
	###


################################################################################

	


	# SPlus:
	x = (nelson.dat[,"RGNP"]@data)[,1]
	x = x[!is.na(x)] 
	x = exp(x[1:62])
	
	# - c - ct - nc - 
	lag = 1 # always one for "pp"
	bw = floor(4 * (length(x)/100)^(2/8)) 
	win  = "bartlett"
	
	round(unitroot(x, method = "pp", trend = "c",  stat = "t", lags = lag, 
		bandwidth = bw, window = win)$sval, 5)
	round(unitroot(x, method = "pp", trend = "ct", stat = "t", lags = lag, 
		bandwidth = bw, window = win)$sval, 5)
	
	round(unitroot(x, method = "pp", trend = "c",  stat = "n", lags = lag, 
		bandwidth = bw, window = win)$sval, 5)
	round(unitroot(x, method = "pp", trend = "ct", stat = "n", lags = lag, 
		bandwidth = bw, window = win)$sval, 5)
	
	
	
	
	# R - urca
	require(urca)
	data(nporg)
	x = nporg[, "gnp.r"]	
	x = x[!is.na(x)] 
	
	ur.pp(x, type = "Z-tau",   model = "constant")
	ur.pp(x, type = "Z-tau",   model = "trend")
	
	ur.pp(x, type = "Z-alpha", model = "constant")
	ur.pp(x, type = "Z-alpha", model = "trend")
	
	
	
	# tseries:
	flag = TRUE
	pp.test(x, alternative = "explosive",  type = "Z(t_alpha)", lshort = flag)$statistic 
	pp.test(x, alternative = "stationary", type = "Z(t_alpha)", lshort = flag)$statistic  
	pp.test(x, alternative = "explosive",  type = "Z(alpha)",   lshort = flag)$statistic  
	pp.test(x, alternative = "stationary", type = "Z(alpha)",   lshort = flag)$statistic  


	
	
# ------------------------------------------------------------------------------


\noindent In the next example we show how to reproduce the numbers
of the table performing a Monte carlo simulation:


\vspace{0.5cm}


\addtocounter{example}{+1}


\begin{footnotesize}
\textbf{$\square$ Example 3.5.\theexample: Compare Asymptotic and MC
Results for a Series of length 1000}

    \begin{verbatim}
    # Simulate p-values:
    > set.seed(1953)
    > nobs = nsim = 1000
    > DF = rep(0, nsim)
    > for (i in 1:nsim) {
        BN.moments = wiener(nobs)
        DF[i] = BN.moments$intWdW/sqrt(BN.moments$intW2) }

    # Compare Asymptotic Limit - Table vs MC:
    > STAT = as.numeric(adfTable("nc")["Inf", ])
    > PVAL.TAB = as.numeric(colnames.adfTable("nc")))
    > PVAL.MC = NULL
    > for (i in 1:length(STAT)) {
        PVAL.MC = c(PVAL.MC, length(DF[DF<=STAT[[i]]])/1000) }

    # Output:
    > data.frame(STAT, PVAL.TAB, PVAL.MC)

       STAT PVAL.TAB PVAL.MC
    1 -2.58    0.010   0.009
    2 -2.23    0.025   0.029
    3 -1.95    0.050   0.053
    4 -1.62    0.100   0.104
    5  0.89    0.900   0.898
    6  1.28    0.950   0.947
    7  1.62    0.975   0.973
    8  2.00    0.990   0.984
    \end{verbatim}

\end{footnotesize}



# ------------------------------------------------------------------------------





\noindent The following two examples show how to simulate time
series processes specified by equations (\ref{DF1}) and (\ref{DF3})
using recursive filtering implemented in the function
\texttt{filter()}. We test two cases $\phi=1$ and $\phi=0.99$ ...


\vspace{0.5cm}


\addtocounter{example}{+1}


\begin{footnotesize}
\textbf{$\square$ Example 1.5.\theexample: DF Test of Type
"no-constant"}

    \begin{verbatim}
    # Recursive Filter: x[i] = s[i] + filter*x[i-1]
    > set.seed(4711)
    > s = rnorm(5000)

    # phi=1 non-stationary Series:
    > x = as.ts(filter(x = s, filter = 1, method = "recursive"))
    > plot(x, type = "l", main = paste("phi =", phi))
    > .dfTest(x, type = "nc")

    # phi = 0.99 stationary Series
    > x = as.ts(filter(x = s, filter = 1, method = "recursive"))
    > plot(x, type = "l", main = paste("phi =", phi))
    > .dfTest(x, type = "nc")
    \end{verbatim}

\end{footnotesize}


\vspace{0.5cm}


\addtocounter{example}{+1}


\begin{footnotesize}
\textbf{$\square$ Example 1.5.\theexample: DF Test of Type "constant
+ trend"}

    \begin{verbatim}
    # Recursive Filter: x[i] = s[i] + filter*x[i-1]
    > set.seed(4711)
    > c1 = 0.01
    > c2 = 1e-6
    > s = c1 + c2*(1:5000) + rnorm(5000)

    # phi=1 non-stationary Series:
    > x = as.ts(filter(x = s, filter = 1, method = "recursive"))
    > plot(x, type = "l", main = paste("phi =", phi))
    > adfTest(x, type = "ct")

    # phi = 0.99 stationary Series
    > x = as.ts(filter(x = s, filter = 1, method = "recursive"))
    > plot(x, type = "l", main = paste("phi =", phi))
    > adfTest(x, type = "ct")
    \end{verbatim}

\end{footnotesize}



# ------------------------------------------------------------------------------


	.dfTest = function(x)
	{	
        # Regression and Summary:
        x.diff = diff(x)[-1]
        x.lag.1 = x[2:(length(x)-1)]
        LM = lm(x.diff ~ x.lag.1 - 1)
        test = list(lm = summary(LM))

        # Statistic:
        STAT = test$lm$coefficients[1]/test$lm$coefficients[2]
        names(STAT) = "Dickey-Fuller"
        test$statistic = STAT

        # P Value:
        PVAL = padf(q = STAT, n.sample = length(x), trend = "nc")
        names(PVAL) = ""
        test$p.value = PVAL

        # Return Value:
        new("fHTEST", call = match.call(), data = list(x = x),
            test = test, title = "Dickey Fuller Test", 
            description = " Test Type: nc")
    }
    

	set.seed(4711)
	x = rnorm(1001)
	.dfTest(x)
	x = diffinv(x)
	.dfTest(x)
    
# ------------------------------------------------------------------------------


### 3 Compute ADF Test for Chicken and Egg Data, compare with STATA/Splus


    # Compute the ADF Test for the "eggs" data and campare the results
    # with those obtained from the STATA and SPLUS functions:

    # Data:
    eggs = c(
        3581, 3532, 3327, 3255, 3156, 3081, 3166, 3443, 3424, 3561, 3640,
        3840, 4456, 5000, 5366, 5154, 5130, 5077, 5032, 5148, 5404, 5322,
        5323, 5307, 5402, 5407, 5500, 5442, 5442, 5542, 5339, 5358, 5403,
        5345, 5435, 5474, 5540, 5836, 5777, 5629, 5704, 5806, 5742, 5502,
        5461, 5382, 5377, 5408, 5608, 5777, 5825, 5625, 5800, 5656)
    chic = c(
        468491, 449743, 436815, 444523, 433937, 389958, 403446, 423921, 
        389624, 418591, 438288, 422841, 476935, 542047, 582197, 516497, 
        523227, 467217, 499644, 430876, 456549, 430988, 426555, 398156, 
        396776, 390708, 383690, 391363, 374281, 387002, 369484, 366082, 
        377392, 375575, 382262, 394118, 393019, 428746, 425158, 422096, 
        433280, 421763, 404191, 408769, 394101, 379754, 378361, 386518, 
        396933, 400585, 392110, 384838, 378609, 364584)
    year = 1930:1983
    ###
    
    # Plot:
    plot(chic, type = "l")  
    ### 
      
    # STATA - Results:
    statisticSTATA = c(nc = -0.712, c = -1.618, ct = -1.998)
    p.valueSTATA   = c(nc = NA,     c =  0.474, ct =  0.603)   
    # SPLUS - Results:
    if (FALSE) {
        # Splus Code:
        chic = as.vector(ts[, 1])
        unitroot(x = chic, trend = "nc", lags = 2, asymptotic = FALSE)
        unitroot(x = chic, trend = "c",  lags = 2, asymptotic = FALSE)
        unitroot(x = chic, trend = "ct", lags = 2, asymptotic = FALSE) }    
    statisticSPLUS = c(nc = -0.7122, c = -1.618,  ct = -1.998 )
    p.valueSPLUS   = c(nc =  0.4034, c =  0.4663, ct =  0.5886) 
    ###
  
    # Start Settings:
    ans1 = ans2 = NULL
    rowNames = c("nc", "c", "ct")   
    # Compute from adfTest:
    for (type in rowNames) {
        res1 = adfTest(x = chic, type = type, lags = 1)
        res2 = unitrootTest(x = chic, type = type, lags = 1)
        ans1 = rbind(ans1, c(res1$statistic, res1$p.value)) 
        ans2 = rbind(ans2, c(res2$statistic, res2$p.value)) 
    }        
    # Bind:
    ans = cbind(ans1, ans2)    
    # Add SPLUS results:
    ans = cbind(round(ans, digits = 3), statisticSPLUS, p.valueSPLUS)    
    # Add STATA results:
    ans = cbind(round(ans, digits = 3), statisticSTATA, p.valueSTATA)   
    # Add row and column names:
    rownames(ans) = rowNames
    colnames(ans) = c(
        "sADF", "pADF", "sJGM", "pJGM", "sSPLUS", "pSPLUS", "sSTATA", "pSTATA")
    ###
    
    # Print All Result:
    print(ans)
    ###
    