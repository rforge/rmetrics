#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 3.3
#   Time Series with Long Range Dependence
#
# List of Examples, Exercises and Code Snippets:
#
#   3.3.1 Example: Simulate Gaussian FARIMA Model
#       * Code Snippet: fbmSim
#   3.3.2 Example: Simulate Fractal Brownian Motion
#   3.3.3 Example: Simulate Fractal Gaussian Noise
#   3.3.4 Example: Simulate FGN by Beran's, Durbin's, and Paxson's Methods
#   3.3.5 Example: True FARIMA Covariances and Fourier Transform
#       * Example: True FGN Covariances and Fourier Transform
#   3.3.6 Example: Aggregated Variance Method
#   3.3.7 Example: Differenced Aggregated Variance Method
#   3.3.8 Example: Absolute Values of the Aggregated Series
#   3.3.9 Example: Higuchi's or Fractal Dimension Method
#   3.3.10 Example: Peng's or Variance of Residuals Method
#   3.3.12 Example: The R/S Method
#   3.3.13 Example: The Periodogram Method
#   3.3.14 Example: The Boxed or Modified Periodogram Method
#   3.3.15 Example: The Whittle Estimator
#   3.3.16 Example: Averaged Hurst Exponent from 50 Samples
#   3.3.17 Example: The Wavelet Estimator
#
#
# Author:
#	(C) 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################
# Time Series Simulation


### 3.3.1 Example: Simulate Gaussian FARIMA Model

	# Create Figure 3.3.1
	###
	
    # FARIMA(0, d, 0)
    x.ts = farimaSim(n = 100, model = list(d = 0.25))
    print(x.ts)
    par(mfrow = c(2, 2), cex = 0.7)
    plot(x.ts, main = "FARIMA(0, 0.25, 0)", col = "steelblue")
    grid()
    ###

    # Try also: FARIMA(2, d, 1)
    mode = model = list(ar = c(0.5, -0.5), d = 0.25, ma = 0.1)
    x.ts = farimaSim(n = 100, model = model)
    plot(x.ts, main = "FARIMA(2, 0.25, 1)", col = "steelblue")
    grid()
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snippet: fbmSim

    # Show Argument List for 'fbmSim':
    > args(fbmSim)
    # function(n = 100, H = 0.7,
    #    method = c("mvn", "chol", "lev", "circ", "wave"),
    #    waveJ = 7, doplot = TRUE, fgn = FALSE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.2 Example: Simulate Fractal Brownian Motion

    # All 5 Methods:
    methods = c("mvn", "chol", "lev", "circ", "wave")
	###
	
    # Generate FBM Series and Plot it:
    par (mfrow = c(3, 2), cex = 0.7)
    for (method in methods) {
        set.seed(1953)
        fbmSim(1000, method = method, doplot = TRUE)
    }
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.3 Example: Simulate Fractal Gaussian Noise

    # Graph Frame:
	par (mfrow = c(3, 2), cex = 0.7)
	###
	
	# All 5 Methods:
    methods = c("mvn", "chol", "lev", "circ", "wave")
	###
	
    # Generate FGN Series and Plot it:
    for (method in methods) {
        set.seed(1953)
        fbmSim(1000, method = method, doplot = TRUE, fgn = TRUE)
    }
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.4 Example: Simulate FGN by Beran's, Durbin's and Paxson's Methods

   	# Graph Frame:
	par (mfrow = c(3, 1), cex = 0.7)
	###
	
	# Beran's Method: 
   	plot(fgnSim(n = 200, H = 0.75), type = "l", 
   		col = "steelblue", 
     	ylim = c(-3, 3), xlab = "time", ylab = "x(t)", 
     	main = "FGN Beran")
	grid()
	###
   
	# Durbin's Method:
	plot(fgnSim(n = 200, H = 0.75, method = "durbin"), type = "l",
		col = "steelblue",
	 	ylim = c(-3, 3), xlab = "time", ylab = "x(t)", 
	 	main = "FGN Durbin")
	grid()
	###
	
	# Paxson's Method:
	plot(fgnSim(n = 200, H = 0.75, method = "paxson"), 
		type = "l", col = "steelblue",
	 	ylim = c(-3, 3), xlab = "time", ylab = "x(t)", 
	 	main = "FGN Paxson")
    grid()
    ###
    
    
################################################################################
# True Model Statistics


### Example 3.3.5: True FARIMA Covariances and Fourier Transform
			
	# Graph Frame:
	par (mfrow = c(2,2), cex = 0.7)
	###
	
	# Parameters:
	n = 10000
	d = 0.2
	H = d + 0.5
	set.seed(1953)
	x = farimaSim(n, model = list(d = 0.2))
	###
	
	# Sample and True ACF:
	acf(x, main = "FARIMA: ACF")
	y = farimaTrueacf(41, H)
	points(0:40, y, col = "red")
	###
	
	# Simple - Fit Hurst Exponent from True ACF:
	coef = lsfit(x = log(1:40), y = log(y[-1]))$coef
	beta = coef[[2]]
	H.estimated = (beta+2)/2
	H.estimated
	###
	
	# Sample and True Spectrum:
	s = spectrum(x, plot = FALSE)$spec
	plot(s, type = "h", ylim = c(0, 60), main = "FARIMA: Spectrum")
	y = Re(farimaTruefft(n, H))
	lines(0:(n/2-1), y[1:(n/2)], col = "red")
	###
	
	# Double Logarithmic Plot:
	# plot(log(1:length(s)), log(s), type = "l", main = "log-log Spectrum")
	# lines(log(1:(n/2)), log(y[1:(n/2)]), col = "red")
	###
	
	# Simple Fit - Hurst Exponent from True Spectrum:
	coef = lsfit(x = log(1:length(s)), y = log(s))$coef
	beta = coef[[2]]
	H = (1-beta)/2
	H
	###
	
	
# ------------------------------------------------------------------------------
	

### Example: True FGN Covariances and Fourier Transform	
	
	# Parameters:
	n = 10000
	H = 0.7
	set.seed(1953)
	x = fgnSim(n, H)
	###
	
	# Sample and True ACF:
	acf(x, main = "FGN: ACF")
	y = fgnTrueacf(41, H)
	points(0:40, y, col = "red")
	###
	
	# Simple Fit - Hurst Exponent from True ACF:
	coef = lsfit(x = log(1:40), y = log(y[-1]))$coef
	beta = coef[[2]]
	H.estimated = (beta+2)/2
	H.estimated
	###
	
	# Sample and True Spectrum:
	s = spectrum(x, plot = FALSE)$spec
	plot(s, type = "h", ylim = c(0, 60), main = "FGN: Spectrum")
	y = Re(fgnTruefft(n, H))
	lines(0:(n/2-1), y[1:(n/2)], col = "red")
	###
	
	# Double Logarithmic Plot:
	# plot(log(1:length(s)), log(s), type = "l", main = "log-log Spectrum")
	# lines(log(1:(n/2)), log(y[1:(n/2)]), col = "red")
	###
	
	# Simple Fit - Hurst Exponent from True Spectrum:
	coef = lsfit(x = log(1:length(s)), y = log(s))$coef
	beta = coef[[2]]
	H = (1-beta)/2
	H
	###


# ------------------------------------------------------------------------------	
		
	
### Code Snippet: fHURST
	
	# Get Class:
	getClass("fHURST")
	###
	
	
################################################################################
# Estimation of the Hurst Exponent


### 3.3.6 Example: Aggregated Variance Method

    # Graphics Frame - Figure 3.3.6:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Fit - Method 1:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    aggvarFit(x, cut.off = 10^c(0.7, 3.0), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------	
   

### 3.3.7 Example: Differenced Aggregated Variance Method

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 2:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    diffvarFit(x, cut.off = 10^c(0.7, 2.5), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------	
   

### 3.3.8 Example: Absolute Values of the Aggregated Series

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 3:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    absvalFit(x, cut.off = 10^c(0.7, 3), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------	
   

### 3.3.9 Example: Higuchi's or Fractal Dimension Method

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 4:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    higuchiFit(x, cut.off = 10^c(0.7, 3), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------	
   

### 3.3.10 Example: Peng's or Variance of Residuals Method

    # Graphics Frame - Figure 3.3.6:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Fit - Method 5:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    pengFit(x, cut.off = 10^c(0.7, 3.0), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.12 Example: The R/S Method

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 6:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    rsFit(x, cut.off = 10^c(0.7, 2.5), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.13 Example: The Periodogram Method

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 7:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    perFit(x, cut.off = 0.10, doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.14 Example: The Boxed or Modified Periodogram Method

    # Graphics Frame - continue ...
    ###
    
    # Fit - Method 8:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    boxperFit(x, cut.off = 0.10, doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.3.15 Example: The Whittle Estimator

	# Fit - Method 9:
    set.seed(1953)
    x = fgnSim(n = 10000, H = 0.7)
    whittleFit(x)
    ###


# ------------------------------------------------------------------------------


### 3.3.16 Example: Averaged Hurst Exponent from 50 Samples

   	# Graphics Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    ###
    
    # Be patient, this will take some time ...
    set.seed(1953)
    nominalH = 0.7
    H1 = H2 = H3 = H4 = H5 = H6 = H7 = H8 = H9 = NULL
    for (i in 1:50) {
        x = fgnSim(n = 10000, H = nominalH)
        NEXT.H1 = aggvarFit(x, cut.off = 10^c(0.7, 3.0))@hurst$H
          H1 = c(H1, NEXT.H1)
        NEXT.H2 = diffvarFit(x, cut.off = 10^c(0.7, 2.5))@hurst$H
          H2 = c(H2, NEXT.H2)
        NEXT.H3 = diffvarFit(x, cut.off = 10^c(0.7, 3.0))@hurst$H
          H3 = c(H3, NEXT.H3)
        NEXT.H4 = higuchiFit(x)@hurst$H
          H4 = c(H4, NEXT.H4)
        NEXT.H5 = pengFit(x)@hurst$H
          H5 = c(H5, NEXT.H5)
        NEXT.H6 = rsFit(x)@hurst$H
          H6 = c(H6, NEXT.H6)
        NEXT.H7 = perFit(x, cut.off = 0.10)@hurst$H
          H7 = c(H7, NEXT.H7)
        NEXT.H8 = boxperFit(x)@hurst$H
          H8 = c(H8, NEXT.H8)
        NEXT.H9 = whittleFit(x)@hurst$par
          H9 = c(H9, NEXT.H9)
        cat("\n", i, round(
            c(NEXT.H1, NEXT.H2, NEXT.H3, NEXT.H4, NEXT.H5,
              NEXT.H6, NEXT.H7, NEXT.H8, NEXT.H9), digits = 4))
    }
    # Combine Columnwise to a Matrix:
    H.mat = cbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
    ###
    
    # Mean Values:
    round(colMeans(H.mat), digits = 3)
    #    H1    H2    H3    H4    H5    H6    H7    H8    H9
    # 0.672 0.706 0.733 0.671 0.686 0.708 0.707 0.662 0.700
    ###
    
    # Standard Deviations:
    round(colStdevs(H.mat), digits = 3)
    #    H1    H2    H3    H4    H5    H6    H7    H8    H9
    # 0.034 0.061 0.043 0.026 0.015 0.075 0.021 0.021 0.007
    ###
    
    # Create a Box Plot:
    boxplot(data.frame(H.mat)-nominalH)
    ###
    
    
################################################################################
# Wavelet Estimator


### 3.3.17 Example: Wavelet Estimator Using Simulated Data

    # Fit:
    set.seed(1953)
    x = fgnSim(n = 1000, H = 0.7, method = "beran")
    waveletFit(x)
    ###

    # Have a Look on Changing the Octave Setting:
    H = NULL
    for (end in 3:8)
       H = c(H, waveletFit(x = x, octave = c(2, end))@hurst$H)
    data.frame(end = 3:8, H)
    #   end         H
    # 1   3 0.5662117
    # 2   4 0.5587855
    # 3   5 0.6892903
    # 4   6 0.8712615
    # 5   7 0.8712615
    # 6   8 0.8712615
    ###
  
    
# ------------------------------------------------------------------------------
  

### Example: Wavelet Estimator Using Market Data
    
	data(usdchf)
	USDCHF.TS = as.timeSeries(usdchf, format = "%Y%m%d%H%M")
	x = USDCHF.VOLATILITY = absSeries(returnSeries(USDCHF.TS))
	
	
# ##############################################################################

	