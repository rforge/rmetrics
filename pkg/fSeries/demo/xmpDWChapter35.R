#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 3.5
#   Nonlinear and Chaotic Time Series
#
# List of Examples, Exercises and Code Snippets:
#
#   3.5.1 Example: Plot Tent Map
#       * Example: Plot Logistic Map
#   3.5.2 Example: Bifurcation Diagram of the Logistic Map
#   3.5.3 Example: Plot Henon Map
#   3.5.4 Example: Plot Ikeda Map
#       * Code Snippet: 'ikedaSim' Function
#   3.5.5 Example: Plot Lorentz Attractor  
#   3.5.6 Example: Plot Roessler Attractor 
#       * Example: Roessler ODE Solver Test
#       * Example: Lorentz ODE Solver Test
#   3.5.7 Example: Determine Time Delay for Roessler Map
#   3.5.8 Example: Determine Embedding Dimension for Roessler Map
#       * Example: USDDEM - Mutual Information
#       * Example: Roessler - Phase Space Reconstruction
#       * Example: Lorentz - Phase Space Reconstruction
#       * Example: Lorentz Attractor - 3D Plot
#		* Example: Lyapunov Slider
#		* Example: False Nearest Neighbours Slider
#		* Example: Henon Map Slider
#		* Example: Teraesvirta Neural Network Test for Nonlinearity
#	    * Example: White Neural Network Test for Nonlinearity
#		* Example: BDS Time Series Test
#
# Author:
#	(C) 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################
# Simulation of Chatic Time Series:


### 3.5.1 Example: Plot Tent Map

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
		
	# Tent Map:
	a = 1.99
	x = tentSim(n = 1000, parms = a, start = pi/8, doplot = TRUE)[ ,1]
	###
		
	# Plot Series:
	plot(x, type = "l", col = "steelblue")	
	abline(h = 0.5, col = "grey", lty = 3)
	title(main = paste("Tent Map \na =", as.character(a)))
	###	
	
	# Tent Map - Another Control Parameter:
	a = sqrt(2)
	x = tentSim(n = 1000, parms = a, start = pi/8, doplot = TRUE)[ ,1]
	plot(x, type = "l", col = "steelblue")	
	abline(h = 0.5, col = "grey", lty = 3)
	title(main = paste("Tent Map \na =", as.character(a)))
	###

	
# ------------------------------------------------------------------------------


### Example: Plot Logistic Map

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Logistic Map:
	for (r in c(3, 4)) {
		# Map:
		x = logisticSim(parms = r, start = pi/8, doplot = TRUE)[, 1]
		# Series:
		plot(x, type = "l", col = "steelblue")	
		title(main = paste("Logistic Series \nr =", r))
	}
	###


	
# ------------------------------------------------------------------------------


### 3.5.2 Example: Bifurcation Diagram of the Logistic Map

	# Graph Frame:
	par(mfrow = c(2, 1), cex = 0.7)
	plot(c(0,4), c(0,1), type = "n", xlab = "r", ylab = "x")
	title(main = "Logisit Map"))
	###
	
	# Bifurcation Diagram:
	for ( r in seq(0, 4, length = 801) ) {
		N = floor(r * 200 + 1)
		print(N)
		x = rep(r, times = N)
		y = logisticSim(n = N, n.skip = 100, parms = r, start = pi/8)[ ,1]
		# Fill Diagram:
		points(x, y, cex = 0.25, col = "steelblue")
	}
	###
	
	# Exercise: 
	# Create Bifurcation Diagram of the Tent Map
	###
	
	
# ------------------------------------------------------------------------------


### 3.5.3 Example: Plot Henon Map

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
		
	# Show the strange attractor for a=1.4 and b=0.3:
	x = henonSim(doplot = TRUE)
	plot.ts(x[, 1], col = "steelblue", xlab = "x")
	title(main = "Henon Series \n a = 1.4  b = 0.3")
	###

	# Show the evolution of the point (0,0) for parameters 
	# a=0.2 and b=0.9991 and b=-0.9999
	x = henonSim(n = 5000, n.skip = 0, parms = c(a = 0.2, b = 0.991), 
    	start = c(0, 0), doplot = TRUE)
    x = henonSim(n = 5000, n.skip = 0, parms = c(a = 0.2, b = -0.9999), 
    	start = c(0, 0), doplot = TRUE)
    ###  	
    	
    	
# ------------------------------------------------------------------------------


### 3.5.4 Example: Plot Ikeda Map

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
		
	# Show the strange attractor for the default
    # setting a=0.4, b=6, and c=0.9:
    z = ikedaSim(doplot = TRUE)
    ###

    # Series Plots - the first 100 Values:
    n = 1:100
    plot(n, z[1:100, 1], col = "steelblue",
        ylab = "x = Re(z)", type = "l",
        main = "Ikeda Series \n a = 0.4  b = 6  c = 0.9")
    plot(n, z[1:100, 2], col = "steelblue",
        ylab = "y = Im(z)", type = "l",
        main = "Ikeda Series \n a = 0.4  b = 6  c = 0.9")
    ###
    	
    	
# ------------------------------------------------------------------------------


### Code Snippet: 'ikedaSim' Function

    # Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Write a function to generate the Ikeda Map - Use complex arithmetic:
	.ikedaSim =
    function (n = 1000, parms = c(a = 0.4, b = 6, c = 0.9))
    {
        # Settings:
        a = complex(real = 0, imag = parms[1])
        b = complex(real = 0, imag = parms[2])
        c = parms[3]
        z = rep(complex(real = 0, imag = 0), times = n)
        # Iterate:
        for (i in 2:n)
            z[i] = 1 + c*z[i-1]*exp(a-b/(1+abs(z[i-1])^2))
        # Return Value:
        data.frame(Re(z), Im(z))
    }
    ###

    # Try:
    plot(.ikedaSim(), cex = 0.25, main = "Ikeda Map")
    ###

        
# ------------------------------------------------------------------------------


### 3.5.5 Example: Plot Lorentz Attractor

	# Graph Frame:
	par(mfcol = c(3, 2), cex = 0.7)
	###
	
	# Lorentz Attractor - Use default values:
    ans = lorentzSim(times = seq(0, 40, by = 0.01),
        parms = c(sigma = 16, r = 45.92, b = 4),
        start = c(-14, -13, 47), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.5.6 Example: Plot Roessler Attractor

	# Graph Frame:
	par(mfcol = c(3, 2), cex = 0.7)
	###
	
	# Roessler Attractor - Use default values:
    ans = roesslerSim(times = seq(0, 100, by = 0.01),
        parms = c(a = 0.2, b = 0.2, c = 8),
        start = c(-1.894, -9.92, 0.025), doplot = TRUE)
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Roessler ODE Solver Test

	# Compare the Roessler maps generated by the 'tseriesChaos'
	# and Rmetrics 'fSeries' package by computing their mutual
	# information.
	# Note, the difference is in usage of the Runge-Kutta solver
	# in 'fSeries' and of the "lsoda" solver in 'tseriesChaos'.
	###
	
	# Graph Frame:
	par(mfrow = c(2,2), cex = 0.7)
	###
	
	# What delivers 'tseriesChaos' ?
	require(tseriesChaos)
	lags = 60
	plot(rossler.ts, main = "lsoda Solver")
	m = mutual(rossler.ts, lag.max = lags)
	plot(0:lags, m, type = "b")
	###
	
	# What delivers 'fSeries' ?
	rossler2.mat = roesslerSim(time = seq(0, 650, by = 0.1),  
		parms = c(a = 0.15, b = 0.2, c = 10), 
		start = c(0, 0, 0), doplot = FALSE )
	rossler2.ts = ts(rossler2.mat[,2], start = 0, end = 650, deltat = 0.1)
	plot(rossler2.ts, main = "rk4 Solver")
	m2 = mutual(rossler2.ts, lag.max = lags)
	plot(0:lags, m2, type = "b")
	points(0:lags, m, pch = 19, col = "red", cex = 0.5)
	###
	
	# Almost the same ...
	###
	
		
# ------------------------------------------------------------------------------


### Example: Lorentz ODE Solver Test
	
	# Graph Frame:
	par(mfrow = c(2,2), cex = 0.7)
	###
	
	# What delivers 'tseriesChaos' ?
	require(tseriesChaos)
	lags = 60
	plot(lorenz.ts, main = "lsoda Solver")
	m = mutual(lorenz.ts, lag.max = lags)
	plot(0:lags, m, type = "b")
	###
	
	# What delivers 'fSeries' ?
	# Note the definition of the sign of 'b' is opposite!
	lorenz2.mat = lorentzSim(time = seq(0, 100, by = 0.05),  
		parms = c(sigma = 10, r = 28, b = 8/3), 
		start = c(5, 5, 5), doplot = FALSE )
	lorenz2.ts = ts(lorenz2.mat[,4], start = 0, end = 100, deltat = 0.05)
	plot(lorenz2.ts, main = "rk4 Solver")
	m2 = mutual(lorenz2.ts, lag.max = lags)
	plot(0:lags, m2, type = "b")	
	points(0:lags, m, pch = 19, col = "red", cex = 0.5)
	###
	
	# Almost the same ...
	###
	
	m3 = mutual(as.vector(lorenz2.ts), lag.max = lags)
	points(0:lags, m3, pch = 19, col = "blue", cex = 0.5)
	###
	
	
# ------------------------------------------------------------------------------


### 3.5.7 Example: Determine Time Delay for Roessler Map

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Create Default Series:
	deltat = 0.01
	times = seq(0, 100, by = deltat)
	roessler.dat = roesslerSim(times, start = c(4.72, 1.43, 0.015),
		doplot = FALSE)
	roessler.x = roessler.dat[, 2]
	roessler.ts = ts(roessler.x, start = 0, end = 100, deltat = deltat)
	plot(roessler.ts, main = "Series: Roessler", col = "steelblue")
	###
	
	# Determine tau from ACF:
	ac = acfPlot(roessler.ts, lag.max = 2/deltat, 
		main = "Autocorrelation", col = "steelblue")$acf
	tau.ac = length(ac[ac>0])
	abline(v = tau.ac, lty = 3)
	print(tau.ac)
	###
		
	# Determine tau from Mutual Information:
	mi = mutualPlot(roessler.ts, partitions = 20, lag.max = 2*tau.ac)
	tau.mi = which(mi == min(mi))[[1]] - 1
	abline(v = tau.mi, lty = 3)
	print(tau.mi)
	###
	
	
# ------------------------------------------------------------------------------


### 3.5.8 Example: Determine Embedding Dimension for Roessler Map

	# First execute Example 3.5.7, if not already done just before ...
	###
	
	# Determine Embedding m from false nearest neigbours:
	falsennPlot(roessler.ts, m = 6, d = tau.mi, t = 40, rt = 10, ylim = c(0,1)) 
	abline(h = 0, lty = 3) 
	###

	
# ------------------------------------------------------------------------------	
		

### Example: USDDEM - Mutual Information

	USDDEM = as.timeSeries(data(usddem30u))
	USDDEM.RET = returnSeries(USDDEM, digits = 12)[, 1]
	usddem.ts = as.ts(as.vector(USDDEM.RET))
	M = mutual(usddem.ts, lag.max = 100)
	plot(log(M))
	###
	
	usddemVol.ts = as.ts(100*abs(as.vector(USDDEM.RET)))
	M = mutual(usddemVol.ts, partitions = 22, lag.max = 672)
	plot(log(M))
	acf(usddemVol.ts, lag.max = 336)
	###
	
	usddemRet.ts = as.ts(100*as.vector(USDDEM.RET))
	M = mutual(usddemVol.ts, partitions = 22, lag.max = 672)
	plot(M[-1])
	acf(usddemRet.ts, lag.max = 672)
	###
	
	# Try an iid sequence:
	N = length(usddem.ts)
	Mean = mean(100*usddem.ts)
	Sd = sd(100*usddem.ts)
	rnorm.ts = as.ts(rnorm(N, Mean, Sd))
	M = mutual(rnorm.ts, partitions = 22, lag.max = 672)
	plot(M[-1], type = "b", pch = 19)
	###
	

# ------------------------------------------------------------------------------


### Example: Roessler - Phase Space Reconstruction

	# Determine m from false nearest neigbours:
	falsennPlot(roessler.ts, m = 6, d = tau.mi, t = 40, rt = 10, ylim = c(0,1)) 
	abline(h = 0, lty = 3) 
	
	# Reconstruction:
	par(mfrow = c(2, 2), cex = 0.7)
	for (d in c(50, 150, 250)) {
		x = .embeddPSR(roessler.ts, m = 3, d = d)
		plot(x[,1], x[,2], type = "l", col = "steelblue")
		title (main = paste("Delay =", d))
	}
	plot(roessler.dat[, 2:3], type = "l", col = "steelblue")
	title(main = "Roessler")
	###
	
	
# ------------------------------------------------------------------------------
 
    
### Example: Lorentz - Phase Space Reconstruction

	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)
	###
	
	# Create Default Series:
	parms.map = c(sigma = 16, r = 46, b = 4)
	start.map = c(-12.3, -12.5, 43)
	lorentz.dat = lorentzSim(times, parms = parms.map, start = start.map, 
		doplot = FALSE)
	lorentz.z = lorentz.dat[, 2]
	lorentz.ts = ts(lorentz.z, start = 0, end = 40, deltat = 0.01)
	plot(lorentz.ts, main = "Series", col = "steelblue")
	##
	
	# Determine tau from ACF:
	ac = acfPlot(lorentz.ts, lag.max = 100, main = "", col = "steelblue")$acf
	title(main = "Autocorrelation")
	tau.ac = length(ac[ac>0])
	abline(v = tau.ac, lty = 3)
	print(tau.ac)
	###
		
	# Determine tau from Mutual Information:
	mi = mutualPlot(lorentz.ts, partitions = 22, lag.max = 20)
	tau.mi = which(mi == min(mi))[[1]] - 1
	abline(v = tau.mi, lty = 3)
	print(tau.mi)
	###
	
	# Determine m from false nearest neigbours:
	falsennPlot(lorentz.ts, m = 6, d = tau.mi, t = 100, rt = 10, ylim = c(0,1)) 
	abline(h = 0, lty = 3)
	# Result is 3 ... 6
	###
	
	# Reconstruction:
	par(mfrow = c(2, 2), cex = 0.7)
	for (d in c(5, 10, 15)) {
		x = .embeddPSR(lorentz.ts, m = 3, d = d)
		plot(x[,1], x[,2], type = "l", col = "steelblue")
		title (main = paste("Delay =", d))
	}
	plot(lorentz.dat[, 2:3], type = "l", col = "steelblue")
	title(main = "Lorentz")
	###
	

# ------------------------------------------------------------------------------


### Example: Lorentz Attractor - 3D Plot

	require(scatterplot3d)
	par(mfrow = c(2, 2), cex = 0.7)
	lorentz.mat = lorentzSim(doplot = FALSE)[, -1]
	scatterplot3d(lorentz.mat, type = "l", color = "steelblue",
		main = "Lorentz")
	roessler.mat = roesslerSim(doplot = FALSE)[, -1]
	scatterplot3d(roessler.mat, type = "l", , color = "steelblue",
		main = "Roessler")
   	###
   	

# ------------------------------------------------------------------------------


### Example: Lyapunov Slider

	x = logisticSim(n = 5000, parms = c(r = 4), start = pi/8)[,1]
	lyapunovPlot(x, m=1, d=1, t=40, ref=200, s=100, eps=2, k = 1, doplot = TRUE) 

	x = logisticSim(n = 5000, parms = c(r = 4), start = pi/8)[,1]
	falsennPlot(x, m=10, d=1, t=10, rt=10)
	
	# Write Function:
	.logisticLyapunovSlider =
	function()
	{   # A function implemented by Diethelm Wuertz
	
	    # FUNCTION:
	    
	    # Graph Frame:
	    par(mfrow = c(1, 1), cex = 0.7)
	    
	    # Internal Function:
	    refresh.code = function(...)
	    {
	        # Sliders:
	        N = .sliderMenu(no = 1)
	        r = .sliderMenu(no = 2)
	        m = .sliderMenu(no = 3)
	        d = .sliderMenu(no = 4)
	        t = .sliderMenu(no = 5)
	        ref = .sliderMenu(no = 6)
	        s = .sliderMenu(no = 7)
	        eps = .sliderMenu(no = 8)
	        
	        # Plot Data:     
	        x = logisticSim(n = N, parms = c(r = r), start = pi/8)[,1]
			y = lyapunovPlot(x, m, d, t, ref, s, eps, k=1, doplot = FALSE)
			plot.ts(y, ylim = c(-10, 2))
			abline(h = 0, lty = 3)
	        
	        # Reset Frame:
	        par(mfrow = c(1, 1), cex = 0.7)
	    }
	  
	    # Open Slider Menu:
	    .sliderMenu(refresh.code,
	       names =       c(  "N",  "r", "m", "d", "t", "ref", "s", "eps"),
	       minima =      c( 1000,    0,   1,   1,   5,    10,  10,    10),
	       maxima =      c(10000,    4,  10,  10, 100,   500, 500,   500),
	       resolutions = c(  500,  0.1,   1,   1,   5,    10,  10,    10),
	       starts =      c( 1000,    4,   1,   1,  20,    10,  10,    10))
	}
		
	
	.logisticLyapunovSlider()


# ------------------------------------------------------------------------------


### Example: False Nearest Neighbours Slider

	x <<- logisticSim(n = 5000, parms = c(r = 4), start = pi/8)[,1]
	
	# Write Function:
	.falsennSlider =
	function()
	{   # A function implemented by Diethelm Wuertz
	
	    # FUNCTION:
	    
	    # Graph Frame:
	    par(mfrow = c(1, 1), cex = 0.7)
	    
	    # Internal Function:
	    refresh.code = function(...)
	    {
	        # Sliders:
	        m = .sliderMenu(no = 1)
	        d = .sliderMenu(no = 2)
	        t = .sliderMenu(no = 3)
	        rt = .sliderMenu(no = 4)
	       
	        # Plot Data:     
			y = falsennPlot(x, m, d, t, rt, doplot = TRUE)
	
	        
	        # Reset Frame:
	        par(mfrow = c(1, 1), cex = 0.7)
	    }
	  
	    # Open Slider Menu:
	    .sliderMenu(refresh.code,
	       names =       c(  "m", "d", "t", "rt"),
	       minima =      c(   1,   1,   5,   10),
	       maxima =      c(  10,  10, 100,  500),
	       resolutions = c(   1,   1,   5,   10),
	       starts =      c(   1,   1,  20,   10))
	}
	.falsennSlider()
	
	
# ------------------------------------------------------------------------------	


### Example: Henon Map Slider

	# Write Function:
	.henonSlider =
	function()
	{   # A function implemented by Diethelm Wuertz
	
	    # Henon Map
	    #   henonSim(x, mean = 0, sd = 1)
	        
	    # FUNCTION:
	    
	    # Graph Frame:
	    par(mfrow = c(2, 2), cex = 0.7)
	    
	    # Internal Function:
	    refresh.code = function(...)
	    {
	        # Sliders:
	        N = .sliderMenu(no = 1)
	        skip = .sliderMenu(no = 2)
	        a = .sliderMenu(no = 3)
	        b = .sliderMenu(no = 4)
	        cex = .sliderMenu(no = 5)
	        
	        # Plot Data:     
	        ans = henonSim(n = N, n.skip = skip, parms = c(a=a, b=b), 
	        	start = c(0, 0), doplot = FALSE)
	        	
	        test1 = FALSE
	        test1 = any(!is.finite(c(ans[,1], ans[,2])))
	        test2 = FALSE
	        test2 = any(is.na(c(ans[,1], ans[,2])))
	        
	        if (!test1 & !test2) {    
	        	print(head(ans))
	        	plot(ans, pch = 19, cex = cex)  
	        	title(main = paste("Henon Map \n a = ",
	        		as.character(a), " b =", as.character(b)))
	        	plot(ans[,1], type = "p", pch = 19, cex = cex)
	        	plot(ans[,1], type = "l", cex = cex)
	    	}
	        
	        # Reset Frame:
	        par(mfrow = c(2, 2), cex = 0.7)
	    }
	  
	    # Open Slider Menu:
	    .sliderMenu(refresh.code,
	       names =       c(  "N",  "skip",  "a",   "b",  "cex"),
	       minima =      c( 1000,      0,     0,    -1,   0.10),
	       maxima =      c(10000,    100,     5,    +1,   2.00),
	       resolutions = c(  500,      1,  0.01,  0.01,   0.05),
	       starts =      c( 1000,    100,  1.40,  0.30,   0.25))
	}
	

# ------------------------------------------------------------------------------


### Example: Teraesvirta Neural Network Test for Nonlinearity	
	
	# Time Series:
	par(mfrow = c(2,2), cex = 0.6)
	n = 1000
	x = runif(1000, -1, 1)  
	x = as.ts(x)
	plot(x)
	tnnTest(x)
	###
		
	# Generate time series which is nonlinear in mean:
	x[1] = 0.0
	for (i in (2:n)) {
		x[i] = 0.4*x[i-1] + tanh(x[i-1]) + rnorm (1, sd = 0.5) }
	x = as.ts(x)
	plot(x)
	tnnTest(x)
	###
	

# ------------------------------------------------------------------------------


### Example: White Neural Network Test for Nonlinearity

	# Time Series:
	n = 1000
	x = runif(1000, -1, 1)  
	x = as.ts(x)
	plot(x)
	wnnTest(x)
	###
		
	# Generate time series which is nonlinear in mean:
	x[1] = 0.0
	for (i in (2:n)) 
		x[i] = 0.4*x[i-1] + tanh(x[i-1]) + rnorm (1, sd = 0.5) 
	x = as.ts(x)
	plot(x)
	wnnTest(x)
	###
	

# ------------------------------------------------------------------------------


###	Example: BDS Time Series Test


	# This example shows how to perform the BDS time series test.
	###
	
	# BDS Test: iid Time Series:	
	par(mfrow = c(3, 1))
	x = rnorm(100)
	ts.plot(x, type = "l", main = "iid Time Series")
	bdsTest(x, m = 3)
	###
		
	# BDS Test: Non Identically Distributed Time Series:
	x = c(rnorm(50), runif(50))
	ts.plot(x, type = "l", main = "Non-iid Time Series")
	bdsTest(x, m = 3)  
	###
		
	# BDS Test: Non Independent Innovations from Quadratic Map:
	x = rep(0.2, 100)
	for (i in 2:100) x[i] = 4*(1-x[i-1])*x[i-1]
	ts.plot(x, type="l", main="Quadratic Map")
	bdsTest(x, m = 3)
	###


################################################################################

