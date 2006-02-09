#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 7.5
#   Monte Carlo Simulations of Options 
#
# List of Examples, Exercises and Code Snippets
#
#   Example: Compute Halton low discrepancy numbers
#   Example: Compute Sobol low discrepancy numbers
#   Example: Valuate Option by Monte Carlo Simulation
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
# Low Discrepancey Sequences


### Example: Compute Halton low discrepancy numbers

	# This example calculates quasi random numbers created from
	# Halton's low discrepancy sequence. Plot the sequence, its
	# autocorrelation function and histogram. Compare the results
	# in low and high dimensions.
	###

	# Settings:
	par(mfrow = c(3, 2), cex = 0.5)
	par(ask = TRUE)
	###

	# Uniform Quasi Random Numbers:
	# Part 1: Calculate 20 sequences in 5 dimensions:
	runif.halton(n = 20, dimension = 5, init = TRUE)
	# Part 2a: Re-Initialize and Calculate 10 Sequences in 5 dimensions:
	runif.halton(n = 10, dimension = 5, init = TRUE)
	# Part 2b: Calculate next 10 sequences in 5 dimensions
	runif.halton(n = 10, dimension = 5, init = FALSE)
	# Extract the direction numbers from the seed:
	runif.halton.seed
	###

	# Create uniform distributed numbers:
	x = runif.halton(n = 500, dimension = 25)
	plot(x[,1], x[,2], main = "Uniform Halton: 1 - 2")
	plot(x[,24], x[,25], main = "Uniform Halton: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Uniform Halton: d=1")
	acf(x[ ,25], main = "Uniform Halton: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	###

	# Create normal distributed numbers:
	x = rnorm.halton(n = 500, dimension = 25)
	plot(x[,1], x[,2], main = "Normal Halton: 1 - 2")
	plot(x[,24], x[,25], main = "Normal Halton: 24 - 25")	
	# Autocorrelation:
	acf(x[ ,1], main = "Normal Halton: d=1")
	acf(x[ ,25], main = "Normal Halton: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	par(ask = FALSE)
	###
	
	
# ------------------------------------------------------------------------------


### Example: Compute Sobol low discrepancy numbers

	# This example calculates the first 20 Sobol numbers in 5 
	# dimension. In a second step the generator is called twice, 
	# the first time with re-initialization flag set to TRUE. 
	# Print the seed and extract the direction numbers.
	###

	# Settings:
	par(mfrow = c(3, 2), cex = 0.5)
	par(ask = TRUE)
	###

	# Uniform Quasi Random Numbers:
	# Part 1: Calculate 20 sequences in 5 dimensions:
	runif.sobol(n = 20, dimension = 5, scrambling = 0, init = TRUE)
	# Part 2a: Re-Initialize and Calculate 10 Sequences in 5 dimensions:
	runif.sobol(n = 10, dimension = 5, scrambling = 0, init = TRUE)
	# Part 2b: Calculate next 10 sequences in 5 dimensions
	runif.sobol(n = 10, dimension = 5, scrambling = 0, init = FALSE)
	# Extract the direction numbers from the seed:
	runif.sobol.seed
	###

	# Create uniform distributed numbers:
	x = runif.sobol(n = 500, dimension = 25, scrambling = 0)
	plot(x[,1], x[,2], main = "Uniform Sobol: 1 - 2")
	plot(x[,24], x[,25], main="Uniform Sobol: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Uniform Sobol: d=1")
	acf(x[ ,25], main = "Uniform Sobol: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	###

	# Create normal distributed numbers: 
	x = rnorm.sobol(n = 500, dimension = 25, scrambling = 1)
	plot(x[,1], x[,2], main = "Normal Sobol: 1 - 2")
	plot(x[,24], x[,25], main = "Normal Sobol: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Normal Sobol: d=1")
	acf(x[ ,25], main = "Normal Sobol: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	par(ask = FALSE)
	###


################################################################################
# Monte Carlo Option Pricing 


### Example: Valuate Option by Monte Carlo Simulation

	# How to perform a Monte Carlo Simulation?
   	###
   	
	# First Step:   
	# Write a function to generate the option's innovations. 
	# Use scrambled normal Sobol numbers:
	sobolInnovations = function(mcSteps, pathLength, init, ...) 
	{
		# Create Normal Sobol Innovations:
	   	innovations = rnorm.sobol(mcSteps, pathLength, init, ...)
	   	# Return Value:
	   	innovations 
	}
   	###
   	
	# Second Step: 
	# Write a function to generate the option's price paths.  
   	# Use a Wiener path:
   	wienerPath = function(eps) 
   	{	
		# Note, the option parameters must be globally defined!
		# Generate the Paths:
		path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
		# Return Value:
     	path 
	}
	###
      
	# Third Step:   
	# Write a function for the option's payoff
   	# Example 1: use the payoff for a plain Vanilla Call or Put:
   	plainVanillaPayoff = function(path) 
   	{ 
	 	# Note, the option parameters must be globally defined!
	 	# Compute the Call/Put Payoff Value:
	 	ST = S*exp(sum(path))
	 	if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
		if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
	 	# Return Value:
	 	payoff 
	}  
   	# Example 2: use the payoff for an arithmetic Asian Call or Put:
   	arithmeticAsianPayoff = function(path) 
   	{ 
	 	# Note, the option parameters must be globally defined!
	 	# Compute the Call/Put Payoff Value:
	 	SM = mean(S*exp(cumsum(path)))
	 	if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
	 	if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
	 	# Return Value:
	 	payoff 
	}
	###

	# Final Step:   
	# Set Global Parameters for the plain Vanilla/Arithmetic Asian Options:
   	TypeFlag <<- "c"; S <<- 2; X <<- 2
   	Time <<- 1/12; sigma <<- 0.5; r <<- 0.05; b <<- 0.05
   	# Do the Asian Simulation with scrambled random numbers:
   	# Short run:
   	mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 1000, 
     	mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations, 
     	path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
     	antithetic = TRUE, standardization = FALSE, trace = TRUE, 
     	scrambling = 2, seed = 4711)  
	# Plot the MC Iteration Path:
	par(mfrow = c(1, 1), cex = 0.7)
	mcPrice = cumsum(mc)/(1:length(mc))
	plot(mcPrice, type = "l", main = "Arithmetic Asian Option", 
		xlab = "Monte Carlo Loops", ylab = "Option Price")
	# Turnbull Wakeman Asian Approximation:
	TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = 2, SA = 2, 
   	  X = 2, Time = 1/12, time = 1/12, tau = 0 , r = 0.05, b = 0.05, 
   	  sigma = 0.5)
   	###
    
	# A Long Run:
	if {FALSE}
	mc = MonteCarloOption(delta.t = 1/360, pathLength = 360, mcSteps = 1000, 
      	mcLoops = 1000, init = TRUE, innovations.gen = sobolInnovations, 
     	path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
     	antithetic = TRUE, standardization = FALSE, trace = TRUE, 
     	scrambling = 2, seed = 4711)
    # Compare with:
    # 0.24668  This Monte Carlo Run
   	# 0.24979  Turnbull Wakeman Approximation
   	# 0.248    Geman Eydeland
   	# 0.247    Euler PDE
   	# 0.247    Post-Widder PDE
   	# 0.24642  Fourier Transformation
   	# 0.24640  Linetzky's Approach
   	###
	
 
################################################################################

