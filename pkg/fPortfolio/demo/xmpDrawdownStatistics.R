
#
# Example
#	Investigate the Maximum Drawdown of a Brownian Motion
#
# Description:
# 	The example shows how to compute the expectation value E[D]
#	of Drawdowns for Brownian Motion on a strand of length "horizon"
#	with given drift "mu" and variance "sigma".
#
# Content:
#	1. Simulation of the Maximum Drawdown Process
#   2. Functions for QN() and QP() from Magdon-Ismail et al.
#   Density, Probability and RVS for mean Max-Drawdowns
#	2. Scaling Behavior of the Average Drawdown
#	3. Compute and Plot Auxiliary function Q(x)
#	4. Investigate Asymptotic Bheaviour of Q(x)
#	5. Make a table for the Q(x) Functions:
#
# References:
#	Magdon-Ismail M., Atiya A.F., Pratap A., Abu-Mostafa Y.S. (2003);
#		On the Maximum Drawdown of a Brownian Motion,
#		Preprint, CalTech, Pasadena USA, p. 24. 
#
# Last Check:
#	2006-02-05 ERRORS
#
# Author:
#	(C) 2003-2006, Diethelm Wuertz, GPL
#

################################################################################
# Load Required Package:
	
	# Load:
	require(fPortfolio)
	###


# ------------------------------------------------------------------------------
# 1. Simulation of the Maximum Drawdown Process
	
	
	# Graph Frame:
	par(mfcol = c(3, 2), cex = 0.5)
	###
	
	# Generate Drawdons from Time Series:
	horizon = 500	# horizon of the investor, time T
	mu = 0.0		# Drift of Brownian Motion
	sigma = 1.0		# Standard Deviation of Brownian Motion	
	DD = NULL
	for (i in 1:3) {
		# Generate the log-Return Process:
		X = rnorm(n=horizon, mean=mu, sd=sigma)
		# Generate the log-Price Process:
		Y = cumsum(X)
		# Generate the Drawdown Process:
		D = cummax(Y)-Y	
		# Evaluate the Maximum Drawdown:
		h = max(D)	
		# Plot the log-Price Process:
		ts.plot(Y, main = "Time Series") 
		lines(cummax(Y), col = "steelblue") 
		DD = cbind(DD, D) 
	}	
	###
	
	# Plot Drawdown Series:
	for (i in 1:3) {
		# Plot the Drawdown Process:
		plot(DD[,i], type = "h", main = "Drawdowns") }	
	###
	
	
# ------------------------------------------------------------------------------
# 2. Functions for QN() and QP() from Magdon-Ismail et al.
	
	# Function QN():
	QN = 
	function(x) {	
	 	# Compute mu's:
	 	mu = sqrt(x) * sqrt(2/1000)
	 	# Normalized Expectation Value:
	 	Q = maxddStats(mean = -mu, sd = 1, horizon = 1000) * mu / 2 	
	 	# Return Value:
	 	Q 
	}	
	###

	# Function QP():
	QP = 
	function(x) {	
	 	# Compute mu's:
	 	mu = sqrt(x) * sqrt(2/1000)
	 	# Normalized Expectation Value:
	 	Q = maxddStats(mean = mu, sd = 1, horizon = 1000) * mu / 2 	
	 	# Return Value:
	 	Q 
	}
	###				
		

# ------------------------------------------------------------------------------
# Drawdown Distribution - No trend	

	# Settings:
	horizon = 1000		         # horizon of the investor, time T
	samples = 5000		         # number of MC samples, N -> infinity
	xlim = c(0, 5)*sqrt(horizon) # Range of expected Drawdons
	N = 1000
	###
			
	# Plot Histogram of Simulated Max Drawdowns:
	par(mfrow = c(2, 1), cex = 0.7)
	h = rmaxdd(n = samples, mean = 0, sd = 1, horizon = horizon)
	hist(x = h, n = 50, probability = TRUE, xlim = xlim, 
		main = "Max. Drawdown Density")	
	###
		
	# Compare with True Density:
	n = 200
	x = seq(0, xlim[2], length = n)
	d = dmaxdd(x = x, sd = sigma, horizon = horizon, N = 1000)
	lines(x, d, lwd = 2, col = "steelblue4")
	###
	
	# Count Frequencies of Drawdowns Greater or Equal to "h":
	n = 50
	x = seq(0, xlim[2], length = n)
	g = rep(0, times = n)
	for (i in 1:n) g[i] = length (h[h>x[i]]) / samples
	plot(x, g, type ="h", xlab = "q", main = "Max. Drawdown Probability")
	###
		
	# Compare with True Probability "G_D(h)":
	n = 50
	x = seq(0, xlim[2], length = n)
	p = pmaxdd(q = x, sd = sigma, horizon = horizon, N = 5000)
	lines(x, p, lwd = 2, col="steelblue4") 
	###
	
	# Estimate the Sample Mean and Compare with the True Mean:
	sample.mean = mean(h)
	sample.mean
	true.mean = 2 * sqrt(pi/8) * sigma * sqrt(horizon)
	true.mean
	###
	
	
################################################################################	

#
#	Example - averageDrawdown:
#
#		We consider the "Continuous Time Limit" (CTL)
#
# 		The example shows how to compute the expectation value E[D]
#		of Drawdowns for Brownian Motion on a strand of length "horizon"
#		with given drift "mu" and variance "sigma".
#
#		1.	Density, Probability and RVS for mean Max-Drawdowns
#		2. 	Scaling Behavior of the Average Drawdown
#		3.	Compute and Plot Auxiliary function Q(x)
#		4.	Investigate Asymptotic Bheaviour of Q(x)
#		5. 	Make a table for the Q(x) Functions
#
#	Reference:
#		


# ------------------------------------------------------------------------------
# Plot Density and Probability Function for Zero Drift:

	# Density:
	par(mfcol = c(2, 1), cex = 0.7)
	x = seq(0, 25, length = 500)
	d = dmaxdd(x, sd = 1, horizon = 10)
	plot(x, d, type = "l", xlim = c(0, 100), ylab = "D", main = "Density")
	grid()
	lines(x*2, dmaxdd(x*2, sd = 1, horizon = 100), col = "red")
	lines(x*4, dmaxdd(x*4, sd = 1, horizon = 1000), col = "blue")
	lines(x*4, 0*x)
	###
	
	# Probability:
	x = seq(0, 25, length = 500)
	p = pmaxdd(x, sd = 1, horizon = 10)
	plot(x, p, type="l", xlim = c(0, 100), ylab = "P", main = "Probability")
	grid()
	lines(x*2, pmaxdd(x*2, sd = 1, horizon = 100), col = "red")
	lines(x*4, pmaxdd(x*4, sd = 1, horizon = 1000), col = "blue")
	lines(x*4, 0*x)
	###
	
	# Random Variates:
	r = rmaxdd(100, mean = 0, sd = 1, horizon = 1000)
	###

		
# ------------------------------------------------------------------------------
# Show Scaling Behaviour for the Expectation Value of Drawdowns

	# Note, if we multiply the "horizon" by tau, and if we reduce
	# at the same time the drift by a factor 1/sqrt(tau), then
	# the drawdowns are enlarged by a factor of sqrt(tau)!
	mu.norm = seq(-1, 1, by = 0.1)
	sigma = 1
	###
	
	# Consider a short and long investment horizon:
	par(mfrow = c(1, 1))
	horizon = c(10, 100, 1000)
	for (i in 1:3) {
		mu = mu.norm / sqrt(horizon[i])
		h = maxddStats(mean = mu, sd = 1, horizon = horizon[i])
		h.norm = h / sqrt(horizon[i])
		# Plot Expectation:
		if (i == 1) plot(mu.norm, h.norm, xlab="mu*sqrt(T)", 
			ylab = "E[h]/sqrt(T)", main = "T = 10, 100, 1000")
		if (i > 1) points(mu.norm, h.norm, pch = 2+i, col = i) 
	}
	grid()
	###
	
			
# ------------------------------------------------------------------------------
# Plot the Auxiliary Function Q(x):
	
	# Reproduce Figure 1 from Magdon-Ismael et al. [2003]
	par(mfrow = c(1, 1))
	sigma = 1; horizon = 1000
	x = seq(0, 5, by = 0.01)
	plot(x, QN(x), type = "l", lwd = 2, ylab = "Q(x)", 
		main = "Comparison of Q(x)")
	grid()
	lines(x, QP(x), col = "red", lwd = 2)
	Q0 = sqrt(pi/8)*sqrt(2*x)
	lines(x, Q0, col = "steelblue4", lwd = 2)
	###
	
			
# ------------------------------------------------------------------------------
# Investigate the Asymptotic Behavior of the Q(x) Functions:
	
	# Reproduce Figure 2 and 3 from Magdon-Ismael et al. [2003]
	par(mfrow = c(2, 2))
	sigma = 1; horizon = 1000
	# QN - Figure 2 from Magdon-Ismael et al. [2003]
	x = seq(0, 2.5, by=0.01)
	plot(x, QN(x), type="l", lwd=2, ylab="Q(x)", 
		main="Asymptotics of Qn(x)")
	lines(x, x+0.5, col="red", lwd=2); grid()
	# QP - Figure 3 from Magdon-Ismael et al. [2003]
	x = exp(seq(-6.2, 9.5, length=100))
	plot(log(x), QP(x), type="l", lwd=2, ylab="Q(x)", 
		main="Asymptotics of Qp(x)")
	lines(log(x), log(x)/4 + 0.49088, col="steelblue4", lwd = 2)
	grid()
	###
	
	
# ------------------------------------------------------------------------------
# Make a Table for the Q Function:

	# Reproduce Table in Appendix B from Magdon-Ismael et al. [2003]
 	# For QP(xp):
 	xn = c(
 		0.0005, 0.0010, 0.0015, 0.0020, 0.0025, 0.0050, 0.0075, 0.0100, 0.0125,
 		0.0150, 0.0175, 0.0200, 0.0225, 0.0250, 0.0275, 0.0300, 0.0325, 0.0350,
 		0.0375, 0.0400, 0.0425, 0.0450, 0.0475, 0.0500, 0.0550, 0.0600, 0.0650,
 		0.0700, 0.0750, 0.0800, 0.0850, 0.0900, 0.0950, 0.1000, 0.1500, 0.2000,
 		0.2500, 0.3000, 0.3500, 0.4000, 0.5000, 1.0000, 1.5000, 2.0000, 2.5000,
 		3.0000, 3.5000, 4.0000, 4.5000, 5.0000)	
 	Qn = QN(xn)
 	# For QN(x):
 	xp = c(
 		0.0005, 0.0010, 0.0015, 0.0020, 0.0025, 0.0050, 0.0075, 0.0100, 0.0125, 
 		0.0150, 0.0175, 0.0200, 0.0225, 0.0250, 0.0275, 0.0300, 0.0325, 0.0350, 
 		0.0375, 0.0400, 0.0425, 0.0450, 0.0500, 0.0600, 0.0700, 0.0800, 0.0900, 
 		0.1000, 0.2000, 0.3000, 0.4000, 0.5000, 1.5000, 2.5000, 3.5000, 4.5000,
		10.000, 20.000, 30.000, 40.000, 50.000, 150.00, 250.00, 350.00, 450.00, 
		1000.0, 2000.0, 3000.0, 4000.0, 5000.0)
	Qp = QP(xp)
	# Print Table:
	data.frame(xp, Qp, xn, Qn)
	###
 	

################################################################################
#   Example - rmaxdd:
#
#		The example shows how to simulate a Brownian Motion path
#		on a strand of length "horizon" with given drift "mu" and
#		variance "sigma" and how to generate the associated Drawdown
#		process, from which we compute the Maximum Drawdon.
 


# ------------------------------------------------------------------------------
# 	Example 2:

	# Evaluate the "Time Under Water":
	TUW = diff(which (diff(cummax(cumsum(X))) != 0))
	rev(sort(TUW))
	###

	
# ------------------------------------------------------------------------------
#	Example 3:

	# Compute averaged Maximum Drawdowns and 
	# compare with theoretical result:
	# Use the functions "rmaxdd" and "maxddStats" ...
	gamma = sqrt(pi/8)
	aveDD = aveDDt = NULL
	horizon = 12
	mu = seq(0.0, 0.10, length = 10)
	sigma = 0.1 * 2^(1:10)/2
	aveDD = aveDDt = matrix(rep(0, time = 100), 10)
	for (i in 1:length(sigma)) {
		for (j in 1:length(mu)) {
			# The simulated Values from subsamples:
			aveDD[i,j]  = mean(rmaxdd(20000, mu[i], sigma[i], horizon))
			# The theoretical Value:
			aveDDt[i,j] = maxddStats(mean = mu[i], sd = sigma[i], horizon)
		}
	}	
	###
	
	# Plot Result:
	mean(aveDD/aveDDt)
	sqrt(var(as.vector(aveDD/aveDDt)))
	par(mfrow=c(1,1))
	perspPlot(x=1:10, y = 1:10, aveDD/aveDDt, zlim = c(0.6, 0.8))
	###
	
	# Plot:
	par(mfrow = c(2, 2))
	plot(aveDD, aveDDt, cex = 0.1)
	plot(log(sigma), aveDD, cex = 0.1, ylim = c(min(aveDD), max(aveDDt)))
	points(log(sigma), aveDDt, cex = 0.1, col = "red")
	log(aveDD) - log(aveDDt)
	###

	
################################################################################
# Utility Functions:


L.maxdd = 
function(h, mu, sigma = 1, horizon = 100) 
{	# A function implemented by Diethelm Wuertz:

	# Description:
	#	Computes the L-function for a vector of drawdowns "h"
	#	which adds up to "G_D(h)":
	
	# Arguments:
	#	h - Vector of drawdowns.
	#	mu - trend (drift) for the Brownian motion.
	#	sigma - Variance of the Brownian motion.
	#	horizon	- Horizon of the investor, the length of path.
	
	# Details:
	#	The L function is defined by equation (4) given in 
	#	M. Magdon-Ismail et al., (2000);
	#	"On the Maximum Drawdown of a Brownian Motion"
	#   Preprint available from magdon@rpi.edu
	
	# FUNCTION:
	
	# Settings:
	L = NULL
	
	# Loop over all drawdowns:
	for (i in 1:length(h)) {
		H = h[i]
		s = sign(mu-sigma^2/H)
		if (s < 0) {
			L[i] = 0 }
		if (s == 0 ) {
			L[i] = 3 * (1-exp(-0.5*mu^2*horizon/sigma^2)) / exp(1)}
		if (s > 0) {
			const = sigma^2/(mu*H)
			fun = function(x, const) {tanh(x)-const*x}
			eta = uniroot(fun, interval=c(1e-9, 1/1e-9), const=const)$root
			L[i] = 2*sigma^4*eta*sinh(eta)*exp(-mu*H/sigma^2)*(1-exp(
				-(mu^2)*horizon/(2*sigma^2)+sigma^2*eta^2*horizon/(2*H^2)))/
				(sigma^4*eta^2 - mu^2*H^2 + sigma^2*mu*H) } }
	
	# Return Value:
	L 
}	
	
	
# ------------------------------------------------------------------------------


L.maxdd.XMP = 
function(N = 200, mu = 0.1, sigma = 1, horizon = 1000)
{
	# Description:
	#	Example function for L.maxdd()
	
	# Arguments:
	#	N - number of Drawdowns
	#	mu - Drift
	#	sigma Standard deviation
	#	horizon - Investment Horizon

	# FUNCTION:
	
	# The central point H, where L=3/e for an infinite Horizon:
	H = sigma^2/mu

	# A Sequence of N drawdowns:
	h = (1:(5*N))*(H/N)

	# Compute and Plot L for different horizons:
	par(mfrow = c(1, 1))
	plot(x = c(0, max(h)), y = c(0,3)/exp(1), type = "n", 
		xlab = "Drawdown h", ylab = "L", main = "L - function")
	for (i in 1:3) {
		L = L.maxdd(h, mu, sigma, horizon = 10^i)
		lines(h, L, lty = 3, col = i) }
	points(H, 3/exp(1), pch = 19)
	
	# Return Value:
	list(N = N, mu = mu, sigma = sigma, horizon = horizon, H = H, h = h)
}


# ******************************************************************************


theta.roots = 
function(n, h, mu, sigma = 1, eps = 1e-9) 
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Calculates the Positive Roots: "tan(x) - c*x"
	#	for arbitrary trend constant c = sigma^2 / (mu*h)

	# Details:
	#	The eigenvalue condition is defined by equation (4) given in 
	#	M. Magdon-Ismail et al., (2000);
	#	"On the Maximum Drawdown of a Brownian Motion"
	#   Preprint available from magdon@rpi.edu
	
	# FUNCTION:
	
	# Trendless:
	if (mu == 0) return( theta = (2*(1:n)-1)*pi/2 )
	
	# With Trend:
	const = sigma^2/(mu*h)
	fun = function(x, const) {tan(x) - const*x}
	lower = upper = theta = rep(0, times=n)
	for (i in 1:n) {
		upper[i] = (2*i-1)*pi/2 + (1-sign(mu))*pi/4
		lower[i] = upper[i]-pi/2
		interval = c(lower[i]+eps, upper[i]-eps)
		theta[i] = uniroot(fun, interval, const = const)$root }
	
	# Return Value:
	theta 
}
			

# ------------------------------------------------------------------------------


theta.roots.XMP = 
function(n = 5, h = 1, mu = 0.1, sigma = 1, eps = 1e-9) 
{
	# Description:
	#	Example function for theta.roots()
	
	# FUNCTION:
	
	# Plot Root Graph:	
	const = sigma^2/(mu*h)
	x = seq(0, 1.01*(2*n-1)*pi/2, by=1.01*(2*n-1)*pi/2/5000 )
	y = tan(x)
	ylin = const*x
	plot(x, y, ylim = c(-150, 150), type = "n", 
		main = "Positive Roots: tan(x)-cx")
	lines(x, y, lty=3)
	lines(x, ylin, col = "red")
	lines(x, -ylin, col = "blue")
	lines(x, ylin*0)

	# Compute Roots:
	zeros = (2*(1:n)-1)*pi/2
	x.pos = theta.roots(n, h, mu, sigma)		
	x.neg = theta.roots(n, h, -mu, sigma)	
	x.zero = theta.roots(n, h, 0)	
	cbind(x.pos, x.zero, x.neg)
	
	# Add Zeros to Plot:
	points(x.pos, const*x.pos, pch = 19)
	points(x.zero, 0*x.zero)
	points(x.neg, -const*x.neg, pch = 19)
}	
	
	
################################################################################
# Brownian motion with drift:

	# To do ...
	

################################################################################

