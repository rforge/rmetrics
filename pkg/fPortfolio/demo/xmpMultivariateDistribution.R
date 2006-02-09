
#
# Example: 
#	Multivariate Skew-Normal and Student-t Disributions
# 
# Description: 
#	This example shows how to simulate Skew-Normal and Skew-Student-t
#	distributions and how to fit their model parameters. The example
#	also shows how to use the plot functions. 
#
# Author:
#	(C) 2003-2004, Diethelm Wuertz, GPL
#


################################################################################
# Generate random variates:

	require(fMultivar)
	
	# Normal random deviates
	r1 = rmvsnorm(100, dim = 1)
	head(r1)
	par(mfcol = c(3, 2), cex = 0.7)
	ts.plot(as.ts(r1), xlab = "r", main = "Normal 1d")
	# Generate random correlated bivariate deviates ...
	r2 = rmvsnorm(100, dim = 2, Omega = matrix(c(1, 0.5, 0.5, 1), 2)) 
	head(r2)
	ts.plot(as.ts(r2), xlab = "r", col = 2:3, main = "Normal 2d")
	# A more general case ...
	r3 = rmvsnorm(100, dim = 3, mu = c(-1, 0, 1), alpha = c(1, 1, 1))
	head(r3)
	ts.plot(as.ts(r3), xlab = "r", col = 2:4, main = "Skew Normal 3d")	
	#
	# Student-t: Random Deviates
	r1 = rmvst(100, dim = 1)
	ts.plot(as.ts(r1), xlab = "r", main = "Student-t 1d")
	r2 = rmvst(100, dim = 2, Omega = matrix(c(1, 0.5, 0.5, 1), 2))
	ts.plot(as.ts(r2), xlab = "r", col = 2:3, main = "Student-t 2d")
	r3 = rmvst(100, dim = 3, mu = c(-1, 0, 1), alpha = c(1, -1, 1), df = 5)
	ts.plot(as.ts(r3), xlab = "r", col = 2:4, main = "Skew Student-t 3d")	
	###
	
	
# Compute densities, the univariate case:

	# Normal case:
	par(mfrow = c(2, 2), cex = 0.7)
	x = seq(-3, 3, length = 51)
	y = dmvsnorm(x, dim = 1)
	plot(x, y, type = "l", col = "steelblue4")
	title(main = "Normal")
	y = pmvsnorm(x, dim = 1)
	plot(x, y, type = "l", col = "steelblue4")
	title(main = "Normal")
	#
	# Student-t case:
	x = seq(-3, 3, length = 51)
	y = dmvst(x, dim = 1, mu = -0.5, Omega = 1, alpha = -1)
	plot(x, y, type = "l", col = "steelblue4")
	title(main = "Student-t")
	y = pmvst(x, dim = 1)
	plot(x, y, type = "l", col = "steelblue4")
	title(main = "Student-t")
	###
	
	
# Compute densities, the bivariate case:

	# Generate grid points:
	require(fExtremes) # for function gridVector()
	x = seq(-3, 3, length = 51)
	X = gridVector(x,x)$X
	Y = gridVector(x,x)$Y
	#
	# The bivariate normal case:
	Z = matrix(dmvsnorm(cbind(X, Y), dim = 2), length(x))
	par (mfrow = c(2, 2), cex = 0.7)
	persp(x, x, Z, theta = -40, phi = 30, col = "steelblue4") 
	image(x, x, Z)
	contour(x, x, Z, add = TRUE)
	#
	# The bivariate skew-Student-t case:
	Omega = matrix(c(1, 0.5, 0.5, 1), 2)
	alpha = c(-1, 1)
	Z = matrix(dmvst(cbind(X, Y), dim = 2, Omega = Omega, 
		alpha = alpha, df = 3), length(x))
	persp(x, x, Z, theta = -40, phi = 30, col = "steelblue4") 
	image(x, x, Z)
	contour(x, x, Z, add = TRUE)
	###
	

# Compute probabilities:

	# Generate grid points:
	require(fExtremes) # for function gridVector()
	x = seq(-3, 3, length = 31)
	X = gridVector(x,x)$X
	Y = gridVector(x,x)$Y
	#
	# The bivariate normal case:
	Z = matrix(pmvsnorm(cbind(X, Y), dim = 2), length(x))
	par (mfrow = c(2, 2), cex = 0.7)
	persp(x, x, Z, theta = -40, phi = 30, col = "steelblue4") 
	image(x, x, Z)
	contour(x, x, Z, add = TRUE)
	#
	# The bivariate skew-Student-t case:
	Omega = matrix(c(1, 0.5, 0.5, 1), 2)
	alpha = c(-1, 1)
	Z = matrix(pmvst(cbind(X, Y), dim = 2, Omega = Omega, 
		alpha = alpha, df = 3), length(x))
	persp(x, x, Z, theta = -40, phi = 30, col = "steelblue4") 
	image(x, x, Z)
	contour(x, x, Z, add = TRUE)
	###

	
# Show arguments of fitting function:

	args(mvFit)
	###

# Parameter Fit - Univariate Distribution:

	# Normal:
	fit1 = mvFit(x = rmvsnorm(100, dim = 1), method = "snorm")
	fit1
	par(mfcol = c(5,3), cex = 0.5)
	plot(fit1, which = "all")
	#
	# Student-t: Fixed number of degrees of freedom:
	fit2 = mvFit(x = rmvst(100, dim = 1), method = "st", fixed.df = 4)
	fit2
	plot(fit2, which = rep(TRUE, 5))
	#
	# Student-t: Estimate includes degrees of freedom:
	fit3 = mvFit(x = rmvst(100, dim = 1), method = "st", fixed.df = NA)
	fit3
	plot(fit3, which = "all")
	###
	
	
# Parameter Fit - Bivariate Distribution:
	
	# Normal:
	fit1 = mvFit(x = rmvsnorm(100, 2), method = "snorm")
	fit1
	par(mfrow = c(1, 1), cex = 0.7)
	plot(fit1, which = c(TRUE, FALSE, FALSE, FALSE, FALSE))
	par(mfrow = c(2, 2), cex = 0.7)
	plot(fit1, which = !c(TRUE, FALSE, FALSE, FALSE, FALSE))
	#	
	# Student-t: Fixed number of degrees of freedom:
	fit2 = mvFit(x = rmvst(100, 5), method = "st", fixed.df = 4)
	fit2
	par(mfrow = c(1, 1), cex = 0.7)
	plot(fit2, which = c(TRUE, FALSE, FALSE, FALSE, FALSE))
	####
	

################################################################################

		