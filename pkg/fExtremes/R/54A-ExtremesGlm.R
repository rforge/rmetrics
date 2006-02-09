
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:				   GEV MODELLING FROM ISMEV:
#  gevglmFit				Fits GEV Distribution
#   print.gevglmFit		     Print Method for object of class "gevglm"
#   plot.gevglmFit		     Plot Method for object of class "gevglm"
#   summary.gevglmFit		 Summary Method for object of class "gevglm"
# FUNCTION:                ADDITIONAL PLOTS:
#  gevglmprofPlot			Profile Log-likelihoods for Stationary GEV Models
#  gevglmprofxiPlot	        Profile Log-likelihoods for Stationary GEV Models
################################################################################


gevglmFit =
function(x, y = NULL, gumbel = FALSE, mul = NULL, sigl = NULL, shl = NULL, 
mulink = identity, siglink = identity, shlink = identity, show = FALSE, 
method = "Nelder-Mead", maxit = 10000, ...)
{	# A function written by Diethelm Wuertz

	# Description:
	#	Fits GEV Distribution
	
	# Note:
	#	This is a function wrapper to the functions 'gev.fit' and
	#	'gum.fit' which are part of the R package 'ismev'.
		
	# FUNCTION:
	
	# Fit - Use gev.fit() and gum.fit() from R's ismev Package:
	call = match.call()
	if (gumbel) {
		fitted = gum.fit(xdat = x, ydat = y, mul = mul, sigl = sigl,  
			mulink = mulink, siglink = siglink, show = show, 
			method = method, maxit = maxit, ...) }
	else {
		fitted = gev.fit(xdat = x, ydat = y, mul = mul, sigl = sigl, shl = shl, 
			mulink = mulink, siglink = siglink, shlink = shlink, show = show, 
			method = method, maxit = maxit, ...) }
	fitted$gumbel = gumbel
	
	# Standard Errors and Covariance Matrix:
	if (gumbel) {	
		# Parameters - We take the same order as in gevFit:
		mle = rev(fitted$mle)
		names(mle) = c("sigma", "mu")
		se = rev(fitted$se)
		names(se) = c("sigma", "mu")
		covar = fitted$cov
		covar[1,1] = fitted$cov[2,2]
		covar[2,2] = fitted$cov[1,1] }
	else {
		# Parameters - We take the same order as in gevFit:
		mle = rev(fitted$mle)
		names(mle) = c("xi", "sigma", "mu")
		se = rev(fitted$se)
		names(se) = c("xi", "sigma", "mu")
		covar = fitted$cov
		covar[1,1] = fitted$cov[3,3]
		covar[3,3] = fitted$cov[1,1]
		covar[1,2] = covar[2,1] = fitted$cov[2,3]
		covar[2,3] = covar[3,2] = fitted$cov[1,2]  }
	fitted$covar = covar
				
	# Calculate Residuals:
	if (gumbel) {
		# GUMBEL:
		xi = 0
		sigma = mle[1]
		mu = mle[2] 
		residuals = exp( - exp( - (fitted$data - mu)/sigma)) }
	else {
		# GEV:
		xi = fitted$mle[1]
		sigma = fitted$mle[2]
		mu = fitted$mle[3]
		residuals = (1 + (xi * (fitted$data - mu))/sigma)^(-1/xi) }		
	
	# Add:
	fit = list()
	fit$fit = fitted
	fit$call = match.call()
	fit$type = c(if(gumbel) "gumglm" else "gevglm", "mle")
	fit$par.ests = mle
	fit$par.ses = se
	fit$residuals = residuals
	fit$fitted.values = x - residuals
	fit$llh = fitted$nllh
	fit$converged = fitted$conv
	
	# Return Value:
	class(fit) = "gevglmFit"
	fit	
}


# ******************************************************************************


print.gevglmFit =
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Print method for objects of class 'gevglmFit'
	
	# FUNCTION:

	# Function Call:
	cat("\nCall:\n")
	cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 		
	
	# Estimation Type:
	cat("\nEstimation Type:", x$type, "\n")	
	
	# Fitted Parameters:
	cat("\nEstimated Parameters:\n")
	print(x$par.ests)
	cat("\n")
	
	# Return Value:
	invisible(x)
}


# ------------------------------------------------------------------------------


plot.gevglmFit = 
function(x, which = "ask", ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Plot method for objects of class 'gevglmFit'
	
	# FUNCTION:
	
	# Settings:
	fit = x
	
	# Internal "plot.n" Function:
	plot.1 <<- function(x, ...) {
		if (x$gumbel) x$mle = c(x$mle, 0)
		gev.pp(x$mle, x$data) }
	plot.2 <<- function(x, ...) {
		if (x$gumbel) x$mle = c(x$mle, 0)
		gev.qq(x$mle, x$data) }
	plot.3 <<- function(x, ...) {
		if (x$gumbel) { 
			fit$mle = c(x$mle, 0)
			gum.rl(x$mle, x$cov, x$data) }
		else { 
			gev.rl(x$mle, x$cov, x$data) } }
	plot.4 <<- function(x, ...) {
		if (x$gumbel) x$mle = c(x$mle, 0)
		gev.his(x$mle, x$data) }
	plot.5 <<- function(x, ...) {
		n = length(x$data)
		z = (1:n)/(n + 1)
		plot(z, exp( - exp( - sort(x$data))), 
	    	xlab = "empirical", ylab = "model")
		abline(0, 1, col = 4)
		title("Residual Probability Plot") }
	plot.6 <<- function(x, ...) {
		n = length(x$data)
		z = (1:n)/(n + 1)
		plot( - log( - log(z)), sort(x$data), 
	   		xlab = "empirical", ylab = "model")
		abline(0, 1, col = 4)
		title("Residual Quantile Plot (Gumbel Scale)") }
				
	# Plot:
	if (fit$fit$trans) { 
		# Non-Stationary Plots: plot 11-12
		interactivePlot(
			x = x$fit,
			choices = c(
				"Residual Probability Plot",  
				"Residual Quantile Plot"),
			plotFUN = c(
				"plot.5", 
				"plot.6"),
			which = which) }	 	
	 else { 
		# Stationary Plots: plot 01-04
		interactivePlot(
			x = x$fit,
			choices = c(
				"Residual Probability Plot", 
		    	"Residual Quantile Plot",
				"Return Level Plot", 
				"Density Plot"),
			plotFUN = c(
				"plot.1", 
				"plot.2",
				"plot.3", 
				"plot.4"),
			which = which) }
	
	# Return Value:
	invisible(x)
}


# ------------------------------------------------------------------------------


summary.gevglmFit =
function(object, doplot = TRUE, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

	# Description:
	#	Summary method for objects of class 'gevglmFit'
	
	# FUNCTION:
		    
	# Print:
	print(object, ...)
	
	# Summary:
	cat("\nStandard Deviations:\n"); print(object$par.ses)
	cat("\nLog-Likelihood Value: ", object$llh)
	cat("\nType of Convergence:  ", object$converged, "\n") 
	cat("\n")
	
	# Plot:
	if (doplot) plot(object, which = which, ...)
	cat("\n")
	
	# Return Result
	invisible(object)
}


# ******************************************************************************


gevglmprofPlot =
function(object, m, xlow, xup, conf = 0.95, nint = 100)
{	# A function implemented by Diethelm Wuertz

  	# Description:
  	#	Profile Log-likelihoods for Stationary GEV Models.
  	
  	# FUNCTION:
  	
  	# Compute:
  	if (object$fit$gumbel) {
		stop("Not for Gumbel type distributions") }
	else {
		gev.prof(z = object$fit, m = m, xlow = xlow, xup = xup , conf = conf, 
			nint = nint) }
}


# ------------------------------------------------------------------------------


gevglmprofxiPlot =
function(object, xlow, xup, conf = 0.95, nint = 100)
{	# A function implemented by Diethelm Wuertz

	# Description:
  	#	Profile Log-likelihoods for Stationary GEV Models.
  	
  	# FUNCTION:
	
	# Compute:
	if (object$fit$gumbel) {
		stop("Not for Gumbel type distributions") }
	else {
		gev.profxi(z = object$fit, xlow = xlow, xup = xup, conf = conf, 
			nint = nint) }
}


# ******************************************************************************

