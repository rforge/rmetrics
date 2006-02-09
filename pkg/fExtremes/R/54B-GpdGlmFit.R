
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
# FUNCTION:					GPD MODELLING FROM ISMEV:
#  gpdglmFit				 Fits GPD Distribution
#   print.gpdglmFit		      Print Method for object of class "gpdglm"
#   plot.gpdglmFit		      Plot Method for object of class "gpdglm"
#   summary.gpdglmFit		  Summary Method for object of class "gevglm"
# FUNCTION:                 ADDITIONAL PLOTS:
#  gpdglmprofPlot			 Profile Log-likelihoods for Stationary GPD Models
#  gpdglmprofxiPlot	         Profile Log-likelihoods for Stationary GPD Models
################################################################################


gpdglmFit =
function(x, threshold = min(x), npy = 365, y = NULL, sigl = NULL, shl = NULL, 
siglink = identity, shlink = identity, show = FALSE, method = "Nelder-Mead", 
maxit = 10000, ...)
{   # A function implemented by Diethelm Wuertz

	# Description:
	
	# FUNCTION:
    
    # Function Call:
    call = match.call()
    # Fit Parameters:
    fitted = gpd.fit(xdat = x, threshold = threshold, npy = npy, ydat = y, 
    	sigl = sigl, shl = shl, siglink = siglink, shlink = shlink, 
    	show = show, method = method, maxit = maxit, ...)
    # Add names attribute:
    names(fitted$se)  = names(fitted$mle) = c("sigma", "mle")
    	
    # Add:
    fit = list()
    fit$fit = fitted
	fit$call = call
	fit$type = c("gpdglm", "mle")
	fit$par.ests = fitted$mle
	fit$par.ses = fitted$se
	fit$residuals = fitted$residuals
	fit$fitted.values = x - fitted$residuals
	fit$llh = fitted$nllh
	fit$converged = fitted$conv
	
	# Return Value:
	class(fit) = "gpdglmFit"
    fit
}


# ------------------------------------------------------------------------------


print.gpdglmFit =
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Print Method for an object of class 'gpdglmFit'
	
	# FUNCTION:

	# Print Call:
	cat("\nCall:\n")
	cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "") 	
	
	if (fit$fit$trans) {	
		# Still to do:
		print.default(x) }	
	else {
		# Estimation Type:
		cat("\nEstimation Type:", x$type, "\n")	
		# Fitted Parameters:
		cat("\nEstimated Parameters:\n")
		print(x$par.ests)
		cat("\n") }
		
	# Return Value:
	invisible(x)
}


# ------------------------------------------------------------------------------


plot.gpdglmFit =
function(x, which = "all", ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Print Method for an object of class 'gpdglmFit'
	
	# FUNCTION:
	
	# Plot Functions:
	if(x$fit$trans) {
       	plot.1 <<- function(x, ...) {
	       	n = length(x$data)
	       	plot( 
	       	 	x = (1:n)/(n + 1),
	       		y = 1 - exp( - sort(x$data)), 
	       		xlab = "Empirical", 
       			ylab = "Model")
       		abline(0, 1, col = 4)
       		title("Residual Probability Plot") }
       	plot.2 <<- function(x, ...) {
       		n = length(x$data)
	       	plot( 
       			x = - log( 1 - (1:n)/(n+1) ), 
       			y = sort(x$data), 
       			ylab = "Empirical", 
       			xlab = "Model")
       		abline(0, 1, col = 4)
       		title("Residual Quantile Plot (Exptl. Scale)") } }
    else {
       	plot.1 <<- function(x, ...) {
	       	gpd.pp(x$mle, x$threshold, x$data) }
       	plot.2 <<- function(x, ...) {
	       	gpd.qq(x$mle, x$threshold, x$data) }
       	plot.3 <<- function(x, ...) {
	       	gpd.rl(x$mle, x$threshold, x$rate, x$n, x$npy, 
       			x$cov, x$data, x$xdata) }
       	plot.4 <<- function(x, ...) {
	       	gpd.his(x$mle, x$threshold, x$data) } }
	       	       	
	# Plot:
	if (fit$fit$trans) {
		interactivePlot(
			x = x$fit,
			choices = c(
				"Excess Distribution",
				"QQ-Plot of Residuals"),
			plotFUN = c(
				"plot.1", 
				"plot.2"),
			which = which) }
	else {
		interactivePlot(
			x = x$fit,
			choices = c(
				"Probability Plot",
				"Quantile Plot",
				"Return Level Plot", 
				"Histogram Plot"),
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


summary.gpdglmFit =
function(object, doplot = TRUE, which = "all", ...) 
{	# A function written by Diethelm Wuertz

	# Description:
	#	Summary Method for an object of class 'gpdglmFit'
	
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
	
	# Return Value:
	invisible(object)
}


# ******************************************************************************


gpdglmprofPlot =
function(fit, m, xlow, xup, conf = 0.95, nint = 100, ...)
{	# A function implemented by Diethelm Wuertz

  	# Description:
  	#	Profile Log-likelihoods for Stationary GPD Models
  	
  	# FUNCTION:
  	
  	# Compute:
	gpd.prof(z = fit$fit, m = m, xlow = xlow, xup = xup , conf = conf, 
		nint = nint) 
}


# ------------------------------------------------------------------------------


gpdglmprofxiPlot =
function(fit, xlow, xup, conf = 0.95, nint = 100, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
  	#	Profile Log-likelihoods for Stationary GPD Models

	# FUNCTION:
	
	# Compute:
	gpd.profxi(z = fit$fit, xlow = xlow, xup = xup, conf = conf, 
			nint = nint, ...) 
}


# ******************************************************************************

