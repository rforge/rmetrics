#
# Examples from the Monograph:
# 	"Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 5.3
#   Extremes via Point Processes
#
# List of Examples, Exercises and Code Snippets:
#
#	Example: POT Point Process - Figure 5.3.1 
#
#   *** This list is not yet complete ***
#
# Author:
#	(C) 2002-2004, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


### Load Library:

	# Load:
	require(fExtremes)
	###
	
	
# ------------------------------------------------------------------------------


### Example: POT Point Process - Figure 5.3.1

	# Plot the point Process p(n) for n = 5, 10, 100, 500, 1000, 
	# 10000 respectivel with the X(i) exponentially distributed

	# Settings:
	par(mfrow = c(2, 3), cex = 0.7)	
	x = seq(-2, 4, length = 200)
	set.seed(671)
	###
	
	# Point Process:
	an = function(n) {1}
  	bn = function(n) {log(n)}
	n = c(5, 10, 100, 500, 1000, 10000)
	titles = c("n = 5", "n = 10", "n = 100", "n = 500", 
		"n = 1000", "n = 10000")
	x = rexp(n[length(n)])
	###

	# Graphs:
	for ( i in 1:length(n) ) {
		plot( (1:n[i])/(n[i]+1), (x[1:n[i]]-bn(n[i]))/an(n[i]),
			xlab = "x", ylab = "y", ylim = c(-10, 3), pch = 19,
			main = titles[i], col = "steelblue", cex = 0.5)
		print(bn(n[i])) 
		abline(h = -bn(n[i]), lty = 3)
	}
	###
	
	
################################################################################		


### Example: POT Mean Residual Life Plot

	# Create a mean residual life plot. Include
	# approximate confidence intervals, by default 95%

	# Graph Frame:
 	par(mfrow = c(2, 2), cex = 0.6)
 	BMW.RET = -as.timeSeries(data(bmw.ret))
 	DAX.RET = -as.timeSeries(data(dax.ret))
 	###
 	
	# Mean Residual Life Plot - BMW Data: 
	mrlPlot(-BMW.RET)
	mtext("BMW Losses", line = 0.5, cex = 0.5)
	###
	
	# Mean Residual Life Plot - BMW Data: 
	mrlPlot(-DAX.RET)
	mtext("DAX Losses", line = 0.5, cex = 0.5)
	###
	
	
# ------------------------------------------------------------------------------
	

### Example: POT Parameter Estimation

	#   Estimate the parameters (xi, mu, sigma) for a data vector
	#   x from the point process over a threshold u using the
	#   function ppFit().  
	
	# Settings:
 	par(mfrow = c(3, 2), cex = 0.6)
 	data(nyseres)
 	data = nyseres[, 1]
 	###
	
	# NYSE Residuals:
	plot(data, type = "l", ylim = c(-0.22, +0.22), 
		main = "log Returns")
	mtext("NYSE Residuals", line = 0.5, cex = 0.5)
	###
		
	# Point Process of Threshold Exceedences:  
	u = 0.02
	y = data[data < -u]
	x = (1:length(data))[data < -u]
	points(x, y, col = 2)
	plot(x, -y-u, type = "h", main = "Peaks over Threshold:")
	###
			
	# Point Process Fit:
	fit = gpdFit(x = -data, nextremes = length(x), type = "mle") 
	print(fit)
	summary(fit)
	###

	
# ------------------------------------------------------------------------------


### Example: POT Parameter Estimation

	#   Estimate the parameters (xi, mu, sigma) for a data vector
	#   x from the point process over a threshold u using the
	#   function ppFit().  

	# Settings:
 	par(mfcol = c(3, 2), cex = 0.6)
 	data(nyseres)
 	data = nyseres[, 1]
	###
 	
	# NYSE Residuals:
	plot(data, type = "l", ylim = c(-0.22, +0.22), main = "log Returns")
	###	
	
	# Point Process of Threshold Exceedences:  
	u = 0.02
	y = data[data < -u]
	x = (1:length(data))[data < -u]
	points(x, y, col = 5)
	###	
	
	# Point Process Fit:
	fit = ppFit(x = -data, threshold = u, nobs = 252)
	print(fit)
	summary(fit)
	###

	
################################################################################

