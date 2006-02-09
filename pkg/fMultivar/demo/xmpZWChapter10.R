
#
# Example:
# 	A Compendium for R and Rmetrics users to the book 
#     "Modeling Financial Time Series with S-Plus" 
#     written by E. Zivot and J. Wang
#   ISBN 0-387-95549-6
#
# Details:
#   Examples from Chapter 10
#   Part I: Linear SUR
#
# Notes:
#   This is not a COPY of the S-Plus "example.ssc" files accompanying the
#     book of Zivot and Wang. It is worth to note that this file contents a 
#     new implementation of the examples tailored to Rmetrics based on R.
# 	Diethelm Wuertz
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#
# Author:
#	(C) 2002-2004, Diethelm Wuertz, GPL
#


################################################################################
# Requirements:

	# Packages:
	require(fBasics)
	require(fSeries)
	###
	

################################################################################
## Chapter 10.3.2 -  Analysis of SUR Models


	# SUR estimation of exchange rate system
	# p. 353 
	args(SUR)
	###
	
	
	# Example 59
	# Load from File and Extract non NA Data Records
	# p. 354
	data(surex1.ts)
	head(surex1.ts)
	surex1.ts = as.timeSeries(surex1.ts, format = "%d-%b-%Y")
	head(surex1.ts)
	# Column Variables:
	colIds(surex1.ts)
	# .FP.lag1 are one month forward premia
	# .diff are future returns on spot currency
	###

	# Create List of Formulas for Regression:
	# p. 354
	formula.list = list(
	  USCNS.diff ~ USCN.FP.lag1, USDMS.diff ~ USDM.FP.lag1,
	  USFRS.diff ~ USFR.FP.lag1, USILS.diff ~ USIL.FP.lag1,
	  USJYS.diff ~ USJY.FP.lag1, USUKS.diff ~ USUK.FP.lag1)
	###
	
	 	  
	# SUR Estimation:
	# Note, start sample in August 1978 to eliminate NAs for USJY
	# p. 354
	surex1.ts = cutSeries(surex1.ts, from = "1978-08-01", to = end(surex1.ts))
	head(surex1.ts)
	dim(seriesData(surex1.ts))
	# Fit:
	sur.fit = SUR(formula.list, data = surex1.ts)
	# Or: > eqnsFit(formulas = formula.list, data = surex1.ts, method = "SUR")
	class(sur.fit)
	# Print the SUR Estimate:
	sur.fit
	# Note, the printed report from R is slightly different from
	# that one produced by S-Plus. The differences are:
	#   The time period for the input time series is not printed,
	#	Standard errors and t-values are printed additionally,
	#	SSR, MSE, and R-Squared measures are computed additionally 
    ###
    
	
	# The summary method provides more detailed information ...
	# p. 355
	summary(sur.fit)
	# Again the printed report from R is different from S-Plus
	# Durbin-Watson Stat is not printed:
	#	dw <- colSums((diff(res))^2)/colSums(res^2)

	
	# Compare Results from R and SPlus:
    # -----------------------------------------------------
	# Results from R:
    #              Estimate  Std. Error  t value  Pr(>|t|)   
	# (Intercept)   -0.0031     0.0012   -2.5911    0.0102   
	# USCN.FP.lag1  -1.6602     0.5886   -2.8207    0.0052  
	# (Intercept)    0.0006     0.0025    0.2545    0.7994 
	# USDM.FP.lag1   0.4847     0.2195    2.2085    0.0283   
	# (Intercept)    0.0013     0.0024    0.5415    0.5887    
	# USFR.FP.lag1   0.9995     0.2088    4.7867    0.0000 
	# (Intercept)   -0.0006     0.0029   -0.2110    0.8331  
	# USIL.FP.lag1   0.4589     0.3623    1.2664    0.2067  
	# (Intercept)    0.0078     0.0031    2.5302    0.0121
	# USJY.FP.lag1  -1.7748     0.6975   -2.5445    0.0117
	# (Intercept)   -0.0036     0.0027   -1.3325    0.1841  
	# USUK.FP.lag1  -1.3068     0.6342   -2.0605    0.0406
 	# ----------------------------------------------------
	# Results from S-Plus:
    #                 Value  Std. Error  t value  Pr(>|t|) 
	#  (Intercept)  -0.0031     0.0012   -2.5943    0.0101 
	# USCN.FP.lag1  -1.6626     0.5883   -2.8263    0.0052 
	#  (Intercept)   0.0006     0.0024    0.2384    0.8118  
	# USDM.FP.lag1   0.5096     0.2089    2.4392    0.0155  
	#  (Intercept)   0.0013     0.0024    0.5550    0.5795  
	# USFR.FP.lag1   1.0151     0.1993    5.0928    0.0000  
	#  (Intercept)  -0.0006     0.0028   -0.2071    0.8361 
	# USIL.FP.lag1   0.4617     0.3584    1.2883    0.1990 
	#  (Intercept)   0.0078     0.0031    2.5226    0.0124 
	# USJY.FP.lag1  -1.7642     0.6961   -2.5342    0.0120 
	#  (Intercept)  -0.0035     0.0027   -1.3256    0.1864 
	# USUK.FP.lag1  -1.2963     0.6317   -2.0519    0.0414 
	# ----------------------------------------------------
	

	# Graphical Summaries of each equation
	# p. 356/357
	# Figure 10.1
	# plot(sur.fit)
	# Sorry not yet available ...
	###
	
	
	# Compute Iterated SUR Estimator
	# p. 357
	sur.fit2 = SUR(formula.list, data = surex1.ts, maxiter = 999)
	sur.fit2 = eqnsFit(formulas = formula.list, data = surex1.ts, 
	  method = "SUR", maxiter = 999)
	sur.fit2
	# ... converged after 6 iterations
	###

	
	# Compare non-iterated and iterated SUR
	# p. 357/358
	# > cbind(coef(sur.fit),coef(sur.fit2))
	# Use:
	cbind(sur.fit@fit$coef,sur.fit2@fit$coef)
	###

	
	# Residual Correlation Matrix
	# p. 358
	# > sd.vals = sqrt(diag(sur.fit$Sigma))
	# > cor.mat = sur.fit$Sigma/outer(sd.vals,sd.vals)
	# It's not necessary to do this, R's summary method for SUR
	# objects prints these matrices!
	summary(sur.fit)
	# or just extract matrix ...
	sur.fit@fit$rcor
	###
	
	
	# Compute Wald Statistic
	# p. 358/359
	bigR = matrix(0, 6, 12)
	bigR[1,2] = bigR[2,4] = bigR[3,6] = bigR[4,8] = bigR[5,10] = bigR[6,12] = 1
	rr = rep(1, 6)
	bHat = as.vector(sur.fit@fit$coef)
	avar = bigR %*% sur.fit@fit$vcov %*% t(bigR)
	Wald = t((bigR%*%bHat-rr)) %*% solve(avar) %*% (bigR%*%bHat-rr)
	Wald
	1 - pchisq(Wald, 6)
	# ... the data reject the unbiased hypothesis
	### 
	
	
	# Compute LR statistic
	# ... the restricted model must first be estimated
	# formula.list = list(
	#	(USCNS.diff - USCN.FP.lag1) ~ 1,
	#	(USDMS.diff - USDM.FP.lag1) ~ 1,
	#	(USFRS.diff - USFR.FP.lag1) ~ 1,
	#	(USILS.diff - USIL.FP.lag1) ~ 1,
	#	(USJYS.diff - USJY.FP.lag1) ~ 1,
	#	(USUKS.diff - USUK.FP.lag1) ~ 1)
	# sur.fit2r = SUR(formulas = formula.list, data = surex1.ts, maxiter = 999)
	# sur.fit2r
	# Statistic
	# nobs = nrow(sur.fit2r@fit$residuals)
	# LR = nobs*(
	#	determinant(sur.fit2r$Sigma, logarithm = TRUE)$modulus -
	#	determinant(sur.fit2$Sigma, logarithm = TRUE)$modulus )
	# as.numeric(LR)
	# 1 - pchisq(LR, 6)
	### Sorry, not yet implemented !
	
	
################################################################################
# Chapter 10.4 - Non-Linear Seemingly Unrelated Regressions


    # Sorry not yet available ...
    # ... contributed package "systemfit"
	###
	
	
################################################################################

