
################################################################################
# Loading different timeseries for CRISP resp. Scherer/Martin


largecap.ts = read.table(paste(DIR, "largecap.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
largeCap = as.timeSeries(largecap.ts)
largeCap

midcap.ts = read.table(paste(DIR, "midcap.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
midCap = as.timeSeries(midcap.ts)

midcapD.ts = read.table(paste(DIR, "midcapD.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
midCapD = as.timeSeries(midcapD.ts)

smallcap.ts = read.table(paste(DIR, "smallcap.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
smallCap = as.timeSeries(smallcap.ts)

microcap.ts = read.table(paste(DIR, "microcap.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
microCap = as.timeSeries(microcap.ts)

returns.three.ts = read.table(paste(DIR, "returns.three.ts.csv" , sep = ""),
    header = TRUE, sep = ";")
r3 = as.timeSeries(returns.three.ts)

 
################################################################################
# Robust Estimation:
# Three assets:

Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mean", cov = "cov"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
r3.meanCov = portfolioFrontier(data = r3, spec = Spec, constraintsString = Constraints)    
ans = r3.meanCov

Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mcd", cov = "mcd"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
r3.mcdMcd = portfolioFrontier(data = r3, spec = Spec, constraintsString = Constraints)    

    
    
Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mean", cov = "mcd"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
r3.meanMcd = portfolioFrontier(data = r3, spec = Spec, constraintsString = Constraints)    
 

Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mcd", cov = "cov"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
r3.mcdCov = portfolioFrontier(data = r3, spec = Spec, constraintsString = Constraints)    

# ------------------------------------------------------------------------------
# Small caps
Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mean", cov = "cov"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
smallCap.meanCov = portfolioFrontier(data = smallCap, spec = Spec, constraintsString = Constraints)    
ans = smallCap.meanCov

Spec = portfolioSpec()
Spec = setEstimator(Spec, estimator = c(mean = "mcd", cov = "mcd"))
Constraints = c(
 "minW[c(1:3)] = 0.0"
 )
smallCap.mcdMcd = portfolioFrontier(data = smallCap, spec = Spec, constraintsString = Constraints)    



# ------------------------------------------------------------------------------
# Plotting EF three assets:

   
frontierPlot(r3.meanCov, frontier = "u", pch = 19)
frontierPlot(r3.mcdMcd, frontier = "u", pch = 12, add = TRUE)

par(mfrow = c(2,1))

frontierPlot(r3.meanCov, frontier = "u", pch = 19)
frontierPlot(r3.meanMcd, frontier = "u", pch = 12,  add = TRUE)

frontierPlot(r3.meanCov, frontier = "u", pch = 19)
frontierPlot(r3.mcdCov, frontier = "u", pch = 12, add = TRUE)




# ------------------------------------------------------------------------------
# Plotting EF small cap:

   
frontierPlot(smallCap.meanCov, frontier = "u", pch = 19)
frontierPlot(smallCap.mcdMcd, frontier = "u", pch = 12, add = TRUE)


################################################################################
# Covariance Ellipses Plot:


require(MASS)
X = as.matrix(usPortfolioData())
x = list(Robust = MASS::cov.mcd(X)$cov, Classical = cov(X))
covEllipsesPlot(x[1])
covEllipsesPlot(x)


################################################################################

