### Name: SkewHyperbolicDistribution
### Title: Skewed Hyperbolic Student's t-Distribution
### Aliases: dskewhyp pskewhyp qskewhyp rskewhyp
### Keywords: distribution

### ** Examples

param <- c(0,1,40,10)
par(mfrow=c(1,2))
range <- skewhypCalcRange(param=param, tol=10^(-2))

#curves of density and distribution
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the \n Skew Hyperbolic Distribution")
curve(pskewhyp, range[1], range[2], n=500, param=param)
title("Distribution Function of the \n Skew Hyperbolic Distribution")

#curves of density and log density
par(mfrow=c(1,2))
data <- rskewhyp(1000, param=param)
curve(dskewhyp(x, param=param), range(data)[1], range(data)[2],
      n=1000, col=2)
hist(data, freq=FALSE, add=TRUE)
title("Density and Histogram of the\n Skew Hyperbolic Distribution")
logHist(data, main="Log-Density and Log-Histogram of\n the Skew
      Hyperbolic Distribution")
curve(dskewhyp(x, param=param, log=TRUE), range(data)[1], range(data)[2],
      n=500, add=TRUE, col=2)




