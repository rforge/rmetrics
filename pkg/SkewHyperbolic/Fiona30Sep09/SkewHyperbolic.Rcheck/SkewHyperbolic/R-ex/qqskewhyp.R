### Name: qqskewhyp
### Title: Skew Hyperbolic Student's t-Distribution Quantile-Quantile and
###   Percent-Percent Plots
### Aliases: qqskewhyp ppskewhyp
### Keywords: distribution hplot

### ** Examples

par(mfrow = c(1,2))
param <- c(0,1,20,10)
y <- rskewhyp(500, param=param)
qqskewhyp(y, param=param, main="Skew Hyperbolic\n Q-Q Plot")
ppskewhyp(y, param=param, main="Skew Hyperbolic\n P-P Plot")



