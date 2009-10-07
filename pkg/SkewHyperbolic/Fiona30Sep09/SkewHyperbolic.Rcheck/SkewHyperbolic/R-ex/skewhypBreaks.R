### Name: skewhypBreaks
### Title: Break points for the Skew Hyperbolic Student's t-Distribuiton
### Aliases: skewhypBreaks skewhypCalcRange ddskewhyp
### Keywords: distribution

### ** Examples

param <- c(0,1,10,10)
range <- skewhypCalcRange(param=param, tol=10^(-3))

#plots of density and derivative
par(mfrow=c(2,1))
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the Skew\n Hyperbolic Distribution")
curve(ddskewhyp(x, param=param), range[1], range[2], n=1000)
title("Derivative of the Density\n of the Skew Hyperbolic Distribution")

#plot of the density marking the break points
par(mfrow=c(1,1))
range <- skewhypCalcRange(param=param, tol=10^(-6))
curve(dskewhyp(x, param=param), range[1], range[2], n=1000)
title("Density of the Skew Hyperbolic Distribution\n with Breakpoints")
breaks <- skewhypBreaks(param=param)
abline(v=breaks)



