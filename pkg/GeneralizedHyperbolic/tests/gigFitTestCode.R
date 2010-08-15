require(GeneralizedHyperbolic)
require(actuar)

param <- c(1, 1, 1)
dataVector <- rgig(500, param = param)

## See how well gigFit works
gigFit(dataVector)
gigFit(dataVector, printOut = TRUE)

## See how well it works if the starting parameter values are a long way from the actual values
gigFit(dataVector, startValues = "US", paramStart = c(5, 5, 5), printOut = TRUE)

## See how well gigFit works in the limiting cases of Gamma and Inverse Gamma
dataVector2 <- rgamma(500, shape = 1, rate = 1)
gigFit(dataVector2)

dataVector3 <- rinvgamma(500, shape = 1, rate = 1)
gigFit(dataVector3)

## Use nlm instead of default
gigFit(dataVector, method = "nlm")
