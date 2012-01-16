### Test standardized generalized hyperbolic distribution
###
### David Scott, 4/1/2011

### Obtain functions for testing
require(GeneralizedHyperbolic)

## Choose parameters at random from test sets
data(ghypParam)
testParam <- ghypLargeParam
paramSampleSize <- 1

## sample parameter values
np <- NROW(testParam)
paramNum <- sample(1:np, paramSampleSize, replace = FALSE)
param <- testParam[paramNum,]
param
lambda <- param[5]
ghypMean(param = param)
ghypVar(param = param)

## convert to standardized parameters
(newParam <- ghypScale(0, 1, param = param))
ghypMean(param = newParam)
ghypVar(param = newParam)

(rhozetaPars <- ghypChangePars(1, 2, param))
(standPars <- ghypStandPars(rhozetaPars[3], rhozetaPars[4], lambda))

## try some other mean and sd
(newParam <- ghypScale(1, 1, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
(newParam <- ghypScale(1, 2, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
(newParam <- ghypScale(2, 2, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
(newParam <- ghypScale(-2, 2, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
