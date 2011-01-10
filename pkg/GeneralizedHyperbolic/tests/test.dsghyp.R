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
param <- ghypChangePars(1, 2, testParam[paramNum,])
param

## convert to standardized parameters
paramStar <- ghypStandPars(param[3], param[4], param[5])
ghypMean(param = paramStar)
ghypVar(param = paramStar)


