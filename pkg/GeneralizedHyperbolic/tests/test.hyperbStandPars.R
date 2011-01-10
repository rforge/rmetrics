### Test standardized hyperbolic distribution
###
### David Scott, 4/1/2011

### Obtain functions for testing
require(GeneralizedHyperbolic)

## Choose parameters at random from test sets
data(hyperbParam)
testParam <- hyperbLargeParam
paramSampleSize <- 1

## sample parameter values
np <- NROW(testParam)
paramNum <- sample(1:np, paramSampleSize, replace = FALSE)
param <- ghypChangePars(1, 2, c(testParam[paramNum,],1))
param

## convert to standardized parameters
paramStar <- hyperbStandPars(param[3], param[4])
hyperbMean(param = paramStar)
hyperbVar(param = paramStar)


