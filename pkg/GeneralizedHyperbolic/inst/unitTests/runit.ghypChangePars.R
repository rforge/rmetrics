### Unit tests of function ghypChangePars

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make
### Functions with name graphicstest.* are run by make if
### LEVEL=graphics in call to make

test.ghypChangePars <- function()
{
  ## Purpose: Level 1 test of hypChangePars
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date:  4 Jan 2011, 06:11

  ## select a random parameter value for testing
  ## eliminate problem values first
  data(ghypParam)
  testParam <- ghypLargeParam
  paramSampleSize <- 1

  ## sample parameter values
  np <- NROW(testParam)
  paramNum <- sample(1:np, paramSampleSize, replace = FALSE)
  param1 <- testParam[paramNum,]

  ## get other parameterizations
  param2 <- ghypChangePars(1, 2, param1)
  param3 <- ghypChangePars(1, 3, param1)
  param4 <- ghypChangePars(1, 4, param1)
  param5 <- ghypChangePars(1, 5, param1)

  ## check all pairs
  param11 <- ghypChangePars(1, 1, ghypChangePars(1, 1, param1))
  checkTrue(max(abs(param11 - param1)) < 10^(-14),
            msg = paste("param1 =", param1, "param11 =", param11))
  param12 <- ghypChangePars(2, 1, ghypChangePars(1, 2, param1))
  checkTrue(max(abs(param12 - param1)) < 10^(-14),
            msg = paste("param1 =", param1, "param12 =", param12))
  param13 <- ghypChangePars(3, 1, ghypChangePars(1, 3, param1))
  checkTrue(max(abs(param13 - param1)) < 10^(-14),
            msg = paste("param1 =", param1, "param13 =", param13))
  param14 <- ghypChangePars(4, 1, ghypChangePars(1, 4, param1))
  checkTrue(max(abs(param14 - param1)) < 10^(-14),
            msg = paste("param1 =", param1, "param14 =", param14))
  param15 <- ghypChangePars(5, 1, ghypChangePars(1, 5, param1))
  checkTrue(max(abs(param15 - param1)) < 10^(-14),
            msg = paste("param1 =", param1, "param15 =", param15))

  param21 <- ghypChangePars(1, 2, ghypChangePars(2, 1, param2))
  checkTrue(max(abs(param21 - param2)) < 10^(-14),
            msg = paste("param1 =", param1, "param11 =", param11))
  param22 <- ghypChangePars(2, 2, ghypChangePars(2, 2, param2))
  checkTrue(max(abs(param22 - param2)) < 10^(-14),
            msg = paste("param2 =", param2, "param22 =", param22))
  param23 <- ghypChangePars(3, 2, ghypChangePars(2, 3, param2))
  checkTrue(max(abs(param23 - param2)) < 10^(-14),
            msg = paste("param2 =", param2, "param23 =", param23))
  param24 <- ghypChangePars(4, 2, ghypChangePars(2, 4, param2))
  checkTrue(max(abs(param24 - param2)) < 10^(-14),
            msg = paste("param2 =", param2, "param24 =", param24))
  param25 <- ghypChangePars(5, 2, ghypChangePars(2, 5, param2))
  checkTrue(max(abs(param25 - param2)) < 10^(-14),
            msg = paste("param2 =", param2, "param25 =", param25))






}
