### Unit tests of functions  in sampleMoments

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make

test.sampleMoments <- function()
{
  ## Purpose: Level 1 test of sampleMoments
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Local, Date: 03 Feb 2010, 23:00

  ## Check sample moments from normal
  n <- 10000
  x <- rnorm(n)
  distSkew <- 0
  distKurt <- 0
  sampSkew <- skewness(x)
  sampKurt <- kurtosis(x)
  diffSkew <- abs(distSkew - sampSkew)
  diffKurt <- abs(distKurt - sampKurt)
  tolSkew <- 0.04
  tolKurt <- 0.05
  checkTrue(diffSkew < tolSkew)
  checkTrue(diffKurt < tolKurt)

  ## Check sample moments from gamma
  n <- 10000
  x <- rgamma(n, shape = 1)
  distSkew <- 2
  distKurt <- 6
  sampSkew <- skewness(x)
  sampKurt <- kurtosis(x)
  diffSkew <- abs(distSkew - sampSkew)
  diffKurt <- abs(distKurt - sampKurt)
  tolSkew <- 0.1
  tolKurt <- 1
  checkTrue(diffSkew < tolSkew)
  checkTrue(diffKurt < tolKurt)

  return()
}

