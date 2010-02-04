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
  ## Author: David Scott, Date: 03 Feb 2010, 23:00

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
  tolKurt <- 0.1
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

graphicstest.sampleMoments <- function()
{
  ## Purpose: Check sample moment functions graphically
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date:  4 Feb 2010, 12:07

  ## Standard normal
  sampSizes <- c(5,10,20,50,100,200,500,1000,2000,5000,10000)
  results <- matrix(nrow = length(sampSizes), ncol = 4,
                    dimnames = list(NULL,c("tsk","ssk","tku","sku")))
  results <- as.data.frame(results)
  results[, 1] <- 0
  results[, 3] <- 0
  for (i in (1:length(sampSizes))){
    x <- rnorm(sampSizes[i])
    results[i, 2] <- skewness(x)
    results[i, 4] <- kurtosis(x)
  }

  ## plot results
  with(results, {plot(sampSizes, ssk,
                     xlab = "Sample Size", ylab = "Sample Skewness")
                 lines(sampSizes, tsk)
                 })
  title("Sample and Theoretical Skewness: Standard Normal")
  with(results, {plot(sampSizes, sku,
                     xlab = "Sample Size", ylab = "Sample Kurtosis")
                 lines(sampSizes, tku)})
  title("Sample and Theoretical Kurtosis: Standard Normal")


  ## Gamma(1,1)
  sampSizes <- c(5,10,20,50,100,200,500,1000,2000,5000,10000)
  results <- matrix(nrow = length(sampSizes), ncol = 4,
                    dimnames = list(NULL,c("tsk","ssk","tku","sku")))
  results <- as.data.frame(results)
  results[, 1] <- 2
  results[, 3] <- 6
  for (i in (1:length(sampSizes))){
    x <- rgamma(sampSizes[i], shape = 1)
    results[i, 2] <- skewness(x)
    results[i, 4] <- kurtosis(x)
  }

  ## plot results
  with(results, {plot(sampSizes, ssk,
                     xlab = "Sample Size", ylab = "Sample Skewness")
                 lines(sampSizes, tsk)
                 })
  title("Sample and Theoretical Skewness: Gamma(1,1)")
  with(results, {plot(sampSizes, sku,
                     xlab = "Sample Size", ylab = "Sample Kurtosis")
                 lines(sampSizes, tku)})
  title("Sample and Theoretical Kurtosis: Gamma(1,1)")
 


  return()
}


