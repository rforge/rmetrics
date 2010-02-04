### Unit tests of functions  in momChangeAbout

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make

test.momChangeAbout <- function()
{
  ## Purpose: Level 1 test of momChangeAbout
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: David Scott, Date:  4 Feb 2010, 14:34


  ## Gamma distribution
  k <- 4
  shape <- 2
  old <- 0
  new <- 1
  sampSize <- 1000000
  x <- rgamma(sampSize, shape)
  
  ## Sample moments
  s4new <- mean((x - new)^k)
  s3new <- mean((x - new)^3)
  

  ## Calculate 1st to 4th raw moments 
  m <- numeric(k)
  for (i in 1:k){
    m[i] <- gamma(shape + i)/gamma(shape)
  }
  
  
  ## Calculate 4th moment about new 
  m4new <- momChangeAbout(k, m, old, new)
  ## Calculate 3rd about new
  m3new <- momChangeAbout(3, m, old, new)

  ## Compare with sample values
  s4tol <- 0.01
  s3tol <- 0.01
  checkTrue(abs(s4new - m4new) < s4tol)
  checkTrue(abs(s3new - s3new) < s3tol)
  
  return()
}
