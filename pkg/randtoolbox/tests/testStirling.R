library(randtoolbox)

#compare two implementations

f <- function(n)
{
  res <- numeric(n+1)
  res[1:2] <- c(0,1)   

  for(i in 1:(n-1))
  {
    # i <- 1
    k <- 1:(i+1) -1
    res[1:(i+2)] <- c(k * res[1:(i+1)], 0) + c(0, res[1:(i+1)]) 
  }
  res
}

g <- function(n)
{
  if( n == 0)
    res <- 1
  if(n > 0)
    res <- c(0,1)   
  
  if(n > 1)
  {
    for(i in 1:(n-1))
    {
      k <- 1:length(res) -1
      res <- c(k * res, 0) + c(0, res) 
    }
  }
  return(res)    
}

cbind(stirling(10) , f(10))
cbind(stirling(12) , f(12))
system.time(f(2000))
system.time(stirling(2000))
 
# library(microbenchmark)
# microbenchmark(f(1000), g(1000))
# microbenchmark(f(2000), g(2000))
