library(randtoolbox)
library(parallel)

#init can be used to continue the sequence
halton(1, init=TRUE)
halton(10, init=FALSE)
#should be the same
halton(11, init=TRUE)

#init can be used to continue the sequence
sobol(1, init=TRUE)
sobol(10, init=FALSE)
#should be the same
sobol(11, init=TRUE)


sobol(1:10, scramb=3, init=TRUE, seed=4711) 
sobol(1:10, scramb=3, init=TRUE, seed=6523) 



if (.Platform$OS.type == "windows")
{
  parallel <- "snow"
  type <- "PSOCK"
}else
{
  parallel <- "multicore" 
  type <- "FORK"
}



func <- function(i, n)
{
  u <- sobol(n, scramb=3, init=TRUE, seed=4711*i)
  c(quantile(u), mean=mean(u), var=var(u))
}

clus <- parallel::makeCluster(2, type = type)
res <- parallel::parSapply(clus, 1:2, func, n=1e4)
parallel::stopCluster(clus)

res

