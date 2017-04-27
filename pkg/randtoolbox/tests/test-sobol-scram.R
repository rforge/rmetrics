library(randtoolbox)



#Kemal Dincer, bug report
umat<- sobol(n=2^15,dim=12,scrambling=3,seed=1776)
sum(umat > 1)
umat <- sobol(n=2^10,dim=12,scrambling=1,seed=5742)
sum(umat > 1)
umat <- sobol(n=2^15,dim=12,scrambling=1,seed=1716)
sum(umat > 1)
umat <- sobol(n=2^15,dim=22,scrambling=2,seed=12345678)
sum(umat >= 1)


#Dan Southern, bug report
umat <- sobol(n=2000, dim=13, seed=832121780, scrambling = 1)
sum(umat > 1)

#Marius Hofert, bug report
umat <- sobol(2e5, dim=10, scrambling=1, seed=2185)
sum(umat > 1)

#Nicolas Chopin, bug report
umat <- sobol(2000,dim=2,seed=1352,scrambling=1)
sum(umat > 1)

#Makoto Matsumoto and Mutsuo Saito, bug report
if(FALSE)
{
  n <- 10^8
  umat <- sobol(n, dim = 3, init =TRUE, scrambling = 1)
  sum(umat > 1)
}


#further tests
if(FALSE)
{
umat<- sobol(n=10^4,dim=1111,scrambling=3)
sum(umat > 1)
for(i in 0:10)
{
  umat<- sobol(n=10^5,dim=1111,scrambling=3, seed=i*10^5)
  cat("seed", i*10^5, "error", sum(umat > 1), "\n")
}

}
