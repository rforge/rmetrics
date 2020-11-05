library(randtoolbox)

#simple check
umat <- sobol(n=1e5, dim=1111)
sum(umat > 1 | umat < 0)

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

# guido gruetzner bug report
if(FALSE)
{
  umat <- sobol(10, dim=1, init=TRUE, seed=4711, scrambling=1)
  sum(umat >1)
  tt <- sobol(500, dim = 80, init = FALSE, scrambling = 1)
  sum(tt > 1)
  sum(tt < 0)
}

#further tests
if(FALSE)
{
  umat<- sobol(n=10^4,dim=1111,scrambling=1)
  sum(umat > 1)
  for(i in 0:10)
  {
    umat<- sobol(n=10^5,dim=1111,scrambling=3, seed=i*10^5)
    cat("seed", i*10^5, "error", sum(umat > 1), "\n")
  }
  
}




#Code by Art Owen

simsobcheck = function( nlist=2^c(1:10), d=3, rep=10, verbose=FALSE ){
  # First check convergence rate
  # The result is that it clearly gets only O( n^-2 ) variance not O( n^-3 )
  
  f = function(u){
    sum(u)^2
  }
  
  ans = matrix(0,length(nlist),rep )
  
  seed = 0
  for( i in 1:length(nlist) ){
    n = nlist[i]
    for( j in 1:rep ){
      seed = seed+1
      u = sobol(n,dim=d,scrambling=1,seed=seed)
      if( d==1 )
        u = matrix(u,ncol=1)
      mu = mean(apply(u,1,f))
      ans[i,j] = mu
    }
    if( verbose) print(ans[i,])
  }  
  ans
}

figsimsobcheck = function(fn="figsimsobcheck.pdf", export=TRUE){
  
  if(export) 
    pdf(fn)
  
  nlist = 2^c(2:17)
  plot(nlist,apply(simsobcheck(nlist = nlist,d=3 ),1,var),log="xy",xlab="n",ylab="variance",
       main = "Variance vs n, references 1/n 1/n^2 1/n^3")
  lines(nlist,nlist^-1)
  lines(nlist,nlist^-2)
  lines(nlist,nlist^-3)
  
  if(export) 
    dev.off()
}
if(FALSE)
figsimsobcheck(export=FALSE)

