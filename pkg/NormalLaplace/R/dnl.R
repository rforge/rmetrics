######Density function###################################################

dnl <- function (x, mu = 0, sigma = 1, alpha = 1, beta = 1,
                 param = c(mu,sigma,alpha,beta), log = FALSE)
{
  mu  <-  param[1]
  sigma  <-  param[2]
  alpha  <-  param[3]
  beta  <-  param[4]
  a <- dnorm((x - mu)/sigma, log = TRUE)
  b <- millsR(alpha*sigma - (x - mu)/sigma, log = TRUE)
  c <- millsR(beta*sigma + (x - mu)/sigma, log = TRUE)
  d <- log(alpha*beta/(alpha + beta))
  ldnl1 <- d + a + b
  ldnl2 <- d + a + c
  dnl <- exp(ldnl1) + exp(ldnl2)
  ## return the results
  return(dnl)
}




######Distribution function###################################################
pnl  <-  function (q, mu = 0, sigma = 1, alpha = 1, beta= 1,
                      param = c(mu,sigma,alpha,beta))
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]

    a <- dnorm((q-mu)/sigma)
    b <- millsR(alpha*sigma-(q-mu)/sigma)
    c <- millsR(beta*sigma+(q-mu)/sigma)
    d <- pnorm((q-mu)/sigma)
    pnl <- d-a*((beta*b-alpha*c)/(alpha+beta))
#return the results
return(pnl)
}


######Random number function###############################################
rnl  <-  function (n, mu = 0, sigma = 1, alpha = 1, beta= 1,
                   param = c(mu,sigma,alpha,beta))
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]

    ##generate random variates
    Theta <- c(1/beta,1/alpha,0)
    w <- rskewlap(n,Theta)
    z  <-  rnorm(n,0,sigma^2)
    rnl  <- z+w

    ##return the results
    return(rnl)
}

qnl  <-  function(p, mu = 0, sigma = 1, alpha = 1, beta= 1,
                  param = c(mu,sigma,alpha,beta), log = FALSE,
                  tol = 10^(-5), nInterpol = 100, subdivisions = 100,...)      
{
  if(length(param) < 4){
    stop("parameter vector must contain 4 values") 
  } 
  
  mu  <-  param[1]
  sigma  <-  param[2] 
  alpha  <-  param[3] 
  beta  <-  param[4]
  
  maxp<-max(p[p < 1])
  upper <- mu+sigma
  while(pnl(upper, param ) < maxp){
    upper <- upper + sigma
  }
  
  minp<-min(p[p > 0])
  lower <- mu-sigma
  while(pnl(lower, param )> minp){
    lower <- lower - sigma
  }

  xValues  <-  seq(lower,upper,length=nInterpol)


pnlValues  <-  pnl(xValues,param)
pnlSpline  <-  splinefun(xValues,pnlValues) 
q  <-  rep(NA,length(p)) 
   for(i in 1:length(p)){ 
     zeroFun <- function(x){ 
       pnlSpline(x)-p[i] 
     } 
     if((0<p[i]) & (p[i]<1)) 
    q[i]  <-  uniroot(zeroFun,interval=c(lower,upper),...)$root
     if(p[i]==0) q[i]  <-  -Inf
     if(p[i]==1) q[i]  <-  Inf
     if((p[i]<0)|(p[i]>1)) q[i]  <-  NA
   } 
   return(q) 
} # End of qnl()
