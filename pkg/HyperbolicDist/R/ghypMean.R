### Function to calculate the theoretical mean of a 
### generalized hyperbolic distribution given its parameters.
ghypMean <- function(Theta){
  Theta <- as.numeric(Theta)
  if(length(Theta)==4) Theta <- c(1,Theta)
  lambda <- Theta[1]
  alpha <- Theta[2]
  beta <- Theta[3]
  delta <- Theta[4]
  mu <- Theta[5]

  gamma <- sqrt(alpha^2 - beta^2)
  
  mu + delta*beta*RLambda(delta*gamma, lambda)/gamma
} ## End of ghypMean() 

### Function to calculate the theoretical variance of a 
### generalized hyperbolic distribution given its parameters.
ghypVar <- function(Theta){
  Theta <- as.numeric(Theta)
  if(length(Theta)==4) Theta <- c(1,Theta)
  lambda <- Theta[1]
  alpha <- Theta[2]
  beta <- Theta[3]
  delta <- Theta[4]
  mu <- Theta[5]  

  gamma <- sqrt(alpha^2 - beta^2)

  (delta/gamma)*RLambda(delta*gamma, lambda) + ((beta*delta/gamma)^2)*
    (MLambda(delta*gamma, lambda) - RLambda(delta*gamma, lambda)^2)
} ## End of ghypVar()


### Function to calculate the theoretical mode point of a 
### generalized hyperbolic distribution given its parameters.
ghypMode <- function(Theta){
  Theta <- as.numeric(Theta)
  if(length(Theta)==4) Theta <- c(1,Theta)
  lambda <- Theta[1]
  alpha <- Theta[2]
  beta <- Theta[3]
  delta <- Theta[4]
  mu <- Theta[5]

  modeFun <- function(x){
    log(dghyp(x, Theta))
  }
  start <- ghypMean(Theta)
  optResult <- optim(start, modeFun,
                     control = list(fnscale = -1, maxit = 1000),
                     method = "BFGS")
  if (optResult$convergence == 0){
    mode <- optResult$par
  }else{
    mode <- NA
  }
  mode
} ## End of ghypMode()
