### Function to calculate the theoretical mean of a 
### generalized inverse Gaussian distribution given its parameters.
gigMean <- function (Theta) {
  Theta <- as.numeric(Theta)
  lambda <- Theta[1]
  chi <- Theta[2]
  psi <- Theta[3]
  omega <- sqrt(chi*psi)
  eta <- sqrt(chi/psi)
  eta*RLambda(omega, lambda)
}## End of gigMean() 

### Function to calculate the theoretical variance of a 
### generalized inverse Gaussian distribution given its parameters.
gigVar <- function(Theta){
  Theta <- as.numeric(Theta)
  lambda <- Theta[1]
  chi <- Theta[2]
  psi <- Theta[3]
  omega <- sqrt(chi*psi)
  eta <- sqrt(chi/psi)
  eta^2*WLambda2(omega, lambda)
} ## End of gigVar()

### Function to calculate the theoretical skewness of a 
### generalized inverse Gaussian distribution given its parameters.
gigSkew <- function(Theta){
  Theta <- as.numeric(Theta)
  lambda <- Theta[1]
  chi <- Theta[2]
  psi <- Theta[3]
  omega <- sqrt(chi*psi)
  eta <- sqrt(chi/psi)
  WLambda3(omega, lambda)/WLambda2(omega, lambda)^(3/2)
} ## End of gigSkew()

### Function to calculate the theoretical kurtosis of a 
### generalized inverse Gaussian distribution given its parameters.
gigKurt <- function(Theta){
  Theta <- as.numeric(Theta)
  lambda <- Theta[1]
  chi <- Theta[2]
  psi <- Theta[3]
  omega <- sqrt(chi*psi)
  eta <- sqrt(chi/psi)
  WLambda4(omega, lambda)/WLambda2(omega, lambda)^2
} ## End of gigKurt()


### Function to calculate the theoretical mode point of a 
### generalized inverse Gaussian distribution given its parameters.
gigMode <- function(Theta) {
  Theta <- as.numeric(Theta)
  lambda <- Theta[1]
  chi <- Theta[2]
  psi <- Theta[3]
  (lambda - 1 + sqrt((lambda - 1)^2 + chi*psi))/psi
} ## End of gigMode()
