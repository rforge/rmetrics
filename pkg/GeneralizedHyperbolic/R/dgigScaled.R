### Function to calculate the density of the
### generalized inverse Gaussian distribution
dgigScaled <- function(x, chi = 1, psi = 1, lambda = 1,
                       param = c(chi, psi, lambda), KOmega = NULL) {

    ## check parameters
    parResult <- gigCheckPars(param)
    case <- parResult$case
    errMessage <- parResult$errMessage

    if (case == "error")
        stop(errMessage)

    param <- as.numeric(param)
    chi <- param[1]
    psi <- param[2]
    lambda <- param[3]

    omega <- sqrt(chi*psi)
    if (is.null(KOmega))
        KOmega <- besselK(omega, nu = lambda, expon.scaled = TRUE)
    ## gigDensity <- ifelse(x > 0, (psi/chi)^(lambda/2)/
    ##                      (2*KOmega)*x^(lambda - 1) *
    ##                      exp(-(1/2)*(chi*x^(-1) + psi*x) + omega), 0)
    ## gigDensity
    (psi/chi)^(lambda/2)/
        (2*KOmega)*x^(lambda - 1)*exp(-(1/2)*(chi*x^(-1) + psi*x) + omega)
} ## end of dgigScaled()


## x <- (1:10000)/1000
## y <- dgigScaled(x, 60, 60, 0.5)
## y[1]
