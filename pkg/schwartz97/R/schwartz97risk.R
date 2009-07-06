## <---------- ---------- ---------- ---------- ---------- ---------->
## Compute risk measures

setGeneric("VaRfutures",
           function(level = 0.95, ttm = 1, object, ...)
           standardGeneric("VaRfutures"),
           package = "schwartz97")

VaRfutures.default <- function(level = 0.95, deltat = 0.1, ttm = 1, g0 = 50, delta0 = 0,
                               mu = 0.1, sigmaS = 0.3, kappa = 1,
                               alpha = 0, sigmaE = 0.5, rho = 0.75,
                               r = 0.05, lambda = 0, alphaT = NULL)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT = alpha - lambda / kappa
    }

    mu.fut <- .mu.fut.schwartz2factor(log(g0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm - deltat)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho,
                                            ttm - deltat)

    return(g0 * (log(g0) - mu.fut + sigma.fut * qnorm(level)))
}

setMethod("VaRfutures", signature(level = "ANY", ttm = "ANY",
                                  object = "missing"),
          VaRfutures.default)

VaRfutures.schwartz2factor <- function(level = 0.95, deltat = 0.1, ttm = 1, object, r = 0.05,
                                       lambda = 0, alphaT = NULL)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
    }
    g0 <- pricefutures(ttm, object, r = 0.05, alphaT = alphaT)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(VaRfutures(level = level, deltat = deltat, ttm = ttm, g0 = g0,
                      delta0 = delta0,
                      mu = mu, sigmaS = sigmaS, kappa = kappa,
                      sigmaE = sigmaE, rho = rho,
                      r = r, alphaT = alphaT))
}

setMethod("VaRfutures", signature(level = "ANY", ttm = "ANY",
                                  object = "schwartz2factor"),
          VaRfutures.schwartz2factor)

VaRfutures.fit.schwartz2factor <- function(level = 0.95, deltat = 0.1, ttm = 1, object)
{
    tmp.coef <- coef(object)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho
    r <- tmp.coef$r
    alphaT <- tmp.coef$alphaT

    g0 <- pricefutures(ttm, object)

    return(VaRfutures(level = level, deltat = deltat, ttm = ttm, g0 = g0,
                      delta0 = delta0,
                      mu = mu, sigmaS = sigmaS, kappa = kappa,
                      sigmaE = sigmaE, rho = rho, r = r,
                      alphaT = alphaT))
}

setMethod("VaRfutures", signature(level = "ANY", ttm = "ANY",
                                  object = "fit.schwartz2factor"),
          VaRfutures.fit.schwartz2factor)


## <---------- ---------- ---------- ---------- ---------- ---------->
setGeneric("ESfutures",
           function(level = 0.95, ttm = 1, object, ...)
           standardGeneric("ESfutures"),
           package = "schwartz97")

ESfutures.default <- function(level = 0.95, deltat = 0.1, ttm = 1, g0 = 50, delta0 = 0,
                              mu = 0.1, sigmaS = 0.3, kappa = 1,
                              alpha = 0, sigmaE = 0.5, rho = 0.75,
                              r = 0.05, lambda = 0, alphaT = NULL)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT = alpha - lambda / kappa
    }

    mu.fut <- .mu.fut.schwartz2factor(log(g0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm - deltat)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE,
                                            rho, ttm - deltat)

    return(g0 * (log(g0) - mu.fut +
                 sigma.fut * dnorm(qnorm(level)) / (1 - level)))
}

setMethod("ESfutures", signature(level = "ANY", ttm = "ANY",
                                 object = "missing"),
          ESfutures.default)

ESfutures.schwartz2factor <- function(level = 0.95, deltat = 0.1, ttm = 1, object, r = 0.05,
                                      lambda = 0, alphaT = NULL)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
    }
    g0 <- pricefutures(ttm, object, r = 0.05, alphaT = alphaT)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(ESfutures(level = level, deltat = deltat, ttm = ttm, g0 = g0,
                     delta0 = delta0,
                     mu = mu, sigmaS = sigmaS, kappa = kappa,
                     sigmaE = sigmaE, rho = rho,
                     r = r, alphaT = alphaT))
}

setMethod("ESfutures", signature(level = "ANY", ttm = "ANY",
                                  object = "schwartz2factor"),
          ESfutures.schwartz2factor)

ESfutures.fit.schwartz2factor <- function(level = 0.95, deltat = 0.1, ttm = 1, object)
{
    tmp.coef <- coef(object)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho
    r <- tmp.coef$r
    alphaT <- tmp.coef$alphaT

    g0 <- pricefutures(ttm, object)

    return(ESfutures(level = level, deltat = deltat, ttm = ttm, g0 = g0,
                     delta0 = delta0,
                     mu = mu, sigmaS = sigmaS, kappa = kappa,
                     sigmaE = sigmaE, rho = rho, r = r,
                     alphaT = alphaT))
}

setMethod("ESfutures", signature(level = "ANY", ttm = "ANY",
                                 object = "fit.schwartz2factor"),
          ESfutures.fit.schwartz2factor)
