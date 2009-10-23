## <---------- ---------- ---------- ---------- ---------- ---------->
## Compute risk measures

setGeneric("VaRfutures",
           function(level = 0.95, time = 0.1, ttm = 1, object, ...)
           standardGeneric("VaRfutures"),
           package = "schwartz97")

VaRfutures.default <- function(level = 0.95, time = 0.1, ttm = 1, g0 = 50,
                               type = c("return", "nominal"), delta0 = 0,
                               mu = 0.1, sigmaS = 0.3, kappa = 1,
                               alpha = 0, sigmaE = 0.5, rho = 0.75,
                               r = 0.05, lambda = 0, alphaT = NULL)
{
    type <- match.arg(type)

    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }

    ## compute spot log-price x0 (inverting futures pricing formula)
    compA <- .A.schwartz2factor(kappa = kappa, sigmaS = sigmaS,
                                sigmaE = sigmaE, rho = rho,
                                alphaT = alphaT, r = r, ttm = ttm)
    compB <- .B.schwartz2factor(kappa = kappa, ttm = ttm)
    x0 <- log(g0) - compA - compB * delta0

    ## compute and return VaR
    mu.fut <- .mu.fut.schwartz2factor(x0, delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, time, ttm)
    sigma.fut <- sqrt(.sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE,
                                                 rho, time, ttm))

    if(type == "return")
        ans <- log(g0) - mu.fut + sigma.fut * qnorm(level)
    else{
        level <- 1 - level
        ans <- exp(mu.fut + 0.5 * sigma.fut^2) *
            exp(sigma.fut * (qnorm(level) - 0.5 * sigma.fut))
    }
    return(ans)
}

setMethod("VaRfutures", signature(level = "ANY", time = "ANY",
                                  ttm = "ANY", object = "missing"),
          VaRfutures.default)

VaRfutures.schwartz2factor <- function(level = 0.95, time = 0.1, ttm = 1,
                                       object, type = c("return", "nominal"),
                                       r = 0.05, lambda = 0, alphaT = NULL)
{
    type <- match.arg(type)
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
    }
    g0 <- pricefutures(ttm, object, r = r, alphaT = alphaT)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(VaRfutures(level = level, time = time, ttm = ttm, g0 = g0,
                      type = type, delta0 = delta0,
                      mu = mu, sigmaS = sigmaS, kappa = kappa,
                      sigmaE = sigmaE, rho = rho,
                      r = r, alphaT = alphaT))
}

setMethod("VaRfutures", signature(level = "ANY", time = "ANY",
                                  ttm = "ANY",
                                  object = "schwartz2factor"),
          VaRfutures.schwartz2factor)

VaRfutures.fit.schwartz2factor <- function(level = 0.95, time = 0.1, ttm = 1,
                                           object, type = c("return", "nominal"))
{
    type <- match.arg(type)
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

    return(VaRfutures(level = level, time = time, ttm = ttm, g0 = g0,
                      type = type, delta0 = delta0,
                      mu = mu, sigmaS = sigmaS, kappa = kappa,
                      sigmaE = sigmaE, rho = rho, r = r,
                      alphaT = alphaT))
}

setMethod("VaRfutures", signature(level = "ANY", time = "ANY",
                                  ttm = "ANY",
                                  object = "fit.schwartz2factor"),
          VaRfutures.fit.schwartz2factor)


## <---------- ---------- ---------- ---------- ---------- ---------->
setGeneric("ESfutures",
           function(level = 0.95, time = 0.1, ttm = 1, object, ...)
           standardGeneric("ESfutures"),
           package = "schwartz97")

ESfutures.default <- function(level = 0.95, time = 0.1, ttm = 1, g0 = 50,
                              type = c("return", "nominal"), delta0 = 0,
                              mu = 0.1, sigmaS = 0.3, kappa = 1,
                              alpha = 0, sigmaE = 0.5, rho = 0.75,
                              r = 0.05, lambda = 0, alphaT = NULL)
{
    type <- match.arg(type)

    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }

    ## compute spot log-price x0 (inverting futures pricing formula)
    compA <- .A.schwartz2factor(kappa = kappa, sigmaS = sigmaS,
                                sigmaE = sigmaE, rho = rho,
                                alphaT = alphaT, r = r, ttm = ttm)
    compB <- .B.schwartz2factor(kappa = kappa, ttm = ttm)
    x0 <- log(g0) - compA - compB * delta0

    ## compute and return VaR
    mu.fut <- .mu.fut.schwartz2factor(x0, delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, time, ttm)

    sigma.fut <- sqrt(.sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE,
                                                 rho, time, ttm))

    if(type == "return")
        ans <- log(g0) - mu.fut +
                 sigma.fut * dnorm(qnorm(level)) / (1 - level)
    else{
        level <- 1 - level
        ans <- 1 / level * exp(mu.fut + 0.5 * sigma.fut^2) *
            pnorm(qnorm(level) - sigma.fut)
    }

    return(ans)
}

setMethod("ESfutures", signature(level = "ANY", time = "ANY",
                                 ttm = "ANY", object = "missing"),
          ESfutures.default)

ESfutures.schwartz2factor <- function(level = 0.95, time = 0.1, ttm = 1, object,
                                      type = c("return", "nominal"), r = 0.05,
                                      lambda = 0, alphaT = NULL)
{
    type <- match.arg(type)
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
    }
    g0 <- pricefutures(ttm, object, r = r, alphaT = alphaT)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(ESfutures(level = level, time = time, ttm = ttm, g0 = g0,
                     type = type, delta0 = delta0,
                     mu = mu, sigmaS = sigmaS, kappa = kappa,
                     sigmaE = sigmaE, rho = rho,
                     r = r, alphaT = alphaT))
}

setMethod("ESfutures", signature(level = "ANY", time = "ANY",
                                 ttm = "ANY", object = "schwartz2factor"),
          ESfutures.schwartz2factor)

ESfutures.fit.schwartz2factor <- function(level = 0.95, time = 0.1, ttm = 1,
                                          object, type = c("return", "nominal"))
{
    type <- match.arg(type)
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

    return(ESfutures(level = level, time = time, ttm = ttm, g0 = g0,
                     type = type, delta0 = delta0,
                     mu = mu, sigmaS = sigmaS, kappa = kappa,
                     sigmaE = sigmaE, rho = rho, r = r,
                     alphaT = alphaT))
}

setMethod("ESfutures", signature(level = "ANY", time = "ANY",
                                 ttm = "ANY",
                                 object = "fit.schwartz2factor"),
          ESfutures.fit.schwartz2factor)
