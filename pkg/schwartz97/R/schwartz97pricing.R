setGeneric("pricefutures",
           function(ttm = 1, object, ...)
           standardGeneric("pricefutures"),
           package = "schwartz97")

pricefutures.default <- function(ttm = 1, s0 = 50, delta0 = 0,
                                 sigmaS = 0.3, kappa = 1, alpha = 0,
                                 sigmaE = 0.5, rho = 0.75,
                                 r = 0.05, lambda = 0, alphaT = NULL)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT = alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    compA <- .A.schwartz2factor(kappa = kappa, sigmaS = sigmaS,
                                sigmaE = sigmaE, rho = rho,
                                alphaT = alphaT, r = r, ttm = ttm)
    compB <- .B.schwartz2factor(kappa = kappa, ttm = ttm)

    return(s0 * exp(delta0 * compB + compA))
}

setMethod("pricefutures", signature(ttm = "ANY", object = "missing"),
          pricefutures.default)

pricefutures.schwartz2factor <- function(ttm = 1, object, r = 0.05,
                                         lambda = 0, alphaT = NULL)
{
    tmp.coef <- coef(object)

    delta0 <- tmp.coef$delta0
    sigmaS <- tmp.coef$sigmaS
    alpha <- tmp.coef$alpha
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho


    if(missing(lambda) & missing(alphaT)){
        warning("Both 'alphaT' and 'lambda' are missing!\n",
                "The market price of convenience yield risk is set to zero.")
        alphaT <- alpha
    }else if(missing(alphaT)){
        alphaT = alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    return(pricefutures(ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                        sigmaS = sigmaS, kappa = kappa, sigmaE = sigmaE,
                        rho = rho, r = r, alphaT = alphaT))

}

setMethod("pricefutures", signature(ttm = "ANY", object = "schwartz2factor"),
          pricefutures.schwartz2factor)

pricefutures.fit.schwartz2factor <- function(ttm = 1, object)
{
    tmp.coef <- coef(object)

    delta0 <- tmp.coef$delta0
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho
    r <- tmp.coef$r
    alpha <- tmp.coef$alpha
    lambda <- tmp.coef$lambda
    alphaT <- alpha - lambda / kappa

    return(pricefutures(ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                        sigmaS = sigmaS, kappa = kappa, sigmaE = sigmaE,
                        rho = rho, r = r, alphaT = alphaT))
}

setMethod("pricefutures", signature(ttm = "ANY",
                                    object = "fit.schwartz2factor"),
          pricefutures.fit.schwartz2factor)


## <---------- ---------- ---------- ---------- ---------- ---------->
setGeneric("priceoption",
           function(type = c("call", "put"), time = 0.5, Time = 1,
                    K = 40, object, ...)
           standardGeneric("priceoption"),
           package = "schwartz97")

priceoption.default <- function(type = c("call", "put"), time = 0.5,
                                Time = 1, K = 40, g0 = 50,
                                sigmaS = 0.3, kappa = 1,
                                sigmaE = 0.5, rho = 0.75, r = 0.05)
{
    if(Time < time)
        stop("Choose parameters 'time', 'Time' such that 'Time' >= 'time'!")

    G <- g0[1]

    type <- match.arg(type)
    sigma <- .sigma.opt.schwartz2factor(time = time, Time = Time,
                                        kappa = kappa, sigmaE = sigmaE,
                                        rho = rho)
    d <- (log(G / K) + c(0.5, -0.5) * sigma^2) / sigma
    P <- exp(-r * time)

    if(type == "call")
    {
        call <- P * (G * pnorm(d[1]) - K * pnorm(d[2]))
        return(call)
    }else{
        put <- P * (K * pnorm(-d[2]) - G * pnorm(-d[1]))
        return(put)
    }
}

setMethod("priceoption", signature(type = "ANY", time = "ANY",
                                   Time = "ANY", K = "ANY",
                                   object = "missing"),
          priceoption.default)

priceoption.schwartz2factor <- function(type = c("call", "put"),
                                        time = 0.5, Time = 1, K = 40,
                                        object, r = 0.05, lambda = 0, alphaT = NULL)
{
    if(Time < time)
        stop("Choose parameters 'time', 'Time' such that 'Time' >= 'time'!")

    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'alphaT' and 'lambda' are missing!\n",
                "The market price of convenience yield risk is set to zero.")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    G <- pricefutures(ttm = Time - time, object = object, r = r, alphaT = alphaT)

    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    type <- match.arg(type)

    return(priceoption(type = type, time = time, Time = Time, K = K,
                       g0 = G, sigmaS = sigmaS, kappa = kappa,
                       sigmaE = sigmaE, rho = rho, r = r))
}

setMethod("priceoption", signature(type = "ANY", time = "ANY",
                                   Time = "ANY", K = "ANY",
                                   object = "schwartz2factor"),
          priceoption.schwartz2factor)

priceoption.fit.schwartz2factor <- function(type = c("call", "put"),
                                            time = 0.5, Time = 1, K = 40, object)
{
    if(Time < time)
        stop("Choose parameters 'time', 'Time' such that 'Time' >= 'time'!")

    G <- pricefutures(ttm = Time - time, object = object)

    tmp.coef <- coef(object)
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    r <- object@r

    type <- match.arg(type)

    return(priceoption(type = type, time = time, Time = Time, K = K,
                       g0 = G, sigmaS = sigmaS, kappa = kappa,
                       sigmaE = sigmaE, rho = rho, r = r))
}

setMethod("priceoption", signature(type = "ANY", time = "ANY",
                                   Time = "ANY", K = "ANY",
                                   object = "fit.schwartz2factor"),
          priceoption.fit.schwartz2factor)
