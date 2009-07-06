### < ====================================================================== >
setGeneric("pstate",
           function(lower, upper, time = 1, object, ...)
           standardGeneric("pstate"))


### < ---------------------------------------------------------------------- >
pstate.default <- function(lower, upper, time = 1, s0 = 50, delta0 = 0,
                           mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
                           sigmaE = 0.5, rho = 0.75, ...)
{
    lower[1] <- log(lower[1])
    upper[1] <- log(upper[1])

    mean <- .mu.state.schwartz2factor(log(s0), delta0, mu, sigmaS, kappa,
                                      alpha, sigmaE, rho,
                                      time)

    sigma <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho, time)

    return(pmvnorm(lower = lower, upper = upper, mean = mean,
                   sigma = sigma, ...))
}

setMethod("pstate", signature(lower = "ANY", upper = "ANY", time = "ANY",
                              object = "missing"),
          pstate.default)

### < ---------------------------------------------------------------------- >
pstate.schwartz2factor <- function(lower, upper, time = 1, object, ...)
{
    tmp.coef <- coef(object)
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(pstate.default(lower, upper, time = time, s0 = tmp.coef$s0,
                          delta0 = delta0, mu = mu, sigmaS = sigmaS,
                          kappa = kappa, alpha = alpha, sigmaE = sigmaE,
                          rho = rho, ...))
}

setMethod("pstate", signature(lower = "ANY", upper = "ANY", time = "ANY",
                              object = "schwartz2factor"),
          pstate.schwartz2factor)
### < ---------------------------------------------------------------------- >


### < ====================================================================== >
setGeneric("dstate",
           function(x, time = 1, object, ...)
           standardGeneric("dstate"))

dstate.default <- function(x, time = 1, s0 = 50, delta0 = 0, mu = 0.1,
                           sigmaS = 0.3, kappa = 1, alpha = 0,
                           sigmaE = 0.5, rho = 0.75, ...)
{
  if(is.vector(x)){
    x <- rbind(x)
  }

  x <- as.matrix(x) # in case x is a data.frame

  x[,1] <- log(x[,1])

  mean <- .mu.state.schwartz2factor(log(s0), delta0, mu, sigmaS, kappa,
                                    alpha, sigmaE, rho, time)

  sigma <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho, time)

##   uniform.margins <- cbind(pnorm(x[,1], mean[1], sqrt(sigma[1,1])),
##                            pnorm(x[,2], mean[2], sqrt(sigma[2,2])))

##   dens <- dmvnorm(qnorm(uniform.margins), sigma = cov2cor(sigma)) *
##     dnorm(x[,1], mean[1], sqrt(sigma[1,1]))/exp(x[,1]) *
##       dnorm(x[,2], mean[2], sqrt(sigma[2,2])) /
##         dnorm(qnorm(uniform.margins[,1]))/ dnorm(qnorm(uniform.margins[,2]))

  dens <- dmvnorm(x, mean = mean, sigma = sigma) / exp(x[,1])

  return(dens)
}


setMethod("dstate", signature(x = "ANY", time = "ANY", object = "missing"),
          dstate.default)
### < ---------------------------------------------------------------------- >

dstate.schwartz2factor <- function(x, time = 1, object, ...)
{
    tmp.coef <- coef(object)
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(dstate.default(x, time = time, s0 = tmp.coef$s0, delta0 = delta0,
                          mu = mu, sigmaS = sigmaS, kappa = kappa,
                          alpha = alpha, sigmaE = sigmaE, rho = rho, ...))
}

setMethod("dstate", signature(x = "ANY", time = "ANY",
                              object = "schwartz2factor"),
          dstate.schwartz2factor)
### < ---------------------------------------------------------------------- >

### < ====================================================================== >
setGeneric("qstate",
           function(p, time = 1, object, ...)
           standardGeneric("qstate"))


### < ---------------------------------------------------------------------- >
qstate.default <- function(p, time = 1, s0 = 50, delta0 = 0, mu = 0.1,
                           sigmaS = 0.3, kappa = 1, alpha = 0,
                           sigmaE = 0.5, rho = 0.75,
                           tail = "lower.tail", ...)
{
    mean <- .mu.state.schwartz2factor(log(s0), delta0, mu, sigmaS, kappa,
                                      alpha, sigmaE, rho, time)

    sigma <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho, time)

    quant <- qmvnorm(p = p, tail = tail, mean = mean, sigma = sigma, ...)
    quant$quantile <- c(exp(quant$quantile), quant$quantile)

    return(quant)
}

setMethod("qstate", signature(p = "ANY", time = "ANY", object = "missing"),
          qstate.default)
### < ---------------------------------------------------------------------- >

qstate.schwartz2factor <- function(p, time = 1, object,
                                   tail = "lower.tail", ...)
{
    tmp.coef <- coef(object)
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(qstate.default(p, time = time, s0 = tmp.coef$s0, delta0 = delta0,
                          mu = mu, sigmaS = sigmaS,
                          kappa = kappa, alpha = alpha,
                          sigmaE = sigmaE, rho = rho,
                          tail = tail, ...))
}

setMethod("qstate", signature(p = "ANY", time = "ANY",
                              object = "schwartz2factor"),
          qstate.schwartz2factor)
### < ---------------------------------------------------------------------- >

### < ====================================================================== >
setGeneric("rstate",
           function(n, time = 1, object, ...)
           standardGeneric("rstate"))

### < ---------------------------------------------------------------------- >
rstate.default <- function(n, time = 1, s0 = 50, delta0 = 0,
                           mu = 0.1, sigmaS = 0.3,
                           kappa = 1, alpha = 0,
                           sigmaE = 0.5, rho = 0.75,
                           method = "chol")
{
    mean <- .mu.state.schwartz2factor(log(s0), delta0, mu, sigmaS, kappa,
                                      alpha, sigmaE, rho, time)

    sigma <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho, time)

    rand <- rmvnorm(n = n, mean = mean, sigma = sigma, method = method)
    rand[,1] <- exp(rand[,1])
    colnames(rand) <- c("S", "delta")
    return(rand)
}

setMethod("rstate", signature(n = "ANY", time = "ANY", object = "missing"),
          rstate.default)
### < ---------------------------------------------------------------------- >

rstate.schwartz2factor <- function(n, time = 1, object,
                                   method = "chol")
{
    tmp.coef <- coef(object)
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(rstate.default(n, time = time, s0 = tmp.coef$s0, delta0 = delta0,
                          mu = mu, sigmaS = sigmaS,
                          kappa = kappa, alpha = alpha,
                          sigmaE = sigmaE, rho = rho,
                          method = method))
}

setMethod("rstate", signature(n = "ANY", time = "ANY",
                              object = "schwartz2factor"),
          rstate.schwartz2factor)
### < ---------------------------------------------------------------------- >


### < ====================================================================== >
setGeneric("simstate",
           function(n, time = 1, object, ...)
           standardGeneric("simstate"))

### < ---------------------------------------------------------------------- >
simstate.default <- function(n, time = 1, s0 = 50, delta0 = 0,
                             mu = 0.1, sigmaS = 0.3,
                             kappa = 1, alpha = 0,
                             sigmaE = 0.5, rho = 0.75,
                             method = "chol")
{
    if(n <= 1){
        stop("Use 'rstate' if n <= 1!")
    }

    deltat <- time/n

    sigma <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho,
                                          time = deltat)

    traj <- matrix(NA, ncol = 2, nrow = n,
                   dimnames = list(1:n, c("S", "delta")))
    traj[1, ] <- c(log(s0), delta0)

    increments <- rmvnorm(n = n - 1, mean = c(0, 0),
                          sigma = sigma, method = method)

    for(i in 2:n){

        drift <- .mu.state.schwartz2factor(x0 = traj[i - 1, 1],
                                           delta0 = traj[i - 1, 2],
                                           mu = mu, sigmaS = sigmaS,
                                           kappa = kappa, alpha = alpha,
                                           sigmaE  = sigmaE, rho = rho,
                                           time = deltat, as.mat = FALSE)
        traj[i, ] <- drift + increments[i - 1, ]
    }

    traj[, 1] <- exp(traj[, 1])

    return(traj)
}

setMethod("simstate", signature(n = "ANY", time = "ANY",
                                object = "missing"),
          simstate.default)
### < ---------------------------------------------------------------------- >

simstate.schwartz2factor <- function(n, time = 1, object, method = "chol")
{
    tmp.coef <- coef(object)
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(simstate.default(n, time = time, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            alpha = alpha, sigmaE = sigmaE, rho = rho,
                            method = method))

}

setMethod("simstate", signature(n = "ANY", time = "ANY",
                                object = "schwartz2factor"),
          simstate.schwartz2factor)
### < ---------------------------------------------------------------------- >

### < ====================================================================== >
setGeneric("pfutures",
           function(q, ttm = 1, object, ...)
           standardGeneric("pfutures"))

### < ---------------------------------------------------------------------- >
pfutures.default <- function(q, ttm = 1, s0 = 50, delta0 = 0, mu = 0.1,
                             sigmaS = 0.3, kappa = 1, alpha = 0,
                             sigmaE = 0.5, rho = 0.75,
                             r = 0.05, lambda = 0, alphaT = NULL, ...)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    mu.fut <- .mu.fut.schwartz2factor(log(s0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho, ttm)

    return(pnorm(log(q), mean = mu.fut, sd = sqrt(sigma.fut), ...))
}

setMethod("pfutures", signature(q = "ANY", ttm = "ANY", object = "missing"),
          pfutures.default)
### < ---------------------------------------------------------------------- >

pfutures.schwartz2factor <- function(q, ttm = 1, object, r = 0.05, lambda = 0,
                                     alphaT = NULL, ...)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT <- tmp.coef$alpha - lambda / tmp.coef$kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(pfutures.default(q, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho, r = r,
                            alphaT = alphaT, ...))
}

setMethod("pfutures", signature(q = "ANY", ttm = "ANY",
                                object = "schwartz2factor"),
          pfutures.schwartz2factor)
### < ---------------------------------------------------------------------- >

pfutures.fit.schwartz2factor <- function(q, ttm = 1, object, ...)
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

    return(pfutures.default(q, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho,
                            r = r, alphaT = alphaT, ...))
}

setMethod("pfutures", signature(q = "ANY", ttm = "ANY",
                                object = "fit.schwartz2factor"),
          pfutures.fit.schwartz2factor)
### < ---------------------------------------------------------------------- >

### < ====================================================================== >
setGeneric("dfutures",
           function(x, ttm = 1, object, ...)
           standardGeneric("dfutures"))

dfutures.default <- function(x, ttm = 1, s0 = 50, delta0 = 0, mu = 0.1,
                             sigmaS = 0.3, kappa = 1, alpha = 0,
                             sigmaE = 0.5, rho = 0.75, r = 0.05,
                             lambda = 0, alphaT = NULL, ...)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    mu.fut <- .mu.fut.schwartz2factor(log(s0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho, ttm)

    return(dnorm(log(x), mean = mu.fut, sd = sqrt(sigma.fut), ...) / x)
}

setMethod("dfutures", signature(x = "ANY", ttm = "ANY", object = "missing"),
          dfutures.default)
### < ---------------------------------------------------------------------- >

dfutures.schwartz2factor <- function(x, ttm = 1, object, r = 0.05, lambda = 0,
                                     alphaT = NULL, ...)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT <- tmp.coef$alpha - lambda / tmp.coef$kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(dfutures.default(x, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho, r = r,
                            alphaT = alphaT, ...))

}

setMethod("dfutures", signature(x = "ANY", ttm = "ANY",
                                object = "schwartz2factor"),
          dfutures.schwartz2factor)
### < ---------------------------------------------------------------------- >

dfutures.fit.schwartz2factor <- function(x, ttm = 1, object, ...)
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

    return(dfutures.default(x, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho,
                            r = r, alphaT = alphaT, ...))
}

setMethod("dfutures", signature(x = "ANY", ttm = "ANY",
                                object = "fit.schwartz2factor"),
          dfutures.fit.schwartz2factor)
### < ---------------------------------------------------------------------- >

### < ====================================================================== >
setGeneric("qfutures",
           function(p, ttm = 1, object, ...)
           standardGeneric("qfutures"),
           package = "schwartz97")

qfutures.default <- function(p, ttm = 1, s0 = 50, delta0 = 0, mu = 0.1,
                             sigmaS = 0.3, kappa = 1, alpha = 0,
                             sigmaE = 0.5, rho = 0.75,
                             r = 0.05, lambda = 0, alphaT = NULL, ...)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    mu.fut <- .mu.fut.schwartz2factor(log(s0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho, ttm)

    return(exp(qnorm(p = p, mean = mu.fut, sd = sqrt(sigma.fut), ...)))
}

setMethod("qfutures", signature(p = "ANY", ttm = "ANY", object = "missing"),
          qfutures.default)
### < ---------------------------------------------------------------------- >

qfutures.schwartz2factor <- function(p, ttm = 1, object, r = 0.05, lambda = 0,
                                     alphaT = NULL, ...)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT <- tmp.coef$alpha - lambda / tmp.coef$kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(qfutures.default(p, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho,
                            r = r, alphaT = alphaT, ...))


}
setMethod("qfutures", signature(p = "ANY", ttm = "ANY",
                                object = "schwartz2factor"),
          qfutures.schwartz2factor)
### < ---------------------------------------------------------------------- >

qfutures.fit.schwartz2factor <- function(p, ttm = 1, object, ...)
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

    return(qfutures.default(p, ttm = ttm, s0 = tmp.coef$s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho,
                            r = r, alphaT = alphaT, ...))
}

setMethod("qfutures", signature(p = "ANY", ttm = "ANY",
                                object = "fit.schwartz2factor"),
          qfutures.fit.schwartz2factor)
### < ---------------------------------------------------------------------- >


### < ====================================================================== >
setGeneric("rfutures",
           function(n, ttm = 1, object, ...)
           standardGeneric("rfutures"),
           package = "schwartz97")

rfutures.default <- function(n, ttm = 1, s0 = 50, delta0 = 0, mu = 0.1,
                             sigmaS = 0.3, kappa = 1, alpha = 0,
                             sigmaE = 0.5, rho = 0.75, r = 0.05,
                             lambda = 0, alphaT = NULL)
{
    if((missing(lambda) | missing(alpha)) & missing(alphaT)){
        warning("Both 'alphaT' and ('lambda' or 'alpha') are missing!\n",
                "The mean-level of the convenience yield is set to zero.")
        alphaT <- 0
    }else if(missing(alphaT)){
        alphaT <- alpha - lambda / kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    mu.fut <- .mu.fut.schwartz2factor(log(s0), delta0, mu, sigmaS,
                                      kappa, sigmaE, rho, alpha,
                                      alphaT, r, ttm)

    sigma.fut <- .sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho, ttm)

    return(exp(rnorm(n = n, mean = mu.fut, sd = sqrt(sigma.fut))))
}

setMethod("rfutures", signature(n = "ANY", ttm = "ANY", object = "missing"),
          rfutures.default)
### < ---------------------------------------------------------------------- >

rfutures.schwartz2factor <- function(n, ttm = 1, object, r = 0.05, lambda = 0,
                                     alphaT = NULL)
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        alphaT <- tmp.coef$alpha
    }else if(missing(alphaT)){
        alphaT <- tmp.coef$alpha - lambda / tmp.coef$kappa
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'lambda' is ignored.")
    }

    s0 <- tmp.coef$s0
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(rfutures.default(n, ttm = ttm, s0 = s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho, r = r,
                            alphaT = alphaT))

}

setMethod("rfutures", signature(n = "ANY", ttm = "ANY",
                                object = "schwartz2factor"),
          rfutures.schwartz2factor)
### < ---------------------------------------------------------------------- >

rfutures.fit.schwartz2factor <- function(n, ttm = 1, object)
{
    tmp.coef <- coef(object)

    s0 <- tmp.coef$s0
    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho
    r <- tmp.coef$r
    alphaT <- tmp.coef$alphaT

    return(rfutures.default(n, ttm = ttm, s0 = s0, delta0 = delta0,
                            mu = mu, sigmaS = sigmaS, kappa = kappa,
                            sigmaE = sigmaE, rho = rho,
                            r = r, alphaT = alphaT))

}

setMethod("rfutures", signature(n = "ANY", ttm = "ANY",
                                object = "fit.schwartz2factor"),
          rfutures.fit.schwartz2factor)
### < ---------------------------------------------------------------------- >


### < ====================================================================== >
setGeneric("filter2factor",
           function(data, ttm, object, ...)
           standardGeneric("filter2factor"))

filter2factor.default <- function(data, ttm, s0 = 50, delta0 = 0,
                                  mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
                                  sigmaE = 0.5, rho = 0.75, r = 0.05,
                                  lambda = 0, alphaT = NULL, deltat = 1/260,
                                  meas.sd = rep(1e-3, ncol(data)),
                                  P0 = 0.5 * diag(c(log(s0), abs(delta0))))
{

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        lambda <- 0
    }else if(missing(lambda)){
        lambda <- kappa * (alpha - alphaT)
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'alphaT' is ignored.")
    }

    data <- log(as.matrix(data))

    stateSpace <- .state.space.2f(y = data, ttm = ttm, deltat = deltat,
                                  x0 = log(s0), delta0 = delta0,
                                  kappa = kappa, mu = mu, alpha = alpha, lambda = lambda,
                                  sigmaS = sigmaS, sigmaE = sigmaE, rho = rho,
                                  gg = meas.sd, r = r, d = ncol(data), n = nrow(data))

    filtered.ts <- fkf(a0 = stateSpace$a0,
                       P0 = stateSpace$P0,
                       Tt = stateSpace$Tt,
                       dt = stateSpace$dt,
                       HHt = stateSpace$HHt,
                       yt = stateSpace$yt,
                       Zt = stateSpace$Zt,
                       ct = stateSpace$ct,
                       GGt = stateSpace$GGt)

    state <- cbind(S = exp(filtered.ts$att[1,]),
                   delta = filtered.ts$att[2,])

    return(list(state = state, fkf.obj = filtered.ts))
}
setMethod("filter2factor", signature(data = "ANY", ttm = "ANY",
                                     object = "missing"),
          filter2factor.default)
### < ---------------------------------------------------------------------- >

filter2factor.schwartz2factor <- function(data, ttm, object, r = 0.05,
                                          lambda = 0, alphaT = NULL,
                                          deltat = 1/260,
                                          meas.sd = rep(1e-3, ncol(data)),
                                          P0 = 0.1 * diag(2))
{
    tmp.coef <- coef(object)

    if(missing(lambda) & missing(alphaT)){
        warning("Both 'lambda' and 'alphaT' are missing!\n",
                "The market price of risk is set to zero")
        lambda <- 0
    }else if(missing(lambda)){
        lambda <- tmp.coef$kappa * (tmp.coef$alpha - alphaT)
    }else if(!missing(alphaT) & !missing(lambda)){
      warning("Both 'alphaT' and 'lambda' were passed: 'alphaT' is ignored.")
    }

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    kappa <- tmp.coef$kappa
    alpha <- tmp.coef$alpha
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho

    return(filter2factor.default(data = data, ttm = ttm,
                                 s0 = tmp.coef$s0, delta0 = delta0, mu = mu,
                                 sigmaS = sigmaS, kappa = kappa, alpha = alpha,
                                 sigmaE = sigmaE, rho = rho, r = r,
                                 lambda = lambda, deltat = deltat, meas.sd = meas.sd))

}
setMethod("filter2factor", signature(data = "ANY", ttm = "ANY",
                                     object = "schwartz2factor"),
          filter2factor.schwartz2factor)
### < ---------------------------------------------------------------------- >

filter2factor.fit.schwartz2factor <- function(data, ttm, object)
{

    tmp.coef <- coef(object)

    delta0 <- tmp.coef$delta0
    mu <- tmp.coef$mu
    sigmaS <- tmp.coef$sigmaS
    alpha <- tmp.coef$alpha
    kappa <- tmp.coef$kappa
    sigmaE <- tmp.coef$sigmaE
    rho <- tmp.coef$rho
    r <- tmp.coef$r
    lambda <- tmp.coef$lambda

    return(filter2factor.default(data = data, ttm = ttm,
                                 s0 = tmp.coef$s0, delta0 = delta0, mu = mu,
                                 sigmaS = sigmaS, kappa = kappa, alpha = alpha,
                                 sigmaE = sigmaE, rho = rho, r = r,
                                 lambda = lambda, deltat = object@deltat,
                                 meas.sd = object@meas.sd))

}

setMethod("filter2factor", signature(data = "ANY", ttm = "ANY",
                                     object = "fit.schwartz2factor"),
          filter2factor.fit.schwartz2factor)
### < ---------------------------------------------------------------------- >

futuresplot <- function(x, type = c("futures", "ttm"), ##, "spot"),
                         contr = 1:8, range = c(x$date[1],
                                      x$date[length(x$date)]), ...)
{
    range <- c(order(x$date == range[1])[length(x$date)],
               order(x$date == range[2])[length(x$date)])

    type <- match.arg(type)
    if(type == "futures")
    {
        plot.ts(x$futures[range[1]:range[2],contr],
                plot.type = "single",
                col = contr, xaxt = "n", ...)
        axis(1, at = 1:length(x$date[range[1]:range[2]]),
             label = x$date[range[1]:range[2]], ...)
    }else{
        ## if(type == "ttm")
        ## {
        plot.ts(x$ttm[range[1]:range[2],contr],
                plot.type = "single",
                col = contr, xaxt = "n", ...)
        axis(1, at = 1:length(x$date[range[1]:range[2]]),
             label = x$date[range[1]:range[2]], ...)
        ## }else{
        ##     plot.ts(x$spot[range[1]:range[2]],
        ##             plot.type = "single", xaxt = "n", ...)
        ##     axis(1, at = 1:length(x$date[range[1]:range[2]]),
        ##          label = x$date[range[1]:range[2]], ...)
        ## }
    }
}

## <---------------------------------------------------------------------->
##setMethod("plot", signature(x = "futures",y = "missing"), plot.futures)
### <---------------------------------------------------------------------->
