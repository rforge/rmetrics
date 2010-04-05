## Compute risk measures

### < ====================================================================== >
setGeneric("VaRfutures",
           function(level = 0.95, time = 0.1, ttm = 1, g0, ...)
           standardGeneric("VaRfutures"))

### < ---------------------------------------------------------------------- >
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
  compA <- .A.schwartz2f(kappa = kappa, sigmaS = sigmaS,
                         sigmaE = sigmaE, rho = rho,
                         alphaT = alphaT, r = r, ttm = ttm)
  compB <- .B.schwartz2f(kappa = kappa, ttm = ttm)
  x0 <- log(g0) - compA - compB * delta0

  ## compute and return VaR
  mu.fut <- .mu.fut.schwartz2f(x0, delta0, mu, sigmaS,
                               kappa, sigmaE, rho, alpha,
                               alphaT, r, time, ttm)
  sigma.fut <- sqrt(.sigma.fut.schwartz2f(sigmaS, kappa, sigmaE,
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
                                  ttm = "ANY", g0 = "numeric"),
          VaRfutures.default)

### < ---------------------------------------------------------------------- >
VaRfutures.schwartz2f <- function(level = 0.95, time = 0.1, ttm = 1,
                                  g0, type = c("return", "nominal"),
                                  r = 0.05, lambda = 0, alphaT = NULL)
{
  type <- match.arg(type)
  tmp.coef <- coef(g0)

  if(missing(lambda) & missing(alphaT)){
    warning("Both 'lambda' and 'alphaT' are missing!\n",
            "The market price of risk is set to zero")
    alphaT <- tmp.coef$alpha
  }else if(missing(alphaT)){
    alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
  }
  g0 <- pricefutures(ttm, g0, r = r, alphaT = alphaT)

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
                                  ttm = "ANY", g0 = "schwartz2f"),
          VaRfutures.schwartz2f)

### < ---------------------------------------------------------------------- >
VaRfutures.schwartz2f.fit <- function(level = 0.95, time = 0.1, ttm = 1,
                                      g0, type = c("return", "nominal"))
{
  type <- match.arg(type)
  tmp.coef <- coef(g0)

  delta0 <- tmp.coef$delta0
  mu <- tmp.coef$mu
  sigmaS <- tmp.coef$sigmaS
  kappa <- tmp.coef$kappa
  sigmaE <- tmp.coef$sigmaE
  rho <- tmp.coef$rho
  r <- tmp.coef$r
  alphaT <- tmp.coef$alphaT

  g0 <- pricefutures(ttm, g0)

  return(VaRfutures(level = level, time = time, ttm = ttm, g0 = g0,
                    type = type, delta0 = delta0,
                    mu = mu, sigmaS = sigmaS, kappa = kappa,
                    sigmaE = sigmaE, rho = rho, r = r,
                    alphaT = alphaT))
}

setMethod("VaRfutures", signature(level = "ANY", time = "ANY",
                                  ttm = "ANY", g0 = "schwartz2f.fit"),
          VaRfutures.schwartz2f.fit)


### < ====================================================================== >
setGeneric("ESfutures",
           function(level = 0.95, time = 0.1, ttm = 1, g0, ...)
           standardGeneric("ESfutures"))

### < ---------------------------------------------------------------------- >
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
  compA <- .A.schwartz2f(kappa = kappa, sigmaS = sigmaS,
                         sigmaE = sigmaE, rho = rho,
                         alphaT = alphaT, r = r, ttm = ttm)
  compB <- .B.schwartz2f(kappa = kappa, ttm = ttm)
  x0 <- log(g0) - compA - compB * delta0

  ## compute and return VaR
  mu.fut <- .mu.fut.schwartz2f(x0, delta0, mu, sigmaS,
                               kappa, sigmaE, rho, alpha,
                               alphaT, r, time, ttm)

  sigma.fut <- sqrt(.sigma.fut.schwartz2f(sigmaS, kappa, sigmaE,
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
                                 ttm = "ANY", g0 = "numeric"),
          ESfutures.default)

### < ---------------------------------------------------------------------- >
ESfutures.schwartz2f <- function(level = 0.95, time = 0.1, ttm = 1, g0,
                                 type = c("return", "nominal"), r = 0.05,
                                 lambda = 0, alphaT = NULL)
{
  type <- match.arg(type)
  tmp.coef <- coef(g0)

  if(missing(lambda) & missing(alphaT)){
    warning("Both 'lambda' and 'alphaT' are missing!\n",
            "The market price of risk is set to zero")
    alphaT <- tmp.coef$alpha
  }else if(missing(alphaT)){
    alphaT = tmp.coef$alpha - lambda / tmp.coef$kappa
  }
  g0 <- pricefutures(ttm, g0, r = r, alphaT = alphaT)

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
                                 ttm = "ANY", g0 = "schwartz2f"),
          ESfutures.schwartz2f)

### < ---------------------------------------------------------------------- >
ESfutures.schwartz2f.fit <- function(level = 0.95, time = 0.1, ttm = 1,
                                     g0, type = c("return", "nominal"))
{
  type <- match.arg(type)
  tmp.coef <- coef(g0)

  delta0 <- tmp.coef$delta0
  mu <- tmp.coef$mu
  sigmaS <- tmp.coef$sigmaS
  kappa <- tmp.coef$kappa
  sigmaE <- tmp.coef$sigmaE
  rho <- tmp.coef$rho
  r <- tmp.coef$r
  alphaT <- tmp.coef$alphaT

  g0 <- pricefutures(ttm, g0)

  return(ESfutures(level = level, time = time, ttm = ttm, g0 = g0,
                   type = type, delta0 = delta0,
                   mu = mu, sigmaS = sigmaS, kappa = kappa,
                   sigmaE = sigmaE, rho = rho, r = r,
                   alphaT = alphaT))
}

setMethod("ESfutures", signature(level = "ANY", time = "ANY",
                                 ttm = "ANY", g0 = "schwartz2f.fit"),
          ESfutures.schwartz2f.fit)
