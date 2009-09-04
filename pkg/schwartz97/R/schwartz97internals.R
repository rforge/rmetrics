.clean.rda.data <- function(tmp.list, idx = 1:6)
{
    na.idx <- apply(is.na(tmp.list$futures[,idx]), 1, any)
    tmp.list$futures <- tmp.list$futures[!na.idx, idx]
    tmp.list$maturity <- tmp.list$maturity[!na.idx, idx]
    return(tmp.list)
}

.mu.state.schwartz2factor <- function(x0, delta0, mu, sigmaS, kappa,
                                      alpha, sigmaE, rho,
                                      time, as.mat = FALSE)
{
    mX <- x0 + (mu - 0.5 * sigmaS^2 - alpha) * time +
        (alpha - delta0) * (1 - exp(-kappa * time))/kappa

    mD <- exp(-kappa * time) * delta0 + alpha *(1 - exp(-kappa * time))

    if(!as.mat)
        return(c(mX, mD))
    else
        return(matrix(c(mX, mD), 2))
}

.sigma.state.schwartz2factor <- function(sigmaS, kappa, sigmaE,
                                          rho, time)
{
    vX <- sigmaE^2 / kappa^2 *
        (1 / (2 * kappa) * (1 - exp(-2 * kappa * time)) -
         2 / kappa * (1 - exp(-kappa * time)) + time) +
             2 * sigmaS * sigmaE * rho / kappa *
                 (1 / kappa * (1 - exp(-kappa * time)) - time) +
                     sigmaS^2 * time

    vD <- sigmaE^2 / (2 * kappa) * (1 - exp(-2 * kappa * time))

    vXD <- 1 / kappa *
        ((sigmaS * sigmaE * rho - sigmaE^2 / kappa) *
         (1 - exp(-kappa * time)) + sigmaE^2 / (2 * kappa) *
         (1 - exp(-2 * kappa * time)))

    return(cbind(c(vX, vXD), c(vXD, vD)))
}

.A.schwartz2factor <- function(kappa, sigmaS, sigmaE, rho,
                               alphaT, r, ttm)
{
    term1 <- (r - alphaT + 0.5 * sigmaE^2 / kappa^2 -
              (sigmaS * sigmaE * rho) / kappa) * ttm
    term2 <- 0.25 * sigmaE^2 * (1 - exp(-2 * kappa * ttm)) / kappa^3
    term3 <- (alphaT * kappa + sigmaS * sigmaE * rho -
              sigmaE^2 / kappa) * (1 - exp(- kappa * ttm)) / kappa^2

    return(term1 + term2 + term3)
}



.B.schwartz2factor <- function(kappa, ttm)
{
    return((exp(-kappa * ttm) - 1) / kappa)
}

.mu.fut.schwartz2factor <- function(x0, delta0, mu, sigmaS,
                                    kappa, sigmaE, rho,
                                    alpha, alphaT, r, time, ttm)
{
  compA <- .A.schwartz2factor(kappa, sigmaS, sigmaE, rho,
                              alphaT, r, ttm - time)

  compB <- .B.schwartz2factor(kappa, ttm - time)

  mu.state <- .mu.state.schwartz2factor(x0, delta0, mu, sigmaS, kappa,
                                        alpha, sigmaE, rho, time)

  prod1 <- matrix(c(mu.state, 1), nrow = 1)
  prod2 <- matrix(c(1, compB, compA), ncol = 1)

  return(as.numeric(prod1 %*% prod2))
}

.sigma.fut.schwartz2factor <- function(sigmaS, kappa, sigmaE, rho, time, ttm)
{
    compB <- .B.schwartz2factor(kappa, ttm - time)
    sigma.state <- .sigma.state.schwartz2factor(sigmaS, kappa, sigmaE,
                                                rho, time)

    prod <- matrix(c(1, compB), ncol = 1)
    return(as.numeric(t(prod) %*% sigma.state %*% prod))
}

.sigma.opt.schwartz2factor <- function(time = 0.5, Time = 1, kappa = 1,
                                       sigmaS = 0.3, sigmaE = 0.5,
                                       rho = 0.75)
{
    return(sqrt(sigmaS^2 * time + 2 * sigmaS * sigmaE * rho / kappa *
                (1 / kappa * exp(-kappa * Time) *
                 (exp(kappa * time) - 1) - time) + sigmaE^2 / kappa^2 *
                (time + 1 / (2 * kappa) * exp(-2 * kappa * Time) *
                 (exp(2 * kappa * time) - 1) - 2 / kappa *
                 exp(-kappa * Time) * (exp(kappa * time) - 1))))
}

.state.space.2f <- function(y, ttm, deltat, x0, delta0, kappa,
                            mu, alpha, lambda, sigmaS, sigmaE, rho,
                            gg,
                            ## P0,
                            r, d, n)
{

  ## Transition equation for the Schwartz two-factor model
  ## ------------------------------------------------------------
  ## Exact transition density:
  Tt <- array(c(1, 0, 1/kappa * (exp(-kappa * deltat) - 1),
                exp(-kappa * deltat)), c(2, 2, 1))

  dt <- matrix(c((mu - 1/2 * sigmaS^2 - alpha) * deltat +
                 alpha / kappa * (1 - exp(-kappa * deltat)),
                 alpha * (1 - exp(- kappa * deltat))), 2, 1)

  HHt <- array(.sigma.state.schwartz2factor(sigmaS, kappa, sigmaE,
                                            rho, deltat), c(2, 2, 1))

  ## ------------------------------------------------------------
  ## Density of the linear approx. of the transition equation:
  ##   Tt <- array(matrix(c(1, 0, -deltat, 1 - kappa * deltat), 2, 2),
  ##               c(2, 2, 1))

  ##   dt <- matrix(c((mu - 1/2 * sigmaS^2) * deltat,
  ##                  kappa * alpha * deltat), 2, 1)

  ##   HHt <- array(matrix(c(sigmaS^2 * deltat,
  ##                         rho * sigmaS * sigmaE * deltat,
  ##                         rho * sigmaS * sigmaE * deltat,
  ##                         sigmaE^2 * deltat),
  ##                       2, 2), c(2, 2, 1))
  ## ------------------------------------------------------------


  ## Measurement equation for the Schwartz two-factor model
  yt <- t(y)

  ct <- t(.A.schwartz2factor(kappa = kappa,
                             sigmaS = sigmaS, sigmaE = sigmaE, rho = rho,
                             alphaT = alpha - lambda / kappa,
                             r = r, ttm = ttm))

  Zt <- array(1, c(d,2,n))
  Zt[,2,] <- t(.B.schwartz2factor(kappa = kappa, ttm = ttm))

  GGt <- array(diag(gg^2, d), c(d, d, 1))

  a0 <- c(x0 ,delta0)
  P0 <- HHt[,,1]

  return(list(a0 = a0, P0 = P0, Tt = Tt, dt = dt, HHt = HHt,
              yt = yt, Zt = Zt, ct = ct, GGt = GGt))
}
