check.initializations <- function()
{
  ##library(schwartz97)
  ##library(RUnit)
  data(futures)
  opt.meas.sd <- "scalar"

  r <- 0.05
  data <- soybeans

  finite.idx <- apply(data$futures, 1, function(x)all(is.finite(x))) &
  apply(data$ttm, 1, function(x)all(is.finite(x)))

  fit <- fit.schwartz2f(data$futures[finite.idx,],
                        ttm = data$ttm[finite.idx,],
                        deltat = 1 / 260,
                        mu = 0.1, sigmaS = 0.3,
                        kappa = 1, alpha = 0, sigmaE = 0.5,
                        rho = 0.75, lambda = 0,
                        opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, sigmaS = TRUE,
                          kappa = TRUE, alpha = TRUE, sigmaE = TRUE,
                          rho = TRUE, lambda = TRUE),
                        opt.meas.sd = opt.meas.sd,
                        r = r, silent = FALSE,
                        control = list(maxit = 100, abstol = 1e-4, reltol = 1e-4))

  coefs <- coef(fit)

  state.obj <- schwartz2f(s0 = coefs$s0, delta0 = coefs$delta0,
                          mu = coefs$mu, sigmaS = coefs$sigmaS,
                          kappa = coefs$kappa, alpha = coefs$alpha,
                          sigmaE = coefs$sigmaE, rho = coefs$rho)

### Densities:
  q <- cbind(runif(10, min(data$futures[finite.idx,]), max(data$futures[finite.idx,])),
             runif(10, min(data$ttm[finite.idx,]), max(data$ttm[finite.idx,])))

  checkEquals(dstate(q, time = 1.65, state.obj), dstate(q, time = 1.65, fit),
              "dstate: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(dstate(q, time = 1.65, state.obj),
              dstate(q, time = 1.65, s0 = coefs$s0, delta0 = coefs$delta0,
                     mu = coefs$mu, sigmaS = coefs$sigmaS,
                     kappa = coefs$kappa, alpha = coefs$alpha,
                     sigmaE = coefs$sigmaE, rho = coefs$rho),
              "dstate: Must give same results for schwartz2f objects and call by 'atomic' arguments!")

### Probabilities:
  lower = c(0, -Inf)
  upper <- colMeans(q)

  checkEquals(pstate(lower, upper, time = 1.65, state.obj), pstate(lower, upper, time = 1.65, fit),
              "pstate: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(pstate(lower, upper, time = 1.65, state.obj),
              pstate(lower, upper, time = 1.65, s0 = coefs$s0, delta0 = coefs$delta0,
                     mu = coefs$mu, sigmaS = coefs$sigmaS,
                     kappa = coefs$kappa, alpha = coefs$alpha,
                     sigmaE = coefs$sigmaE, rho = coefs$rho),
              "pstate: Must give same results for schwartz2f objects and call by 'atomic' arguments!")

### Quantiles:
  prob <- 0.5
  checkEquals(qstate(prob, time = 1.65, state.obj), qstate(prob, time = 1.65, fit),
              "qstate: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(qstate(prob, time = 1.65, state.obj),
              qstate(prob, time = 1.65, s0 = coefs$s0, delta0 = coefs$delta0,
                     mu = coefs$mu, sigmaS = coefs$sigmaS,
                     kappa = coefs$kappa, alpha = coefs$alpha,
                     sigmaE = coefs$sigmaE, rho = coefs$rho),
              "qstate: Must give same results for schwartz2f objects and call by 'atomic' arguments!")


### Random numbers:
  n <- 10
  set.seed(11)
  r.1 <- as.numeric(rstate(n, time = 1.65, state.obj))
  set.seed(11)
  r.2 <- as.numeric(rstate(n, time = 1.65, fit))
  set.seed(11)
  r.3 <- as.numeric(rstate(n, time = 1.65, s0 = coefs$s0, delta0 = coefs$delta0,
                           mu = coefs$mu, sigmaS = coefs$sigmaS,
                           kappa = coefs$kappa, alpha = coefs$alpha,
                           sigmaE = coefs$sigmaE, rho = coefs$rho))

  checkEquals(r.1, r.2,
              "rstate: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(r.1, r.3,
              "rstate: Must give same results for schwartz2f objects and call by 'atomic' arguments!")


### Trajectories:
  n <- 10
  set.seed(11)
  r.1 <- as.numeric(simstate(n, time = 1.65, state.obj))
  set.seed(11)
  r.2 <- as.numeric(simstate(n, time = 1.65, fit))
  set.seed(11)
  r.3 <- as.numeric(simstate(n, time = 1.65, s0 = coefs$s0, delta0 = coefs$delta0,
                             mu = coefs$mu, sigmaS = coefs$sigmaS,
                             kappa = coefs$kappa, alpha = coefs$alpha,
                             sigmaE = coefs$sigmaE, rho = coefs$rho))
  checkEquals(r.1, r.2,
              "simstate: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(r.1, r.3,
              "simstate: Must give same results for schwartz2f objects and call by 'atomic' arguments!")


### Futures (alphaT parametrization of risk neutral measure)
  fut.state <- pricefutures(ttm = 1.55, state.obj, alphaT = coefs$alphaT)
  fut.fit <- pricefutures(ttm = 1.55, fit)
  fut.args <- pricefutures(ttm = 1.55, s0 = coefs$s0, delta0 = coefs$delta0,
                           sigmaS = coefs$sigmaS, kappa = coefs$kappa, alpha = coefs$alpha,
                           sigmaE = coefs$sigmaE, rho = coefs$rho,
                           alphaT = coefs$alphaT, r = r)

  checkEquals(fut.state, fut.fit,
              "pricefutures: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(fut.state, fut.args,
              "pricefutures: Must give same results for schwartz2f objects and call by 'atomic' arguments!")

### Futures (lambda parametrization of risk neutral measure)
  fut.state <- pricefutures(ttm = 1.55, state.obj, lambda = coefs$lambda)
  fut.fit <- pricefutures(ttm = 1.55, fit)
  fut.args <- pricefutures(ttm = 1.55, s0 = coefs$s0, delta0 = coefs$delta0,
                           sigmaS = coefs$sigmaS, kappa = coefs$kappa, alpha = coefs$alpha,
                           sigmaE = coefs$sigmaE, rho = coefs$rho,
                           lambda = coefs$lambda, r = r)

  checkEquals(fut.state, fut.fit,
              "pricefutures: Must give same results for schwartz2f and schwartz2f.fit objects!")

  checkEquals(fut.state, fut.args,
              "pricefutures: Must give same results for schwartz2f objects and call by 'atomic' arguments!")


}
