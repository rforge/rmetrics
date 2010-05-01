### <======================================================================>
vcov.schwartz2f <- function(object, time = 1)
{

  if(length(time) != 1){
    stop("'time' must be a scalar!")
  }
  
  tmp.coef <- coef(object)

  sigma.log <- .sigma.state.schwartz2f(sigmaS = tmp.coef$sigmaS,
                                       kappa = tmp.coef$kappa,
                                       sigmaE = tmp.coef$sigmaE,
                                       rho = tmp.coef$rho,
                                       time = time)
  return(sigma.log)
}
### <---------------------------------------------------------------------->
setMethod("vcov", signature(object = "schwartz2f"), vcov.schwartz2f)
### <---------------------------------------------------------------------->


### <======================================================================>
mean.schwartz2f <- function(x, time = 1)
{

  time <- .get.data(time, type = "uv")
  
  .mean <- function(time, object){
    tmp.coef <- coef(object)
    means.log <- .mu.state.schwartz2f(x0 = log(tmp.coef$s0),
                                      delta0 = tmp.coef$delta0,
                                      mu = tmp.coef$mu,
                                      sigmaS = tmp.coef$sigmaS,
                                      kappa = tmp.coef$kappa,
                                      alpha = tmp.coef$alpha,
                                      sigmaE = tmp.coef$sigmaE,
                                      rho = tmp.coef$rho,
                                      time = time)

    sigma.log <- vcov(x, time = time)
    return(c(s.t = exp(means.log[1] + 1/2 * sigma.log[1,1]),
             delta.t = means.log[2]))

  }
  
  ## Vectorize the mean function
  means <- t(apply(matrix(time), 1, .mean, object = x))

  if(length(time) == 1){
    means <- c(s.t = means[1], delta.t = means[2])
  }
  
  return(means)

}
### <---------------------------------------------------------------------->
setMethod("mean", signature(x = "schwartz2f"), mean.schwartz2f)
### <---------------------------------------------------------------------->


## ### <======================================================================>
## mean.schwartz2f.fit <- function(x, time, measure = c("P", "Q"))
## {

##   measure <- match.arg(measure)

##   tmp.coef <- coef(x)

##   means.log <- .mu.state.schwartz2f(x0 = log(tmp.coef$s0),
##                                     delta0 = tmp.coef$delta0,
##                                     mu = tmp.coef$mu,
##                                     sigmaS = tmp.coef$sigmaS,
##                                     kappa = tmp.coef$kappa,
##                                     alpha = tmp.coef$alpha,
##                                     sigmaE = tmp.coef$sigmaE,
##                                     rho = tmp.coef$rho,
##                                     time = time)

##   sigma.log <- vcov(x, time = time)

##   return(c(s.t = exp(means.log[1] + 1/2 * sigma.log[1,1]),
##            delta.t = means.log[2]))

## }
## ### <---------------------------------------------------------------------->
## setMethod("mean", signature(x = "schwartz2f.fit"), mean.schwartz2f.fit)
## ### <---------------------------------------------------------------------->


### <======================================================================>
show.schwartz2f <- function(object)
{
    cat("\n----------------------------------------------------------------\n")
    cat("Schwartz97 two-factor model:\n\n")
    cat("SDE\n")
    cat("d S_t     = S_t   * (mu - delta_t)    * dt + S_t * sigmaS * dW_1\n")
    cat("d delta_t = kappa * (alpha - delta_t) * dt + sigmaE * dW_2\n")
    cat("E(dW_1 * dW_2) = rho * dt\n\n")
    tmp.coef <- coef(object)
    ## tmp.coef.formatted <- sapply(tmp.coef, function(x)sprintf("% .3E", x))
    cat("Parameters\n")
    tmp.coef.formatted <- sapply(tmp.coef, as.character)
    invisible(apply(cbind(names(tmp.coef), tmp.coef.formatted), 1,
                    function(x, max.len)cat(x[1], rep(" ", max.len - nchar(x[1])), ": ", x[2], "\n", sep = ""),
                    max.len = max(nchar(names(tmp.coef.formatted)))))
    cat("----------------------------------------------------------------\n")
}
### <---------------------------------------------------------------------->
setMethod("show", signature(object = "schwartz2f"), show.schwartz2f)
### <---------------------------------------------------------------------->


### <======================================================================>
show.schwartz2f.fit <- function(object)
{
    cat("\n----------------------------------------------------------------\n")
    cat("Fitted Schwartz97 two-factor model:\n\n")
    cat("SDE\n")
    cat("d S_t     = S_t   * (mu - delta_t)    * dt + S_t * sigmaS * dW_1\n")
    cat("d delta_t = kappa * (alpha - delta_t) * dt + sigmaE * dW_2\n")
    cat("E(dW_1 * dW_2) = rho * dt\n\n")
    tmp.coef <- coef(object)
    ## tmp.coef.formatted <- sapply(tmp.coef, function(x)sprintf("% .3E", x))
    cat("Parameters\n")
    tmp.coef.formatted <- sapply(tmp.coef, as.character)
    invisible(apply(cbind(names(tmp.coef), tmp.coef.formatted), 1,
                    function(x, max.len)cat(x[1], rep(" ", max.len - nchar(x[1])), ": ", x[2], "\n", sep = ""),
                    max.len = max(nchar(names(tmp.coef.formatted)))))
    cat("----------------------------------------------------------------\n")
    cat("Optimization information")
    cat("\nConverged:          ", object@converged, sep = "")
    cat("\nFitted parameters:  ")
    fitted.params <- names(object@fitted.params)[object@fitted.params]
    cat(paste(fitted.params, collapse = ", "), "; (Number: ", length(fitted.params), ")", sep = "")
    cat("\nlog-Likelihood:     ", object@llh, sep = "")
    cat("\nNbr. of iterations: ", object@n.iter, sep = "")
    cat("\n----------------------------------------------------------------\n")
}
### <---------------------------------------------------------------------->
setMethod("show", signature(object = "schwartz2f.fit"),
          show.schwartz2f.fit)
### <---------------------------------------------------------------------->


### <======================================================================>
coef.schwartz2f <- function(object)
{
    return(list(s0 = object@s0,
                delta0 = object@delta0,
                mu = object@mu,
                sigmaS = object@sigmaS,
                kappa = object@kappaE,
                alpha = object@alpha,
                sigmaE = object@sigmaE,
                rho = object@rhoSE))
}
### <---------------------------------------------------------------------->
setMethod("coef", signature(object = "schwartz2f"),
          coef.schwartz2f)
### <---------------------------------------------------------------------->
setMethod("coefficients", signature(object = "schwartz2f"),
          coef.schwartz2f)
### <---------------------------------------------------------------------->

### <======================================================================>
coef.schwartz2f.fit <- function(object)
{
    return(list(s0 = object@s0,
                delta0 = object@delta0,
                mu = object@mu,
                sigmaS = object@sigmaS,
                kappa = object@kappaE,
                alpha = object@alpha,
                sigmaE = object@sigmaE,
                rho = object@rhoSE,
                r = object@r,
                lambda = object@lambdaE,
                alphaT = object@alphaT))
}
### <---------------------------------------------------------------------->
setMethod("coef", signature(object = "schwartz2f.fit"),
          coef.schwartz2f.fit)
### <---------------------------------------------------------------------->
setMethod("coefficients", signature(object = "schwartz2f.fit"),
          coef.schwartz2f.fit)
### <---------------------------------------------------------------------->

### <======================================================================>
plot.schwartz2f <- function(x, n = 100, time = 2, dt = 1/52)
{
  trajectories <- lapply(1:n, function(dummy, obj, n, t)simstate(n, t, obj),
                         obj = x, n = time/dt, t = time)
  
  st <- sapply(trajectories, function(x)x[,1])
  deltat <- sapply(trajectories, function(x)x[,2])

  time.seq <- seq(dt, time, by = dt)
  means <- mean(x, time = time.seq)     # Calculate expectations

  log.st.var <- sapply(time.seq, function(t,obj)vcov(obj, t)[1,1], obj = x)
  st.sd <- sqrt((exp(log.st.var) - 1)) * means[,1]
  deltat.sd <- sqrt(sapply(time.seq, function(t,obj)vcov(obj, t)[2,2], obj = x))  

  par(mfrow = c(2, 1))
  par(oma = c(5,0,0,0) + 0.1)
  par(mai = c(0,1,0,0))

  ## plot spot prices
  plot(time.seq, time.seq, ylim = range(st), type = "n",
       main = "", xlab = "", ylab = "S(t)", xaxt = "n")

  apply(st, 2, function(y, x)lines(x, y, col = "grey"), x = time.seq) 
  lines(time.seq, means[,1], col = "red")
  lines(time.seq, means[,1] + st.sd, col = "red", lty = "dashed")

  ## plot convenience yield
  plot(time.seq, time.seq, ylim = range(deltat), type = "n",
       main = "", xlab = "time", ylab = "delta(t)")
  apply(deltat, 2, function(y, x)lines(x, y, col = "grey"), x = time.seq) 
  lines(time.seq, means[,2], col = "red")
  lines(time.seq, means[,2] + deltat.sd, col = "red", lty = "dashed")
  lines(time.seq, means[,2] - deltat.sd, col = "red", lty = "dashed")

}

### <---------------------------------------------------------------------->
setMethod("plot", signature(x = "schwartz2f",y = "missing"), plot.schwartz2f)
### <---------------------------------------------------------------------->
