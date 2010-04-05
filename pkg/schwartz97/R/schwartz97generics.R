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
