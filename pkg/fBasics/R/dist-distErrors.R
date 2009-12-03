

################################################################################


.distStandardErrors <- 
function(fit, obj, x)
{
    # Add Standard Errors and t-Values:
    hessian = tsHessian(x = fit$par, fun = obj, y = x, trace = FALSE)
    colnames(hessian) = rownames(hessian) = names(fit$par)
    fit$cvar = solve(hessian)
    fit$se.coef = sqrt(diag(fit$cvar))
    if (fit$scale) 
        fit$se.coef = fit$se.coef / fit$scaleParams
    fit$tval = fit$par/fit$se.coef
    fit$matcoef = cbind(fit$par, fit$se.coef,
        fit$tval, 2*(1-pnorm(abs(fit$tval))))
    dimnames(fit$matcoef) = list(names(fit$tval), 
        c(" Estimate", " Std. Error", " t value", "Pr(>|t|)"))
        
    # Return Value:
    fit
}
        
      
################################################################################
  
        