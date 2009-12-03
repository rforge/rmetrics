

################################################################################


.distFitPlot <- 
function(fit, x, FUN = "dnig", main = "Parameter Estimation", 
    span = "auto", add = FALSE, ...)
{
    x = as.vector(x)
    if (span == "auto") span = seq(min(x), max(x), length = 501)
    z = density(x, n = 100, ...)
    x = z$x[z$y > 0]
    y = z$y[z$y > 0]
    
    # The Density function must accept multiple parameters 
    #   from the first parameter
    dFun = match.fun(FUN)
    y.points = dnig(span, fit$par)
    ylim = log(c(min(y.points), max(y.points)))
    if (add) {
        lines(x = span, y = log(y.points), col = "steelblue")
    } else {
        plot(x, log(y), xlim = c(span[1], span[length(span)]),
            ylim = ylim, type = "p", xlab = "x", ylab = "log f(x)", ...)
        title(main)
        lines(x = span, y = log(y.points), col = "steelblue")
    }
}
        
      
################################################################################
  
        