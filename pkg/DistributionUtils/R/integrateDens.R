integrateDens <- function(densFn = "norm", lower = -Inf, upper = Inf,
                          subdivisions = 100, ...){
    ## Uses code from distCheck by Diethelm Wuertz, modified by David Scott

    ## CALL <- match.call()
    dfun <- match.fun(paste0("d", densFn))
    ## Integrate density over range
    ## and return 'totalPr' :
    integrate(dfun, lower = lower, upper = upper,
              subdivisions = subdivisions,
              stop.on.error = FALSE, ...)
}


