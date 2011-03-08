## This is from 'fBasics',  but so small we will not import
## nor export, but just use it ...

.unirootNA <-
    function(f, interval, ...,
             lower = min(interval), upper = max(interval),
             f.lower = f(lower, ...), f.upper = f(upper, ...),
             tol = .Machine$double.eps^0.25, maxiter = 1000)
{
    # Arguments:
    #   see 'uniroot'

    # Value:
    #   Returns the x value of f where the root is located. If
    #   no root exists,  NA will be returned instead. In that case,
    #   the function doesn't terminate with an error  as
    #   the standard function uniroot().

    # Example:
    #   .unirootNA(sin, c(1, 2)); .unirootNA(sin, c(-1, 1))

    # If there is no Root:
    if(is.na(f.lower) || is.na(f.upper) || f.lower * f.upper > 0)
        return(NA)
    ## else there is one :
    uniroot(f, interval = interval, ...,
            lower=lower, upper=upper, f.lower=f.lower, f.upper=f.upper,
            tol=tol, maxiter=maxiter)$root
}
