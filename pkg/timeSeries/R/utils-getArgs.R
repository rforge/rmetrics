

################################################################################


.getArgs <- 
    function(f, signature = character())  
{
    # A function implemented by Diethelm Wuertz
    
    # Examples:
    #   getArgs(returns)
    #   getArgs("returns")
    #   getArgs(returns, "timeSeries") 
    #   getArgs("returns", "timeSeries")
    
    # FUNCTION:
    
    fun = getMethod(f, signature)@.Data 
    test = class(try(body(fun)[[2]][[3]], silent = TRUE))
    if (test == "function") {
        ans = args(body(fun)[[2]][[3]])
    } else {
        ans = args(fun)
    } 
    cat(substitute(f), ",", signature, ":\n", sep = "")
    
    # Return Value:
    ans
}


################################################################################

