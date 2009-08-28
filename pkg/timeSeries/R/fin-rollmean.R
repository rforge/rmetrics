

################################################################################
# FUNCTION:
#  rollmean
#  rollmean.default
#  rollmean.matrix
#  rollmean.timeSeries
#  rollmax
#  rollmax.default
#  rollmax.matrix
#  rollmax.timeSeries
#  rollmin           
#  rollmin.default   
#  rollmin.matrix    
#  rollmin.timeSeries
#  rollmedian           
#  rollmedian.default   
#  rollmedian.matrix    
#  rollmedian.timeSeries
################################################################################



# Borrowed from zoo ...


rollmean <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    UseMethod("rollmean")
}


# ------------------------------------------------------------------------------


rollmean.default <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{   
    # FUNCTION:
    
    # roll:
    n <- length(x) 
    y <- x[k:n] - x[c(1, 1:(n-k))]  
    y[1] <- sum(x[1:k])  
    rval <- cumsum(y)/k
    if (na.pad) {
    rval <- switch(match.arg(align),
        "left" = { c(rval, rep(NA, k-1)) },
        "center" = { c(rep(NA, floor((k-1)/2)), rval, 
            rep(NA, ceiling((k-1)/2))) },
        "right" = { c(rep(NA, k-1), rval) })
    }
    
    # Return Value:
    rval
}


# ------------------------------------------------------------------------------


rollmean.matrix <-
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    ans <- apply(x, 2, rollmean.default, k = k, na.pad = na.pad, align = align)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rollmean.timeSeries <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   rollmean(x = X, k = 3)

    # FUNCTION:
    
    # roll:
    ans <- 
        rollmean.matrix(series(x), k = k, na.pad = na.pad, align = align)
    series(x) <- ans
    colnames(x) <- paste(colnames(x), "RMEAN", sep = "_")
    
    # Return Value:
    x
}



################################################################################


rollmax <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    UseMethod("rollmax")
}


# ------------------------------------------------------------------------------


rollmax.default <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    n <- length(x) 
    rval <- rep(0, n) 
    a <- 0
    for (i in k:n) {
        rval[i] <- 
            if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
                max(x[(i-k+1):i]) 
            else 
                max(rval[i-1], x[i]); # max of window = rval[i-1] 
        a <- x[i-k+1] # point that will be removed from window
    }
    rval <- rval[-seq(k-1)]
    if (na.pad) {
        rval <- switch(match.arg(align),
            "left" = { c(rval, rep(NA, k-1)) },
            "center" = { c(rep(NA, floor((k-1)/2)), rval, 
                rep(NA, ceiling((k-1)/2))) },
            "right" = { c(rep(NA, k-1), rval) })
    }
    
    # Return Value:
    rval
} 


# ------------------------------------------------------------------------------


rollmax.matrix <-
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    ans = apply(x, 2, rollmax.default, k = k, na.pad = na.pad, align = align)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rollmax.timeSeries <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   rollmax(x = X, k = 3)
    
    # FUNCTION:
    
    # roll:
    ans <- 
        rollmax.matrix(series(x), k = k, na.pad = TRUE, align = align)
    series(x) <- ans
    colnames(x) <- paste(colnames(x), "RMAX", sep = "_")
    if(!na.pad) x = na.omit(x)
    
    # Return Value:
    x
}


################################################################################


rollmin <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    UseMethod("rollmin")
}


# ------------------------------------------------------------------------------


rollmin.default <- 
function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    ans = -rollmax.default(x, k, na.pad = FALSE, 
        align = c("center", "left", "right"), ...)
        
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


rollmin.matrix <-
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    ans <- apply(x, 2, rollmin.default, k = k, na.pad = na.pad, align = align)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rollmin.timeSeries <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   rollmin(x = X, k = 3)

    # FUNCTION:
    
    # roll:
    ans <- 
        rollmin.matrix(series(x), k = k, na.pad = TRUE, align = align)
    series(x) <- ans
    colnames(x) <- paste(colnames(x), "RMAX", sep = "_")
    if(!na.pad) x = na.omit(x)
    
    # Return Value:
    x
}


################################################################################


rollmedian <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    UseMethod("rollmedian")
}


# ------------------------------------------------------------------------------


rollmedian.default <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    stopifnot(k <= length(x), k %% 2 == 1)
    n <- length(x)
    m <- k %/% 2
    rval <- runmed(x, k, ...)
    attr(rval, "k") <- NULL
    rval <- rval[-c(1:m, (n-m+1):n)]
    if (na.pad) {
        rval <- switch(match.arg(align),
            "left" = { c(rval, rep(NA, k-1)) },
            "center" = { c(rep(NA, floor((k-1)/2)), rval, 
                rep(NA, ceiling((k-1)/2))) },
            "right" = { c(rep(NA, k-1), rval) })
    }
    
    # Return Value:
    rval
}


# ------------------------------------------------------------------------------


rollmedian.matrix <-
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # FUNCTION:
    
    # roll:
    ans <- apply(x, 2, rollmedian.default, k = k, na.pad = na.pad, 
        align = align)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rollmedian.timeSeries <- 
    function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   rollmedian(x = X, k = 3)
    
    # FUNCTION:
    
    # roll:
    ans <- 
        rollmedian.matrix(series(x), k = k, na.pad = TRUE, align = align)
    series(x) <- ans
    colnames(x) <- paste(colnames(x), "RMED", sep = "_")
    if(!na.pad) x = na.omit(x)
    
    # Return Value:
    x
}


################################################################################

