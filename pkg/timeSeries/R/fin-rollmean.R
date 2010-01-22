
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:            DESCRIPTION:
#  rollmean             Returns rolling mean
#  rollmin              Returns rolling minimum
#  rollmax              Returns rolling maximum
#  rolmedian            Returns rolling median
################################################################################


.rollmean.timeSeries <-
    function(x, k, na.pad=FALSE, align=c("center", "left", "right"), ...)
{
    # Description:
    #   Returns rolling mean
    
    # Arguments:
    #   x - an object of class timeSeries 
    #   k - integer width of the rolling window.  
    #   na.pad - a logical. Should NA padding be added at beginning? 
    #   align - character specifying whether result should be left- or 
    #       right-aligned or centered (default). 
    #   ... - furter arguments passed to methods. 
    
    # Note:
    #   Internal function are borrowed from package zoo ...

    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   R = .rollmean.timeSeries(x = X, k = 3); R; plot(R)

    # FUNCTION:

    # Internal Function:
    .rollmean.default <- function(x, k, na.pad = FALSE,
        align = c("center", "left", "right"), ...) 
    {
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
        rval
    }

    # Roll:
    ans <- apply(x, 2, .rollmean.default, k = k, na.pad=na.pad, align=align)

    x <- setDataPart(x[seq.int(1,NROW(ans)),], ans)

    colnames(x) <- paste(colnames(x), "RMEAN", sep = "_")
    if(!na.pad) x = na.omit(x)

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


.rollmax.timeSeries <-
    function(x, k, na.pad=FALSE, align=c("center", "left", "right"), ...)
{
    # Description:
    #   Returns rolling maximum
    
    # Arguments:
    #   x - an object of class timeSeries 
    #   k - integer width of the rolling window.  
    #   na.pad - a logical. Should NA padding be added at beginning? 
    #   align - character specifying whether result should be left- or 
    #       right-aligned or centered (default). 
    #   ... - furter arguments passed to methods. 
    
    # Note:
    #   Internal function are borrowed from package zoo ...
    
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   R = .rollmax.timeSeries(x = X, k = 3); plot(R)

    # FUNCTION:

    # Internal Function:
    .rollmax.default <- function(x, k, na.pad = FALSE,
        align = c("center", "left", "right"), ...) 
    {
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
        rval
    }

    # Roll:
    ans <- apply(getDataPart(x), 2, .rollmax.default, k = k, na.pad=na.pad,
        align=align)

    x <- setDataPart(x[seq.int(1,NROW(ans)),], ans)

    colnames(x) <- paste(colnames(x), "RMAX", sep = "_")
    if(!na.pad) x = na.omit(x)

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


.rollmin.timeSeries <-
    function(x, k, na.pad=FALSE, align=c("center", "left", "right"), ...)
{
    # Description:
    #   Returns rolling minimum
    
    # Arguments:
    #   x - an object of class timeSeries 
    #   k - integer width of the rolling window.  
    #   na.pad - a logical. Should NA padding be added at beginning? 
    #   align - character specifying whether result should be left- or 
    #       right-aligned or centered (default). 
    #   ... - furter arguments passed to methods. 
    
    # Note:
    #   Internal function are borrowed from package zoo ...
    
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol = 2), timeCalendar())
    #   R = .rollmin.timeSeries(x = X, k = 3); R; plot(R)

    # FUNCTION:

    .rollmax.default <- function(x, k, na.pad = FALSE,
        align = c("center", "left", "right"), ...) 
    {
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
        rval
    }

    .rollmin.default <- function(x, k, na.pad = FALSE,
        align = c("center", "left", "right"), ...) 
    {
        ans = - .rollmax.default(x, k, na.pad = FALSE,
            align = c("center", "left", "right"), ...)
        ans
    }


    # Roll:
    ans <- apply(x, 2, .rollmin.default, k=k, na.pad=na.pad, align=align)

    x <- setDataPart(x[seq.int(1,NROW(ans)),], ans)

    colnames(x) <- paste(colnames(x), "RMIN", sep = "_")
    if(!na.pad) x = na.omit(x)

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


.rollmedian.timeSeries <-
    function(x, k, na.pad=FALSE, align=c("center", "left", "right"), ...)
{
    # Description:
    #   Returns rolling median
    
    # Arguments:
    #   x - an object of class timeSeries 
    #   k - integer width of the rolling window. Must be odd for rollmedian. 
    #   na.pad - a logical. Should NA padding be added at beginning? 
    #   align - character specifying whether result should be left- or 
    #       right-aligned or centered (default). 
    #   ... - furter arguments passed to methods. 
    
    # Note:
    #   Internal function are borrowed from package zoo ...
    
    # Example:
    #   X = timeSeries(matrix(rnorm(24), ncol=2), timeCalendar())
    #   R = .rollmedian.timeSeries(x = X, k = 3); R; plot(R)

    # FUNCTION:

    # Internal Function:
    .rollmedian.default <- function(x, k, na.pad = FALSE,
        align = c("center", "left", "right"), ...) 
    {
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
        rval
    }


    # Roll:
    ans <- apply(x, 2, .rollmedian.default, k=k, na.pad=na.pad, align=align)

    x <- setDataPart(x[seq.int(1,NROW(ans)),], ans)

    colnames(x) <- paste(colnames(x), "RMED", sep = "_")
    if(!na.pad) x = na.omit(x)

    # Return Value:
    x
}


################################################################################
