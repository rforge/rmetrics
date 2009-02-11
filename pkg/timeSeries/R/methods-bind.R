#
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
# FUNCTION:                 DESCRIPTION:
#  cbind.timeSeries          Binds columns of two 'timeSeries' objects
#  rbind.timeSeries          Binds rows of two 'timeSeries' objects
################################################################################

cbind.timeSeries <- function(..., deparse.level = 1)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    dots <- list(...)

    if (any(!unlist(lapply(dots, inherits, "timeSeries"))))
        stop("Args must be 'timeSeries' objects")

    units <- unlist(lapply(dots, colnames))
    # change colnames if there are the same
    if (length(unique(units)) != length(units)) {
        for (name in unique(units)) {
            pos <- grep(name, units)
            if (length(pos) != 1)
                units[pos] <- paste(units[pos], seq(pos), sep = ".")
        }
    }

    # FIXME : data.frame

    if (any(unlist(lapply(dots, function(ts) ts@format == "counts")))) {
        if (!diff(range((unlist(lapply(dots, nrow))))))
            stop("number of rows must match")
        return(timeSeries(data=sapply(dots, getDataPart), units = units))
    }

    dots <- lapply(dots, sort)
    tds <- lapply(dots, function(ts) as.numeric(time(ts), "sec"))
    td <- unique(sort(unlist(tds)))

    fun <- function(ts, td, ref) {
        mm <- matrix(NA, ncol = ncol(ts), nrow = length(ref))
        mm[findInterval(td, ref),] <- getDataPart(ts)
        mm}
    data <- mapply(fun, ts = dots, td = tds, MoreArgs = list(ref=td))
    data <- structure(unlist(data), dim = c(length(td), sum(sapply(dots, ncol))))

    # note that new timeSeries get FinCenter of first entry of args
    timeSeries(data = data, charvec = td, units = units, zone = "GMT", FinCenter = finCenter(dots[[1]]))
}

# ------------------------------------------------------------------------------

setMethod("cbind2", c("timeSeries", "timeSeries"),
          function(x, y) cbind(x, y))
setMethod("cbind2", c("timeSeries", "ANY"),
          function(x,y) callGeneric(x, as(y, "timeSeries")))
setMethod("cbind2", c("ANY", "timeSeries"),
          function(x,y) callGeneric(as(x, "timeSeries"), y))
setMethod("cbind2", c("timeSeries", "missing"), function(x,y) x)

# ------------------------------------------------------------------------------

rbind.timeSeries <- function(..., deparse.level = 1)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Check Arguments:
    dots <- list(...)

    if (any(!unlist(lapply(dots, inherits, "timeSeries"))))
        stop("Args must be 'timeSeries' objects")
    if (diff(range((unlist(lapply(dots, ncol))))))
        stop("number of columns must match")

    # FIXME : should be simplified ...
    clnames <- unlist(lapply(dots, colnames))
    clnames <- structure(clnames, dim = c(ncol(dots[[1]]), length(dots)))
    units <- apply(clnames, 1, paste, collapse = "_")

    # FIXME : data.frame

    # Bind:
    # data <- base::rbind(...) # no because S3 method dispatch done in C level
    data <- sapply(seq.int(ncol(dots[[1]])),
                   function(idx, ts) unlist(lapply(ts, .subset, TRUE, idx)),
                   dots)

    if (any(unlist(lapply(dots, function(ts) ts@format == "counts")))) {
        return(timeSeries(data=data, units = units))
    }

    tds <- unlist(lapply(dots, function(ts) as.numeric(time(ts), "sec")))
    ans <- timeSeries(data = data, charvec = tds, zone = "GMT",
                      FinCenter = finCenter(dots[[1]]), units = units)
    sort(ans)
}

# ------------------------------------------------------------------------------

setMethod("rbind2", c("timeSeries", "timeSeries"),
          function(x, y) rbind(x, y))
setMethod("rbind2", c("timeSeries", "ANY"),
          function(x,y) callGeneric(x, as(y, "timeSeries")))
setMethod("rbind2", c("ANY", "timeSeries"),
          function(x,y) callGeneric(as(x, "timeSeries"), y))
setMethod("rbind2", c("timeSeries", "missing"), function(x,y) x)

################################################################################
