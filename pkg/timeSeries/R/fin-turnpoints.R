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
#  .turnpoints2              Returns peaks and pits from a timeSeries
#  .turnpointsStats          Computes turnpoints statistics
#  .turnpointsSeries         Deprecated, use .turnpoints
#  .turnpointsPastecs        Builtin from package pastecs
#  .extract.turnpointsPastecs
#  .plot.turnpointsPastecs
#  .summary.turnpointsPastecs
################################################################################


.turnpoints2 <-
function(x, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns peaks and pits from a timeSeries

    # Arguments:
    #   x - an univariate timeSeries object, e.g. a price or index series
    #   ... - arguments passed to the function na.omit()

    # Example:
    #   .turnpoints2(as.timeSeries(data(msft.dat))[, 4])

    # FUNCTION:

    # Settings:
    stopifnot(isUnivariate(x))

    # Handle Missing Values:
    x = na.omit(x, ...)

    # Convert to Vector:
    X = x
    x = as.vector(x)

    # Turnpoints:
    ans = .turnpointsPastecs(x)
    tp = .extract.turnpointsPastecs(ans)
    data  = cbind(x, tp)
    colnames(data) = c(colnames(X), "TP")
    series(X) = data

    # Return Value:
    X
}


# ------------------------------------------------------------------------------


.turnpointsStats <-
    function(x, doplot = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes turnpoints statistics

    # Arguments:
    #   x - an univariate timeSeries object, e.g. a price or index series
    #   doplot - a logical flag, should an optional plot be displayed?

    # Example:
    #   .turnpoints2(as.timeSeries(data(msft.dat))[, 4])

    # FUNCTION:

    # Settings
    stopifnot(isUnivariate(x))
    X = x
    x = as.vector(x)

    # Turnpoints:
    ans = .turnpointsPastecs(x)

    # Summary Statistics:
    .summary.turnpointsPastecs(ans)

    # Optional Plot:
    if(doplot) .plot.turnpointsPastecs(ans)

    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


.turnpointsSeries =
function(...)
{
    # FUNCTION:
    
    # Deprecated:
    .Deprecated(".turnpointsSeries")
    
    # Return Value:
    .turnpoints2(...)
}


################################################################################
# Package: pastecs
# Title: Package for Analysis of Space-Time Ecological Series
# Version: 1.3-4
# Date: 2006-11-28
# Author: Frederic Ibanez <ibanez@obs-vlfr.fr>,
#   Philippe Grosjean <phgrosjean@sciviews.org> &
#   Michele Etienne <etienne@obs-vlfr.fr>
# Description: Regulation, decomposition and analysis of space-time series.
#   The pastecs library is a PNEC-Art4 and IFREMER
#   (Benoit Beliaeff <Benoit.Beliaeff@ifremer.fr>) initiative
#   to bring PASSTEC 2000
#   (http://www.obs-vlfr.fr/~enseigne/anado/passtec/passtec.htm)
#   functionnalities to R.
# URL: http://www.sciviews.org/pastecs
# Maintainer: Philippe Grosjean <phgrosjean@sciviews.org>
# License: GNU Public Licence 2.0 or above at your convenience
# Depends: boot, stats
# Packaged: Tue Nov 28 15:33:42 2006; Philippe Grosjean


.turnpointsPastecs <-
function(x)
{
    data <- deparse(substitute(x))
    if (is.null(ncol(x)) == FALSE)
        stop("Only one series can be treated at a time")
    # if (exists("is.R") && is.function(is.R) && is.R()) # We are in R
        # Now done with Depends: field require(stats)
    x <- as.vector(x)
    n <- length(x)
    diffs <- c(x[1]-1, x[1:(n-1)]) != x
    uniques <- x[diffs]
    n2 <- length(uniques)
    poss <- (1:n)[diffs]
    exaequos <- c(poss[2:n2], n+1) - poss - 1
    if (n2 < 3) { # We need at least 3 unique values!!!
        warning("Less than 3 unique values, no calculation!")
        nturns <- NA
        firstispeak <- FALSE
        peaks <- rep(FALSE, n2)
        pits <- rep(FALSE, n2)
        tppos <- NA
        proba <- NA
        info <- NA
    } else {
        # The following code is faster in R, but do not work all the time!
        #if (exists("is.R") && is.function(is.R) && is.R()) { # We are in R
        #   ex <- embed(uniques, 3) # Works only in R!
        #   peaks <- c(FALSE, max.col(ex) == 2, FALSE)
        #   pits <- c(FALSE, max.col(-ex) == 2, FALSE)
        #} else { # We are in S+
            m <- n2 - 2
            ex <- matrix(uniques[1:m + rep(3:1, rep(m, 3)) - 1], m)
            peaks <- c(FALSE, apply(ex, 1, max, na.rm=TRUE) == ex[, 2], FALSE)
            pits <- c(FALSE, apply(ex, 1, min, na.rm=TRUE) == ex[, 2], FALSE)
        #}
        tpts <- peaks | pits
        if (sum(tpts) == 0) {   # No turning point
            nturns <- 0
            firstispeak <- FALSE
            peaks <- rep(FALSE, n2)
            pits <- rep(FALSE, n2)
            tppos <- NA
            proba <- NA
            info <- NA
        } else {
            # This way, we consider the last element of duplicates, as
            #   in PASSTEC 2000
            tppos <- (poss + exaequos)[tpts]
            tptspos <- (1:n2)[tpts]
            firstispeak <- tptspos[1] == (1:n2)[peaks][1]
            nturns <- length(tptspos)
            if (nturns < 2) {
                inter <- n2 + 1
                posinter1 <- tptspos[1]
            } else {
                inter <- c(tptspos[2:nturns], n2) -
                    c(1, tptspos[1:(nturns-1)]) + 1
                posinter1 <- tptspos - c(1, tptspos[1:(nturns-1)])
            }
            posinter2 <- inter - posinter1
            posinter <- pmax(posinter1, posinter2)
            proba <- 2 / (inter * gamma(posinter) * gamma(inter - posinter + 1))
            info <- -log(proba, base = 2)
        }
    }
    res <- list(data = data, n = n, points = uniques, pos = (poss + exaequos),
        exaequos = exaequos, nturns = nturns, firstispeak = firstispeak,
        peaks = peaks, pits = pits, tppos = tppos, proba = proba,
        info = info)
    class(res) <- "turnpoints"
    res
}


# ------------------------------------------------------------------------------


.extract.turnpointsPastecs <-
function(e, n, no.tp = 0, peak = 1, pit = -1, ...)
{
    if (missing(n)) n <- -1
    res <- rep(no.tp, length.out= e$n)
    res[e$pos[e$peaks]] <- peak
    res[e$pos[e$pits]] <- pit

    # Keep only the first n points
    if (n < length(res) & n > 0) res <- res[1:n]
    res
}


# ------------------------------------------------------------------------------


.plot.turnpointsPastecs <-
function(x, level = 0.05, lhorz = TRUE, lcol = 2, llty = 2, type = "l",
    xlab = "data number",
    ylab = paste("I (bits), level = ", level*100, "%", sep = ""),
    main = paste("Information (turning points) for:",x$data), ...)
{
    # The next function actually draws the graph
    turnpoints.graph <- function(X, Level, Lhorz, Lcol, Llty, Type, Xlab,
        Ylab, Main, Sub, ...) {
        plot(X$tppos, X$info, type = Type, xlab = Xlab, ylab = Ylab,
            main = Main, ...)
        abline(h = -log(Level, base = 2), lty = Llty, col = Lcol)
    }

    # Return Value:
    invisible(turnpoints.graph(x, level[1], lhorz, lcol, llty, type, xlab,
        ylab, main, ...))
}


# ------------------------------------------------------------------------------


.summary.turnpointsPastecs <-
function(object, ...)
{
    cat("Turning points for:", object$data, "\n\n")
    cat("nbr observations  :", object$n, "\n")
    cat("nbr ex-aequos     :", sum(object$exaequos), "\n")



    if (object$firstispeak) {
        cat("nbr turning points:", object$nturns, "(first point is a peak)\n")
        typep <- c("peak", "pit")
    } else {
        cat("nbr turning points:", object$nturns, "(first point is a pit)\n")
        typep <- c("pit", "peak")
    }

    cat("E(p) =", 2 / 3 * (object$n - 2), "Var(p) =", (16 * object$n - 29) / 90,
        "(theoretical)\n")
    cat("\n")

    # construct the table summarizing all turning points
    typepts <- rep(typep, length.out=object$nturns)
    tablepts <- as.data.frame(list(point = object$tppos, type = typepts,
        proba = object$proba, info = object$info))
    print(tablepts)

    # Return Value:
    invisible(object)
}


################################################################################

