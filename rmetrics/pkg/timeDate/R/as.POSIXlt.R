
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  as.POSIXlt                Converts objects of class POSIXlt
#  as.POSIXlt.default        Default Method
################################################################################


# IMPORTANT NOTE:
#   as.POSIXlt was introduced as generic function in R 2.7.0
#   Preliminarily, keep the function here to ne downward
#   compatible with previous R Versions. DW

## YC : Actually we should not use S3 style methods because we are
## using only S4 classes. Generic will be implicitly created
## thanks to setMethod

## .conflicts.OK = TRUE


## # ------------------------------------------------------------------------------


## as.POSIXlt <-
##     function(x, tz = "")
## {
##     # A function implemented by Diethelm Wuertz

##     # FUNCTION:

##     # Return Value:
##     UseMethod("as.POSIXlt")
## }


## # ------------------------------------------------------------------------------


## as.POSIXlt.default <-
##     function (x, tz = "")
## {
##     # A function implemented by Diethelm Wuertz

##     # FUNCTION:

##     # As Posix:
##     fromchar <- function(x) {
##         xx <- x[1]
##         if (is.na(xx)) {
##             j <- 1
##             while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
##             if (is.na(xx))
##                 f <- "%Y-%m-%d"
##         }
##         if (is.na(xx) || !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) ||
##             !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%S")) ||
##             !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) || !is.na(strptime(xx,
##             f <- "%Y/%m/%d %H:%M")) || !is.na(strptime(xx, f <- "%Y-%m-%d")) ||
##             !is.na(strptime(xx, f <- "%Y/%m/%d"))) {
##             res <- strptime(x, f)
##             if (nchar(tz))
##                 attr(res, "tzone") <- tz
##             return(res)
##         }
##         stop("character string is not in a standard unambiguous format")
##     }
##     if (inherits(x, "POSIXlt"))
##         return(x)
##     if (inherits(x, "Date"))
##         return(.Internal(Date2POSIXlt(x)))
##     tzone <- attr(x, "tzone")
##     if (inherits(x, "date") || inherits(x, "dates"))
##         x <- as.POSIXct(x)
##     if (is.character(x))
##         return(fromchar(unclass(x)))
##     if (is.factor(x))
##         return(fromchar(as.character(x)))
##     if (is.logical(x) && all(is.na(x)))
##         x <- as.POSIXct.default(x)
##     if (!inherits(x, "POSIXct"))
##         stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"",
##             deparse(substitute(x))))
##     if (missing(tz) && !is.null(tzone))
##         tz <- tzone[1]
##     .Internal(as.POSIXlt(x, tz))
## }


################################################################################

