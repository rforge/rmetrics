
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     DESCRIPTION:
#  log                           log has become a generic function
#  log.default                   log default method
#  round                         round has become a generic function
#  round.default                 round default method
#  sample                        sample has become a generic function
#  sample.default                sample default method
#  sort                          sort has become a generic function
#  sort.default                  sort default method
#  var                           var has become a generic function
#  var.default                   var default method
# FUNCTION:                     DESCRIPTION:
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method
# FUNCTION:                     DESCRIPTION:
#  as.matrix.ts                  Converts univariate ts to 1-column matrix
#  as.matrix.mts                 Converts multivariate ts to matrix
# FUNCTION:                     DESCRIPTION:
#  .description                  Sets default description string
#  .unirootNA                    Computes zero without error exit    
#  .interp                       Does Akima Spline Interpolation
# FUNCTION:                     DESCRIPTION:
#  modify                        Modifies a 'timeSeries' object
#  modify.default                Default Method
#  atoms                         Extracts atoms from 'timeSeries' object
#  atoms.default                 Default Method
# FUNCTION:                     DESCRIPTION:
#  .datax                        Loads timeSeries objects from demo files
# FUNCTION/VALUE:               DESCRIPTION: 
#  currentYear                   Sets date of the current year
#  .currentYear                  Sets date of the current year
#  myUnits                       Sets date units
# DATA:                         DATA:
#  MSFT                          Microsoft data set from Yahoo
# FUNCTION:                     DESCRIPTION [REQUIRES DATE]:
#  .fjulian                      Transform formatted dates to julian day numbers
#  .julian                       Implements SPlus like 'julian'
# FUNCTION:                     DESCRIPTION
#  .isISO8601                    Checks if the date/time is ISO8601 formatted
# FUNCTION:                     DESCRIPTION:
#  .isPOSIX                      Checks for an object of class POSIX
################################################################################


.conflicts.OK = TRUE


################################################################################
# FUNCTION:                     DESCRIPTION:
#  log                           log has become a generic function
#  log.default                   log default method
#  round                         round has become a generic function
#  round.default                 round default method
#  sample                        sample has become a generic function
#  sample.default                sample default method
#  sort                          sort has become a generic function
#  sort.default                  sort default method
#  var                           var has become a generic function
#  var.default                   var default method


sort = 
function(x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("sort")
}


# ------------------------------------------------------------------------------


sort.default =
function (x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...) 
{   # A copy of the sort() function from R's base package
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Sort:
    if (isfact <- is.factor(x)) {
        if (index.return) 
            stop("'index.return' only for non-factors")
        lev <- levels(x)
        nlev <- nlevels(x)
        isord <- is.ordered(x)
        x <- c(x)
    } else if (!is.atomic(x)) 
        stop("'x' must be atomic")
    if (has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <- x[!ina]
    }
    if (index.return && !is.na(na.last)) 
        stop("'index.return' only for 'na.last = NA'")
    if (!is.null(partial)) {
        if (index.return || decreasing || isfact || !missing(method)) 
            stop("unsupported options for partial sorting")
        if (!all(is.finite(partial))) 
            stop("non-finite 'partial'")
        y <- .Internal(psort(x, partial))
    } else {
        nms <- names(x)
        method <- if (is.numeric(x)) 
            match.arg(method)
        else "shell"
        switch(method, quick = {
            if (!is.null(nms)) {
                if (decreasing) 
                  x <- -x
                y <- .Internal(qsort(x, TRUE))
                if (decreasing) 
                  y$x <- -y$x
                names(y$x) <- nms[y$ix]
                if (!index.return) 
                  y <- y$x
            }
            else {
                if (decreasing) 
                  x <- -x
                y <- .Internal(qsort(x, index.return))
                if (decreasing) 
                  if (index.return) 
                    y$x <- -y$x
                  else y <- -y
            }
        }, shell = {
            if (index.return || !is.null(nms)) {
                o <- sort.list(x, decreasing = decreasing)
                y <- if (index.return) 
                  list(x = x[o], ix = o)
                else x[o]
            }
            else y <- .Internal(sort(x, decreasing))
        })
    }
    if (!is.na(na.last) && has.na) 
        y <- if (!na.last) 
            c(nas, y)
        else c(y, nas)
    if (isfact) 
        y <- (if (isord) 
            ordered
        else factor)(y, levels = seq(len = nlev), labels = lev)
        
    # Return Value:
    y
}


# ------------------------------------------------------------------------------


sample = 
function(x, ...)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("sample")
}  


# ------------------------------------------------------------------------------

    
sample.default =
function (x, size, replace = FALSE, prob = NULL, ...) 
{
    # Changes:
    #
    
    # FUNCTION:
    
    if (length(x) == 1 && x >= 1) {
        if (missing(size)) 
            size <- x
        .Internal(sample(x, size, replace, prob))
    } else {
        if (missing(size)) 
            size <- length(x)
        x[.Internal(sample(length(x), size, replace, prob))]
    }
}


# ------------------------------------------------------------------------------


round =
function(x, ...)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("round")
}


# ------------------------------------------------------------------------------


round.default =
function (x, digits = 0, ...) 
{
    # Changes:
    #
    
    # FUNCTION:
    
    .Internal(round(x, digits, ...))
}       
            
            
# ------------------------------------------------------------------------------


log = 
function(x, base = exp(1)) 
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("log")
}


# ------------------------------------------------------------------------------


log.default =
function(x, base = exp(1))
{   # A copy of the log() function from R's base package

    # Changes:
    #
    
    # FUNCTION:
    
    # Log:
    if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
}


# ------------------------------------------------------------------------------


outlier = 
function(x, sd = 5, complement = TRUE, ...) 
{
    # FUNCTION:
    
    UseMethod("outlier")
}


# ------------------------------------------------------------------------------


outlier.default = 
function(x, sd = 5, complement = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns outliers
    
    # Arguments:
    #   x - a numeric vector
    #   sd - a numeric value of standard deviations, e.g. 5
    #       means that values larger or smaller tahn five 
    #       times the standard deviation of the series will
    #       be detected.
    #   complement - a logical flag, should the outlier series
    #       or its complements be returned.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Find Outliers:
    SD = sd * sd(x)
    if (complement) {
        ans  = x[x <= SD]
    } else {
        ans = x[x > SD]
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


var = 
function(x, y = NULL, na.rm = FALSE, use) 
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("var")
}


# ------------------------------------------------------------------------------


var.default =
function(x, y = NULL, na.rm = FALSE, use) 
{   # A copy of the var() function from R's base package

    # Changes:
    #
    
    # FUNCTION:
    
    # var:
    if (missing(use)) 
        use <- if (na.rm) "complete.obs" else "all.obs"
    na.method <- 
        pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    else stopifnot(is.atomic(x))
    if (is.data.frame(y)) 
        y <- as.matrix(y)
    else stopifnot(is.atomic(y))
    .Internal(cov(x, y, na.method, FALSE))
}


################################################################################
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method


"rownames<-" = 
function(x, value)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("rownames<-")
}


# ------------------------------------------------------------------------------
    

"rownames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

    # Changes:
    #
    
    # FUNCTION:
    
    # rownames<-:
    dn <- dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(x))) < 1)
            stop("attempt to set rownames on object with no dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 1)
        stop("attempt to set rownames on object with no dimensions")
    if(is.null(value)) dn[1] <- list(NULL) else dn[[1]] <- value
    dimnames(x) <- dn
    x
}


################################################################################


"colnames<-" = 
function(x, value)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("colnames<-")
}


# ------------------------------------------------------------------------------
    

"colnames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

    # Changes:
    #
    
    # FUNCTION:
    
    # colnames<-:
    dn <- dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(x))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    if(is.null(value)) dn[2] <- list(NULL) else dn[[2]] <- value
    dimnames(x) <- dn
    x
}


################################################################################


as.POSIXlt = 
function(x, tz = "")
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("as.POSIXlt")
}


# ------------------------------------------------------------------------------


as.POSIXlt.default =
function (x, tz = "") 
{
    # Changes:
    #
    
    # FUNCTION:
    
    # As Posix:
    fromchar <- function(x) {
        xx <- x[1]
        if (is.na(xx)) {
            j <- 1
            while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
            if (is.na(xx)) 
                f <- "%Y-%m-%d"
        }
        if (is.na(xx) || !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) || 
            !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%S")) || 
            !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) || !is.na(strptime(xx, 
            f <- "%Y/%m/%d %H:%M")) || !is.na(strptime(xx, f <- "%Y-%m-%d")) || 
            !is.na(strptime(xx, f <- "%Y/%m/%d"))) {
            res <- strptime(x, f)
            if (nchar(tz)) 
                attr(res, "tzone") <- tz
            return(res)
        }
        stop("character string is not in a standard unambiguous format")
    }
    if (inherits(x, "POSIXlt")) 
        return(x)
    if (inherits(x, "Date")) 
        return(.Internal(Date2POSIXlt(x)))
    tzone <- attr(x, "tzone")
    if (inherits(x, "date") || inherits(x, "dates")) 
        x <- as.POSIXct(x)
    if (is.character(x)) 
        return(fromchar(unclass(x)))
    if (is.factor(x)) 
        return(fromchar(as.character(x)))
    if (is.logical(x) && all(is.na(x))) 
        x <- as.POSIXct.default(x)
    if (!inherits(x, "POSIXct")) 
        stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", 
            deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone)) 
        tz <- tzone[1]
    .Internal(as.POSIXlt(x, tz))
}


# ------------------------------------------------------------------------------


as.matrix.ts = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Transform: 
    ans = as.matrix.default(unclass(x)) 
    attr(ans, "tsp")<-NULL
    rownames(ans)<-NULL
    colnames(ans)<-NULL
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.matrix.mts = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Transform: 
    ans = as.matrix.default(unclass(x)) 
    attr(ans, "tsp")<-NULL
    rownames(ans)<-NULL
    colnames(ans)<-NULL
    
    # Return Value:
    ans
}


################################################################################


.description =
function()
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Sets default description string:
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Description String:
    ans = paste(as.character(date()), "by user:", Sys.getenv("USERNAME"))
    
    # Return Value:
    ans
}


################################################################################


.unirootNA = 
function(f, interval, lower = min(interval), upper = max(interval), 
tol = .Machine$double.eps^0.25, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Searches the interval from lower to upper for a 
    #   root (i.e., zero) of the function f with respect 
    #   to its first argument. 
    
    # Arguments:
    #   see 'uniroot'
    
    # Value:
    #   Returns the x value of f where the root is located. If
    #   now root exists NA will be returned. In the last case
    #   the function doesn't terminate with an error like in the
    #   case of the standard function uniroot.

    # Details:
    #   R:
    #   uniroot(f, interval, lower = min(interval), upper = max(interval),
    #       tol = .Machine$double.eps^0.25, 
    #       maxiter = 1000, ...)
    #   uniroot(f, interval, lower = min(interval), upper = max(interval), 
    #       tol = .Machine$double.eps^.25, 
    #       keep.xy = F, f.lower = NA,  f.upper = NA, ...) 

    # Example:
    #   .unirootNA(sin, c(1, 2)); .unirootNA(sin, c(-1, 1))
        
    # Changes:
    #
    
    # FUNCTION:
    
    # There is no Root:  
    if (is.null(args(f))) {  
        if (f(lower) * f(upper) >=0) return(NA)  
    } else {
        if (f(lower, ...) * f(upper, ...) >= 0) return(NA)
    } 
      
    # There is a Root:  
    ans = uniroot(f = f, interval = interval, lower = lower, 
        upper = upper, tol = tol, ...)
    
    # Return Value:
    ans$root
}  


################################################################################


# Package: akima
# Version: 0.3-4
# Title: Interpolation of irregularly spaced data
# Author: Fortran code by H. Akima
#   R port by Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
# Maintainer: Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
# Description: Linear or cubic spline interpolation for irregular gridded data
# License: Fortran code: ACM, free for non-commercial use, R functions GPL


# ------------------------------------------------------------------------------


.interp = 
function(x, y, z, xo = seq(min(x), max(x), length = 40),
yo = seq(min(y), max(y), length = 40), 
ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolate:
    if (ncp == 0) {
        # use the old version for linear interpolation
        ans = .interp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
    } else {
        ans = .interp.new(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.interp.new = 
function(x, y, z, xo = seq(min(x), max(x), length = 40),
yo = seq(min(y), max(y), length = 40), linear=FALSE,
ncp = NULL, extrap = FALSE, duplicate = "error", 
dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolation:
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    if (!is.null(ncp)){
        if (ncp != 0){
            cat("ncp not supported, it is automatically choosen by Fortran code\n")
        } else {
            cat("linear interpolation not yet implemented with interp.new().\n")
            stop("use interp.old().")
        }
    }
    if (linear){
      cat("linear interpolation not yet implemented with interp.new().\n")
      stop("use interp.old().")
    }

    drx = diff(range(x))
    dry = diff(range(y))
    if (drx == 0 || dry == 0)
        stop("all data collinear")  # other cases caught in Fortran code
    if (drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n = length(x)
    nx = length(xo)
    ny = length(yo)
    if (length(y) != n || length(z) != n)
        stop("Lengths of x, y, and z do not match")
    xy = paste(x, y, sep =",")
    i = match(xy, xy)
    if (duplicate=="user" && !is.function(dupfun))
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate!="error") {
        centre = function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x))
        }
        if (duplicate!="strip") {
            z = unlist(lapply(split(z,i), centre))
            ord = !duplicated(xy)
            x = x[ord]
            y = y[ord]
            n = length(x)
        } else{
            ord = (hist(i, plot = FALSE, freq = TRUE,
                breaks = seq(0.5, max(i)+0.5,1))$counts == 1)
            x = x[ord]
            y = y[ord]
            z = z[ord]
            n = length(x)
        }
    } else {
        if (any(duplicated(xy)))
            stop("duplicate data points")
    }
    zo = matrix(0, nx, ny)
    storage.mode(zo) = "double"
    miss = !extrap   #if not extrapolating use missing values
    extrap = matrix(TRUE, nx, ny)
    if (!is.null(ncp)){
        if (extrap & ncp == 0)
            warning("Cannot extrapolate with linear option")
    } else {
        if (extrap & linear)
            warning("Cannot extrapolate with linear option")
    }
    ans = .Fortran("sdsf3p",
        as.integer(1),
        as.integer(n),
        as.double(x),
        as.double(y),
        as.double(z),
        as.integer(nx),
        x = as.double(xo),
        as.integer(ny),
        y = as.double(yo),
        z = zo,
        ier = integer(1),
        double(36 * n),
        integer(25 * n),
        extrap = as.logical(extrap),
        near = integer(n),
        nxt = integer(n),
        dist = double(n),
        PACKAGE = "fCalendar")
    temp = ans[c("x", "y", "z", "extrap")]
    if (miss) temp$z[temp$extrap] = NA
     
    # Return Value:
    temp[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interp.old = 
function(x, y, z, xo = seq(min(x), max(x), length = 40),
yo = seq(min(y), max(y), length = 40), 
ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolation:
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    if (ncp>25){
        ncp = 25
        cat("ncp too large, using ncp=25\n")
    }
    drx = diff(range(x))
    dry = diff(range(y))
    if (drx == 0 || dry == 0)
        stop("all data collinear")  # other cases caught in Fortran code
    if (drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n = length(x)
    nx = length(xo)
    ny = length(yo)
    if (length(y) != n || length(z) != n)
        stop("Lengths of x, y, and z do not match")
    xy = paste(x, y, sep =",")
    i = match(xy, xy)
    if (duplicate=="user" && !is.function(dupfun))
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate!="error") {
        centre = function(x) {
            switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x))
        }
        if (duplicate!="strip") {
            z = unlist(lapply(split(z,i), centre))
            ord = !duplicated(xy)
            x = x[ord]
            y = y[ord]
            n = length(x)
        } else{
            ord = (hist(i, plot = FALSE, freq = TRUE,
                breaks = seq(0.5,max(i)+0.5,1))$counts==1)
            x = x[ord]
            y = y[ord]
            z = z[ord]
            n = length(x)
          }
    } else {
        if (any(duplicated(xy)))
            stop("duplicate data points")
    }
    zo = matrix(0, nx, ny)
    storage.mode(zo) = "double"
    miss = !extrap   #if not extrapolating use missing values
    misso = matrix(miss, nx, ny)
    if (extrap & ncp == 0)
        warning("Cannot extrapolate with linear option")
    ans = .Fortran("idsfft",
          as.integer(1),
          as.integer(ncp),
          as.integer(n),
          as.double(x),
          as.double(y),
          as.double(z),
          as.integer(nx),
          as.integer(ny),
          x = as.double(xo),
          y = as.double(yo),
          z = zo,
          integer((31 + ncp) * n + nx * ny),
          double(5 * n),
          misso = as.logical(misso),
          PACKAGE = "fCalendar")
    temp = ans[c("x", "y", "z", "misso")]
    temp$z[temp$misso] = NA
    
    # Return Value:
    temp[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interpp =
function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
duplicate = "error", dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolation:
    # interpp.new has some bugs at the moment (segfaults), so use
    # the old Akima code:
    ans = interpp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.interpp.new = 
function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
duplicate = "error", dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolation:
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    if (is.null(xo))
        stop("xo missing")
    if (is.null(yo))
        stop("yo missing")
    if (ncp>25) {
        ncp = 25
        cat("ncp too large, using ncp=25\n")
    }
    drx = diff(range(x))
    dry = diff(range(y))
    if (drx == 0 || dry == 0)
        stop("all data collinear")  # other cases caught in Fortran code
    if (drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n = length(x)
    np = length(xo)
    if (length(yo)!=np)
        stop("length of xo and yo differ")
    if (length(y) != n || length(z) != n)
        stop("Lengths of x, y, and z do not match")
    xy = paste(x, y, sep =",")
    i = match(xy, xy)
    if (duplicate=="user" && !is.function(dupfun))
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate!="error") {
        centre = function(x) {
            switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x)
               )
        }
        if (duplicate!="strip"){
            z = unlist(lapply(split(z,i), centre))
            ord = !duplicated(xy)
            x = x[ord]
            y = y[ord]
            n = length(x)
        } else{
            ord = (hist(i,plot = FALSE, freq = TRUE, 
                breaks=seq(0.5,max(i)+0.5,1))$counts==1)
            x = x[ord]
            y = y[ord]
            z = z[ord]
            n = length(x)
        }
    } else {
        if (any(duplicated(xy)))
            stop("duplicate data points")
    }
    zo = rep(0, np)
    storage.mode(zo) = "double"
    miss = !extrap   #if not extrapolating use missing values
    extrap = seq(TRUE, np)
    if (extrap & ncp == 0)
        warning("Cannot extrapolate with linear option")
    ans = .Fortran("sdbi3p",
        as.integer(1),
        as.integer(n),
        as.double(x),
        as.double(y),
        as.double(z),
        as.integer(np),
        x = as.double(xo),
        y = as.double(yo),
        z = zo,
        double(17 * n),
        integer(25 * n),
        extrap = as.logical(extrap),
        near = integer(n),
        net = integer(n),
        dist = double(n),
        PACKAGE = "fCalendar")
    temp = ans[c("x", "y", "z", "extrap")]
    if (miss) temp$z[temp$extrap] = NA
    
    # Return Value:
    temp[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interpp.old =
function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
duplicate = "error", dupfun = NULL)
{
    # Changes:
    #
    
    # FUNCTION:
    
    # Interpolation:
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    if (is.null(xo))
        stop("xo missing")
    if (is.null(yo))
        stop("yo missing")
    if (ncp > 25){
        ncp = 25
        cat("ncp too large, using ncp=25\n")
    }
    drx = diff(range(x))
    dry = diff(range(y))
    if (drx == 0 || dry == 0)
        stop("all data collinear")  # other cases caught in Fortran code
    if (drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n = length(x)
    np = length(xo)
    if (length(yo)!=np)
        stop("length of xo and yo differ")
    if (length(y) != n || length(z) != n)
        stop("Lengths of x, y, and z do not match")
    xy = paste(x, y, sep =",")
    i = match(xy, xy)
    if (duplicate=="user" && !is.function(dupfun))
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate != "error") {
        centre = function(x) {
            switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x)
               )
        }
        if (duplicate!="strip"){
            z = unlist(lapply(split(z,i), centre))
            ord = !duplicated(xy)
            x = x[ord]
            y = y[ord]
            n = length(x)
        } else {
            ord = (hist(i, plot = FALSE, freq = TRUE, 
                breaks = seq(0.5, max(i)+0.5,1))$counts == 1)
            x = x[ord]
            y = y[ord]
            z = z[ord]
            n = length(x)
        }
    } else {
        if (any(duplicated(xy)))
            stop("duplicate data points")
    }
    zo = rep(0, np)
    storage.mode(zo) = "double"
    miss = !extrap   #if not extrapolating use missing values
    misso = seq(miss, np)
    if (extrap & ncp == 0)
        warning("Cannot extrapolate with linear option")
    ans = .Fortran("idbvip",
        as.integer(1),
        as.integer(ncp),
        as.integer(n),
        as.double(x),
        as.double(y),
        as.double(z),
        as.integer(np),
        x = as.double(xo),
        y = as.double(yo),
        z = zo,
        integer((31 + ncp) * n + np),
        double(8 * n),
        misso = as.logical(misso),
        PACKAGE = "fCalendar")
    temp = ans[c("x", "y", "z", "misso")]
    temp$z[temp$misso] = NA
    
    # Return Value:
    temp[c("x", "y", "z")]
}


################################################################################
#  modify                    Modifies a 'timeSeries' object
#  modify.default            Default Method
#  atoms                     Extracts atoms from 'timeSeries' object
#  atoms.default             Default Method


modify =
function(x, method, units) 
{   # A function implemented by Diethelm WUertz

    # FUNCTION:
    
    # Return Value:
    UseMethod("modify") 
}


# ------------------------------------------------------------------------------


modify.default =
function(x, method = c("sort", "round", "trunc"), units = NULL )
{   # A function implemented by Diethelm WUertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Modify:
    ans = NA
    if (method[1] == "sort") return(sort(x))
    if (method[1] == "round") return(round(x))
    if (method[1] == "trunc") return(trunc(x))
    
    # Return Value:
    ans
}   


# ------------------------------------------------------------------------------


atoms = 
function(x, ...) 
{   # A function implemented by Diethelm WUertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default = 
function(x, ...) 
{   # A function implemented by Diethelm WUertz
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    invisible(x)
}


################################################################################
#  .datax                    Loads timeSeries objects from demo files


.datax = 
function(..., list = character(0), package = NULL, lib.loc = NULL,
verbose = getOption("verbose"), envir = .GlobalEnv)
{   # An extended copy of the var() function from R's base package

    # Changes:
    #
    
    # FUNCTION:
    
    # data:
    fileExt = function(x) sub(".*\\.", "", x)
    names = c(as.character(substitute(list(...))[-1]), list)
    ## Find the directories of the given packages and maybe the working
    ## directory.
    if (!is.null(package)) {
        if (!is.character(package))
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) warning(
            "datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) warning(
            "datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] = "datasets"
    }
    paths = .find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc))
        paths = c(.path.package(package, TRUE), 
            if(is.null(package)) getwd(), paths)
    paths = unique(paths[file.exists(paths)])
    ## Find the directories with a 'data' subdirectory.
    paths = paths[tools::file_test("-d", file.path(paths, "data"))]
    dataExts = tools:::.make_file_exts("data")
    if(length(names) == 0) {
        ## List all possible data sets.
        ## Build the data db.
        db = matrix(character(0), nr = 0, nc = 4)
        for(path in paths) {
            entries = NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(tools::file_test("-f", file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds'
            if(tools::file_test("-f", INDEX <-
                file.path(path, "Meta", "data.rds"))) {
                entries = .readRDS(INDEX)
            } else {
                ## No index: should only be true for ./data >= 2.0.0
                dataDir = file.path(path, "data")
                entries = tools::list_files_with_type(dataDir, "data")
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries = cbind(entries, "")
                }
            }
            if(NROW(entries) > 0) {
                if(is.matrix(entries) && ncol(entries) == 2)
                    db = rbind(db, cbind(packageName, dirname(path), entries))
                else
                    warning(gettextf("data index for package '%s' is invalid and will be ignored", packageName), domain=NA, call.=FALSE)
            }
        }
        colnames(db) = c("Package", "LibPath", "Item", "Title")

        footer = if(missing(package))
            paste("Use ", sQuote(paste("data(package =",
                ".packages(all.available = TRUE))")), "\n",
                "to list the data sets in all *available* packages.", sep = "")
            else
                NULL
        y = list(title = "Data sets", header = NULL, results = db,
            footer = footer)
        class(y) = "packageIQR"
        return(y)
    }
    paths = file.path(paths, "data")
    for(name in names) {
        found = FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if (tools::file_test("-f", file.path(p, "Rdata.rds"))) {
                rds = .readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found = TRUE
                    if(verbose)
                        cat("name=", name, ":\t found in Rdata.rdb\n")
                    thispkg = sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg = sub("_.*$", "", thispkg) # versioned installs.
                    thispkg = paste("package:", thispkg, sep="")
                    objs = rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = envir,
                             filter = function(x) x %in% objs)
                    break
                }
            }
            ## check for zipped data dir
            if(tools::file_test("-f", file.path(p, "Rdata.zip"))) {
                if(tools::file_test("-f",fp = file.path(p, "filelist")))
                    files = file.path(p, scan(fp, what="", quiet = TRUE))
                else {
                    warning(gettextf("file 'filelist' is missing for directory '%s'", p), domain = NA)
                    next
                }
            } else {
                files = list.files(p, full = TRUE)
            }
            files = files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1) {
                ## more than one candidate
                o = match(fileExt(files), dataExts, nomatch = 100)
                paths0 = dirname(files)
                paths0 = factor(paths0, levels=paths0)
                files = files[order(paths0, o)]
            }
            if (length(files) > 0) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if(verbose)
                        cat("name=", name, ":\t file= ...",
                            .Platform$file.sep, basename(file), "::\t", sep = "")
                    ext = fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if(basename(file) != paste(name, ".", ext, sep = ""))
                        found = FALSE
                    else {
                        found = TRUE
                        zfile = zip.file.extract(file, "Rdata.zip")
                        if(zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                            R = , r =sys.source(zfile, chdir = TRUE,
                            envir = envir), RData = , rdata = , rda =
                            load(zfile, envir = envir), TXT = , txt = , 
                            tab = assign(name,
                            read.table(zfile, header = TRUE), envir = envir),
                            CSV = , csv = assign(name,
                            read.table(zfile, header = TRUE, sep = ";"),
                            envir = envir),
                            found = FALSE)
                    }
                    if (found) break # from files
                }
                if (verbose) cat(if(!found) "*NOT* ", "found\n")
            }
            if (found) break # from paths
        }
        if(!found)
            warning(gettextf("data set '%s' not found", name), domain = NA)
    }
    
    # REQUIRES FCALENDAR !! -> move it to fCalendar
    # DW added:
    for (name in names) {
        # If the data set can be transformed in a timeSeriesobject 
        # then do it ... 
        z = eval(parse(text = paste("x = ", name)))
        # print(c(class = class(z)))
        if (class(z) == "data.frame") {
            tS = as.character(!is.character(try(as.timeSeries(
                eval(parse(text = paste("x = ", name)))), silent = TRUE)))  
            # print(c(tS = as.logical(tS)))
            if (as.logical(tS)) {
                z = as.timeSeries(name)
                eval(parse(text = paste(name, "<<- z")))
            }
        }
    }
    # DW
    
    invisible(names)
}
  

################################################################################  
#  currentYear               Sets date of the current year
#  .currentYear              Sets date of the current year
#  myUnits                   Sets date units


.currentYear = 
function()    
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Check Time Zone:
    TZ = Sys.getenv("TZ")  
    if (TZ[[1]] != "GMT") {
        Sys.putenv(TZ = "GMT")
        TZ.RESET = TRUE
    } else {
        TZ.RESET = FALSE
    }
    
    # Current Year:
    if (class(version) != "Sversion") {
        currentYear = as.POSIXlt(Sys.time())$year + 1900
    } else { 
        currentDate = timeDate(date(), in.format="%w %m %d %H:%M:%S %Z %Y")
        currentYear = as.integer(attr(years(currentDate), "levels"))
    } 
    
    # Return Value:
    if (TZ.RESET) Sys.putenv(TZ = TZ)
    currentYear 
}


# ------------------------------------------------------------------------------


currentYear = .currentYear() 


# ------------------------------------------------------------------------------


myUnits = "days"


################################################################################


.MSFT = 
matrix(c(
    20000927,63.4375,63.5625,59.8125,60.625,53077800,
    20000928,60.8125,61.875,60.625,61.3125,26180200,
    20000929,61,61.3125,58.625,60.3125,37026800,
    20001002,60.5,60.8125,58.25,59.125,29281200,
    20001003,59.5625,59.8125,56.5,56.5625,42687000,
    20001004,56.375,56.5625,54.5,55.4375,68226700,
    20001005,55.5,57.25,55.25,55.375,40549700,
    20001006,55.8125,56.75,54.75,55.5625,30897000,
    20001009,55.625,55.75,53,54.1875,29161800,
    20001010,53.9375,55.5625,53.8125,54.5625,31033100,
    20001011,54,56.9375,54,55.75,50602900,
    20001012,56.3125,56.875,53.8125,54.375,45109800,
    20001013,53.875,54.875,52.125,53.75,52260600,
    20001016,53.5,53.8125,49.5625,50.375,59879500,
    20001017,51.875,52.4375,50.25,50.4375,40638300,
    20001018,49.625,53.25,48.4375,51.75,55268200,
    20001019,58.4375,62.1875,58,61.875,128496600,
    20001020,61.3125,66.125,61.125,65.1875,80189300,
    20001023,64.625,66.25,60.6875,62.125,92585200,
    20001024,62.625,62.9375,60.1875,61.5,47213700,
    20001025,61.9375,63.4375,60.4375,61.25,83801900,
    20001026,61,65.0625,60.8125,64.4375,57413300,
    20001027,64.6875,69.1875,64.625,67.6875,62146200,
    20001030,67.5,70.125,67.375,69.0625,55028800,
    20001031,69,69.5,68,68.875,52237000,
    20001101,68.5,70.0625,68.4375,69.625,40654700,
    20001102,70.375,70.8438,69.625,70.3125,38992600,
    20001103,69.25,69.625,68.0625,68.25,34355500,
    20001106,68.6875,70.125,68.25,69.5,37425700,
    20001107,69.75,71.875,69.5,70.5,52165600,
    20001108,71.125,72.375,68,69.4375,103074700,
    20001109,68.5,71.3125,68.4375,70.875,45529300,
    20001110,69.9375,70.3125,66.8125,67.375,46872200,
    20001113,66.6875,68.125,64.4062,66.4375,41682400,
    20001114,68,69.8125,67.3125,68.8125,42109300,
    20001115,69.0625,70.875,68.6875,70.0625,30211100,
    20001116,69.4375,71.5,68.9375,68.9375,46064300,
    20001117,69.4375,70,67.7969,69.0625,53262800,
    20001120,68.125,68.5,65.5625,67.1875,40078600,
    20001121,67.375,69.25,67.375,67.75,29743800,
    20001122,66.0625,69.5,66,68.25,38171600,
    20001124,69,70.4375,68.5,69.9375,17219600,
    20001127,71.4375,72.25,70.625,70.6875,42653800,
    20001128,69.375,69.75,66.8125,67,63723100,
    20001129,66.8125,67.125,63.25,65.0625,49140200,
    20001130,62,62.0625,57,57.375,98600400,
    20001201,58.0625,60.625,56.0625,56.625,54904900,
    20001204,57.25,59,55.1875,56.4375,40203600,
    20001205,59.1875,60.5,58.25,59.875,50867200,
    20001206,60,60.0625,56.0625,56.6875,45280400,
    20001207,53.4375,54,52.25,53.125,72654200,
    20001208,54.625,55.875,53.4375,54.4375,60469900,
    20001211,55.5,58.75,55,58.0625,47788100,
    20001212,57.8125,60,56.75,58.375,31553000,
    20001213,60.5,60.5,56.8125,57.25,49180200,
    20001214,57.9375,58.7344,55.375,55.5,35600700,
    20001215,51.0469,52,47.75,49.1875,58449900,
    20001218,49,50,47,47.8125,53593700,
    20001219,47.4375,48,44.5,44.8125,60135900,
    20001220,42.8125,44,41.375,41.5,74518900,
    20001221,40.75,45.125,40.3125,43.4375,81586500,
    20001222,44.75,47.125,44.75,46.4375,54775900,
    20001226,46.875,48.5625,45.875,46.875,33470800,
    20001227,46.125,46.8125,45,46.4375,34501900,
    20001228,45.125,46.25,43.875,44.5625,38809600,
    20001229,43.9375,45.8125,43,43.375,49988800,
    20010102,44.125,45,42.875,43.375,41206600,
    20010103,43.1875,48.875,43.125,47.9375,67981100,
    20010104,47.8125,50.5,46.875,48.4375,56198500,
    20010105,48.5,49.875,47.5625,49.125,46707300,
    20010108,48.9375,49.75,46.6875,48.9375,39908800,
    20010109,50,52.625,49.75,51.8125,57482700,
    20010110,51,53.8125,50.75,52.875,45115100,
    20010111,53,55.75,52.3125,55,50927400,
    20010112,54.875,55,52.5,53.5,36856000,
    20010116,53.375,53.5,51.125,52.5625,34231200,
    20010117,53.625,54.875,52.5625,52.9375,36422100,
    20010118,53.6875,56.1875,52.625,55.5,54894400,
    20010119,60,61.4375,58.875,61,104674400,
    20010122,60.75,61,59,60.125,38336500,
    20010123,59.75,60.9375,58.9375,60.5625,35147600,
    20010124,61,63.4375,60.75,62.9375,55227500,
    20010125,62.75,64,61.5625,61.8125,42828700,
    20010126,61,64.3125,61,64,46540000,
    20010129,63.5625,64.625,63.5,64.5,42491900,
    20010130,64.5,64.75,62.875,63.375,28638400,
    20010131,63,63.75,61,61.0625,40949400,
    20010201,60.8125,62.625,60.375,62.375,35896400,
    20010202,62.5,63.375,60.75,60.8125,35550000,
    20010205,60.75,62.0625,60.25,61.9375,25699600,
    20010206,62.0625,63.8125,61.6875,62.5625,48221000,
    20010207,62,65.0625,61.8125,64.6875,63030900,
    20010208,63.75,64.5,62,62.25,44020600,
    20010209,61.3125,61.5625,58.5,59.125,50287600,
    20010212,58.8125,59.4375,57.1875,58.75,35644700,
    20010213,59.625,61.0625,58.125,58.1875,38035300,
    20010214,57.625,59,56.375,58.375,30864200,
    20010215,59,60.1875,57.875,58.8125,32813900,
    20010216,57,58.25,56.125,57.3125,33479200,
    20010220,57.375,58.25,55.375,55.875,30365400,
    20010221,55.25,58.0625,55.1875,56.25,31973600,
    20010222,56.3125,56.8125,53.875,55.1875,50408200,
    20010223,54.4375,57.5,54.3125,56.75,46310300,
    20010226,57.625,59.9375,57.375,59.5625,43968400,
    20010227,59.375,61.1875,58.6719,59.375,49574300,
    20010228,59.5625,60.0781,58.1875,59,42304200,
    20010301,58.5625,59.5,56.25,59.3594,40890800,
    20010302,57.5,58.125,56.4375,56.6875,39900400,
    20010305,57.25,58.625,56.5625,57.4375,24691800,
    20010306,58.625,60,58.375,59.4375,33390900,
    20010307,59.875,61.125,59.3125,60.6875,29871800,
    20010308,60.3125,60.5938,58.4375,59.25,27313000,
    20010309,57.9375,58.1875,54.875,56.6875,51897200,
    20010312,54.6875,55,51.625,51.9375,57188000,
    20010313,52.1875,54.75,52,54.1875,45517800,
    20010314,52.5,55.25,52.1875,54,45343100,
    20010315,55.3125,56.0781,53.5,53.6875,35819200,
    20010316,52.5,55.125,52.4844,54.5625,56424400,
    20010319,54.5,55.5,53.125,54.3125,30518200,
    20010320,54.5625,56.125,52.625,52.6875,45911400,
    20010321,52.25,53.25,49.75,50.0625,62494300,
    20010322,50.5625,54.0625,50.5,54,63181600,
    20010323,54.9375,57,54.375,56.5625,49759800,
    20010326,57.125,57.5,55.5625,56.0625,31559300,
    20010327,56.0625,58.5625,55.875,58.25,47567800,
    20010328,57.375,57.9375,55.375,55.5625,39340800,
    20010329,55.375,57.1875,54.5625,55.375,43492500,
    20010330,55.75,56.1875,53.875,54.6875,45600800,
    20010402,54.8125,56.9375,54.625,55.8125,37962000,
    20010403,55.3125,55.3125,52.75,53.375,47093800,
    20010404,53.375,55,51.0625,51.9375,52023300,
    20010405,53.75,57.375,53.5,56.75,56682000,
    20010406,56.375,57.1875,55.0625,56.1875,46311000,
    20010409,56.57,57.42,55.66,57.15,28147800,
    20010410,57.95,60.09,57.78,59.68,54599700,
    20010411,60.65,61.5,59.7,60.04,54939800,
    20010412,59.56,62.31,59.35,62.18,43760000,
    20010416,61.4,61.58,60.12,60.79,32928700,
    20010417,60.52,62.11,60.04,61.48,42574600,
    20010418,63.39,66.31,63,65.43,78348200,
    20010419,65.81,69,65.75,68.04,79687800,
    20010420,70.3,71.1,68.5,69,96459800,
    20010423,68.11,68.47,66.9,68.25,46085600,
    20010424,68.2,69.93,67.14,67.55,44588300,
    20010425,67.57,69.79,67.25,69.69,38372000,
    20010426,70.07,71,68.25,69.13,59368800,
    20010427,69.53,69.68,66.21,67.12,60786200,
    20010430,68.53,69.06,67.68,67.75,37184100,
    20010501,67.66,70.3,67.6,70.17,41851400,
    20010502,71,71.15,69.35,69.76,46432200,
    20010503,69.25,70.18,68.14,68.53,33136700,
    20010504,68,71.05,67.96,70.75,59769200,
    20010507,70.83,72.15,70.7,71.38,54678100,
    20010508,71.75,72.1,70.75,72.06,37542000,
    20010509,71.24,71.3,69.86,70.4,38338300,
    20010510,71.13,71.24,69.96,70,32167300,
    20010511,69.96,70,68.65,69.4,25564400,
    20010514,69.13,69.2,68.3,68.72,22484000,
    20010515,68.74,69.3,68,68.27,30692800,
    20010516,67.7,69.88,67.33,69.16,45946900,
    20010517,69.1,70.14,67.55,68.17,53492400,
    20010518,67.69,69.2,67.25,68.09,45302700,
    20010521,68.05,69.99,67.75,68.79,51745800,
    20010522,69.45,70.35,69.18,70.31,41727800,
    20010523,70.39,71.6,69.51,69.7,46818700,
    20010524,69.94,71.78,69.27,71.72,40390800,
    20010525,71.66,71.9,70.36,70.91,26373800,
    20010529,70.8,71.75,70.05,70.34,35605400,
    20010530,69.56,70.58,68.65,69.19,43250900,
    20010531,69.49,70.38,68.4,69.18,35341300,
    20010601,69.6,70.7,68.7,70.34,28793800,
    20010604,70.55,71.02,69.8,70.78,21868300,
    20010605,70.76,73.08,70.5,72.6,44727100,
    20010606,72.89,73.48,71.55,72.36,40011400,
    20010607,72.12,73.73,72.08,73.68,33480000,
    20010608,73.7,73.75,72.05,73.19,25933500,
    20010611,72.85,72.85,71.51,72.12,23672800,
    20010612,71.02,72.41,70.81,72.08,33357300,
    20010613,72.05,72.3,70.64,70.69,27651200,
    20010614,70.22,70.55,68.4,68.9,35986200,
    20010615,67.51,68.3,66.4,68.02,54177200,
    20010618,67.95,67.96,66.01,66.88,28423400,
    20010619,68.21,68.85,66.85,67.32,31728700,
    20010620,67.14,69.59,67.1,69.41,32054200,
    20010621,69.15,70.55,68.92,69.84,34801900,
    20010622,70,70.61,68.58,68.83,25546000,
    20010625,69.1,69.81,67.77,68.85,24607800,
    20010626,67.82,70.21,67.7,70.14,31538500,
    20010627,69.86,71.53,69.36,71.14,34599900,
    20010628,71.55,76.15,70.53,72.74,64487800,
    20010629,72.6,73.41,71.4,73,47141900,
    20010702,72.05,73.15,70.15,70.6,36405100,
    20010703,70.3,70.8,69.93,70.47,14018700,
    20010705,70.22,70.72,68.44,68.51,24621300,
    20010706,68.3,68.4,65.67,66.06,33733900,
    20010709,66.2,66.91,65.04,65.69,33238300,
    20010710,65.9,66.25,64.35,64.48,33281300,
    20010711,64.21,66.75,64.2,66.5,36911300,
    20010712,70.7,72.05,70.33,71.6,64039000,
    20010713,71.4,72,70.94,71.34,29467300,
    20010716,71.45,72.16,70.15,71.18,27995400,
    20010717,70.66,72.01,70.14,71.82,31620500,
    20010718,70.6,71.5,69.87,70.57,28795400,
    20010719,71.22,73,71.22,72.57,38274700,
    20010720,68.03,69.4,67.94,69.18,62101800,
    20010723,69.24,69.24,66.35,67.09,39999700,
    20010724,67,67.99,65.7,66.32,33765100,
    20010725,66.26,67.52,65.61,67.48,37032700,
    20010726,67.12,67.32,65.5,66.59,38987000,
    20010727,66.05,66.25,65.05,65.47,32698000,
    20010730,65.65,66.88,65.54,65.8,21098200,
    20010731,66.01,67.39,65.85,66.19,29515800,
    20010801,66.8,66.81,65.76,66.47,27839500,
    20010802,67.21,67.54,66.26,67.45,27099200,
    20010803,67.3,67.36,66,66.89,21630200,
    20010806,66.53,67.12,65.68,66.13,13915800,
    20010807,66.04,67.05,65.99,66.35,15673900,
    20010808,66.51,67.24,64.49,64.86,27498200,
    20010809,64.98,65.55,64.3,65.01,22768100,
    20010810,64.77,65.86,62.9,65.52,25878200,
    20010813,65.24,65.99,64.75,65.83,16337700,
    20010814,65.75,66.09,64.45,64.69,18240600,
    20010815,64.71,65.05,63.2,63.2,19751500,
    20010816,62.84,64.71,62.7,64.62,21952800,
    20010817,63.78,64.13,61.5,61.88,26117100,
    20010820,61.66,62.75,61.1,62.7,24185600,
    20010821,62.7,63.2,60.71,60.78,23555900,
    20010822,61.13,61.15,59.08,60.66,39053600,
    20010823,60.67,61.53,59,59.12,25906600,
    20010824,59.6,62.28,59.23,62.05,31699500,
    20010827,61.9,63.36,61.57,62.31,22281400,
    20010828,62.34,62.95,60.58,60.74,23711400,
    20010829,61.05,61.3,59.54,60.25,24085000,
    20010830,59.04,59.66,56.52,56.94,48816000,
    20010831,56.85,58.06,56.3,57.05,28950400,
    20010904,57.19,59.08,56.07,56.1,33594600,
    20010905,56.18,58.39,55.39,57.74,44735300,
    20010906,56.56,58.39,55.9,56.02,56178400,
    20010907,56.11,57.36,55.31,55.4,44931900,
    20010910,54.92,57.95,54.7,57.58,42235900,
    20010917,54.02,55.1,52.8,52.91,63751000,
    20010918,53.41,55,53.17,54.32,41591300,
    20010919,54.46,54.7,50.6,53.87,63475100,
    20010920,52.35,52.61,50.67,50.76,58991600,
    20010921,47.92,50.6,47.5,49.71,92488300,
    20010924,50.65,52.45,49.87,52.01,42790100,
    20010925,52.27,53,50.16,51.3,42470300,
    20010926,51.51,51.8,49.55,50.27,29262200,
    20010927,50.1,50.68,48,49.96,40595600), byrow = TRUE, ncol = 6)


# ------------------------------------------------------------------------------


MSFT = data.frame(.MSFT)
colnames(MSFT) = c("YYYYMMDD", "Open", "High", "Low", "Close", "Volume")
    

################################################################################
#  .fjulian            Transform formatted dates to julian day numbers
#  .julian             Implements SPlus like 'julian'


.fjulian = 
function(fdates, origin = 19600101, order = 'mdy', cc = NULL, swap = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Transforms formatted dates (fdates) from several formats 
    #   as 8/11/73 11Aug1973, ... into ISO-8601 Gregorian dates
    #   ... makes use of C-Program char_date.c implemented by 
    #   Terry Therneau
    
    # Notes:
    #   cc - Century, becoming obsolete with the introduction of
    #       swap.
    
    # Example:
    #   fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    #   .fjulian(fdates) 
    #   fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    #   .fjulian(fdates, order = 'dmy')
    
    # Note:
    #   R-package "date"
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Requires
    require(date)
    
    # Formats:
    order.vec = switch(order,
        'ymd'= c(1,2,3),
        'ydm'= c(1,3,2),
        'mdy'= c(2,3,1),
        'myd'= c(2,1,3),
        'dym'= c(3,1,2),
        'dmy'= c(3,2,1),
        stop("Invalid value for 'order' option"), 
        PACKAGE = "date")
    nn = length(fdates)
    temp = .C("char_date", 
        as.integer(nn),
        as.integer(order.vec),
        as.character(fdates),
        month = integer(nn),
        day = integer(nn),
        year = integer(nn),
        PACKAGE = "date")
    month = temp[[4]]
    day = temp[[5]]
    year = temp[[6]]
    yy = year - 100 * floor (year/100)
    
    # Swap:
    cc = 19 + trunc(sign(swap-yy)+1)/2
    year = cc*100 + yy
    
    # Origin:
    cc0 = origin %/% 1000000
    yymmdd0 = origin - cc0*1000000
    yy0 = yymmdd0 %/% 10000
    mm0 = yymmdd0 %/% 100 - yy0*100
    dd0 = yymmdd0 - yy0*10000 - mm0*100

    # Result:
    ans = .julian(month, day, year, origin = c(mm0, dd0, cc0*100+yy0))
    
    # Return Value:
    ans
}


# ******************************************************************************


.julian =
function(m, d, y, origin = c(month = 1, day = 1, year = 1960))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   This function is a synonyme for Splus' "julian()" with the
    #   same list of arguments.
    
    # Note:
    #   SPlus like function.

    # Changes:
    #
    
    # FUNCTION:
    
    # Selection:
    .R = TRUE
    .S = FALSE
    
    # Implementation under R:
    if(.R) {    
        only.origin = all(missing(m), missing(d), missing(y))
        if(only.origin) m = d = y = NULL    # return days since origin
        nms = names(d)
        max.len = max(length(m), length(d), length(y))  
        # prepend new origin value and rep out to common max. length:
        m = c(origin[1], rep(m, length = max.len))
        d = c(origin[2], rep(d, length = max.len))
        y = c(origin[3], rep(y, length = max.len))  
        # code from julian date in the S book (p.269)
        y = y + ifelse(m > 2, 0, -1)
        m = m + ifelse(m > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        out = (146097 * c) %/% 4 + (1461 * ya) %/% 4 + 
            (153 * m + 2) %/% 5 + d + 1721119   
        # now subtract the new origin from all dates
        if(!only.origin) {
            if(all(origin == 0)) out = out[-1] else out = out[-1] - out[1] }    
        names(out) = nms
        result = out }
    
    # Synonyme for S:
    if(.S) {
        result = julian(m = m, d = d, y = y, origin. = origin)}

    # Return Value:
    result
}


################################################################################
# FUNCTION:              DESCRIPTION
#  .isISO8601             Checks if the date/time is ISO8601 formatted


.isISO8601 =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks if the date/time string is ISO8601 formatted
    
    # Example:
    #   isISO8601(c("2007-01-01", "2007-12-31" ))
    #   isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    #   isISO8601("2007-Jan-01")[[1]]
    #   isISO8601("2007-01-01 15:00:000")[[1]]
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    stopifnot(class(x) == "character")
    
    # Test: 
    options(warn = -1)
    format = .whichFormat(x)
    ans = FALSE
    if (format == "%Y-%m-%d %H:%M:%S") ans = TRUE
    if (format == "%Y-%m-%d") ans = TRUE
    if (format == "%Y%m%d%H%M%S") ans = TRUE
    if (format == "%Y%m%d") ans = TRUE
    attr(ans, "control")<- format
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION ADDON:      DESCRIPTION:
#  .isPOSIX             Checks for an object of class POSIX


isPOSIX =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks for an object of class POSIX
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    ans = inherits(x, "POSIXt")
    
    # Return Value:
    ans
}


################################################################################

