
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
#  as.matrix.ts                  univariate ts to 1-column matrix
#  as.matrix.mts                 multivariate ts to matrix
# FUNCTION:                     DESCRIPTION:
#  .description                  sets default description string
#  .unirootNA                    
################################################################################


.conflicts.OK = TRUE


################################################################################


sort = 
function(x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...)
{
    # FUNCTION:
    
    UseMethod("sort")
}


# ------------------------------------------------------------------------------


sort.default =
function (x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...) 
{   # A copy of the sort() function from R's base package
    
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


################################################################################


sample = 
function(x, ...)
{
    # FUNCTION:
    
    UseMethod("sample")
}  


# ------------------------------------------------------------------------------

    
sample.default =
function (x, size, replace = FALSE, prob = NULL, ...) 
{
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


################################################################################


round =
function(x, ...)
{
    UseMethod("round")
}


# ------------------------------------------------------------------------------


round.default =
function (x, digits = 0, ...) 
{
    # FUNCTION:
    
    .Internal(round(x, digits, ...))
}       
            
            
################################################################################


log = 
function(x, base = exp(1)) 
{
    # FUNCTION:
    
    UseMethod("log")
}


# ------------------------------------------------------------------------------


log.default =
function(x, base = exp(1))
{   # A copy of the log() function from R's base package

    # FUNCTION:
    
    # Log:
    if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
}


################################################################################


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



################################################################################


var = 
function(x, y = NULL, na.rm = FALSE, use) 
{
    # FUNCTION:
    
    UseMethod("var")
}


# ------------------------------------------------------------------------------


var.default =
function(x, y = NULL, na.rm = FALSE, use) 
{   # A copy of the var() function from R's base package

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


"rownames<-" = 
function(x, value)
{
    # FUNCTION:
    
    UseMethod("rownames<-")
}


# ------------------------------------------------------------------------------
    

"rownames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

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
    # FUNCTION:
    
    UseMethod("colnames<-")
}


# ------------------------------------------------------------------------------
    

"colnames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

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
    UseMethod("as.POSIXlt")
}


# ------------------------------------------------------------------------------


as.POSIXlt.default =
function (x, tz = "") 
{
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


