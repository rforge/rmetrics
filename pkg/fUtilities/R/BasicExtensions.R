
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


# fEcofin::3A-BasicExtensions.R                                                 
################################################################################
# FUNCTION:                 BASIC EXTENSIONS:                                   
#  align                     aligns time series objects by approximation        
#  align.default             align default method                               
#  attach                    attach a database to the R path                    
#  attach.default            attach default method                              
#  log                       log has become a generic function                  
#  log.default               log default method                                 
#  round                     round has become a generic function                
#  round.default             round default method                               
#  sample                    sample has become a generic function               
#  sample.default            sample default method                              
#  sort                      sort has become a generic function                 
#  sort.default              sort default method                                
#  var                       var has become a generic function                  
#  var.default               var default method                                 
#  cov                       var has become a generic function                  
#  cov.default               var default method                                 
#  stdev                     for SPLUS compatibility                            
# FUNCTION:                  COLUMN AND ROW STATISTICS:                         
#  colSums                    colSums has become a generic function             
#  colMeans                   colMeans has become a generic function            
#  rowSums                    rowSums has become a generic function             
#  rowMeans                   roowMeans has become a generic function           
# FUNCTION:                  ROW AND COLUMN NAMES:                              
#  "rownames<-"               rownames<- has become a generic function          
#  "rownames<-.default"       rownames<- default method                         
#  "colnames<-"               colnames<- has become a generic function          
#  "colnames<-.default"       colnames<- default method                         
# FUNCTION:                  DATE AND TIME SERIES FUNCTIONS:                    
#  modify                     Modifies a 'timeSeries' object                    
#  modify.default             Default Method                                    
#  atoms                      Extracts atoms from 'timeSeries' object           
#  atoms.default              Default Method                                    
#  as.POSIXlt                 Converts objects of class POSIXlt                 
#  as.POSIXlt.default         Default Method                                    
#  as.matrix.ts               Converts univariate ts to 1-column matrix         
#  as.matrix.mts              Converts multivariate ts to matrix                
#  Sys.putenv                 depreciated after 2.4.1                           
################################################################################


.conflicts.OK = TRUE


if (!exists("Sys.setenv"))
{
    Sys.setenv =
    function(...)
    {
        x <- list(...)
        nm <- names(x)
        val <- as.character(unlist(x))
        x <- paste(nm, val, sep = "=")
        invisible(.Internal(putenv(x)))
    }
}


################################################################################
# FUNCTION:                     DESCRIPTION:
#  align                         aligns time series objects by approximation
#  align.default                 align default method
#  attach                        attach a database to the R path
#  attach.default                attach default method
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
#  stdev                         for SPLUS compatibility


align =
function(x, y, xout, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("align")
}


# ------------------------------------------------------------------------------


align.default =
function(x, y, xout, method = "linear", n = 50, rule = 1, f = 0,
ties = mean, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Align by Approximation:
    ans = approx(x = x, y = y, xout = xout, method = method, n = n,
        rule = rule, f = f, ties = ties, ...)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


attach =
function(what, pos = 2, name = deparse(substitute(what)),
warn.conflicts = TRUE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("attach")
}


# ------------------------------------------------------------------------------


attach.default <- base::attach


# ------------------------------------------------------------------------------
# sort() has been S3 generic in 'base' since 2.4.0
# Otherwise use something that works here

if(getRversion() < "2.4.0") {

    sort <- function (x, decreasing = FALSE, ...)
    {
        if (!is.logical(decreasing) || length(decreasing) != 1)
            stop("'decreasing' must be a length-1 logical vector.\nDid you intend to set 'partial'?")
        UseMethod("sort")
    }
    
    sort.default <- function(x, decreasing = FALSE, ...) {
        if (is.object(x))
        x[order(x, decreasing = decreasing)]
        else base::sort(x, decreasing = decreasing, ...)
    }

}# endif {only for outdated R}


# ------------------------------------------------------------------------------


sample =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("sample")
}


# ------------------------------------------------------------------------------


sample.default =  
function (x, size, replace = FALSE, prob = NULL, ...)
{
    base::sample(x, size, replace = replace, prob = prob)
}


# ------------------------------------------------------------------------------


round =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("round")
}


# ------------------------------------------------------------------------------


round.default =
function(x, digits = 0) 
{
    base::round(x, digits)
}


# ------------------------------------------------------------------------------


log =
function(x, base = exp(1))
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("log")
}


# ------------------------------------------------------------------------------


log.default = 
function(x, base = exp(1)) 
{
    base::log(x, base)
}


# ------------------------------------------------------------------------------


outlier =
function(x, sd = 5, complement = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

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
        names(ans) = as.character(which(x > SD))
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


var =
function(x, y = NULL, na.rm = FALSE, use)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("var")
}


# ------------------------------------------------------------------------------


var.default <- stats::var


# ------------------------------------------------------------------------------


cov =
function(x, y = NULL, use = "all.obs",
    method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("cov")
}


# ------------------------------------------------------------------------------


cov.default <- stats::cov


# ------------------------------------------------------------------------------


cor =
function(x, y = NULL, use = "all.obs",
    method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("cor")
}


# ------------------------------------------------------------------------------


cor.default <- stats::cor


# ------------------------------------------------------------------------------


stdev =
function(x, na.rm = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    stats::sd(x = x, na.rm = na.rm)
}


################################################################################
# FUNCTION:                     COLUMN AND ROW STATISTICS:
#  colSums                       colSums has become a generic function
#  colMeans                      colMeans has become a generic function
#  rowSums                       rowSums has become a generic function
#  rowMeans                      roowMeans has become a generic function


colMeans.default <- base::colMeans


# ------------------------------------------------------------------------------


colSums.default <- base::colSums


# ------------------------------------------------------------------------------


rowMeans.default <- base::rowMeans


# ------------------------------------------------------------------------------


rowSums.default <- base::rowSums


################################################################################
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method


"rownames<-" =
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("rownames<-")
}


# ------------------------------------------------------------------------------


`rownames<-.default` <- base::`rownames<-`


# ------------------------------------------------------------------------------


"colnames<-" =
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("colnames<-")
}


# ------------------------------------------------------------------------------


`colnames<-.default` <- base::`colnames<-`


################################################################################
#  atoms                     Extracts atoms from 'timeSeries' object
#  atoms.default             Default Method


atoms =
function(x, ...)
{   # A function implemented by Diethelm WUertz

    # FUNCTION:

    # Return Value:
    UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default =
function(x, ...)
{   # A function implemented by Diethelm WUertz

    # FUNCTION:

    # Return Value:
    invisible(x)
}


################################################################################


as.POSIXlt =
function(x, tz = "")
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("as.POSIXlt")
}


# ------------------------------------------------------------------------------


as.POSIXlt.default =
function (x, tz = "")
{   # A function implemented by Diethelm Wuertz

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
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Coerces a "ts" object into a matrix

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
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Coerces a multivariate "ts" object into a matrix

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

