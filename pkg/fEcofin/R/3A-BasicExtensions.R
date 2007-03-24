
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


################################################################################
# FUNCTION:                     BASIC EXTENSIONS:
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
# FUNCTION:                     ROW AND COLUMN NAMES:
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method
# FUNCTION:                     DATE AND TIME SERIES FUNCTIONS:
#  modify                        Modifies a 'timeSeries' object
#  modify.default                Default Method
#  atoms                         Extracts atoms from 'timeSeries' object
#  atoms.default                 Default Method
#  as.POSIXlt                    Converts objects of class POSIXlt
#  as.POSIXlt.default            Default Method
#  as.matrix.ts                  Converts univariate ts to 1-column matrix
#  as.matrix.mts                 Converts multivariate ts to matrix
#  Sys.putenv                    depreciated after 2.4.1
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


attach.default = 
function(what, pos = 2, name = deparse(substitute(what)), 
warn.conflicts = TRUE) 
{   # # A copy of the attach() function from R's base package

    # FUNCTION:
    
    # Attach:
    checkConflicts <- function(env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".First.lib", ".Last.lib", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics")
        sp <- search()
        for (i in seq_along(sp)) {
            if (identical(env, as.environment(i))) {
                db.pos <- i
                break
            }
        }
        ob <- objects(db.pos, all = TRUE)
        if (.isMethodsDispatchOn()) {
            these <- objects(db.pos, all = TRUE)
            these <- these[substr(these, 1, 6) == ".__M__"]
            gen <- gsub(".__M__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__M__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != ".GlobalEnv"]
            ob <- ob[!(ob %in% gen)]
        }
        ipos <- seq_along(sp)[-c(db.pos, match(c("Autoloads", 
            "CheckExEnv"), sp, 0))]
        for (i in ipos) {
            obj.same <- match(objects(i, all = TRUE), ob, nomatch = 0)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                is_fn1 <- sapply(same, function(x) exists(x, 
                  where = i, mode = "function", inherits = FALSE))
                is_fn2 <- sapply(same, function(x) exists(x, 
                  where = db.pos, mode = "function", inherits = FALSE))
                same <- same[is_fn1 == is_fn2]
                if (length(same)) {
                  cat("\n\tThe following object(s) are masked", 
                    if (i < db.pos) 
                      "_by_"
                    else "from", sp[i], if (sum(sp == sp[i]) > 
                      1) 
                      paste("( position", i, ")"), ":\n\n\t", 
                    same, "\n\n")
                }
            }
        }
    }
    if (pos == 1) {
        warning("*** 'pos=1' is not possible; setting 'pos=2' for now.\n", 
            "*** Note that 'pos=1' will give an error in the future")
        pos <- 2
    }
    if (is.character(what) && (length(what) == 1)) {
        if (!file.exists(what)) 
            stop(gettextf("file '%s' not found", what), domain = NA)
        name <- paste("file:", what, sep = "")
        value <- .Internal(attach(NULL, pos, name))
        load(what, envir = as.environment(pos))
    } else {
        value <- .Internal(attach(what, pos, name))
    }
    if (warn.conflicts && !exists(".conflicts.OK", envir = value, 
        inherits = FALSE)) {
        checkConflicts(value)
    }
    if ((length(objects(envir = value, all = TRUE)) > 0) && 
        .isMethodsDispatchOn()) 
        methods:::cacheMetaData(value, TRUE)
        
    invisible(value)
}


# ------------------------------------------------------------------------------


sort = 
function(x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
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
{   # A copy of the sample() function from R's base package

    # FUNCTION:
    
    # Sample:
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
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    UseMethod("round")
}


# ------------------------------------------------------------------------------


round.default =
function (x, digits = 0, ...) 
{   # A copy of the round() function from R's base package

    # FUNCTION:
    
    # Return Value:
    .Internal(round(x, digits, ...))
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
{   # A copy of the log() function from R's base package

    # FUNCTION:
    
    # Log:
    if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
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
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
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

