
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
# FUNCTION:                    DESCRIPTION
# log                           log has become a generic function
# log.default
# round
# round.default        
# sample
# sample.default
# sort                          sort has become a generic function
# sort.default
# var                           var has become a generic function
# var.default
#
# "rownames<-"                  rownames<- has become a generic function
# "rownames<-.default"
# "colnames<-"                  colnames<- has become a generic function
# "colnames<-.default"
################################################################################


.conflicts.OK = TRUE


################################################################################


sort = 
function(x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...)
{
    UseMethod("sort")
}


# ------------------------------------------------------------------------------


sort.default =
function (x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE, ...) 
{   # A copy of the sort() function from R's base package
    
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
    UseMethod("sample")
}  


# ------------------------------------------------------------------------------

    
sample.default =
function (x, size, replace = FALSE, prob = NULL, ...) 
{
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
    .Internal(round(x, digits, ...))
}       
            
            
################################################################################


log = 
function(x, base = exp(1)) 
{
    UseMethod("log")
}


# ------------------------------------------------------------------------------


log.default =
function(x, base = exp(1))
{   # A copy of the log() function from R's base package

    # Log:
    if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
}


################################################################################


outlier = 
function(x, sd = 5, complement = TRUE, ...) 
{
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
    UseMethod("var")
}


# ------------------------------------------------------------------------------


var.default =
function(x, y = NULL, na.rm = FALSE, use) 
{   # A copy of the var() function from R's base package

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
    UseMethod("rownames<-")
}


# ------------------------------------------------------------------------------
    

"rownames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

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
    UseMethod("colnames<-")
}


# ------------------------------------------------------------------------------
    

"colnames<-.default" =
function(x, value)
{   # A modfied copy from R's base package

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

