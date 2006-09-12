
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
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHOS                 MODIFICATION METHODS:
#  diff.timeSeries        Differences a 'timeSeries' object
#  lag.timeSeries         Lags a 'timeSeries' object
#  merge.timeSeries       Merges two 'timeSeries' objects
#  rbind.timeSeries       Binds rows of two 'timeSeries' objects
#  cumsum.timeSeries      Returns cumulated sums of 'timeSeries' objects
#  scale.timeSeries       Centers and/or scales a 'timeSeries' object
#  var.timeSeries         Returns variance for a 'timeSeries' object
# METHODS                MATHEMATICAL OPERATIONS ON DATA:
#  Ops.timeSeries         Returns group 'Ops' for a 'timeSeries' object
#  abs.timeSeries         Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries        Returns sqrt values of a 'timeSeries' object
#  exp.timeSeries         Returns exponentials of a 'timeSeries' object
#  log.timeSeries         Returns logarithms of a 'timeSeries' object
#  sign.timeSeries        Returns the signs of a 'timeSeries' object
#  quantile.timeSeries    Produces sample quantiles of a 'timeSeries' object
# METHODS                SUBSETTING METHODS ON DATA:
#  [.timeSeries           Subsets of a 'timeSeries' object
#  cut.timeSeries         Cuts a block from a 'timeSeries' object
#  head.timeSeries        Returns the head of a 'timeSeries' object
#  tail.timeSeries        Returns the tail of a 'timeSeries' object
#  outlier.timeSeries     Removes outliers from a 'timeSeries' object  
# METHODS                DIM OPERATIONS ON DATA: 
#  dim.timeSeries         Returns dimension of a 'timeSeries' object
#  dimnames.timeDSeries   Returns dimension names of a 'timeSeries' object
#  colnames<-.timeSeries  Assigns column names to a 'timeSeries' object
#  rownames<-.timeSeries  Assigns row names to a 'timeSeries' object
#  is.array.timeSeries    Allows that NCOL and NROW work properly
################################################################################


################################################################################
#  diff.timeSeries      Differences a 'timeSeries' object
#  lag.timeSeries       Lags a 'timeSeries' object
#  merge.timeSeries     Merges two 'timeSeries' objects
#  scale.timeSeries     Centers and/or scales a 'timeSeries' object
#  var.timeSeries       Returns variance for a 'timeSeries' object


diff.timeSeries = 
function(x, lag = 1, diff = 1, trim = FALSE, pad = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Difference 'timeSeries' objects.
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   lag - an integer indicating which lag to use. 
    #       By default 1.
    #   diff - an integer indicating the order of the difference.
    #       By default 1.
    #   trim - a logical. Should NAs at the beginning of the
    #       series be removed?
    #   pad - a umeric value with which NAs should be replaced
    #       at the beginning of the series.

    # Value:
    #   Returns a differenced object of class 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
        
    # Convert:
    y = as.matrix(x)
        
    # Check NAs:
    if (any(is.na(y))) stop("NAs are not allowed in time series")
        
    # Difference:
    z = diff(y, lag = lag, difference = diff)

    # Trim:
    if (!trim) {
        diffNums = dim(y)[1] - dim(z)[1]
        zpad = matrix(0*y[1:diffNums, ] + pad, nrow = diffNums)
        rownames(zpad) = rownames(y)[1:diffNums] 
        z = rbind(zpad, z)
    }
      
    # Record IDs:
    df = x@recordIDs
    if (trim) {
        if (sum(dim(df)) > 0) {
            TRIM = dim(df)[1] - dim(z)[1]
            df = df[-(1:TRIM), ]
        }
    }
            
    # Return Value:
    timeSeries(data = z, charvec = rownames(z), units = colnames(z),
        format = x@format, zone = x@FinCenter, FinCenter = x@FinCenter, 
        recordIDs = df, title = x@title, documentation = x@documentation)
}


# ------------------------------------------------------------------------------


lag.timeSeries = 
function(x, k = 1, trim = FALSE, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lags 'timeSeries' objects.
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   k - an integer indicating which lag to use. 
    #       By default 1.
    #   trim - a logical. Should NAs at the befinning of the
    #       series be removed? 
    
    # Value:
    #   Returns a lagged object of class 'timeSeries'.
 
    # Changes:
    #
    
    # FUNCTION:
    
    # Column Names:
    colNames = units
    
    # Internal Function:
    tslagMat = function(x, k = 1) {
        # Internal Function:
        tslag1 = function(x, k) {
            y = x
            if (k > 0) y = c(rep(NA, times = k), x[1:(length(x)-k)])
            if (k < 0) y = c(x[(-k+1):length(x)], rep(NA, times = -k))
            y }
        # Bind:
        ans = NULL
        for (i in k) {
            ans = cbind(ans, tslag1(x, i)) }
        # As Vector:
        if (length(k) == 1) ans = as.vector(ans)
        # Return Value:
        ans }
        
    # Convert:
    y = as.data.frame(x)
    y = as.matrix(y)
    Dim = dim(y)[2]
    
    # Lag on each Column:
    z = NULL
    for (i in 1:Dim) {
        ts = tslagMat( y[, i], k = k)     #, trim = FALSE)
        z = cbind(z, ts) }
    
    # Add Names:
    rownames(z) = rownames(y)    
    colnames(z) = rep(colnames(y), each = length(k)) 
        
    # Return Value:
    ans = timeSeries(data = z, charvec = rownames(z), units = colnames(z),
        format = x@format, FinCenter = x@FinCenter,
        title = x@title, documentation = x@documentation)
    
    # Trim:
    if (trim) {
        idx = !is.na(apply(ans@Data, 1, sum))
        ans = ans[idx,] 
    }
        
    # Augment Colnames:
    a = colnames(z)
    kcols = rep(k, times = ncol(y))
    b = paste("[", kcols, "]", sep="") 
    ab = paste(a, b, sep = "")
    colnames(ans@Data) <- ab
    
    # Record IDs:
    df = x@recordIDs
    if (trim) {
        if (sum(dim(df)) > 0) {
            TRIM = dim(df)[1] - dim(ans)[1]
            df = df[-(1:TRIM), ]
        }
    }
    ans@recordIDs = df
    
    # Return Value:
    ans      
}


# ------------------------------------------------------------------------------


merge.timeSeries =
function(x, y, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Merges two 'timeSeries' objects 
    
    # Arguments:
    #   x, y - 'timeSeries' objects
    #   units - Optional user specified units
 
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Manipulate in matrix form:
    positions = as.character(c(x@positions, y@positions))
    LENGTH = length(as.character(seriesPositions(x)))
    DUP = duplicated(positions)[1:LENGTH]
    DUP2 = duplicated(positions)[-(1:LENGTH)]
    M1 = as.matrix(x)
    M2 = as.matrix(y)
    dim1 = dim(M1) 
    dim2 = dim(M2) 
    X1 = matrix(rep(NA, times = dim1[1]*dim2[2]), ncol = dim2[2])
    X2 = matrix(rep(NA, times = dim2[1]*dim1[2]), ncol = dim1[2])
    colnames(X1) = colnames(M2) 
    NC = (dim1 + dim2)[2]+1
    Z = rbind(cbind(M1, X1, DUP), cbind(X2, M2, DUP2))
    Z = Z[order(rownames(Z)), ]
    NC1 = dim1[2]+1
    IDX = (1:(dim1+dim2)[1])[Z[, NC] == 1]
    Z[IDX-1, NC1:(NC-1)] = Z[IDX, NC1:(NC-1)]
    Z = Z[!Z[, NC], -NC]
    
    # Create time series:
    ans = timeSeries(data = Z, charvec = rownames(Z), zone =
        x@FinCenter, FinCenter = x@FinCenter, units = c(x@units, y@units))
    
    # Optionally add user specified units:
    if (!is.null(units)) {
        ans@units = units
        colnames(ans@Data) <- units
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------ 
 
   
rbind.timeSeries =
function(x, y)
{   # A function implemented by Diethelm Wuertz

    # Check Arguments:
    stopifnot(is.timeSeries(x) & is.timeSeries(y))
    stopifnot(dim(x)[2] == dim(y)[2])
    
    # Bind:
    x@positions = c(x@positions, y@positions)   
    x@Data = as.matrix(rbind(x@Data, y@Data))
    x@recordIDs = as.data.frame(rbind(x@recordIDs, y@recordIDs))
    
    # Return Value
    x
}  



# ------------------------------------------------------------------------------


cumsum.timeSeries = 
function (x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns cumulated sums of 'timeSeries' objects
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Cumulate:
    x@Data = colCumsums(x@Data)
    
    # Return Value:
    x    
}


# ------------------------------------------------------------------------------


rev.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time reverts a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns a reverted object of class 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Revert:
    x@Data = apply(x@Data, 2, rev)
    x@positions = rev(x@positions)
    
    # Revert IDs:
    DF = x@recordIDs
    DIM = dim(DF)
    if (sum(DIM) > 0) {
        df = rev(DF[, 1])
        if (DIM[2] > 1) 
            for (i in 2:DIM[2]) df = data.frame(df, rev(DF[, i]))
        colnames(df) <- colnames(DF)
        rownames(df) <- x@positions
        x@recordIDs = df
    }

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


scale.timeSeries =
function(x, center = TRUE, scale = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Centers and/or scales a 'timeSeries' object.

    # Changes:
    #
    
    # FUNCTION:
    
    # Scale:
    x@Data = scale(x = x@Data, center = center, scale = scale)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


var.timeSeries =
function (x, y = NULL, na.rm = FALSE, use) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns variance/covariance for a 'timeSeries' object
    
    # FUNCTION:
    
    # Variance:
    if (missing(use)) 
        use <- if (na.rm) "complete.obs" else "all.obs"
    na.method <- 
        pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    if (is.timeSeries(x)) {
        x <- as.matrix(x)
    } else {
        stopifnot(is.atomic(x))
    }
    if (is.timeSeries(y)) {
        y <- as.matrix(y)
    } else {
        stopifnot(is.atomic(y))
    }
    
    # Covariance:
    ans = .Internal(cov(x, y, na.method, FALSE))
    
    # Return Value:
    ans
}


################################################################################
#  Ops.timeSeries         Returns group 'Ops' for a 'timeSeries' object
#  abs.timeSeries         Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries        Returns sqrt values of a 'timeSeries' object
#  exp.timeSeries         Returns exponentials of a 'timeSeries' object
#  log.timeSeries         Returns logarithms of a 'timeSeries' object
#  quantile.timeSeries    Produces sample quantiles of a 'timeSeries' object


Ops.timeSeries = 
function(e1, e2 = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Uses group 'Ops' generic functions for 'timeSeries' objects
    
    # Arguments:
    #   e1, e2 - two objects of class 'timeSeries'.
    
    # Value:
    #   Returns an object of class 'timeSeries'.

    # Changes:
    #
    
    # FUNCTION:
    
    # Save:
    s1 = e1
    s2 = e2
    
    # Which one is a 'timeSeries' object?
    i1 = inherits(e1, "timeSeries")
    i2 = inherits(e2, "timeSeries")
    
    # Match positions and FinCenter?
    if (i1 && i2) {
        if (!identical(as.vector(e1@positions), as.vector(e2@positions))) 
            stop("positions slot must match")
        if (!identical(e1@FinCenter, e2@FinCenter)) 
            stop("FinCenter slot must match") 
    }
            
    # Extract Data Slot:
    if (i1) e1 = e1@Data
    if (i2) e2 = e2@Data   
        
    # Compute:
    s = NextMethod(.Generic)
    
    # Make timeSeries:
    if ( i1)        { s1@Data = s; s = s1 }
    if (!i1 &&  i2) { s2@Data = s; s = s2 } 
    if ( i1 && !i2) s@units = s1@units
    if (!i1 &&  i2) s@units = s2@units
    if ( i1 &&  i2) s@units = paste(s1@units, "_", s2@units, sep = "")
    colnames(s@Data) = s@units
    
    df = data.frame()
    if (i1) {
        if (dim(s1@recordIDs)[1] > 0) 
            df = s1@recordIDs 
    }
    if (i2) {
        if (dim(s2@recordIDs)[1] > 0) 
            df = s2@recordIDs 
    }
    if (i1 & i2) {
        if (dim(s1@recordIDs)[1] > 0 & dim(s2@recordIDs)[1] > 0) 
            df = data.frame(s1@recordIDs, s2@recordIDs)
    }
    s@recordIDs = df
    
    # Return Value:
    s
}
    
    
# ------------------------------------------------------------------------------


abs.timeSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns absolute values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = abs(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


sqrt.timeSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns logarithmic values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = sqrt(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


exp.timeSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns exponential values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = exp(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


log.timeSeries = 
function(x, base = exp(1)) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns logarithmic values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = log(x@Data, base = base)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


sign.timeSeries = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the signs of a 'timeSeries' object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Which sign ?
    x@Data = sign(x@Data)
    
    # Return Value;
    x 
}
    


# ------------------------------------------------------------------------------


quantile.timeSeries = 
function(x, probs = 0.95, ...)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   x - an object of class 'timeSeries'. The quantiles will be 
    #       computed for the selected column.
    #   probs - a numeric value or numeric vector with probabilities.
    #   column - the selected column    
    
    # Examples:
    #   quantile(as.timeSeries(data(daxRet)))
    
    # FUNCTION:
    
    # Take the appropriate column:
    if (dim(x)[[2]] > 1) stop("x must be an univariate time series")
    x = as.vector(x[, 1])

    # Compute Quantiles:
    ans = quantile(x, probs, ...)
    
    # Return Value:
    ans
}


################################################################################
#  [.timeSeries           Subsets of a 'timeSeries' object
#  cut.timeSeries         Cuts a block from a 'timeSeries' object
#  head.timeSeries        Returns the head of a 'timeSeries' object
#  tail.timeSeries        Returns the tail of a 'timeSeries' object
#  outlier.timeSeries     Removes outliers from a 'timeSeries' object 


"[.timeSeries" =
function(x, i = min(1, nrow(x@Data)):nrow(x@Data), 
j = min(1, ncol(x@Data)):ncol(x@Data))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts or replaces subsets from 'timeSeries' objects
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   i, j - subset indexes.
    
    # Value:
    #   Returns a subset from an object 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check Timezone:
    TZ = Sys.getenv("TZ")  
    if (TZ[[1]] != "GMT") {
        Sys.putenv(TZ = "GMT")
        TZ.RESET = TRUE
    } else {
        TZ.RESET = FALSE
    }
    
    # Subsets:
    if(missing(i)) { i <- min(1, nrow(x@Data)):nrow(x@Data) }
    if(missing(j)) { j <- min(1, ncol(x@Data)):ncol(x@Data) }
        
    # Subset:
    subx <- x@Data[i, j, drop = FALSE]
    x@Data = subx
    x@positions = x@positions[i]
    x@units = colnames(subx)
    
    # Record IDs:
    if (sum(dim(x@recordIDs)) > 0) {
        x@recordIDs <- x@recordIDs[i, , drop = FALSE]
    }
        
    
    # Return Value:
    if (TZ.RESET) Sys.putenv(TZ = TZ)
    x
}         


# ------------------------------------------------------------------------------


.cut.timeSeries = 
function(x, from, to, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cuts out a piece from a 'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the 
    #       blocks
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Cut:
    Positions = seriesPositions(x)
    if (missing(from)) {
        from = Positions[1]
    } else {
        from = timeDate(from)
    }
    if (missing(to)) {
        to = rev(Positions)[1]
    } else {
        to = timeDate(to)
    }
    Units = x@units
    colNames = colnames(x@Data)
    test = (Positions >= from & Positions <= to)
    Data = as.matrix(x@Data)[test, ]
    Data = as.matrix(Data)
    
    # Replace Data Slot:
    x@Data = Data
    x@positions = x@positions[test]
    x@units = Units
    x@recordIDs = data.frame()
    colnames(x@Data) = colNames
    
    # Return Value:
    x
}


cut.timeSeries = 
function (x, from, to, ...) 
{
    # From - to - Positions -- Only one Interval!
    Positions = as.POSIXct(x@positions)
    if (missing(from)) {
        from = Positions[1] 
    } else {
        from = timeDate(from)
        from = from@Data[1]
    }
    if (missing(to)) {
        to = rev(Positions)[1] 
    } else {
        to = timeDate(to)
        to = to@Data[1]
    }
    
    # Note, Test is fastest with POSIXct:
    test = (Positions >= from & Positions <= to)
    Index = (1:length(test))[test] 
    if (length(Index) == 0) return()
    
    # Compose Series:
    x@positions = x@positions[Index] 
    x@Data = as.matrix(x@Data[Index, ])  
    colnames(x@Data) = x@units
    x@recordIDs = as.data.frame(x@recordIDs[Index, ])
    
    # Return value:
    x
}


# ------------------------------------------------------------------------------


head.timeSeries = 
function(x, n = 6, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the head of a 'timeSeries' objects
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns the head of an object of class 'timeSeries'.
 
    # Changes:
    #
    
    # FUNCTION:
    
    # Head:
    ans = x[1:n, ]
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


tail.timeSeries = 
function(x, n = 6, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the tail of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns the tail of an object of class 'timeSeries'.
 
    # Changes:
    #
    
    # FUNCTION:
    
    # Tail:
    N = dim(x)[1]
    ans = x[(N-n+1):N, ]
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


outlier.timeSeries = 
function(x, sd = 10, complement = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns outliers in a timeSeries object or the complement
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   sd - a numeric value of standard deviations, e.g. 10
    #       means that values larger or smaller tahn ten 
    #       times the standard deviation of the series will
    #       be removed.
    #   complement - a logical flag, should the outler series
    #       or its complement be returns.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check if univariate Series:
    if (dim(x@Data)[2] != 1) 
        stop("Supports only univariate timeSeries Objects")
    
    # Find Outliers:
    SD = sd * sd(x@Data)
    if (complement) {
        x  = x[abs(x@Data) <= SD]
    } else {
        x = x[abs(x@Data) > SD]
    }
    
    # Return Value:
    x
}


################################################################################
#  dim.timeSeries         Returns dimension of a 'timeSeries' object
#  dimnames.timeDSeries   Returns dimension names of a 'timeSeries' object
#  colnames<-.timeSeries  Assigns column names to a 'timeSeries' object
#  rownames<-.timeSeries  Assigns row names to a 'timeSeries' object
#  is.array.timeSeries    Allows that NCOL and NROW work properly


dim.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the dimension of a 'timeSeries' object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Dimension:
    ans = dim(x@Data)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


dimnames.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the dimension names of a 'timeSeries' object
   
    # Changes:
    #
    
    # FUNCTION:
    
    # Dimension Names:
    ans = dimnames(x@Data)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


"colnames<-.timeSeries" =
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Assigns column names to a 'timeSeries' object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Assign Column Names:
    X = x@Data
    dn <- dimnames(X)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(X))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    if(is.null(value)) dn[2] <- list(NULL) else dn[[2]] <- value
    dimnames(X) <- dn   
    
    # DW addded for timeSeries objects 
    x@Data = X
    x@units = colnames(X)
    x
}


# ------------------------------------------------------------------------------


"rownames<-.timeSeries" =
function(x, value)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Assigns row names to a 'timeSeries' object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Assign Row Names:
    X = x@Data
    dn <- dimnames(X)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(X))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    if(is.null(value)) dn[1] <- list(NULL) else dn[[1]] <- value
    dimnames(X) <- dn 
    
    # DW addded for timeSeries objects 
    x@Data = X
    x@positions = rownames(X)
       
    # Return Value: 
    x
}


# ------------------------------------------------------------------------------


is.array.timeSeries = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Allows that NCOL and NROW work properly
   
    # Changes:
    #
    
    # FUNCTION:
    
    # Is an array:
    ans = TRUE
    
    # Return Value:
    ans    
}

   
################################################################################

