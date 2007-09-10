
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
# FUNCTION:                 DESCRIPTION:
#  fapply                    Applies a function to 'timeSeries' windows
# METHOD:                   MODIFICATION METHODS:
#  .align.timeSeries         Aligns a timeSeries object
#  aggregate.timeSeries      Aggregates a 'timeSeries' object
#  diff.timeSeries           Differences a 'timeSeries' object
#  lag.timeSeries            Lags a 'timeSeries' object
#  merge.timeSeries          Merges two 'timeSeries' objects
#  rbind.timeSeries          Binds rows of two 'timeSeries' objects
#  cumsum.timeSeries         Returns cumulated sums of 'timeSeries' objects
#  scale.timeSeries          Centers and/or scales a 'timeSeries' object
#  mean.timeSeries           Returns column means for a 'timeSeries' object
#  var.timeSeries            Returns variance for a 'timeSeries' object
#  cov.timeSeries            Returns  covariance for a 'timeSeries' object
#  cor.timeSeries            Returns correlations for a 'timeSeries' object
# METHOD:                   MATHEMATICAL OPERATIONS ON DATA:
#  Ops.timeSeries            Returns group 'Ops' for a 'timeSeries' object
#  abs.timeSeries            Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries           Returns sqrt values of a 'timeSeries' object
#  exp.timeSeries            Returns exponentials of a 'timeSeries' object
#  log.timeSeries            Returns logarithms of a 'timeSeries' object
#  sign.timeSeries           Returns the signs of a 'timeSeries' object
#  quantile.timeSeries       Produces sample quantiles of a 'timeSeries' object
# METHOD:                   DATABASE ATTACHEMENT:
#  attach.timeSeries         Attaches a 'timeSeries' object
# METHOD:                   SUBSETTING METHODS ON DATA:
#  [.timeSeries              Subsets of a 'timeSeries' object
#  cut.timeSeries            Cuts a block from a 'timeSeries' object
#  windows.timeSeries        Windows a piece from a 'timeSeries' object.
#  head.timeSeries           Returns the head of a 'timeSeries' object
#  tail.timeSeries           Returns the tail of a 'timeSeries' object
#  outlier.timeSeries        Removes outliers from a 'timeSeries' object  
# METHOD:                   DIM OPERATIONS ON DATA: 
#  dim.timeSeries            Returns dimension of a 'timeSeries' object
#  dimnames.timeDSeries      Returns dimension names of a 'timeSeries' object
#  colnames<-.timeSeries     Assigns column names to a 'timeSeries' object
#  rownames<-.timeSeries     Assigns row names to a 'timeSeries' object
#  is.array.timeSeries       Allows that NCOL and NROW work properly
################################################################################


################################################################################
# FUNCTION:              DESCRIPTION:
#  fapply                 Applies a function to 'timeSeries' windows


fapply =
function(x, from, to, FUN, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Applies a function to 'timeSeries' windows
    
    # Details:
    #   This function can be used to aggregate and coursen a 
    #   'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   from, to - two 'timeDate' position vectors which size the blocks
    #   FUN - function to be applied, by default 'colAvgs'
    
    # Value:
    #   Returns a S4 object of class 'timeSeries' if FUN returns 
    #   a time series object, otherwise a list, where the entries
    #   for each window is the output of the function FUN.
    
    # Notes:
    #   The size of the 'moving' window and the selection of an
    #   'adj'-acent endpoint are not needed, all the information
    #   is kept in the 'from' and 'to' position vectors.
  
    # FUNCTION:
    
    # Check object:
    if (class(x) != "timeSeries") stop("s is not a timeSeries object")
    
    # Monthly and Quarterly from and to:
    if (is.null(from) & is.null(to)) {
        if (by[1] == "monthly") {
            # Use monthly blocks:
            from = unique(timeFirstDayInMonth(seriesPositions(x)))
            to = unique(timeLastDayInMonth(seriesPositions(x)))
        } else if (by[1] == "quarterly") {
            from = unique(timeFirstDayInQuarter(seriesPositions(x)))
            to = unique(timeLastDayInQuarter(seriesPositions(x)))
        } else {
            stop("by must be eiter monthly or quarterly")
        } 
        from@FinCenter = to@FinCenter = x@FinCenter
    }
    
    # Column Names:
    colNames = units
    
    # Function:
    fun = match.fun(FUN)
    
    # Blocks:
    j.pos = as.POSIXct(seriesPositions(x))
    j.from = as.POSIXct(from)
    j.to = as.POSIXct(to)
    
    # Iterate:
    y = x@Data
    pos = seriesPositions(x)
    rowNames = rownames(x@Data)
    
    # Compute for the first window ...
    i = 1
    test = (j.pos >= j.from[i] & j.pos <= j.to[i])
    # make sure that cutted is a matrix ...
    cutted = as.matrix(y[test, ])
    ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
    ans = fun(cutted, ...)
    
    if (is.timeSeries(ans)) {
        rowBind = ans
        for (i in 2:from@Dim) {
            test = (j.pos >= j.from[1] & j.pos <= j.to[1])
            # make sure that cutted is a matrix ...
            cutted = as.matrix(y[test, ])
            ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
            ans = fun(cutted, ...)
            rowBind = rbind(rowBind, ans) 
        }
        rownames(rowBind) = as.character(to)
        if (is.null(colNames)) {
            units = x@units 
        } else {
            units = colNames 
        }
        # Return Value:
        ans = timeSeries(data = rowBind, charvec = as.character(to), 
            units = units, format = format, zone = x@zone, FinCenter = 
            x@FinCenter, recordIDs = x@recordIDs, title = x@title, 
            documentation = x@documentation, ...) 
        return(ans)
    } else {  
        listBind = list()
        listBind[1] = ans
        for (i in 2:from@Dim) {
            test = (j.pos >= j.from[i] & j.pos <= j.to[i])
            # make sure that cutted is a matrix ...
            cutted = as.matrix(y[test, ])
            ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
            ans = fun(cutted, ...)
            listBind[i] = ans 
        }
        # Return Value:
        ans = listBind
        attr(ans, "control") <- list(x = x, from = from, to = to)
        return(invisible(ans))
    }
    
    # Return Value:
    return()
} 


################################################################################
# METHOS:                MODIFICATION METHODS:
#  .align.timeSeries      Aligns a timeSeries object
#  aggregate.timeSeries   Aggregates a 'timeSeries' object
#  diff.timeSeries        Differences a 'timeSeries' object
#  lag.timeSeries         Lags a 'timeSeries' object
#  merge.timeSeries       Merges two 'timeSeries' objects
#  rbind.timeSeries       Binds rows of two 'timeSeries' objects
#  cumsum.timeSeries      Returns cumulated sums of 'timeSeries' objects
#  scale.timeSeries       Centers and/or scales a 'timeSeries' object
#  mean.timeSeries        Returns column means for a 'timeSeries' object
#  var.timeSeries         Returns variance for a 'timeSeries' object
#  cov.timeSeries         Returns  covariance for a 'timeSeries' object
#  cor.timeSeries         Returns correlations for a 'timeSeries' object


.align.timeSeries = 
function(x, method = c("before", "after", "interp"), startOn = "hours",
by = "30 m")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Aligns a 'timeSeries' object
    
    # FUNCTION:
 
    # Settings:
    method = match.arg(method)
    numberOfColumns = dim(x)[2]
    typeMethod = c(interp = "linear", before = "constant", after = "constant")
    fMethod = c(interp = 0.5, before = 0, after = 1)
    by = .by2seconds(by)
    
    # Convert to GMT:
    tD = timeDate(x@positions, zone = x@FinCenter, FinCenter = "GMT")

    # Convert to POSIX:
    Positions = as.POSIXct(tD, tz = "GMT")
    N = length(Positions)
    Start = as.POSIXct(trunc(Positions[1], startOn), tz = "GMT")  
    End   = as.POSIXct(trunc(Positions[N], startOn), tz = "GMT") + 24*3600
    print(Start)
    print(End)
    
    # Compute Positions:
    X = as.integer(difftime(Positions, Start, units = "sec"))
    
    # Compute New Positions:
    timeDiff = as.integer(difftime(End, Start, units = "secs"))
    lengthOut = trunc(timeDiff/by) + 1
    posix = seq(from = Start, by = paste(by, "sec"), length.out = lengthOut)
    newX = as.integer(difftime(posix, Start, units = "secs"))  
    
    # Align:  
    matY = NULL
    for (i in 1:numberOfColumns) {
        Y = as.vector(x[, i])
        newY = approx(X, Y, xout = newX, method = typeMethod[method], 
            f = fMethod[method])$y
        matY = cbind(matY, newY)
        
    }

    # Create Series in local time:
    print(head(as.character(posix)))
    ans = timeSeries(matY, as.character(posix), 
        units = x@units, zone = "GMT", FinCenter = x@FinCenter,
        recordIDs = data.frame())
    
    # Return Value:
    ans       
}


# ------------------------------------------------------------------------------


aggregate.timeSeries =
function(x, by = c("monthly", "quarterly"), FUN = colMeans, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Aggregates a 'timeSeries' object
    
    # Details:
    #   This function can be used to aggregate and coursen a 
    #   'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   by - calendarical block, only active when both 'from' 
    #       and 'to' are NULL
    #   FUN - function to be applied, by default 'colAvgs'
    #   units - a character vector with column names, allows to 
    #       overwrite the column names of the input 'timeSeries'
    #       object.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    
    # Settings:
    format = x@format
    zone = x@FinCenter
    FinCenter = x@FinCenter
    recordIDs = data.frame()
    title = x@title
    documentation = x@documentation
    
    # Check object:
    stopifnot(is.timeSeries(x))
    
    # Monthly and Quarterly from and to timeDate Objects:
    if (by[1] == "monthly") {
        # Use monthly blocks:
        from = unique(timeFirstDayInMonth(seriesPositions(x)))
        to = unique(timeLastDayInMonth(seriesPositions(x)))
    } else if (by[1] == "quarterly") {
        from = unique(timeFirstDayInQuarter(seriesPositions(x)))
        to = unique(timeLastDayInQuarter(seriesPositions(x)))
    } else {
        stop("by must be eiter monthly or quarterly")
    } 
    from@FinCenter = to@FinCenter = FinCenter
    
    # Column Names:
    colNames = units
    
    # Function:
    fun = match.fun(FUN)
    
    # Blocks:
    j.pos = as.POSIXct(seriesPositions(x))
    j.from = as.POSIXct(from)
    j.to = as.POSIXct(to)
    
    # Iterate:
    y = x@Data
    pos = seriesPositions(x)
    rowNames = rownames(x@Data)
    rowBind = NULL
    for (i in 1:from@Dim) {
        test = (j.pos >= j.from[i] & j.pos <= j.to[i])
        # make sure that cutted is a matrix ...
        cutted = as.matrix(y[test, ])
        ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
        ans = fun(cutted, ...)
        rowBind = rbind(rowBind, ans) 
    }
    rownames(rowBind) = as.character(to)
    if (is.null(colNames)) {
        units = x@units 
    } else {
        units = colNames 
    }
    
    # Return Value:
    timeSeries(data = rowBind, charvec = as.character(to), units = units, 
        format = format, zone = zone, FinCenter = FinCenter, recordIDs = 
        recordIDs, title = title, documentation = documentation, ...)       
} 


# ------------------------------------------------------------------------------


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

    # FUNCTION:
        
    # Convert:
    y = as.matrix(x)
        
    # Check NAs:
    # if (any(is.na(y))) stop("NAs are not allowed in time series")
        
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

    # FUNCTION:
    
    # Scale:
    x@Data = scale(x = x@Data, center = center, scale = scale)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


mean.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns column means for a 'timeSeries' object
    
    # FUNCTION:
    
    # Return Value:
    colMeans(x@Data)
}


# ------------------------------------------------------------------------------


var.timeSeries =
function(x, y = NULL, na.rm = FALSE, use) 
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


# ------------------------------------------------------------------------------


cov.timeSeries =
function(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns variance/covariance for a 'timeSeries' object
    
    # FUNCTION:
    
    # Settings:
    x = x@Data
    if (!is.null(y)) y = y@Data
    
    # CoVariance:
    ans = cov.default(x, y, use = use, method = method) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


cor.timeSeries =
function(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns correlations for a 'timeSeries' object
    
    # FUNCTION:
    
    # Settings:
    x = x@Data
    if (!is.null(y)) y = y@Data
    
    # CoVariance:
    ans = cor.default(x, y, use = use, method = method) 
    
    # Return Value:
    ans
}


################################################################################
# METHODS:               MATHEMATICAL OPERATIONS ON DATA:
#  Ops.timeSeries         Returns group 'Ops' for a 'timeSeries' object
#  abs.timeSeries         Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries        Returns sqrt values of a 'timeSeries' object
#  exp.timeSeries         Returns exponentials of a 'timeSeries' object
#  log.timeSeries         Returns logarithms of a 'timeSeries' object
#  sign.timeSeries        Returns the signs of a 'timeSeries' object
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
    #   x - an uni- or multivariate return series of class 'timeSeries'. 

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
    #   x - an uni- or multivariate return series of class 'timeSeries'. 
  
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
    #   x - an uni- or multivariate return series of class 'timeSeries'. 

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
    #   x - an uni- or multivariate return series of class 'timeSeries'. 
 
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
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries'. 
    
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
    #   x - an uni- or multivariate return series of class 'timeSeries'.
    #   probs - a numeric value or numeric vector with probabilities.
    #   column - the selected column    
    
    # Examples:
    #   quantile(as.timeSeries(data(daxRet)))
    
    # FUNCTION:
    
    # Take the appropriate column:
    na.rm = TRUE
    X = as.matrix(x)
    ans = apply(if (na.rm) na.omit(X) else X, 2, quantile, probs = probs)
 
  
    # Return Value:
    ans
}


################################################################################
# METHODS:               DATABASE ATTACHEMENT:
#  attach.timeSeries      Attaches a 'timeSeries' object


attach.timeSeries = 
function(what, pos = 2, name = deparse(substitute(what)), 
warn.conflicts = TRUE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Attaches a 'timeSeries' object   
    
    # FUNCTION:
    
    # Convert to data.frame Object:    
    what.df = as.data.frame(what)
    
    # Attach:
    return(attach.default(what.df, pos, name, warn.conflicts))
}


################################################################################
# METHODS:               SUBSETTING METHODS ON DATA:
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

    # FUNCTION:
    
    # Check Timezone:
    TZ = Sys.getenv("TZ")  
    if (TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
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
    if (TZ.RESET) Sys.setenv(TZ = TZ)
    x
}         


# ------------------------------------------------------------------------------


cut.timeSeries = 
function (x, from, to, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cuts out a piece from a 'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the 
    #       blocks
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    
    from = timeDate(from)
    to = timeDate(to)
    Positions = seriesPositions(x)
    Units = x@units
    colNames = colnames(x@Data)
    test = (Positions >= from & Positions <= to)
    Data = as.matrix(x@Data)[test, ]
    Data = as.matrix(Data)
    x@Data = Data
    x@positions = x@positions[test]
    x@units = Units
    x@recordIDs = data.frame()
    colnames(x@Data) = colNames
    
    # Return value:
    x
}


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

    # FUNCTION:
    
    # Check:
    stopifnot(is.timeSeries(x))
    if (!is(from, "timeDate"))
        from = as.timeDate(x, zone = x@FinCenter, FinCenter = x@FinCenter)
    if (!is(to, "timeDate"))
        to = as.timeDate(x, zone = x@FinCenter, FinCenter = x@FinCenter)
    
    Positions = seriesPositions(x)   
    if (missing(from)) from = Positions[1]
    if (missing(to)) to = rev(Positions)[1]
    Positions = as.POSIXct(Positions, tz = "GMT")
    from = as.POSIXct(from, tz = "GMT")
    to = as.POSIXct(to, tz = "GMT")
    
    # Cut:
    test = (Positions >= from & Positions <= to)
    Index = (1:length(test))[test]
    if (length(Index) == 0) return()
    
    # Return value:
    x[Index, ]
}


# ------------------------------------------------------------------------------


window.timeSeries =  
function (x, from, to, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Windows a piece from a 'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the 
    #       blocks
    
    # Details:
    #   from and to, are both included in the window.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    
    from = timeDate(from)
    to = timeDate(to)
    Positions = seriesPositions(x)
    Units = x@units
    colNames = colnames(x@Data)
    test = (Positions >= from & Positions <= to)
    Data = as.matrix(x@Data)[test, ]
    Data = as.matrix(Data)
    x@Data = Data
    x@positions = x@positions[test]
    x@units = Units
    x@recordIDs = data.frame()
    colnames(x@Data) = colNames
    
    # Return value:
    x
}


# ------------------------------------------------------------------------------


head.timeSeries = 
function(x, n = 6, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the head of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns the head of an object of class 'timeSeries'.

    # FUNCTION:
    
    # Head:
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            Head = head(cbind(x@Data, as.matrix(x@recordIDs)), n = n, ...)
        } else {
            Head = head(x@Data, n = n, ...)
        }  
    } else {
        Head = head(x@Data, n = n, ...)
    }
    
    # Return Value:
    Head
}


# ------------------------------------------------------------------------------


tail.timeSeries = 
function(x, n = 6, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the tail of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns the tail of an object of class 'timeSeries'.
 
    # FUNCTION:
    
    # Tail:
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            Tail = tail(cbind(x@Data, as.matrix(x@recordIDs)), n = n, ...)
        } else {
            Tail = tail(x@Data, n = n, ...)
        }  
    } else {
        Tail = tail(x@Data, n = n, ...)
    }
    
    # Return Value:
    Tail
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
# METHODS:               DIM OPERATIONS ON DATA: 
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
  
    # FUNCTION:
    
    # Is an array:
    ans = TRUE
    
    # Return Value:
    ans    
}

   
################################################################################

