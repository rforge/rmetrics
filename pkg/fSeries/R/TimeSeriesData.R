
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
#  merge.timeSeries          Merges two 'timeSeries' objects
#  rbind.timeSeries          Binds rows of two 'timeSeries' objects
#  lag.timeSeries            Lags a 'timeSeries' object
#  diff.timeSeries           Differences a 'timeSeries' object
#  apply
#  fapply                    Applies a function to 'timeSeries' windows
#  .align.timeSeries         Aligns a timeSeries object 
#  aggregate.timeSeries      Aggregates a 'timeSeries' object 
#  scale.timeSeries          Centers and/or scales a 'timeSeries' object
#  roll.timeSeries
# FUNCTION:
#  cumsum.timeSeries         Returns cumulated sums of 'timeSeries' objects
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
        ## DW can this happen - check ?
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
        ## DW [] -> [[]]
        listBind[[1]] = ans
        for (i in 2:from@Dim) {
            test = (j.pos >= j.from[i] & j.pos <= j.to[i])
            # make sure that cutted is a matrix ...
            cutted = as.matrix(y[test, ])
            ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
            ans = fun(cutted, ...)
            ## DW [] -> [[]]
            listBind[[i]] = ans 
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

   
################################################################################

