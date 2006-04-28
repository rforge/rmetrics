
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
# FUNCTION:            GENERATION OF TIME SERIES OBJECTS:
#  'timeSeries'         S4 Class definition for a 'timeSeries' object
#  timeSeries           Creates a 'timeSeries' object from scratch
#  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
#  returnSeries         Computes returns from a 'timeSeries' object  
#  applySeries          Applies a function to blocks of a 'timeSeries'
# FUNCTION:            REPRESENTATION OF TIME SERIES OBJECTS:
#  seriesData           Extracts data slot from 'timeSeries' object
#  seriesPositions      Extracts positions slot from 'timeSeries' object
#  isUnivariate         Tests if an object of class 'timeSeries' is univariate
#  isMultivariate       Tests if an object of class 'timeSeries' is multivariate
# METHODS:             PRESENTATION OF A TIME SERIES:
#  print.timeSeries     S3: Print method for a 'timeSeries' object
#  plot.timeSeries      S3: Plot method for a 'timeSeries' object
#  lines.timeSeries     S3: Lines method for a 'timeSeries' object
#  points.timeSeries    S3: Lines method for a 'timeSeries' object
# METHODS              MATHEMATICAL OPERATIONS:
#  Ops.timeSeries       S3: Arith method for a 'timeSeries' object
#  exp.timeSeries       S3: Returns exponentials of a 'timeSeries' object
#  abs.timeSeries       S3: Returns abolute values of a 'timeSeries' object
#  sqrt.timeSeries      S3: Returns sqrt values of a 'timeSeries' object
#  log.timeSeries       S3: Returns logarithms of a 'timeSeries' object
#  quantile.timeSeries  S3: produces sample quantiles of a 'timeSeries' object
# METHODS              SUBSETTING METHODS:
#  [.timeSeries         S3: subsets of a 'timeSeries' object
#  cut|cutSeries        S3: cuts a block from a 'timeSeries' object
#  head.timeSeries      S3: returns the head of a 'timeSeries' object
#  tail.timeSeries      S3: returns the tail of a 'timeSeries' object
#  start.timeSeries     S3: Extracts start date of a 'timeSeries' object 
#  end.timeSeries       S3: Extracts end date of a 'timeSeries' object
#  outlier.timeSeries   S3: Removes outliers from a 'timeSeries' object   
# METHODS:             CREATES A TIMESERIES FROM OTHER OBJECTS:
#  is.timeSeries        S3: Tests for a 'timeSeries' object
#  as.timeSeries        S3: Defines method for a 'timeSeries' object
#  as.timeS*.default    S3: Returns the input
#  as.timeS*.numeric    S3: Transforms a numeric vector into a 'timeSeries'
#  as.timeS*.data.frame S3: Transformas a 'data.frame' into a 'timeSeries'
#  as.timeS*.matrix     S3: Transformas a 'matrix' into a 'timeSeries'
#  as.timeS*.ts         S3: Transforms a 'ts' object into a 'timeSeries'
#  as.timeS*.character  S3: Loads and transformas from a demo file
#  as.timeS*.zoo        S3: Transforms a 'zoo' object into a 'timeSeries'
# METHODS:             TRANSFORMS ATIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeS*     S3: Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeS*     S3: Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.t*     S3: Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries     S3: Converts a 'timeSeries' to a 'ts'
# FUNCTION:            OPERATIONS ON TIME SERIES OBJECTS:
#  merge.timeSeries     S3: Merges a 'timeSeries' object with a 'matrix'
#  rev.timeSeries       S3: Reverts a 'timeSeries' object
#  diff.timeSeries      S3: Differences a 'timeSeries' object
#  lag.timeSeries       S3: Lags a 'timeSeries' object 
# FUNCTION:            FOR DAILY OPERATIONS:
#  dummyDailySeries     Creates a dummy daily 'timeSeries' object
#  alignDailySeries     Aligns a 'timeSeries' object to new positions 
#  ohlcDailyPlot        Plots open–high–low–close bar chart         
################################################################################


################################################################################
# GENERATION OF TIMESERIES OBJECTS:
#   We have defined a 'timeSeries' class which is in many aspects similar
#   to the S-Plus class with the same name, but has also some important
#   differeneces. The class has seven Slots, the 'Data' slot which holds 
#   the time series data in matrix form, the 'position' slot which holds
#   the time/date as a character vector, the 'format' and 'FinCenter'
#   slots which are the same as for the 'timeDate' object, the 'units'
#   slot which holds the column names of the data matrix, and a 'title'
#   and a documentation' slot which hold descriptive character strings.
#   Date and time is managed in the same way as for 'timeDate' objects.


# ------------------------------------------------------------------------------

require(methods)


# ------------------------------------------------------------------------------


setClass("timeSeries", 
    # A class implemented by Diethelm Wuertz
    
    # Description:
    #   Class representatation for 'timeSeries' Objects.
   
    # CLASS:
    
    representation(
        Data = "matrix",
        positions = "character",
        format = "character",
        FinCenter = "character",      
        units = "character",
        recordIDs = "data.frame",
        title = "character",
        documentation = "character")    
)
   

# ------------------------------------------------------------------------------


timeSeries =
function (data, charvec, units = NULL, format = "ISO", zone = myFinCenter, 
FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL, 
documentation = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a 'timeSeries' object from scratch.
    
    # Arguments:
    #   data -a 'data frame or a 'matrix' object of numeric data.
    #   charvec - a character vector of dates and times.
    #   units - an optional units string, NULL defaults an empty 
    #       string.
    #   format - the format specification of the input character 
    #       vector.
    #   zone - the time zone or financial center where the data were 
    #       recorded.
    #   FinCenter - a character with the the location of the  
    #       financial center named as "continent/city". 
    #   recordIDS - stores record IDs in form of a data frame
    #   title - an optional title string, if not specified the inputs 
    #       data name is deparsed.
    #   documentation - optional documentation string.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    #   positions - these are the POSIX date/time strings created  
    #       by default from the dimnames of the data matrix, or 
    #       alternatively if the data are read from a CSV file, 
    #       the first column is expected to hold the positions, 
    #       and the column name the "format" string.
    
    # Details:
    #    This is a minimal implementation of the SPLUS "timeSeries" 
    #    object.
    
    # Example:
    #   data.mat = matrix(round(rnorm(30),2), 10)
    #   charvec =  paste("2004-01-", c(paste("0", 1:9, sep=""), 10:30), sep="")
    #   timeSeries(data.mat, charvec)
   
    # FUNCTION:

    # Trace:
    if (FinCenter == "") FinCenter = "GMT"
    trace = FALSE
    
    # To Character Vector:
    charvec = as.character(charvec)
    
    # To Matrix:
    data = as.matrix(data)
         
    # Format:
    if (format == "" | format == "ISO") {
        if (nchar(charvec[1]) == 10) 
            format = "%Y-%m-%d" else format = "%Y-%m-%d %H:%M:%S" } 

    # Create 'timeDate' object:     
    timeDates = timeDate(charvec = charvec, format = format, zone = zone, 
        FinCenter = FinCenter) 
        
    # Add Dimnames:
    rownames(data) = format.POSIXlt(timeDates@Data)
    if (is.null(units)) {
        if (is.null(colnames(data))) {
            units = paste("TS.", 1:dim(data)[2], sep = "")
        } else {
            units = colnames(data)
        }
    }
    colnames(data) = units
    
    # Record IDs:
    if (sum(dim(recordIDs)) > 0) rownames(recordIDs) = charvec
    
    # Add title and Documentation:
    if (is.null(title)) title = "Time Series Object"
    if (is.null(documentation)) documentation = as.character(date())
    
    # Result:
    ans = new("timeSeries", 
        Data = as.matrix(data), 
        positions = rownames(data),
        format = timeDates@format,
        FinCenter = timeDates@FinCenter,  
        units = as.character(units), 
        recordIDs = recordIDs,
        title = as.character(title), 
        documentation = as.character(documentation) 
    ) 
    
    # Return Value:
    ans          
}


# ------------------------------------------------------------------------------


# 'read.timeSeries' has been replaced by Series.


# ------------------------------------------------------------------------------


readSeries =
function(file, header = TRUE, sep = ";", zone = myFinCenter, 
FinCenter = myFinCenter, title = NULL, documentation = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Reads from a spreadsheet and creates a 'timeSeries' object
    
    # Arguments:
    #   file - the filename of a spreadsheet data set from which
    #       to import the data records.
    #   header - 
    #   sep - 
    #   zone - the time zone or financial center where the data were 
    #       recorded.
    #   FinCenter - a character with the the location of the  
    #       financial center named as "continent/city". By default
    #       an empty string which means that internally "GMT" will
    #       be used.
    #   title - an optional title string, if not specified the inputs 
    #       data name is deparsed.
    #   documentation - optional documentation string.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # Notes:
    #   Note we expect that the header of the spreadsheet file in
    #   the first cell holds the time/date format specification! 
    
    # FUNCTION:
    
    # Read Data:
    df = read.table(file = file, header = header, sep = ";", ...)

    
    # Create Time Series:
    ans = as.timeSeries(df)
        
    # Add title and Documentation:
    if (is.null(title)) ans@title = "Time Series Object"
    if (is.null(documentation)) ans@documentation = as.character(date())
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    UseMethod("as.timeSeries")
}


# ------------------------------------------------------------------------------


as.timeSeries.default =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


as.timeSeries.numeric =
function(x, ...)
{   # A function implemented by Diethelm Wuertz


    # Create a dummay daily 'timeSeries' object:
    ans = dummyDailySeries(x)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.ts =
function(x, ...)
{   # A function implemented by Diethelm Wuertz


    # Create a dummay daily 'timeSeries' object:
    ans = dummyDailySeries(as.vector(x))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.character =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   as.timeSeries(data(nyse))
    
    x = eval(parse(text = eval(x)))
    as.timeSeries(x)
}



# ------------------------------------------------------------------------------


as.timeSeries.data.frame =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Examples:
    #   data(bmwRet); head(as.timeSeries(data(bmwRet)))

    # FUNCTION:
    
    # Check if the first column has a valid ISO-format:
    charvec = as.character(as.vector(x[, 1]))
    isoFormat = c("%Y%m%d", "%Y-%m-%d", "%Y%m%d%H%M", "%Y-%m-%d %H:%M",
        "%Y%m%d%H%M%S", "%Y-%m-%d %H:%M:%S")
    isoCheck = 0
    for (i in 1:4) {
        Test = !is.na(strptime(charvec, isoFormat[i])) 
        if (Test[1]) isoCheck = i
    }
    if (isoCheck == 0) {
        stop("Could not identify format type") 
    } else {
        format = isoFormat[isoCheck]
    }
    
    # Transform to Matrix:
    if (dim(x)[2] == 2) {
        X = matrix(x[, -1], ncol = dim(x)[2] - 1) 
    } else {
        X = x[, -1]
    }
    colNames = colnames(X) = colnames(x)[-1]
    Numeric = NULL
    for (i in 1:length(X[1, ])) {
        if (is.numeric(X[1, i])) Numeric = c(Numeric, i)
    }   
    if (is.null(numeric)) {
        stop("x contains no numeric columns") 
    } else {
        data = as.matrix(X[, Numeric])
        colnames(data) = colNames[Numeric]
        if (length(Numeric) != length(X[1, ])) {
            recordIDs = data.frame(X[, -Numeric])
            colnames(recordIDs) = colnames(X)[-Numeric]
        }
    }
     
    # Create Time Series Object:                          
    ans = timeSeries(data = data, charvec = charvec, 
        units = colnames(data), format = format, zone = "GMT", 
        FinCenter = "GMT", recordsIDs = recordIDs)  
        
    # Return Value:    
    ans
}



# ------------------------------------------------------------------------------


as.timeSeries.matrix =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    x = as.data.frame(x)
    as.timeSeries(x)
}


# ------------------------------------------------------------------------------


as.timeSeries.zoo =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    timeSeries(data = as.matrix(x), charvec = attr(x, "index"), 
        units = colnames(x), , format = format, , zone = "GMT",
        FinCenter = "GMT")

}


# ******************************************************************************


is.timeSeries = 
function (object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests for a 'timeSeries' object.
    
    # Arguments:
    #   object - a 'timeSeries' object to be tested.
    
    # Value:
    #   Returns 'TRUE' or 'FALSE' depending on whether its
    #   argument is of 'timeSeries' type or not.
        
    # FUNCTION:
    
    # Check:
    ans = inherits(object, "timeSeries")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


print.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an S4 object of class "timeSeries"
        
    # Arguments:
    #   x - an object of class "timeSeries"
    
    # Value:
    #   Prints a 'timeSeries' object.
        
    # FUNCTION:
        
    # Return Value:
    print.default(x@Data)
}

  
# ------------------------------------------------------------------------------


plot.timeSeries =
function(x, reference.grid = TRUE, lty = 1, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "timeSeries"
        
    # Arguments:
    #   x - a "timeSeries" object
    #   reference.grid - a logical value. Should a grid be
    #       added to the plot?
    
    # Example:
    #   data(DowJones30); x = as.timeSeries(DowJones30)
    #   par(mfrow = c(2, 1)); plot(x[,1:3]); plot(x[,3])
    
    # Value:
    #   Plots a 'timeSeries' object.
    
    # FUNCTION:
    
    # Internal Functions:
    # Partial Copy from "its" Package
    # Makes "its" and "Hmisc" obsolete
    # ... these internal functions will be replaced in a future version!
    setClass("its", 
        representation("matrix", dates = "POSIXt"))
    # Time Series:
    .its <<-  
    function(x, dates = as.POSIXct(x = strptime(dimnames(x)[[1]], format = 
    "%Y-%m-%d")), names=dimnames(x)[[2]], format = "%Y-%m-%d",...) {
        if(!is(dates, "POSIXt")) stop("dates should be in POSIX format")
        dates = as.POSIXct(dates)
        if(is.null(dim(x))){dim(x) = c(length(x),1)}
        # addDimnames:
        if(is.null(dimnames(x))) {dimnames(x) = list(NULL,NULL)}
        if(is.null(dimnames(x)[[1]])&(nrow(x)>0)) dimnames(x)[[1]] = 1:nrow(x)
        if(is.null(dimnames(x)[[2]])&(ncol(x)>0)) dimnames(x)[[2]] = 1:ncol(x) 
        # Continue:
        if(!(nrow(x) == length(dates))) 
            {stop("dates length must match matrix nrows")}
        if(!(ncol(x) == length(names))) 
            {stop("names length must match matrix ncols")}
        dimnames(x)[[1]] = format(dates,format=format,...)
        dimnames(x)[[2]] = names
        return(new("its" ,x, dates = dates)) }
    # Time Series Plot:
    .itsPlot <<- 
    function(x, y, colvec = 1:ncol(x), type = "l", ltypvec = 1, lwdvec = 1, 
    yrange, format, at, reference.grid, ...) {
        if (missing(yrange)){ylim = range(x, na.rm = TRUE)} else {ylim = yrange}
        firstp = TRUE
        xdates = x@dates
        n = dim(x)[1]
        m = dim(x)[2]
        # Make line control parameters correct length
        colveclong = rep(colvec, length.out = m)
        ltypveclong = rep(ltypvec, length.out = m)
        lwdveclong = rep(lwdvec, length.out = m)
        for (i in 1:m){
            vpoints = c(1,which(!is.na(x[,i])),n)
            xxx = x[,i] 
            for (j in 1:ncol(xxx)) {
                if(!firstp){par(new = TRUE)} else {firstp = FALSE}
                plot(x = xdates[vpoints], y = xxx[vpoints, j], type = type,
                    col = colveclong[i], ylim = ylim, lty = ltypveclong[i],
                    lwd = lwdveclong[i], xaxt = "n", ...) } }
        if (reference.grid) grid()
        axis.POSIXct(x = xdates[vpoints], side = 1, at = at, format = format) }
    # "[" Method:   
    "[.its" <<- function(x, i, j, drop, ...) {
        if (match("dates", names(list(...)),0) > 0) {
            dates = list(...)[["dates"]]
            if (!missing(i)) stop("cannot specify both dates and i")
            if (!is(dates, "POSIXt")) stop("dates should be in POSIX format")
            dates = as.POSIXct(dates)
            i = match(dates, dates(x))
            if (any(is.na(i))) stop("some dates are not found") }
        if (missing(drop)) {drop = FALSE}
        if (missing(i)) {i = min(1,nrow(x)):nrow(x)}
        if (missing(j)) {j = min(1,ncol(x)):ncol(x)}
          subx <- x@.Data[i, j, drop = drop]
          dates <- x@dates[i]
          ans <- new("its", subx, dates = dates)
        return(ans) }
        
    # Transform:
    x.its = .its(x@Data, dates = as.POSIXct(seriesPositions(x)), 
        format = x@format)
            
    # Plot:
    .itsPlot(x.its, ltypvec = lty, reference.grid = reference.grid, ...)
   
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


lines.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "timeSeries"
        
    # Arguments:
    #   x - a "timeSeries" object
        
    # Value:
    #   Plots a 'timeSeries' object.
    
    # FUNCTION:
   
    # Add to Plot:
    lines(x = as.POSIXct(seriesPositions(x)), y = x@Data, ...)
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


points.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "timeSeries"
        
    # Arguments:
    #   x - a "timeSeries" object
        
    # Value:
    #   Plots a 'timeSeries' object.
    
    # FUNCTION:
   
    # Add to Plot:
    points(x = as.POSIXct(seriesPositions(x)), y = x@Data, ...)
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------
 

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
        if (!identical(e1@positions, e2@positions)) 
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


rev.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time revert 'timeSeries' objects
    
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


revSeries = 
function(x, ...) 
{
    rev(x, ...) 
}


# ------------------------------------------------------------------------------


diff.timeSeries = 
function(x, lag = 1, diff = 1, trim = FALSE, pad = NA, ...) 
{
    diffSeries(x, lag, diff, trim, pad, ...) 
}


# ------------------------------------------------------------------------------


diffSeries = 
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
    #   trim - a logical. Should NAs at the befinning of the
    #       series be removed?
    #   pad - a umeric value with which NAs should be replaced
    #       at the beginning of the series.

    # Value:
    #   Returns a differenced object of class 'timeSeries'.
    
    # FUNCTION:
        
    # Convert:
    y = as.data.frame(x)
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
            TRIM = dim(df)[1] - dim(x)[1]
            df = df[-(1:TRIM), ]
        }
    }
            
    # Return Value:
    timeSeries(data = z, charvec = rownames(z), units = colnames(z),
        format = x@format, FinCenter = x@FinCenter, recordIDs = df,
        title = x@title, documentation = x@documentation)
}


# ------------------------------------------------------------------------------


lag.timeSeries = 
function(x, k = 1, trim = FALSE, units = NULL, ...)
{
    lagSeries(x, k, trim, units, ...)
}


# ------------------------------------------------------------------------------


lagSeries = 
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


outlierSeries = 
function(x, sd = 10, complement = TRUE) 
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


# ------------------------------------------------------------------------------


exp.timeSeries = 
function(x, base = exp(1)) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns exponential values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = exp(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


expSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns exponential values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
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
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = log(x@Data, base = base)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


logSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns logarithmic values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = log(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


abs.timeSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns absolute values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = abs(x@Data)
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


absSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns absolute values of a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
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
    
    # FUNCTION:
    
    # Absolute Values:
    x@Data = sqrt(x@Data)
    
    # Return Value:
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
 
    # FUNCTION:
    
    # Tail:
    N = dim(x)[1]
    ans = x[(N-n+1):N, ]
    
    # Return Value:
    ans
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
# REPRESENTATION OF TIMESERIES OBJECTS:
#   This is a collection of functions to represent 'timeSeries' objects
#   in different forms. Included are functions to extract the data slot 
#   from 'timeSeries' object, to extract the position slot, to extracts 
#   the start and end date of a 'timeSeries' object, and to convert an 
#   an univariate "timeSeries" to a vector or 'timeSeries' objects to a
#   matrix or data frame.


seriesData =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.
    
    # Arguments:
    #   object - a 'timeSeries' object
    
    # Value:
    #    Returns an object of class 'matrix'.
    
    # FUNCTION:
    
    # Test:
    if(class(object) != "timeSeries") stop("Object is not a time Series")
    
    # Get Data Slot:
    ans = object@Data
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


seriesPositions =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the positions of a 'timeSeries' objects and 
    #   converts them to a 'timeDate' object.
    
    # Arguments:
    #   object - a 'timeSeries' object
    
    # Value:
    #   Returns 'timeSeries' positions as 'timeDate' objects.
    
    # FUNCTION:
        
    # Create 'timeDate' Object:
    ans = timeDate(charvec = object@positions, format = object@format, 
        zone = object@FinCenter, FinCenter = object@FinCenter)   
        
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


isUnivariate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if an object of class timeSeries is univariate
    
    # FUNCTION:
    
    # is univariate ?
    if (class(x) == "timeSeries") {
        # timeSeries Object ?
        DIM = dim(x@Data)[2]
        # Univariate ?
        if (DIM == 1) {
            ans = TRUE 
        } else {
            ans = FALSE
        }
    } else {
        stop("x is not an object of class timeSeries")
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


isMultivariate = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Examples:
    #   isMultivariate(as.timeSeries(data(daxRet)))
    
    # FUNCTION:
    
    # Test:
    ans = !isUnivariate(x)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


start.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first position as a character string
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns the first time/date as an object of class 'timeDate'.
  
    # FUNCTION:
    
    # S3 Method:
    ans = start.timeDate(seriesPositions(x))

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


end.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last position as a character string
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns the last time/date as an object of class 'timeDate'.
    
    # FUNCTION:
    
    # S3 Method:
    ans = end.timeDate(seriesPositions(x))

    # Return Value:
    ans
}


# ******************************************************************************


as.vector.timeSeries =
function(x, mode = "any") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a univariate "timeSeries" to a vector

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data slot of 'timesSeries' object as a vector.
        
    # FUNCTION:
        
    # Check:
    if (class(x) != "timeSeries") 
        stop("x is not a timeSeries object!")
    if (dim(as.matrix(x))[[2]] != 1) 
        stop("x is not a univariate timeSeries object!")
        
    # Convert:
    rownames = dimnames(x)[[1]]
    x = x@Data
    class(x) = "numeric"
    x = as.vector(x)
    names(x) = rownames
    
    # Return Value:
    x 
}
    

# ------------------------------------------------------------------------------


as.matrix.timeSeries =
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a matrix

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data slot of a 'timesSeries' object as a vector.
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "timeSeries") stop("x is not a timeSeries object!")
        
    # Convert:
    ans = as.matrix(x@Data) # is matrix
        
    # Return Value:
    ans 
}
    

# ------------------------------------------------------------------------------


as.data.frame.timeSeries =
function(x, row.names = NULL, optional = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a data.frame
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   row.names, optional - not used
    
    # Value:
    #   Returns the data slot of a 'timesSeries' object as a data frame.
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "timeSeries") stop("x is not a timeSeries object!")
        
    # Convert:
    dimNames = dimnames(x@Data)
    ans = as.matrix(x@Data) 
    dimnames(ans) = dimNames
    ans = as.data.frame(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.ts.timeSeries = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a colum from a 'timeSeries' object into an object
    #   of class 'ts'.
    
    # Example:
    #   x = as.timeSeries(data(daxRet)); as.ts(x[1:50, ])
    
    # Transform:
    if (isUnivariate(x)) {
        ans = as.ts(as.vector(x@Data[, 1]), ...)
    } else if (isMultivariate(x)) {
        ans = as.ts(x@Data, ...)
    }
    
    # Add Attribute:
    attr(ans, "positions") = seriesPositions(x)
    
    # Return Value:
    ans
}
    

################################################################################
# MATHEMATICAL OPERATIONS ON TIMESERIES OBJECTS:
#   This is a collection of functions to perform mathematical operations
#   on 'timeSeries' objects. Included are functions to apply a function 
#   to margins of a 'timeSeries', to cut out a piece from a 'timeSeries' 
#   object, to ggregates and coursene a 'timeSeries' object, to merge 
#   a 'timeSeries' object with a 'matrix', and to compute returns from 
#   a 'timeSeries' object.


applySeries =
function(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
FUN = colAvgs, units = NULL, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Apply a function to the margins of a 'timeSeries' object
    
    # Details:
    #   This function can be used to aggregate and coursen a 
    #   'timeSeries' object.
    
    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   from, to - two 'timeDate' position vectors which size the 
    #       blocks
    #   blocks - only active when both from and to are NULL
    #   FUN - function to be applied, by default 'colAvgs'
    #   units - a character vector with column names, allows to 
    #       overwrite the column names of the input 'timeSeries'
    #       object.
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # Notes:
    #   The size of the 'moving' window and the selection of an
    #   'adj'-acent endpoint are not needed, all the information
    #   is kept in the 'from' and 'to' position vectors.
  
    # FUNCTION:
    
    # Check object:
    if (class(x) != "timeSeries") stop("s is not a timeSeries object")
    
    # Monthly from and to:
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
        units = colNames }
    
    # Return Value:
    timeSeries(data = rowBind, charvec = as.character(to), units = units, 
        format = x@format, zone = x@FinCenter, , FinCenter = x@FinCenter, 
        title = x@title, documentation = x@documentation, ...)       
}   


# ------------------------------------------------------------------------------


cut.timeSeries = 
function(x, from, to, ...)
{
    cutSeries(x, from, to, ...)
}


# ------------------------------------------------------------------------------


cutSeries = 
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

    
# ------------------------------------------------------------------------------


merge.timeSeries = 
function(x, y, units = NULL, ...)
{
    mergeSeries(x, y, units, ...)
}


# ------------------------------------------------------------------------------


mergeSeries =
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
    
    # Execute old version if y is a matrix:
    if (is.matrix(y)) return(.mergeSeries(x, y, units))
    
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
    ans = timeSeries(data = Z, charvec = rownames(Z), FinCenter =
        "GMT", units = c(x@units, y@units))
    
    # Optionally add user specified units:
    if (!is.null(units)) {
        ans@units = units
        colnames(ans@Data) <- units
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.mergeSeries = 
function(x, y, units = NULL)
{   # A function implemented by Diethelm Wuertz
    
    # IMPORTANT:
    #   This is the old version where 'y' is a matrix with the same 
    #   row dimension as 'x'.
    
    # Description:
    #   Merges a 'timeSeries' with a 'matrix' object 
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   y - a numeric matrix with the same number of rows as x
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # FUNCTION:
    
    # Test Input:
    if (class(x) != "timeSeries") stop("x must be a timeSeries")
    if (!is.matrix(y)) stop("y must be a matrix")
    xRows = dim(x@Data)[1]
    yRows = dim(as.matrix(y))[1]
    if (xRows != yRows) stop("x and y must be of same length")
    
    # Bind Data:
    x@Data = cbind(x@Data, y)
    
    # Add Column Names:
    if (is.null(colnames(y))) 
        colnames(y) <- paste("Y", 1:(dim(y)[2]), sep="")
    colnames(x@Data) <- c(x@units, colnames(y))
    if (!is.null(units)) {
        x@units = units
        colnames(x@Data) <- units }     
    
    # Return Value:
    new("timeSeries", 
        Data = x@Data, 
        positions = x@positions, 
        format = as.character(x@format), 
        FinCenter = as.character(x@FinCenter),
        units = colnames(x@Data), 
        recordIDs = data.frame(),
        title = as.character(x@title), 
        documentation = as.character(x@documentation) )          
}


# ------------------------------------------------------------------------------


returnSeries =
function(x, type = c("continuous", "discrete"), percentage = FALSE, 
trim = TRUE, digits = 4, units = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes returns from a financial price series
    
    # Arguments:    
    #   x - a univariate or multivariate 'timeSeries' object or a  
    #       numeric vector or matrix.
    #   type - a character string specifying how to compute the
    #       returns. Valid choices are: "continuous" and "discrete". 
    #       For the default type = "continuous", the returns are 
    #       calculated as the logarithm differences, otherwise if 
    #       type = "discrete", the returns are calculated as 
    #       percentage changes. 
    #   percentage - by default FALSE, if TRUE the series will be  
    #       expressed in % changes.
    #   trim - a logical flag, by default TRUE, the first missing 
    #       observation in the return series will be removed. 
    #   digits - an integer value, the number of digits of the 
    #       series of returns.
    #   units - a character value or vector which allows to set new 
    #       instrument names to the Data matrix

    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Internal Function for One Column Object:
    getReturnsForOneColumn =
    function(x = x, type = type, percentage = percentage) {
        # Object has to be a vector:
        x = as.vector(x)
        # Continuous: Calculate Log Returns:
        if (type == "continuous") { 
                x = c(NA, diff(log(x))) }   
        # Discrete: Calculate Returns:
        if (type == "discrete") { 
            x = c(NA, diff(c(x, NA))/x)[-(length(x)+1)] }   
        # Percentage Return ?
        if (percentage) { x = x*100 }
        # Return Value:
        x }
        
    # Result:
    if (class(x) == "timeSeries") {
        y = seriesData(x)
        ans = NULL
        for ( i in 1:(dim(y)[[2]]) ) {
            ans = cbind(ans, getReturnsForOneColumn(x = y[,i], 
                type = type, percentage = percentage)) }
        rownames(ans) = rownames(y)
        colnames(ans) = colnames(y)
        ans = new("timeSeries", 
            Data = as.matrix(ans), 
            positions = as.character(x@positions), 
            format = as.character(x@format), 
            FinCenter = as.character(x@FinCenter), 
            units = as.character(x@units), 
            recordIDs = data.frame(),
            title = as.character(x@title), 
            documentation = as.character(x@documentation) ) 
    } else {  
        x = as.vector(x)        
        ans = getReturnsForOneColumn(x = x, type = type, 
            percentage = percentage) 
    }
            
    # Trim:
    if (trim) ans = ans[-1, ]
    # DW: round replaced by signif
    # if (percentage) digits = digits - 2
    ans@Data = signif(ans@Data, digits = digits)
    # DW
    
    # Add New Units:
    if (!is.null(units)){
        ans@units = units
        colnames(ans@Data) = units
    }
    
    # Return Value:
    ans
}


################################################################################


dummyDailySeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a dummy daily time Series
    
    # Arguments:
    #   x - a numeric vector 
    #   origin - the first date in the series
    
    # Example:
    #   dummyDailySeries(rnorm(100))
    
    # FUNCTION:
    
    # Check:
    stopifnot(is.numeric(x))
    
    # Time Series:
    positions = timeSequence(from = "1970-01-01", length.out = length(x),
        FinCenter = "GMT")
    ans = timeSeries(data = matrix(x, ncol = 1), charvec = positions,
        units = "TS", FinCenter = "GMT")
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


alignDailySeries = 
function (x, method = c("before", "after", "interp", "fillNA"), 
include.weekends = FALSE, units = NULL) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Aligns an univariate 'timeSeries' object to new positions
    
    # Arguments:
    #   x - an object of class "timeSeries".
    #   method - 
    #       "before" - use the data from the row whose position is
    #           just before the unmatched position; 
    #       "after" - use the data from the row whose position is
    #           just after the unmatched position; 
    #       "linear" - interpolate linearly between "before" and 
    #           "after". 
    #       "fillNA" - fill missing days with NA values
    #   include.weekends - a logical value. Should weekend dates be 
    #       included or removed?
    
    # FUNCTION:
    
    # Settings:
    method = method[1]
    
    # Internal Function
    # Univariate Time Series Alignment:
    alignDailySeries.OneColumn = 
    function (x, method = method, include.weekends = include.weekends) {
        # Settings:
        units = x@units
        FinCenter = x@FinCenter
        # Units:
        # myUnits <<- "days"
        myUnits <- "days"
        # Fill with NAs:
        if (method == "fillNA") {
            colsX = 1
            dtCount = as.integer(julian(seriesPositions(x)))
            cbind(dtCount, x@Data)
            newX = rep(NA, times = colsX * (max(dtCount) - min(dtCount) + 1))
            newX = matrix(newX, ncol = colsX)
            index = dtCount - min(dtCount) + 1
            newX[index, ] = x@Data
            colnames(newX) = colnames(x@Data)
            newPos = (min(dtCount):max(dtCount)) * 24 * 3600
            class(newPos) = "POSIXct"
            newPos = as.POSIXlt(newPos)
            td = timeSeries(newX, newPos, units = colnames(newX), 
                zone = x@FinCenter, FinCenter = x@FinCenter)
        } else {
            # Interpolate with real Values:
            # Wich method ?
            if (method == "interp") {
                method = "linear"
                f = 0.5 }
            if (method == "before") {
                method = "constant"
                f = 0 }
            if (method == "after") {
                method = "constant"
                f = 1 }
            # Get Positions and Data:
            positions = seriesPositions(x)
            u = as.integer(julian(positions))
            v = as.vector(x@Data[, 1])
            # Approximate:
            #   method - specifies the interpolation method to be used.  
            #       Choices are "linear" or "constant".
            #   f - For method="constant" a number between 0 and 1 inclusive,
            #       indicating a compromise between left- and right-continuous
            #       step functions. If 'y0' and 'y1' are the values to the left
            #       and right of the point then the value is 'y0*(1-f)+y1*f' so
            #       that 'f=0' is right-continuous and 'f=1' is left-continuous.    
            x = u[1]:u[length(u)]
            y = approx(u, v, xout = x, method = method, f = f)$y
            # Create timeSeries:
            poschar = as.character(positions)
            td = timeSeries(y, timeSequence(from = poschar[1], 
                to = poschar[length(poschar)], FinCenter = FinCenter, 
                format = "%Y-%m-%d"), FinCenter = FinCenter)
            td@format = "%Y-%m-%d" }
        # Handle Weekends:
        if (!include.weekends) {
            # Internal Functions:
            is.weekday = function(x) {
                # x - a 'timeDate' object
                wday = as.POSIXlt(as.POSIXct(x))$wday
                return(!(wday == 0 | wday == 6)) }
            # Test:
            test = is.weekday(seriesPositions(td))
            td@Data = as.matrix(td@Data[test, 1])
            td@positions = td@positions[test] }
        # Units:
        td@units = units
        colnames(td@Data) = units
        ans = td
        # Return Value:
        ans }
        
    # First Column:
    ans = alignDailySeries.OneColumn(x = x[, 1], method = method, 
        include.weekends = include.weekends)
        
    # Next Columns:
    DimX = dim(x@Data)[2]
    if ( DimX > 1 ) {
        for ( i in 2:DimX ) {
            ans.add = alignDailySeries.OneColumn(x = x[, i], 
                method = method, include.weekends = include.weekends)
            ans = mergeSeries(ans, ans.add@Data) }  
    }
         
    # Add New Units:
    if (!is.null(units)){
        ans@units = units
        colnames(ans@Data) = units
    }       
    
    # Return Value:
    ans
   
}


# ------------------------------------------------------------------------------


ohlcDailyPlot =
function(x, volume = TRUE, colOrder = c(1:5), units = 1e6, xlab = 
c("Date", "Date"), ylab = c("Price", "Volume"), main = c("O-H-L-C", "Volume"), 
grid.nx = 7, grid.lty = "solid", ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Plots open–high–low–close bar chart 
    
    # Reference:
    #   Build on top of Adrian Trapletti's plotOHLC()
    #   function from his R-package "tseries".
    
    # FUNCTION:
    
    # Units:
    # myUnits <<- "days"
    myUnits <<- "days"
    
    # Internal Function:
    addDailyNA = function(x) {
        # Fill:
        colsX = 5
        dtCount = as.integer(julian(seriesPositions(x)))
        cbind(dtCount, x@Data)
        newX = rep(NA, times = colsX*(max(dtCount) - min(dtCount) + 1))
        newX = matrix(newX, ncol = colsX)
        index = dtCount - min(dtCount) + 1
        newX[index, ] = x@Data
        colnames(newX) = colnames(x@Data)
        newPos = (min(dtCount):max(dtCount)) * 24 * 3600
        class(newPos) = "POSIXct"
        newPos = as.POSIXlt(newPos) 
        # New Daily Time Series:
        ans = timeSeries(newX, newPos, units = colnames(newX), 
            zone = x@FinCenter, FinCenter = x@FinCenter)
        # Return Value:
        ans }
     
    # Next:   
    x.filled = addDailyNA(x[, colOrder])
    jul = as.integer(julian(seriesPositions(x.filled)))
    X = ts(x.filled@Data[, 1:4], start = min(jul), end = max(jul))
    
    # plotOHLC - require ( tseries ) :
    plotOHLC = function (x, xlim = NULL, ylim = NULL, xlab = "Time", 
    ylab, col = par("col"), bg = par("bg"), axes = TRUE, frame.plot = 
    axes, ann = par("ann"), main = NULL, date = c("calendar", "julian"), 
    format = "%Y-%m-%d", origin = "1899-12-30", ...) {
        if ((!is.mts(x)) || (colnames(x)[1] != "Open") || (colnames(x)[2] != 
            "High") || (colnames(x)[3] != "Low") || (colnames(x)[4] != 
            "Close")) stop("x is not a open/high/low/close time series")
        xlabel <- if (!missing(x)) deparse(substitute(x)) else NULL
        if (missing(ylab)) ylab <- xlabel
        date <- match.arg(date)
        time.x <- time(x)
        dt <- min(lag(time.x) - time.x)/3
        if (is.null(xlim)) xlim <- range(time.x)
        if (is.null(ylim)) ylim <- range(x[is.finite(x)])
        plot.new()
        plot.window(xlim, ylim, ...)
        segments(time.x, x[, "High"], time.x, x[, "Low"], col = col[1], bg = bg)
        segments(time.x - dt, x[, "Open"], time.x, x[, "Open"], col = col[1], 
            bg = bg)
        segments(time.x, x[, "Close"], time.x + dt, x[, "Close"], 
            col = col[1], bg = bg)
        if (ann) title(main = main, xlab = xlab, ylab = ylab, ...)
        if (axes) {
            if (date == "julian") {
                axis(1, ...)
                axis(2, ...) 
            } else {
                n <- NROW(x)
                lab.ind <- round(seq(1, n, length = 5))
                D <- as.vector(time.x[lab.ind] * 86400) + as.POSIXct(origin, 
                    tz = "GMT")
                DD <- format.POSIXct(D, format = format, tz = "GMT")
                axis(1, at = time.x[lab.ind], lab = DD, ...)
                axis(2, ...) } }
        if (frame.plot) box(...) }
    
    # Plot OHLC: 
    plotOHLC(X, origin = "1970-01-01", xlab = xlab[1], ylab = ylab[1])
    print(axTicks(1))
    print(axTicks(2))
    title(main = main[1])
    grid(nx = grid.nx, ny = NULL, lty = grid.lty, ...)
    
    # Include Volume?
    if (volume) {
        Volume = x[, 5]/units
        plot(Volume, reference.grid = FALSE, type = "h", 
            xlab = xlab[2], ylab = ylab[2])
        title(main = main[2])
        grid(nx = grid.nx, ny = NULL, lty = grid.lty, ...) }
    
    # Return value:
    invisible()  
}   

   
################################################################################
# ADDONS:


"colnames<-.timeSeries" =
function(x, value)
{
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
{
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


dim.timeSeries =
function(x)
{
    # This allows that ncol and nrow work properly
    # Note: dim is .Primitive("dim")
    dim(x@Data)
}


# ------------------------------------------------------------------------------


dimnames.timeSeries =
function(x)
{
    # This allows that colnames and rownames work properly
    # Note: dimnames is .Primitive("dimnames")
    dimnames(x@Data)
}


# ------------------------------------------------------------------------------


is.array.timeSeries = 
function(x)
{
    # This allows that NCOL and NROW work properly
    # Note: is.array is .Primitive("is.array")
    TRUE
}


# ------------------------------------------------------------------------------


scale.timeSeries =
function(x, center = TRUE, scale = TRUE)
{
    x@Data = scale(x = x@Data, center = center, scale = scale)
    x
}


# ------------------------------------------------------------------------------


summary.timeSeries =
function(object, ...)
{
    summary(as.matrix(object), ...)
}


# ------------------------------------------------------------------------------


t.timeSeries =
function(x)
{
    ans = t(x@Data)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


var.timeSeries =
function (x, y = NULL, na.rm = FALSE, use) 
{
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


as.vector.zoo =
function(x, mode = "any") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a univariate "zoo" series to a vector

    # Arguments:
    #   x - a 'zoo' object
    
    # Example:
    #   require(tseries); as.vector(get.hist.quote("IBM", quote = "Close"))
    
    # Value:
    #   Returns the data of an 'zoo' object as a named vector.
        
    # FUNCTION:
        
    # Check:
    if (class(x) != "zoo") 
        stop("x is not a timeSeries object!")
    if (dim(x)[[2]] != 1) 
        stop("x is not an univariate zoo object!")
        
    # Convert:
    Names = as.character(attr(x, "index"))
    x = unclass(x)[,1]
    names(x) = Names
    attr(x, "index") = NULL
    
    # Return Value:
    x 
}
    

# ------------------------------------------------------------------------------


as.matrix.zoo =
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a matrix

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data of an 'zoo' object as a named matrix.
    
    # Example:
    #   require(tseries); as.matrix(get.hist.quote("IBM"))
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "zoo") 
        stop("x is not a timeSeries object!")
    if (dim(x)[[2]] <= 1) 
        stop("x is not a multivariate zoo object!")
        
    # Convert:
    Names = as.character(attr(x, "index"))
    x = unclass(x)
    rownames(x) = Names
    attr(x, "index") = NULL
        
    # Return Value:
    x 
}


# ------------------------------------------------------------------------------


quantile.zoo = 
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
    
    # Convert to timeSeries:
    ans = quantile(as.timeSeries(x), ...)
       
    # Return Value:
    ans
}
   

################################################################################ 