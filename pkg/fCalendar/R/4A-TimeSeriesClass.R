
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
#  durationSeries       Computes durations from a 'timeSeries' object
#  midquoteSeries       Computes mid quotes from a 'timeSeries' object
#  spreadSeries         Computes spreads from a 'timeSeries' object
#  applySeries          Applies a function to blocks of a 'timeSeries'
#  orderStatistics      Compute order statistic of a 'timeSeries'
# FUNCTION:            DATA SLOT AND CLASSIFICATION OF TIME SERIES OBJECTS:
#  seriesData           Extracts data slot from 'timeSeries' object
#  isUnivariate         Tests if object of class 'timeSeries' is univariate
#  isMultivariate       Tests if object of class 'timeSeries' is multivariate
# METHODS:             PRINT AND PLOT FUNCTIONS:
#  print.timeSeries     Prints a 'timeSeries' object
#  summary.timeSeries   Summarizes a 'timeSeries' object
#  plot.timeSeries      Plots a 'timeSeries' object
#  points.timeSeries    Adds points to a 'timeSeries' plot
#  lines.timeSeries     Adds lines to a 'timeSeries' plot
# FUNCTION:            FOR DAILY OPERATIONS:
#  dummyDailySeries     Creates a dummy daily 'timeSeries' object
#  alignDailySeries     Aligns a 'timeSeries' object to new positions 
#  ohlcDailyPlot        Plots open–high–low–close bar chart    
# FUNCTION:
#  .modelSeries         Models a timeSeries object to use formulas
################################################################################


require(methods)


################################################################################
# FUNCTION:            GENERATION OF TIME SERIES OBJECTS:
#  'timeSeries'         S4 Class definition for a 'timeSeries' object
#  timeSeries           Creates a 'timeSeries' object from scratch
#  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
#  returnSeries         Computes returns from a 'timetimehhhhh' object  
#  applySeries          Applies a function to blocks of a 'timeSeries'
#  orderStatistics      Compute order statistic of a 'timeSeries'


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
function (data, charvec, units = NULL, format = NULL, zone = myFinCenter, 
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
   
    # Changes:
    #
    
    # FUNCTION:

    # Trace:
    if (FinCenter == "") FinCenter = "GMT"
    trace = FALSE
    
    # Missing charvec:
    if (missing(charvec)) {
        N = dim(as.matrix(data))[1]
        charvec = timeSequence(from = "1970-01-01", length.out = N, 
            zone = "GMT", FinCenter = "GMT")
    }
    
    # charvector | Time Positions:
    if (is.timeDate(charvec)) { 
        timeDates = charvec 
    } else {       
        timeDates = timeDate(charvec = charvec, 
            format = NULL, zone = zone, FinCenter = FinCenter) 
    }       
        
    # Data | Dimension Names:
    if (is.timeSeries(data)) {
        recordIDs = data@recordIDs
        data = data@Data
        rownames(data) = as.character(timeDates)
        units = colnames(data)  
    } else { 
        data = as.matrix(data)
        rownames(data) = as.character(timeDates)
        if (is.null(units)) {
            if (is.null(colnames(data))) {
                units = paste("TS.", 1:dim(data)[2], sep = "")
            } else {
                units = colnames(data)
            }
        }
        colnames(data) = units
    }
    
    # Record IDs:
    # DW:
    # No double row Names in data.frames - this generates problems!
    # if (sum(dim(recordIDs)) > 0) 
        # rownames(recordIDs) = as.character(charvec)
    
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
    
    # Changes:
    #
    
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
    
    # Changes:
    #
    
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


# ------------------------------------------------------------------------------


durationSeries = 
function(x, trim = FALSE, units = c("secs", "mins", "hours"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes durations from a financial price series
    
    # Arguments:    
    #   x - a univariate or multivariate 'timeSeries' object or a  
    #       numeric vector or matrix.
    #   trim - a logical flag, by default TRUE, the first missing 
    #       observation in the return series will be removed. 
    #   units - a character value or vector which allows to set the 
    #       units in which the durations are measured

    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Positions and Durations:
    pos = seriesPositions(x)
    dur = c(NA, diff(as.integer(difftime(pos, pos[1], units = units[1]))))
    
    # Data Matrix:
    x@Data = matrix(dur, dimnames = list(x@positions, "Duration"))
    if (trim) x = x[-1, ]
    
    # Return Series:
    x
}


# ------------------------------------------------------------------------------


midquoteSeries = 
function(x, which = c("Bid", "Ask"))
{   
    # Compute Mid Quotes:
    midQuotes = 0.5 * ( x[, which[1]] + x[, which[2]] ) 
    
    # Return Value:
    midQuotes
}


# ------------------------------------------------------------------------------


spreadSeries = 
function(x, which = c("Bid", "Ask"), tickSize = NULL)
{   
    # Compute Spread:
    Spread = x[, which[2]] - x[, which[1]] 
    if (!is.null(tickSize)) Spread@Data = round(Spread@Data/tickSize)
    
    # Return Value:
    Spread
}


# ------------------------------------------------------------------------------


applySeries =
function(x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
FUN = colAvgs, units = NULL, format = x@format, zone = x@FinCenter, 
FinCenter = x@FinCenter, recordIDs = data.frame(), title = x@title,
documentation = x@documentation, ...)
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
    #   by - calendarical block, only active when both 'from' 
    #       and 'to' are NULL
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
  
    # Changes:
    #
    
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
        from@FinCenter = to@FinCenter = FinCenter
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
        format = format, zone = zone, FinCenter = FinCenter, recordIDs = 
        recordIDs, title = title, documentation = documentation, ...)       
} 


.applySeries = 
function (x, from = NULL, to = NULL, by = c("monthly", "quarterly"), 
FUN = colAvgs, units = NULL, ...) 
{
    # Old/Alternative Version
    
    # Chreck for 'timeSeries' Object:
    stopifnot(is.timeSeries(x))
    stopifnot(is.timeDate(from) | is.null(from))
    stopifnot(is.timeDate(to) | is.null(to))
    
    # Allow for colMeans:
    if (substitute(FUN) == "colMeans") FUN = "colAvgs"
    
    # Monthly and Quarterly from and to:
    if (is.null(from) & is.null(to)) {
        by = match.arg(by)
        if (by == "monthly") {
            from = unique(timeFirstDayInMonth(seriesPositions(x)))
            to = unique(timeLastDayInMonth(seriesPositions(x)))
        }
        else if (by == "quarterly") {
            from = unique(timeFirstDayInQuarter(seriesPositions(x)))
            to = unique(timeLastDayInQuarter(seriesPositions(x)))
        }
        from@FinCenter = to@FinCenter = x@FinCenter
    }
    
    # Start Cutting Process:
    fun = match.fun(FUN)
    cutted = NULL
    i = 1
    
    # Find First Interval which is not empty:
    while (is.null(cutted)) {
        cutted = cut(x, from[i], to[i])
        if (!is.null(cutted)) {
            # Non empty Interval:
            ans = fun(cutted, ...)
        }
        i = i + 1
    }
    # Continue up to the end:
    for (j in i:from@Dim) {
        cutted = cut(x, from[j], to[j])
        if (!is.null(cutted)) {
            # Non empty Interval:
            newAns = fun(cutted, ...)
            ans = rbind(ans, newAns)
        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


orderStatistics =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the order statistics for a 'timeSeries object
    
    # Value:
    #   A named list with the order statistics for each column of
    #   the inputted series.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Order Statistics
    Units = x@units
    nUnits = length(Units)
    ans = list()
    for (i in 1:nUnits) {
        X = x[, i]
        positions = X@positions
        S = sort(X@Data, index.return = TRUE)
        X@Data = matrix(S$x, ncol = 1)
        X@positions = rownames(X@Data) = positions[S$ix]
        colnames(X@Data) = Units[i]
        TEXT = paste("ans$", Units[i], "=X", sep = "")
        eval(parse(text = TEXT)) 
    }
    
    # Return Value:
    ans
        
}  


################################################################################
#  seriesData           Extracts data slot from 'timeSeries' object
#  isUnivariate         Tests if an object of class 'timeSeries' is univariate
#  isMultivariate       Tests if an object of class 'timeSeries' is multivariate

seriesData =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.
    
    # Arguments:
    #   object - a 'timeSeries' object
    
    # Value:
    #    Returns an object of class 'matrix'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Test:
    if(class(object) != "timeSeries") stop("Object is not a time Series")
    
    # Get Data Slot:
    ans = object@Data
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


isUnivariate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if an object of class timeSeries is univariate
    
    # Changes:
    #
       
    # FUNCTION:
    
    # Is univariate ?
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Test:
    ans = !isUnivariate(x)
    
    # Return Value:
    ans
}


################################################################################
#  print.timeSeries     Prints a 'timeSeries' object
#  summary.timeSeries   Summarizes a 'timeSeries' object
#  plot.timeSeries      Plots a 'timeSeries' object
#  points.timeSeries    Adds points to a 'timeSeries' plot
#  lines.timeSeries     Adds lines to a 'timeSeries' plot


print.timeSeries =
function(x, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an S4 object of class "timeSeries"
        
    # Arguments:
    #   x - an object of class "timeSeries"
    
    # Value:
    #   Prints a 'timeSeries' object.
        
    # Changes:
    #
    
    # FUNCTION:
        
    # Series:
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            print(cbind(x@Data, as.matrix(x@recordIDs)), quote = FALSE, ...)
        } else {
            print(x@Data, ...)
        }  
    } else {
        print(x@Data, ...)
    }
    
    # Control:
    control = attr(x, "control")
    if (!is.null(control)) print(control)
    
    # Return Value:
    invisible(x)
}

  
# ------------------------------------------------------------------------------


summary.timeSeries = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary method for objects of class "timeDate"
    
    # Arguments
    #   x - an object of class "timeDate"
    
    # FUNCTION: 

    # Series Name:
    cat("Time Series:          ")
    cat("\n Name:              ", substitute(object))    
    # Data Matrix:
    Dim = dim(object@Data)
    cat("\nData Matrix:        ")
    cat("\n Dimension:         ", Dim)
    cat("\n Column Names:      ", colnames(object@Data) )
    firstName = rownames(object@Data)[1]
    lastName = rownames(object@Data)[Dim[1]]
    cat("\n Row Names:         ", firstName, " ... ", lastName)
    # Date/Time Positions:
    positions = seriesPositions(object)
    cat("\nPositions:          ")
    cat("\n Start:             ", as.character(start(positions)))
    cat("\n End:               ", as.character(end(positions)))
    # Other Attributes:
    cat("\nAttributes:         ")
    cat("\n Format:            ", object@format)
    cat("\n FinCenter:         ", object@FinCenter)
    cat("\n Units:             ", object@units)
    cat("\n Title:             ", object@title)
    cat("\n Documentation:     ", object@documentation)
    cat("\n") 
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


plot.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   NEW Plot method for an object of class "timeSeries"
   
    # Arguments:
    #   x - a "timeSeries" object 
    
    # FUNCTION:
    
    # Plot:
    plot(x = as.POSIXct(x@positions), y = x@Data, ...)
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


lines.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   NEW Lines method for an object of class "timeSeries"
   
    # Arguments:
    #   x - a "timeSeries" object 
    
    # Example:
    #   plot(MSFT[,1]); lines(MSFT[,1], col = "red")
    
    # FUNCTION:
    
    # Plot:
    lines(x = as.POSIXct(x@positions), y = x@Data, ...)
    
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
    
    # Changes:
    #
    
    # FUNCTION:
   
    # Add to Plot:
    points(x = as.POSIXct(x@positions), y = x@Data, ...)
            
    # Return Value:
    invisible(x)
}


################################################################################
#  dummyDailySeries     Creates a dummy daily 'timeSeries' object
#  alignDailySeries     Aligns a 'timeSeries' object to new positions 
#  ohlcDailyPlot        Plots open–high–low–close bar chart 


dummyDailySeries =
function(x = rnorm(365), units = "X", 
zone = myFinCenter, FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a dummy daily time Series
    
    # Arguments:
    #   x - a numeric vector 
    #   origin - the first date in the series
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    stopifnot(is.numeric(x))
    
    # Time Series:
    if (is.vector(x)) data = matrix(x, ncol = 1)
    if (is.matrix(x)) data = x
    positions = timeSequence(from = "1970-01-01", length.out = 
        length(data[, 1]), zone = zone, FinCenter = FinCenter)
    ans = timeSeries(data = data, charvec = positions, units = units, 
        zone = zone, FinCenter = FinCenter)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


alignDailySeries = 
function (x, method = c("before", "after", "interp", "fillNA"), 
include.weekends = FALSE, units = NULL, zone = myFinCenter, 
FinCenter = myFinCenter) 
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    method = method[1]
    
    # Internal Function
    # Univariate Time Series Alignment:
    alignDailySeries.OneColumn = 
    function (x, method, include.weekends, zone, FinCenter) {
        # Settings:
        units = x@units
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
                zone = zone, FinCenter = FinCenter)
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
        include.weekends = include.weekends, zone = zone, 
        FinCenter = FinCenter)
        
    # Next Columns:
    DimX = dim(x@Data)[2]
    if ( DimX > 1 ) {
        for ( i in 2:DimX ) {
            ans.add = alignDailySeries.OneColumn(x = x[, i], 
                method = method, include.weekends = include.weekends,
                zone = zone, FinCenter = FinCenter)
            ans = merge(ans, ans.add) }  
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
    
    # Changes:
    #    
    
    # FUNCTION:
    
    # Units:
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
# FUNCTION:
#  .modelSeries         Models a timeSeries object to use formulas


.modelSeries = 
function(formula, data, fake = FALSE, lhs = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   data - a timeSeries, a data.frame or a numeric vector
    
    # Details:
    #   Time Series Modelling
    #   Regression Modelling
    #   Data Management
    
    
    # If no respnonse is pecified:
    if (length(formula) == 2) {
        formula = as.formula(paste("x", formula[1], formula[2], collapse = ""))
        stopifnot(!missing(data))
    }

    # Isf data is missing, take the first data set from the search path:
    if (missing(data)) {
        data = eval(parse(text = search()[2]), parent.frame())
    }
    
    if (is.numeric(data)) {
        data = data.frame(data)
        colnames(data) = all.vars(formula)[1]
        lhs = TRUE
    }
    
    # If we consider a faked formula:
    if (fake) {
        response = as.character(formula)[2]
        Call = as.character(match.call()[[2]][[3]])
        method = Call[1]
        predictors = Call[2]
        formula = as.formula(paste(response, "~", predictors))
    }
    
    # If only left hand side is required:
    if (lhs) {
        response = as.character(formula)[2]
        formula = as.formula(paste(response, "~", 1))
    } 
    
    # Create Model Data:
    x = model.frame(formula, data)
    
    # Convert:
    if (class(data) == "timeSeries") x = timeSeries(x)
    if (fake) attr(x, "control") <- method
    
    # Return value:
    x
    
}


################################################################################

