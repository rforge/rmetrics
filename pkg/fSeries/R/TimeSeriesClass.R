
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
# FUNCTION:                 GENERATION OF TIME SERIES OBJECTS:
#  timeSeries                Creates a 'timeSeries' object from scratch
#  readSeries                Reads a spreadsheet and creates a 'timeSeries'
#  returnSeries              Computes returns from a 'timeSeries' object
#   getReturns           
#  cumulatedSeries           Computes cumulated returns from a 'timeSeries'  
#  durationSeries            Computes durations from a 'timeSeries' object
#  midquoteSeries            Computes mid quotes from a 'timeSeries' object
#  spreadSeries              Computes spreads from a 'timeSeries' object
#  applySeries               Applies a function to blocks of a 'timeSeries'
#  orderStatistics           Compute order statistic of a 'timeSeries'
# FUNCTION:                 DATA SLOT AND CLASSIFICATION:
#  seriesData                Extracts data slot from 'timeSeries' object
#  isUnivariate              Tests if 'timeSeries' object is univariate
#  isMultivariate            Tests if 'timeSeries' object is multivariate  
# FUNCTION:                 FOR MONTHLY OPERATIONS:
#  rollMonthlyWindows        Returns start/end dates for rolling time windows
#  rollMonthlySeries         Rolls Monthly a 'timeSeries' on a given period 
# FUNCTION:                 DESCRIPTION:
#  .modelSeries              Models a timeSeries object to use formulas
################################################################################


require(methods)


################################################################################
# FUNCTION:            GENERATION OF TIME SERIES OBJECTS:
#  timeSeries           Creates a 'timeSeries' object from scratch
#  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
#  returnSeries         Computes returns from a 'timetimehhhhh' object  
#   getReturns
#  applySeries          Applies a function to blocks of a 'timeSeries'
#  orderStatistics      Compute order statistic of a 'timeSeries'


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
    if (is(charvec, "timeDate")) { 
        timeDates = charvec 
    } else {   
        if (is.null(format)) format = .whichFormat(charvec)    
        timeDates = timeDate(charvec = charvec, 
            format = format, zone = zone, FinCenter = FinCenter) 
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
    new("timeSeries", 
        Data = as.matrix(data), 
        positions = rownames(data),
        format = timeDates@format,
        FinCenter = timeDates@FinCenter,  
        units = as.character(units), 
        recordIDs = recordIDs,
        title = as.character(title), 
        documentation = as.character(documentation) 
    )
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


getReturns = 
function(x, type = c("continuous", "discrete"), percentage = FALSE, 
trim = FALSE, digits = 12, units = NULL)
{   # A function implemented by Diethelm Wuertz

    # Return Series:
    ans = returnSeries(x, type, percentage, trim, digits, units)
        
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


cumulatedSeries = 
function(x, index = 100) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes durations from a financial price series
    
    # Arguments:    
    #   x - a univariate or multivariate 'timeSeries' object.
    #   index - a numeric value to which the cumulated return 
    #       series will be indexed.
    
    # FUNCTION:
    
    # Cumulated Series:
    ans = index * exp(colCumsums(x))
    
    # Return Values:
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
    stopifnot(is.timeSeries(x),
              is(from, "timeDate") || is.null(from),
              is(to,   "timeDate") || is.null(to))
    
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

    # FUNCTION:
    
    # Test:
    ans = !isUnivariate(x)
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION:            FOR MONTHLY OPERATIONS:
#  rollMonthlyWindows   Returns start and end dates for rolling time windows
#  rollMonthlySeries    Rolls monthly a 'timeSeries' on a given period


rollMonthlyWindows = 
function(x, period = "12m", by = "1m")
{   # A function implemented by Rmetrics

    # Description:
    #   Returns start and end dates for rolling time windows
    
    # Arguments:
    #   x - a 'timeSerie's object of asset returns
    #   period - a character string denoting the length of the rolling
    #       window, e.g. "24m" means 24 months
    #   by - a character string denoting the shift of the rolling window,
    #       e.g. "3m" means one quarter
    
    # FUNCTION:
    
    # Get Window Parameter:
    periodLength = as.numeric(substr(period, 1, nchar(period)-1))
    periodUnit = substr(period, nchar(period), nchar(period))
    byLength = as.numeric(substr(by, 1, nchar(by)-1))
    byUnit = substr(by, nchar(by), nchar(by))
    stopifnot(periodUnit == "m")
    stopifnot(byUnit == "m")
    
    # Get Window Parameter:
    periodLength = as.numeric(substr(period, 1, nchar(period)-1))
    periodUnit = substr(period, nchar(period), nchar(period))
    byLength = as.numeric(substr(by, 1, nchar(by)-1))
    byUnit = substr(by, nchar(by), nchar(by))
    stopifnot(periodUnit == "m")
    stopifnot(byUnit == "m")
    
    # Make Windows - We expect monthly data records ...
    positions = seriesPositions(x)
    Positions = unique(timeFirstDayInMonth(positions))
    numberOfPositions = length(Positions)
    startDates = Positions[1:(numberOfPositions-periodLength)]
    endDates = Positions[(periodLength+1):numberOfPositions]-24*3600
    
    # Windows:
    windows = list(from = startDates, to = endDates)
    attr(windows, "control") = c(start = start(positions), end = end(positions))
    
    # Return Value:
    windows
}


# ------------------------------------------------------------------------------


rollMonthlySeries =
function(x, period = "12m", by = "1m", FUN, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Rolls monthly a 'timeSeries' on a given period 
    
    # Arguments:
    #   x - a 'timeSerie's object of asset returns
    #   period - a character string denoting the length of the rolling
    #       window, e.g. "24m" means 24 months
    #   by - a character string denoting the shift of the rolling window,
    #       e.g. "3m" means one quarter
    #   FUN - function to be applied
    
    # FUNCTION:
    
    # Settings:
    windows = rollMonthlyWindows(x = x[, 1], period = period, by = by)
    
    # Apply Function:
    ans = applySeries(x = x, from = windows$from, to = windows$to, 
        FUN = FUN, ...)
    
    # Return Value:
    ans
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
    if (is(data, "timeSeries")) x = timeSeries(x)
    if (fake) attr(x, "control") <- method
    
    # Return value:
    x
    
}


################################################################################

