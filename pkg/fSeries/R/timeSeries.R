#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 DESCRIPTION:
#  timeSeries                Creates a 'timeSeries' object from scratch
#  .timeSeries
#  .signalSeries
################################################################################


timeSeries <-
    function (data, charvec, units = NULL, format = NULL, zone = myFinCenter,
              FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL,
              documentation = NULL, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Creates a 'timeSeries' object from scratch.

    # Arguments:
    #   data -a numeric 'matrix' object or any other object which
    #       can be transformed by the function as.matrix.
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

    # FUNCTION:

    # Convert data to data.frame:
    if (missing(data)) {
        data = matrix(runif(24), ncol = 2)
        if (is.null(units)) units = c("TS.1", "TS.2")
        charvec <- timeCalendar()
    } else {
        data <- as.matrix(data)
        stopifnot(is.numeric(data))
    }

    # Determine format
    if (missing(format)) {
        if (missing(charvec)) {
            format <- "unknown"
        } else {
            format <- whichFormat(charvec, silent = TRUE)
        }
        if (format == "unknown") {
            format <- whichFormat(rownames(data), silent = TRUE)
            charvec <- rownames(data)
        }
        if (format == "unknown" && !(is.numeric(data[,1]))) {
            format <- whichFormat(data[,1], silent = TRUE)
            charvec <- data[,1]
        }
    }

    # Construct Time Series:
    if (format == "counts" || format == "unknown") {
        # Signal Counts:
        charvec <- signalCounts(1:NROW(data))
        ans <- .signalSeries(data, charvec, units, title, documentation)
    } else {
        # Time Stamps:
        ans <- .timeSeries(data, charvec, units, format, zone, FinCenter,
                           recordIDs, title, documentation, ...)
    }

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

.timeSeries <-
    function (data, charvec, units = NULL, format = NULL, zone = myFinCenter,
              FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL,
              documentation = NULL, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a timeSeries with time stamps

    # Detail:
    #   Internal Function called by timeSeries

    # FUNCTION:

    # Default:
    if(missing(data) && missing(charvec)) {
        data = matrix(rnorm(24), 12)
        charvec = as.character(timeCalendar())
    }

    # This allows data to be a vector as input ...
    if (is.vector(data)) data = matrix(data)

    # Trace:
    if (FinCenter == "") FinCenter = "GMT"
    trace = FALSE

    # Missing charvec:
    if (missing(charvec)) {
        N = dim(as.matrix(data))[1]
        charvec = timeSequence(from = "1970-01-01", length.out = N,
        zone = "GMT", FinCenter = "GMT")
        warning("Dummy dates used in .timeSeries")
    }

    # charvector | Time Positions:
    if (is(charvec, "timeDate")) {
        timeDates = charvec
    } else {
        if (is.null(format)) format = whichFormat(charvec)
        timeDates = timeDate(charvec = charvec,
        format = format, zone = zone, FinCenter = FinCenter)
    }

    # Data | Dimension Names:
    if (is.timeSeries(data)) {
        recordIDs = data@recordIDs
        data = data@Data
        rownames(data) = c(as.character(timeDates))
        units = colnames(data)
    } else {
        data = as.matrix(data)
        rownames(data) = c(as.character(timeDates))
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
    # rownames(recordIDs) = c(as.character(charvec))

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
    attr(ans, "dimension") = c(NCOL(data), NROW(data))

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

.signalSeries <-
    function (data, charvec, units = NULL, title = NULL,
              documentation = NULL, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Creates a timeSeries with signal counts

    # Detail:
    #   Internal Function called by timeSeries

    # FUNCTION:

    # Signal Series:

    # Get data
    data <- if (missing(data)) matrix(runif(24), ncol = 2) else as.matrix(data)
    if (missing(charvec)) charvec <- signalCounts(1:NROW(data))

    # get positions
    nCol <- NCOL(data)
    positions = rownames(data) <- signalCounts(charvec)

    # get units
    if (is.null(units)) units <- colnames(data)
    if (is.null(units)) units <- paste("SS", 1:nCol, sep = ".")
    colnames(data) <- units

    # get title and description
    if(is.null(title)) title = "Signal Series"
    if(is.null(documentation)) documentation = description()

    # Return Value:
    new("timeSeries",
        Data = data,
        positions = positions,
        format = "counts",
        FinCenter = "",
        units = units,
        recordIDs = data.frame(),
        title = title,
        documentation = documentation)
}

################################################################################
