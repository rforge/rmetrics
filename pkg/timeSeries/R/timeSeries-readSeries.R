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
#  readSeries                Reads a CSV file and creates a 'timeSeries'
################################################################################


readSeries <-
function(file, header = TRUE, sep = ";", zone = "", FinCenter = "",
         format, ...)
{
    # A function implemented by Diethelm Wuertz

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
    #   format - the format of the timestamps as recoreded in the
    #       first column of the data in the..
    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # Notes:
    #   Note we expect that the header of the spreadsheet file in
    #   the first cell holds the time/date format specification!

    # FUNCTION:

    # Read Data:
    df <- read.table(file = file, header = header, sep = sep,
        check.names = FALSE, colClasses = c("character", "numeric"), ...)

    # get timeDate from first column with header specifying the format
    charvec <- df[[1]]
    if (missing(format)) format <- names(df)[1]
    td <- timeDate(charvec = charvec, format = format, zone = zone,
        FinCenter = FinCenter)

    # if format provided in file or with format argument, try to guess it
    if (all(is.na(td)))
        warning("Conversion of timestamps to timeDate objects produced only NAs.
  Are you sure you provided the proper format with argument 'format'
  or in the header of your file ?")

    # extract data
    data <- df[-1]

    # Create Time Series from Data Frame:
    ans <- timeSeries(data = data, charvec = td)

    # Return Value:
    ans
}


################################################################################
