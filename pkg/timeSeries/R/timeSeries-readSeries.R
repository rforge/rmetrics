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
function(file, header = TRUE, sep = ";", zone = "", FinCenter = "", ...)
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
    df <- read.table(file = file, header = header, sep = sep,
        check.names = FALSE, colClasses = "character", ...)

    # YC : colClass "character" important otherwise if first column
    # has %Y%m%d%H%M%S format it would be converted to a numeric in
    # the form of 1.XXXXXe+13 ... not good

    # get timeDate from first column with header specifying the format
    charvec <- as.character(df[[1]])
    format <- names(df)[1]
    td <- timeDate(charvec = charvec, format = format, zone = zone,
        FinCenter = FinCenter)

    # Create Time Series from Data Frame:
    data <- sapply(df[-1], as.numeric)
    ans <- timeSeries(data = data, charvec = charvec)

    # Return Value:
    ans
}


################################################################################

