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
#  readSeries                Reads a spreadsheet and creates a 'timeSeries'
################################################################################

readSeries <-
    function(file, header = TRUE, sep = ";", zone = myFinCenter,
    FinCenter = myFinCenter, title = NULL, documentation = NULL, ...)
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
    df = read.table(file = file, header = header, sep = sep, ...)

    # Create Time Series from Data Frame:
    ans = as.timeSeries(df, zone = zone, FinCenter = FinCenter))

    # Add title and Documentation:
    if (is.null(title)) ans@title = "Time Series Object"
    if (is.null(documentation)) ans@documentation = description()

    # Return Value:
    ans
}

