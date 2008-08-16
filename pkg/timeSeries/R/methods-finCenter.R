
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

setMethod("finCenter", "timeSeries",
    function(x)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Description:
    #

    # Example:
    #
    #

    # FUNCTION:

    stopifnot(is(x, "timeSeries"))

    ans <-  x@FinCenter

    ans
})


setMethod("finCenter<-", "timeSeries",
    function(x, value)
{
    # A function implemented by  Yohan Chalabi and Diethelm Wuertz

    # Description:
    #

    # Example:
    #
    #

    # FUNCTION:
    stopifnot(is(x, "timeSeries"))

    if (x@format == "counts")
        stop(as.character(match.call())[1],
             " is for time series and not for signal series.")

    # convert to user financial centre
    positions <- timeDate(charvec = time(x), zone = finCenter(x),
                          FinCenter = value)

    time(x) <- positions

    # Return Value:
    x
})

