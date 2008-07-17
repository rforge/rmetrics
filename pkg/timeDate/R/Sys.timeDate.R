
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  Sys.timeDate              Returns system time as an 'timeDate' object
################################################################################


Sys.timeDate <-
    function(FinCenter = myFinCenter)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns system time as an object of class 'timeDate'

    # Arguments:
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city"

    # Value:
    #   Returns the system time as an object of class 'timeDate'.

    if (FinCenter == "") FinCenter = "GMT"

    # only time at "GMT" is reliable on most systems
    # charvec <- format(Sys.time(), tz = "GMT", usetz = FALSE)

    # System Time:
    time <- Sys.time()
    attr(time, "tzone") <- "GMT"

    # Return
    timeDate(as.character(time), zone = "GMT", FinCenter = FinCenter)
}


################################################################################

