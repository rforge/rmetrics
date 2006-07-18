
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
# FUNCTION:              DESCRIPTION
#  isISO8601              Checks if the date/time is ISO8601 formatted
################################################################################


################################################################################
# FUNCTION:              DESCRIPTION
#  isISO8601              Checks if the date/time is ISO8601 formatted


isISO8601 =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks if the date/time string is ISO8601 formatted
    
    # Example:
    #   isISO8601(c("2007-01-01", "2007-12-31" ))
    #   isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    #   isISO8601("2007-Jan-01")[[1]]
    #   isISO8601("2007-01-01 15:00:000")[[1]]
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Check:
    stopifnot(class(x) == "character")
    
    # Test: 
    options(warn = -1)
    format = .whichFormat(x)
    ans = FALSE
    if (format == "%Y-%m-%d %H:%M:%S") ans = TRUE
    if (format == "%Y-%m-%d") ans = TRUE
    if (format == "%Y%m%d%H%M%S") ans = TRUE
    if (format == "%Y%m%d") ans = TRUE
    attr(ans, "control")<- format
    
    # Return Value:
    ans
}


################################################################################

