
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
# FUNCTION:                 FINANCIAL TIME SERIES:
#  cumulatedSeries           Computes cumulated returns from a 'timeSeries'  
#  durationSeries            Computes durations from a 'timeSeries' object
################################################################################


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


################################################################################

