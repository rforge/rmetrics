
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
# FUNCTION:                 FOR MONTHLY OPERATIONS:
#  rollMonthlyWindows        Returns start/end dates for rolling time windows
#  rollMonthlySeries         Rolls Monthly a 'timeSeries' on a given period 
################################################################################


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

