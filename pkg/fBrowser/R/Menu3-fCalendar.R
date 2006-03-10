
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
# FUNCTION:
#  .fCalendar.PopupMenu
################################################################################


.fCalendar.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fCalendarMenu <<- newToolbarMenu()
     
    
    # Add Menu:
    Label = "Time Date Class"
    subLabel = c(
        "* Example timeDate: x = 12 Random Dates in 2005",
        "* Example timeDate: x = 12 Random Times in 2005",
        "Print timeDate Class Representation",
        "Create timeDate Sequence",
        "Create timeDate Calendar",
        "   The Current Date and Time", 
        "   The Last Day in Month for timeDates",
        "   The N-Day On-Or-After timeDates",
        "   The N-Day On-Or-Before timeDates",
        "   The n-th ocurrance of a N-day for timeDates",
        "   The last N-day in each Month for timeDates",       
        "Print List of Financial Centers",
        "Print DST Rules for a Financial Center",    
        "Print Summary of a timeDate Object")
    Command = c(
        ".fCalendar.TimeDateClass.dates",
        ".fCalendar.TimeDateClass.times",
        ".fCalendar.TimeDateClass.4",
        ".fCalendar.TimeDateClass.5",
        ".fCalendar.TimeDateClass.6",
        ".fCalendar.TimeDateClass.7",
        ".fCalendar.TimeDateClass.8",
        ".fCalendar.TimeDateClass.9",    
        ".fCalendar.TimeDateClass.10",
        ".fCalendar.TimeDateClass.11",
        ".fCalendar.TimeDateClass.12",
        ".fCalendar.TimeDateClass.13",
        ".fCalendar.TimeDateClass.14",
        ".fCalendar.TimeDateClass.15")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Time Date Methods"
    subLabel = c(
        "* Example timeDate: x = 12 Random Dates in 2005",
        "* Example timeDate: x = 12 Random Times in 2005",
        "Print timeDate Class Representation",
        "Extract the first object of a timeDate Vector",
        "Extract the last object of a timeDate Vector",
        "+/- a time Span to/from a timeDate Vector",
        "Sort a timeDate Vector by Time and Date",
        "Round a timeDate Vector to a Given Unit",
        "Truncate a timeDate Vector to a Given Unit",
        "Revert a timeDate Vector",
        "Convert timeDate to a character Vector",
        "Convert timeDate to a data frame",
        "Convert timeDate to a POSIXct Object",
        "Extract Julian Day Counts from timeDate Vector",
        "Extract data.frame from timeDate Atoms")
    Command = c(
        ".fCalendar.TimeDateMethods.dates",
        ".fCalendar.TimeDateMethods.times",
        ".fCalendar.TimeDateMethods.4",
        ".fCalendar.TimeDateMethods.5",
        ".fCalendar.TimeDateMethods.6",
        ".fCalendar.TimeDateMethods.7",
        ".fCalendar.TimeDateMethods.8",
        ".fCalendar.TimeDateMethods.9",
        ".fCalendar.TimeDateMethods.10",
        ".fCalendar.TimeDateMethods.11",
        ".fCalendar.TimeDateMethods.12",
        ".fCalendar.TimeDateMethods.13",
        ".fCalendar.TimeDateMethods.14",
        ".fCalendar.TimeDateMethods.15",
        ".fCalendar.TimeDateMethods.16")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Time Series Class"     
    subLabel = c(
        "* Example timeSeries: x = MSFT|SP500 Returns",
        "Print timeSeries Class Representation",
        "is.timeSeries ?",
        "NYI - Apply a Function to Series",
        "Align a Daily Series",
        "Cut Out a Piece from a Series",
        "Difference a Series",
        "Return a Lagged Series",
        "Merge a Series with a Matrix Object",
        "Get Returns from a Series",
        "Revert a Series in Time")
    Command = c(
        ".fCalendar.TimeSeriesClass.1",
        ".fCalendar.TimeSeriesClass.2",
        ".fCalendar.TimeSeriesClass.is",
        ".fCalendar.TimeSeriesClass.apply",
        ".fCalendar.TimeSeriesClass.align",
        ".fCalendar.TimeSeriesClass.cut",
        ".fCalendar.TimeSeriesClass.diff",
        ".fCalendar.TimeSeriesClass.lag",
        ".fCalendar.TimeSeriesClass.merge",
        ".fCalendar.TimeSeriesClass.returns",
        ".fCalendar.TimeSeriesClass.rev")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)    
    
    # Add Menu:
    Label = "Holiday Calendars"
    subLabel = c(
        "Print Date of Easter & Related Dates",
        "Print Supported Holidays",
        "Return Holiday Date",
        "NYSE Holiday Calendar")
    Command = c(
        ".fCalendar.HolidayCalendars.1",
        ".fCalendar.HolidayCalendars.2",
        ".fCalendar.HolidayCalendars.3",
        ".fCalendar.HolidayCalendars.4")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "High Frequency Data"
    subLabel = c(
        "* Example data.frame: x = FX Reuters USDTHB",
        "FX Parser for Reuters Data",
        "FX Filter for Reuters Data",
        "FX Format by Variables Minutes",
        "* Example data.frame - FX Reuters AUDUSD",
        "Extract Prices from Reuters FX Data",    
        "Compute Log Prices",
        "Compute Log Returns",
        "Cut Series - Prices|logPrices|logRet",
        "Interpolate - Prices|logPrices|logRet",
        "Convert to timeSeries Object",
        "* Example timeSeries: x = FDAX Index Oct 1997",  
        "De-seasonalize in Upsilon Time",
        "De-volatilize Time Series",
        "Plot Daily/Weekly Charts"  )
    Command = c(
        ".fCalendar.HighFrequencyData.1",
        ".fCalendar.HighFrequencyData.2",
        ".fCalendar.HighFrequencyData.3",
        ".fCalendar.HighFrequencyData.4",
        ".fCalendar.HighFrequencyData.5",
        ".fCalendar.HighFrequencyData.6",
        ".fCalendar.HighFrequencyData.7",
        ".fCalendar.HighFrequencyData.8",
        ".fCalendar.HighFrequencyData.9",
        ".fCalendar.HighFrequencyData.10",
        ".fCalendar.HighFrequencyData.11",
        ".fCalendar.HighFrequencyData.12",
        ".fCalendar.HighFrequencyData.13",
        ".fCalendar.HighFrequencyData.14",
        ".fCalendar.HighFrequencyData.15")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
  
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fCalendarMenu, Label = "fCalendar")    
}


################################################################################

