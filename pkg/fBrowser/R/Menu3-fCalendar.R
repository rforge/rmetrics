
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
#  fCalendar PopupMenu


.fCalendar.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fCalendarMenu <<- newToolbarMenu()
    
    # Add Menu:
    Label = "Create a Time Date Object"
    subLabel = c(
        "* Example timeDate: x = 12 Random Dates in 2005",
        "* Example timeDate: x = 12 Random Times in 2005",
        "__________________________________________",
        "Create timeDate Sequence",
        "Create timeDate Calendar",
        "__________________________________________",
        "The Current Date and Time", 
        "The First/Last Day in Month/Quarter",
        "The N-Day On-Or-After timeDates",
        "The N-Day On-Or-Before timeDates",
        "The n-th ocurrance of a N-day for timeDates",
        "The last N-day in each Month for timeDates",  
        "__________________________________________",     
        "Print List of Financial Centers",
        "Print DST Rules for a Financial Center")
    Command = c(
        ".fData.randomDates",
        ".fData.randomTimes",
        "tkSeparator",
        ".fCalendar.TimeDateClass.timeSequence",
        ".fCalendar.TimeDateClass.timeCalendar",
        "tkSeparator",
        ".fCalendar.TimeDateClass.getTime",
        ".fCalendar.TimeDateClass.timeDayInPeriod",
        ".fCalendar.TimeDateClass.timeNdayOnOrAfter",    
        ".fCalendar.TimeDateClass.timeNdayOnOrBefore",
        ".fCalendar.TimeDateClass.timeNthNdayInMonth",
        ".fCalendar.TimeDateClass.timeLastNdayInMonth",
        "tkSeparator",
        ".fCalendar.TimeDateClass.getFinCenters",
        ".fCalendar.TimeDateClass.FinCenter")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Manipulate a timeDate Object"
    subLabel = c(
        "* Example timeDate: x = 12 Random Dates in 2005",
        "* Example timeDate: x = 12 Random Times in 2005",
        "__________________________________________",
        "Extract the first object of a timeDate Vector",
        "Extract the last object of a timeDate Vector",
        "__________________________________________",
        "+/- a time Span to/from a timeDate Vector",
        "Sort a timeDate Vector by Time and Date",
        "Round a timeDate Vector to a Given Unit",
        "Truncate a timeDate Vector to a Given Unit",
        "Revert a timeDate Vector",
        "__________________________________________",
        "Convert timeDate to a character Vector",
        "Convert timeDate to a data frame",
        "Convert timeDate to a POSIXct Object",
        "__________________________________________",
        "Extract Julian Day Counts from timeDate Vector",
        "Extract data.frame from timeDate Atoms")
    Command = c(
        ".fData.randomDates",
        ".fData.randomTimes",
        "tkSeparator",
        ".fCalendar.TimeDateMethods.start",
        ".fCalendar.TimeDateMethods.end",
        "tkSeparator",
        ".fCalendar.TimeDateMethods.plusminus",
        ".fCalendar.TimeDateMethods.sort",
        ".fCalendar.TimeDateMethods.round",
        ".fCalendar.TimeDateMethods.trunc",
        ".fCalendar.TimeDateMethods.rev",
        "tkSeparator",
        ".fCalendar.TimeDateMethods.asCharacter",
        ".fCalendar.TimeDateMethods.asDataFrame",
        ".fCalendar.TimeDateMethods.asPOSIXct",
        "tkSeparator",
        ".fCalendar.TimeDateMethods.julian",
        ".fCalendar.TimeDateMethods.atoms")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Compose and Modify a Time Series"     
    subLabel = c(
        "* Create a timSeries Object",
        "* Demo tS: x = IBM|SP500 Daily Returns",
        "__________________________________________",
        "Cut Out a Piece from a timeSeries",
        "Compute Returns from a timeSeries",
        "Merge a timeSeries with a matrix Object",
        "Align a Daily timeSeries",
        "Edit a timeSeries Object",
        "__________________________________________",
        "Difference a timeSeries",
        "Lag a timeSeries",
        "Revert a timeSeries in Time Order",
        "Apply any Function to a timSeries",)
    Command = c(
        ".fCalendar.TimeSeriesClass.timeSeries",
        ".fData.ibmsp500Daily",
        "tkSeparator",
        ".fCalendar.TimeSeriesClass.cutSeries",
        ".fCalendar.TimeSeriesClass.returnSeries",
        ".fCalendar.TimeSeriesClass.mergeSeries",
        ".fCalendar.TimeSeriesClass.alignDailySeries",  
        ".fCalendar.TimeSeriesClass.alignDailySeries",  
        "tkSeparator",  
        ".fCalendar.TimeSeriesClass.diffSeries",
        ".fCalendar.TimeSeriesClass.lagSeries",
        ".fCalendar.TimeSeriesClass.revSeries",
        ".fCalendar.TimeSeriesClass.applySeries")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)    
    
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Holiday Calendars"
    subLabel = c(
        "Print Date of Easter & Related Dates",
        "Print Supported Holidays",
        "Return Holiday Date",
        "NYSE Holiday Calendar")
    Command = c(
        ".fCalendar.Holidays.easterDate",
        ".fCalendar.Holidays.holidayList",
        ".fCalendar.Holidays.holidayDate",
        ".fCalendar.Holidays.nyseHolidays")
    addToolbarMenu(fCalendarMenu, Label, subLabel, Command)
    
    # Add Menu:
    if (FALSE) { # -- OLD DON'T USE NOW !!
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
        "* Demo tS: x = FDAX Index Oct 1997",  
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
    }

    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fCalendarMenu, Label = "fCalendar")    
}


################################################################################

