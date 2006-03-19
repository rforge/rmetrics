
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
# Load Data


.fData.nyseDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "nyseDaily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "nyseDaily" )       
}


# ------------------------------------------------------------------------------


.fData.dem2gbpDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "dem2gbp", report = report, 
        FUN = function(x) as.ts(as.vector(t(x))) ) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "dem2gbp" )       
}


# ------------------------------------------------------------------------------


.fData.bmwDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "bmwDaily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "bmwDaily" )       
}


# ------------------------------------------------------------------------------


.fData.ibmsp500Daily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "ibmsp500Daily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "ibmsp500Daily" )       
}


# ------------------------------------------------------------------------------


.fData.sp500Monthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "sp500Monthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "sp500Monthly" )       
}


# ------------------------------------------------------------------------------


.fData.spcDaily = 
function()
{   # A function implemented by Diethelm Wuertz

    # SPC 500 Open-High-Low-Close:
    myFunction = function(object2x, report) {
        data(spc1970)
        object <<- as.timeSeries(spc1970[-(1:6319), ])
        if (report) tkTitle(paste(" SPC 500 Daily OHLC Demo Data Set"))
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "spc1970" )       
}


# ------------------------------------------------------------------------------


.fData.sp500IndexMonthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "sp500IndexMonthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "sp500IndexMonthly" )       
}


# ------------------------------------------------------------------------------


.fData.msftsp500Monthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- .tkGetDemoData(Data = "msftsp500Monthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = TRUE,
            report = FALSE),
        subject = "msftsp500Monthly" )       
}


# ------------------------------------------------------------------------------


.fData.randomDates = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Dates in Current Year
    myFunction = function(FinCenter, object2x, report) {
        y = rep(currentYear, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = FinCenter)
        if (report) tkTitle("12 Random Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        subject = "12 Random Dates" )          
}


# ------------------------------------------------------------------------------


.fData.randomTimes = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Times in Current Year 
    myFunction = function(FinCenter, object2x, report) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = FinCenter)
        print(report)
        if (report) tkTitle("12 Random Times")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE ),
        subject = "12 Random Times" )          
}


# ##############################################################################


.fCalendar.HighFrequencyData.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters USDTHB:
    data(usdthb)
    x <<- tkSaveAsX(data = usdthb, subject = "FX Reuters USDTHB")
}


################################################################################

