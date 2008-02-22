
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
# FUNCTION:                 DESCRIPTION:
#  timeNdayOnOrAfter         Computes date in month that is a n-day ON OR AFTER
#  timeNdayOnOrBefore        Computes date in month that is a n-day ON OR BEFORE
# OLD FUNCTIONS:            DESCRIPTION:
#  .on.or.after              Computes date in month that is a nday ON OR AFTER
#  .on.or.before             Computes date in month that is a nday ON OR BEFORE
#  .nth.of.nday              Computes nth ocurrance of a nday in year/month
#  .last.of.nday             Computes the last nday in year/month
# OLD FUNCTIONS:            DESCRIPTION:
#  .sdate                    Computes ISO-8601 dates from Julian day numbers
#  .sjulian                  Computes Julian day numbers from ISO-8601 dates
#  .sday.of.week             Computes day of the week for ISO-8601 dates 
#  .sleap.year               TRUE/FALSE if dates belong to leap years or not
################################################################################


timeNdayOnOrAfter <- 
    function(charvec, nday = 1, format = "%Y-%m-%d", zone = myFinCenter,
    FinCenter = myFinCenter)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes date in month that is a n-day ON OR AFTER

    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character
    #       vector.
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city".

    # Value:
    #   Returns the date in month that is a n-day ON OR AFTER as
    #   a 'timeDate' object.

    # Details:
    #   nday = 1 is a Monday

    # Example:
    #   What date has the first Monday on or after March 15, 1986?
    #   timeNdayOnOrAfter("1986-03-15", 1)

    # Changes:
    #

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"

    # timeDate:
    lt = strptime(charvec, format)

    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) + (nday-lt$wday)%%7)
    class(ct) = "POSIXct"

    # Return Value:
    Sys.setenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = zone,
        FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


timeNdayOnOrBefore <- 
    function(charvec, nday = 1, format = "%Y-%m-%d", zone = myFinCenter,
    FinCenter = myFinCenter)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes date in month that is a n-day ON OR BEFORE

    # Arguments:
    #   charvec - a character vector of dates and times.
    #   nday - an integer vector with entries ranging from
    #       0 (Sunday) to 6 (Saturday).
    #   format - the format specification of the input character
    #       vector.
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city".

    # Value:
    #   Returns the date in month that is a n-day ON OR BEFORE
    #   as a 'timeDate' object.

    # Example:
    #   What date has Friday on or before April 22, 1977?

    # Changes:
    #

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")
    if (FinCenter == "") FinCenter = "GMT"

    # timeDate:
    lt = strptime(charvec, format)

    # On or after:
    ct = 24*3600*(as.integer(julian.POSIXt(lt)) - (-(nday-lt$wday))%%7)
    class(ct) = "POSIXct"

    # Return Value:
    Sys.setenv(TZ = myTZ)
    timeDate(format(ct), format = format, zone = zone,
        FinCenter = FinCenter)
}


################################################################################ 
# FUNCTION:                 DESCRIPTION:
#  .on.or.after              Computes date in month that is a nday ON OR AFTER
#  .on.or.before             Computes date in month that is a nday ON OR BEFORE
#  .nth.of.nday              Computes nth ocurrance of a nday in year/month
#  .last.of.nday             Computes the last nday in year/month


.on.or.after = 
function(year, month, day, nday)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates date in month that is a nday ON OR AFTER 
    #   date(month,day,year)
    
    # Arguments:
    #   year, month, day - calendar atoms given as integers
    #       in the form CCYY, MM, DD.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    
    # Value:
    #   The date, an object of class '.sdate' formatted as integer.
    
    # Example: 
    #   What date has the first Monday on or after March 15, 1986?
    #   .on.or.after(1986, 3, 15, 1)
    
    # FUNCTION:
    
    # .sdate:
    ## "year*10000 + month*100 + day" +
    ##  (nday-.day.of.week(month, day, year))%%7
    .sdate = year*10000+month*100+day
    ans = .sdate(.sjulian(.sdate)+(nday-.day.of.week(month, day, year))%%7)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.on.or.before = 
function(year, month, day, nday)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates date in month that is a nday ON OR BEFORE 
    #   date(month,day,year)
    
    # Arguments:
    #   year, month, day - calendar atoms given as integers
    #       in the form CCYY, MM, DD.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    
    # Value:
    #   The date, an object of class '.sdate' formatted as integer.

    # Example: 
    #   What date has Friday on or before April 22, 1977?
    #   .on.or.before(1977, 4, 22, 5) 
    
    # FUNCTION: 
    
    # .sdate:
    ## "year*10000 + month*100 + day" -
    ##  (-(nday-.day.of.week(month,day,year)))%%7
    .sdate = year*10000+month*100+day
    ans = .sdate(.sjulian(.sdate)-(-(nday-.day.of.week(month,day,year)))%%7)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.nth.of.nday = 
function(year, month, nday, nth)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the "nth" ocurrance of a "nday" (nth = 1, ..., 5) 
    #   in "year,month"
    
    # Arguments:
    #   year, month - calendar atoms given as integers
    #       in the form CCYY, MM.
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    #   nth - an inter numbering the "n-th" ocurrance of a "nday"
    
    # Value:
    #   The date, an object of class '.sdate' formatted as integer.
 
    # Example: 
    #   What date is the second Sunday in October 1980?
    #   .nth.of.nday(1980, 10, 0, 2)
    
    # FUNCTION: 
    
    # .sdate:
    ## "year*10000 + month*100" + 7*nth - 6 +
    ##  (nday-.day.of.week(year,month,7*nth-6))%%7
    .sdate = year*10000+month*100+1
    ans = .sdate(.sjulian(.sdate)+(nth-1)*7+(nday-.day.of.week(month,1,year))%%7) 
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


.last.of.nday = 
function(year, month, lastday, nday)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates the last "nday" in "year/month"
    
    # Arguments:
    #   year, month - calendar atoms given as integers
    #       in the form CCYY, MM.
    #   lastday - an integer which is the last calendar day for
    #       a given "month" and "year".
    #   nday - an integer vector with entries ranging from 
    #       0 (Sunday) to 6 (Saturday).
    
    # Value:
    #   The date, an object of class '.sdate' formatted as integer.
    
    # Example: 
    #   What date has the last Monday in May, 1996?
    #   .last.of.nday(1996, 5, 31, 1)
    
    # FUNCTION:
    
    # .sdate:
    ## "year*10000 + month*100 + lastday" -
    ##  (.day.of.week(year,month,lastday)-nday)%%7
    .sdate = year*10000 + month*100 + lastday
    ans = .sdate(.sjulian(.sdate)-(-(nday-.day.of.week(month,lastday,year)))%%7)
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION:           DESCRIPTION:
#  .sdate              Computes ISO-8601 dates from Julian day numbers
#  .sjulian            Computes Julian day numbers from ISO-8601 dates
#  .sday.of.week       Computes day of the week for ISO-8601 dates 
#  .sleap.year         Returns TRUE/FALSE if dates belong to leap years or not


.sdate = 
function (julians, origin = 19600101)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates Gregorian dates from Julian day numbers
    
    # Arguments:
    #   julians - an integer variable or vector of Julian day 
    #       counts.
    #   origin - the origin of the Julian day counter, formatted
    #       in ISO-8601 date format CCYYMMDD.
    
    # Value:
    #   Returns a vector of dates formatted as ".sdates", i.e.
    #   CCYYMMDD integer values.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Julian Day Numbers to ISO-8601 Gregorian Dates:
    year0 = origin%/%10000
    month0 = (origin-10000*year0)%/%100
    day0 = origin-10000*year0-100*month0
    
    # Month - Day - Year Function:
    mdylist = .month.day.year(julians, origin = c(month0, day0, year0))
 
    # In '.sdate' Format:
    ans = mdylist$year*10000 + mdylist$month*100 + mdylist$day
    
    # Return Value:
    class(ans) = ".sdate"
    ans
} 


# ------------------------------------------------------------------------------


.month.day.year = 
function(jul, origin = c(1, 1, 1960)) 
{
    # shift = .julian(1, 1, 1960, 0)    
    shift = 2436935
    j = jul + shift
    j = j - 1721119
    y = (4 * j - 1) %/% 146097
    j = 4 * j - 1 - 146097 * y
    d = j %/% 4
    j = (4 * d + 3) %/% 1461
    d = 4 * d + 3 - 1461 * j
    d = (d + 4) %/% 4
    m = (5 * d - 3) %/% 153
    d = 5 * d - 3 - 153 * m
    d = (d + 5) %/% 5
    y = 100 * y + j
    y = y + ifelse(m < 10, 0, 1)
    m = m + ifelse(m < 10, 3, -9)
    return(list(month = m, day = d, year = y)) 
}
    

# ------------------------------------------------------------------------------


.sjulian = 
function (.sdates, origin = 19600101)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates Julian day numbers from Gregorian ISO-8601
    #   formatted dates, CCYYMMDD
    
    # Arguments:
    #   .sdates - an integer variable or vector of dates, formatted
    #       in ISO-8601 date format CCYYMMDD.
    #   origin - the origin of the Julian day counter, formatted
    #       in ISO-8601 date format CCYYMMDD.
    
    # Value:
    #   Returns Julian time as days since some origin.  
        
    # Changes:
    #
    
    # FUNCTION:
    
    # Convert:
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
    
    # Internal Function:
    .julian = function(m, d, y, origin = c(month = 1, day = 1, year = 1960)) {  
        only.origin = all(missing(m), missing(d), missing(y))
        if (only.origin) m = d = y = NULL   
        nms = names(d)
        max.len = max(length(m), length(d), length(y))  
        m = c(origin[1], rep(m, length = max.len))
        d = c(origin[2], rep(d, length = max.len))
        y = c(origin[3], rep(y, length = max.len))  
        y = y + ifelse(m > 2, 0, -1)
        m = m + ifelse(m > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        out = (146097 * c) %/% 4 + (1461 * ya) %/% 4 + 
            (153 * m + 2) %/% 5 + d + 1721119   
        if (!only.origin) {
            if(all(origin == 0)) out = out[-1] else out = out[-1] - out[1] }    
        names(out) = nms
        out }

    # ISO-8601 GREGORIAN DATES TO JULIAN DAY NUMBERS:
    year = .sdates%/%10000
    month = (.sdates-10000*year)%/%100
    day = .sdates-10000*year-100*month
    
    # ISO-8601 ORIGIN:
    year0 = origin%/%10000
    month0 = (origin-10000*year0)%/%100
    day0 = origin-10000*year0-100*month0
    
    # Julian:
    ans = .julian(month, day, year, origin = c(month0, day0, year0))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.sday.of.week = 
function(.sdates)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates the day of week from an ISO-8601 formatted date
    
    # Arguments:
    #   .sdates - an integer variable or vector of dates, formatted
    #       in ISO-8601 date format CCYYMMDD.
    
    # Value:
    #   Returns a number between 0 and 6 to specify the day of
    #   the week-0 refers to Sunday.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Convert:
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
        
    # Year - Month - Day:
    # Sunday 0, Monday 1, ..., Saturday 6
    year = .sdates%/%10000
    month = .sdates%/%100 - year*100
    day = .sdates - year*10000 - month*100
    a = (14-month)%/%12
    y = year - a
    m = month + 12*a - 2
    
    # Day of Week:
    ans = (day + y + y%/%4 - y%/%100 + y%/%400 + (31*m)%/%12)%%7
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.day.of.week = 
function (month, day, year) 
{   # A function implemented by Diethelm Wuertz

    ans = .sday.of.week(year * 10000 + month * 100 + day)
    ans
}


# ------------------------------------------------------------------------------


.sleap.year = 
function(.sdates)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates if a year is a leap year or not
    #   takes the value T(rue) for leap year, otherwise F(alse)
    
    # Arguments:
    #   .sdates - an integer variable or vector of dates, formatted
    #       in ISO-8601 date format CCYYMMDD.
    
    # Value:
    #   Returns a logical vector indicating whether the corresponding 
    #   year is a leap year or not.
    
    # Changes:
    #
    
    # FUNCTION:
      
    # Convert:
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
      
    # Year:
    year = .sdates%/%10000
    
    # Leap Years
    ans = year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
    
    # Return Value:
    ans
}


################################################################################

