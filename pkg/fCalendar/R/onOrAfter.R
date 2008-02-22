
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
    timeDate(format(ct), format = format, zone = zone, FinCenter = FinCenter)
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
# Internal Functions


## DW 
##  These are relicts from very old times
##  We should check where these function are needed and if we should
##  replace them with 'timeDate' objects ...


.on.or.after <- function(year, month, day, nday) {      
    .sdate = year*10000+month*100+day
    .sdate(.sjulian(.sdate)+(nday-.day.of.week(month, day, year))%%7) }

.on.or.before <- function(year, month, day, nday) {   
    .sdate = year*10000+month*100+day
    .sdate(.sjulian(.sdate)-(-(nday-.day.of.week(month,day,year)))%%7) }

.nth.of.nday <- function(year, month, nday, nth) {   
    .sdate = year*10000+month*100+1
    .sdate(.sjulian(.sdate)+(nth-1)*7+(nday-.day.of.week(month,1,year))%%7)}

.last.of.nday <- function(year, month, lastday, nday) {   
    .sdate = year*10000 + month*100 + lastday
    .sdate(.sjulian(.sdate)-(-(nday-.day.of.week(month,lastday,year)))%%7) }

.sdate <- function (julians, origin = 19600101) {   
    year0 = origin%/%10000
    month0 = (origin-10000*year0)%/%100
    day0 = origin-10000*year0-100*month0
    mdylist = .month.day.year(julians, origin = c(month0, day0, year0))
    ans = mdylist$year*10000 + mdylist$month*100 + mdylist$day
    class(ans) = ".sdate"
    ans } 

.month.day.year <- function(jul, origin = c(1, 1, 1960)) {
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
    return(list(month = m, day = d, year = y)) }
    
.sjulian <- function (.sdates, origin = 19600101) {   
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
    year = .sdates%/%10000
    month = (.sdates-10000*year)%/%100
    day = .sdates-10000*year-100*month
    year0 = origin%/%10000
    month0 = (origin-10000*year0)%/%100
    day0 = origin-10000*year0-100*month0
    .JULIAN(month, day, year, origin = c(month0, day0, year0)) }
    
.JULIAN = function(m, d, y, origin = c(month = 1, day = 1, year = 1960)) {  
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

.sday.of.week <- function(.sdates) {   
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
    year = .sdates%/%10000
    month = .sdates%/%100 - year*100
    day = .sdates - year*10000 - month*100
    a = (14-month)%/%12
    y = year - a
    m = month + 12*a - 2
    (day + y + y%/%4 - y%/%100 + y%/%400 + (31*m)%/%12)%%7 }

.day.of.week <- function (month, day, year) {  
    .sday.of.week(year * 10000 + month * 100 + day) }

.sleap.year <- function(.sdates) {  
    if (class(.sdates) == ".sdate") .sdates = as.vector(.sdates)
    year = .sdates%/%10000
    year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0) }


################################################################################

