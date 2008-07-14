
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
# FUNCTION:           HOLIDAY CALENDAR FUNCTIONS:
#  holiday             Returns a holiday date of G7 and CH
#  holidayNYSE         Returns 'timeDate' object for full-day NYSE holidays
#  holidayZURICH       Returns 'timeDate' object for ZURICH holidays
################################################################################


test.holiday =
function()
{
    # easter -
    # easter(year = currentYear, shift = 0)
    # Dates for Easter and Good Friday from 2000 until 2010:
    .easter()
    Easter = .easter(2000:2010)
    Easter
    checkTrue(inherits(Easter, "timeDate"))

    GoodFriday = .easter(2000:2010, -2)
    GoodFriday
    checkIdentical(
        target = Easter,
        current = GoodFriday + 2*24*3600)

    HD = holiday(2000:2010, "Easter")
    HD
    checkTrue(inherits(HD, "timeDate"))

    HD = holiday(2000:2010, "GoodFriday")
    HD
    checkTrue(inherits(HD, "timeDate"))

    # holidays -
    Easter(2000:2010)
    GoodFriday(2000:2010)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.holidayNYSE <-
    function()
{
    # Holiday NYSE -
    HD = holidayNYSE(currentYear)
    print(HD)
    checkTrue(inherits(HD, "timeDate"))

    # After July 3, 1959, move Saturday holidays to Friday
    # ... except if at the end of monthly/yearly accounting period
    # this is the last business day of a month.
    publishedHolidays <- c(
        # holidays listed in http://www.nyse.com/events
        "2007-01-01", # New Year's Day
        "2007-01-15", # Martin Luther King, Jr. Day
        "2007-02-19", # Washington's Birthday/Presidents' Day
        "2007-04-06", # Good Friday
        "2007-05-28", # Memorial Day
        "2007-07-04", # Independence Day
        "2007-09-03", # Labor Day
        "2007-11-22", # Thanksgiving Day
        "2007-12-25", # Christmas
        # holidays published on http://nyse.com/holidays
        "2008-01-01", # New Year's Day
        "2008-01-21", # Martin Luther King, Jr. Day
        "2008-02-18", # Washington's Birthday/Presidents' Day
        "2008-03-21", # Good Friday
        "2008-05-26", # Memorial Day
        "2008-07-04", # Independence Day
        "2008-09-01", # Labor Day
        "2008-11-27", # Thanksgiving Day
        "2008-12-25", # Christmas
        "2009-01-01", # New Year's Day
        "2009-01-19", # Martin Luther King, Jr. Day
        "2009-02-16", # Washington's Birthday/Presidents' Day
        "2009-04-10", # Good Friday
        "2009-05-25", # Memorial Day
        "2009-07-03", # Independence Day (observed)
        "2009-09-07", # Labor Day
        "2009-11-26", # Thanksgiving Day
        "2009-12-25") # Christmas
    
    publishedHolidays <- timeDate( publishedHolidays, zone="NewYork",
        FinCenter="NewYork" )
    
    checkTrue(all.equal(
        format(publishedHolidays), 
        format(holidayNYSE(2007:2009))))

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.holidayZURICH =
function()
{
    # Holiday Zurich -                          
    holidayZURICH(currentYear)
    
    # Return Value:
    return()
}


################################################################################

