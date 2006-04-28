
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
# FUNCTION:           HOLIDAY CALENDAR FUNCTIONS:
#  easter              Returns date of easter or related feasts as '.sdate'
#  .easter.sunday       Easter Algorithm
#  holiday             Returns a holiday date of G7 and CH as '.sdate'
# FUNCTION:           TIME DATE HOLIDAY CALENDARS:
#  holiday.NYSE        Returns 'timeDate' object for full-day NYSE holidays
# FUNCTION:           DESCRIPTION:
#  .on.or.after        Computes date in month that is a nday ON OR AFTER date
#  .on.or.before       Computes date in month that is a nday ON OR BEFORE date
#  .nth.of.nday        Computes nth ocurrance of a nday in year/month
#  .last.of.nday       Computes the last nday in year/month
# FUNCTION:           DESCRIPTION:
#  .sjulian            Computes Julian day numbers from ISO-8601 dates
#  .sdate              Computes ISO-8601 dates from Julian day numbers
#  .sday.of.week       Computes day of the week for ISO-8601 dates 
#  .sleap.year         Returns TRUE/FALSE if dates belong to leap years or not
#  .print.sdate        Print method for objects of class ".sdate"
# FUNCTION:           DESCRIPTION:
#  .fjulian             Transform formatted dates to julian day numbers
# FUNCTION:           DESCRIPTION:
#  .julian             Implements SPlus like 'julian'
#  month.day.year      Implements SPlus like 'month.day.year'
#  leap.year           Implements SPlus like 'leap.year'
#  day.of.week         Implements SPlus like 'day.of.week'
################################################################################


################################################################################
# HOLIDAY FUNCTIONS:
#   This is a family of functions dealing with holidays, which I have
#   implemented several years ago. There is a function named 'easter'
#   which allows to calculate the date of easter and related feasts.
#   In addition there are more than 100 functions to calculate the dates
#   of holidays in the G7 countries and Switzerland. Have a look in the  
#   database located in the data directory. In the ASCII database you can 
#   add your personal entries. Furthermore, the function 'holiday.NYSE'
#   is an example how to write your own holiday calendar functions.


.easter.sunday =
function(year) 
{
    # This algorithm holds for any year in the Gregorian Calendar, 
    # which (of course) means years including and after 1583
    a = year%%19
    b = year%/%100
    c = year%%100
    d = b%/%4
    e = b%%4
    f = (b+8)%/%25
    g = (b-f+1)%/%3
    h = (19*a+b-d-g+15)%%30
    i = c%/%4
    k = c%%4
    l = (32+2*e+2*i-h-k)%%7
    m = (a+11*h+22*l)%/%451
    easter.month = (h+l-7*m+114)%/%31 
    p = (h+l-7*m+114)%%31
    easter.day = p+1 
    
    # Return Value:
    year*10000 + easter.month*100 + easter.day 
}

        
# ------------------------------------------------------------------------------


easter = 
function(year = currentYear, shift = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns dates of easter or related feasts
    
    # Arguments:
    #   year - an integer variable or vector for the year(s)
    #       ISO-8601 formatted as "CCYY" where easter or
    #       easter related feasts should be computed.
    #   shift - the number of days shifted from the easter
    #       date. Negative integers are allowed.
    
    # Value:
    #   Returns the date of Easter shifted by 'shift' days, 
    #   ".sdate" formatted, an integer of the form CCYYMMDD.
    
    # Details:
    #   By default the date of Easter is calculated and returned
    #   in ISO format CCYYMMDD as an integer. Changing shift you
    #   can calculate easter related feasts, e.g. "shift=1" returns
    #   the date of Easter Monday, or "shift=-2" returns the date
    #   of Good Friday.
    
    # Examples:
    #   currentYear         # prints current year as integer
    #   easter()            # date of easter this year
    #   easter(2000:2009))  # easter for the 2k decade  
    #   timeDate(easter())  # Convert to timeDate
    #   class(easter())     # what class?
    
    # Notes:
    #   The variable currentYear is set in ".FirstLib"
    #   Calls "month.day.year" and ".sjulian"
    
    # FUNCTION:
        

    # Shift and Compute Easter:
    mdy = month.day.year(.sjulian(.easter.sunday(year))+shift)
    ans = as.integer(mdy$year*10000 + mdy$month*100 + mdy$day)
    
    # Classify as simple integer ISO date format CCYYMMDD
    ans = timeDate(as.character(ans)) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


holiday = 
function(year = currentYear, Holiday = "Easter")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the data of a holiday, year may be a vector.
    
    # Arguments:
    #   year - an integer variable or vector for the year(s) ISO-8601 
    #       formatted as "CCYY" as integers.
    #   holiday - a character string naming the holiday. By default
    #       "Easter". Allowable names are the holidays in the G7 
    #       countries and Switzerland.
    
    # Value:
    #   Returns the date of a listed holiday for the selected
    #   "year"(s), ".sdate" formatted, an integer of the form CCYYMMDD.
    
    # Example:
    #   holiday()
    #   holiday(2000:2009, "USLaborDay")
    #   class(holiday())
    
    # List of Valid Holiday Character Strings:
    #   The following ecclestial and public holidays in
    #       the G7 countries and Switzerland are available:
    #   Holidays Related to Easter:
    #       Septuagesima, Quinquagesima, AshWednesday, PalmSunday,
    #       GoodFriday,  EasterSunday, Easter, EasterMonday, 
    #       RogationSunday, Ascension, Pentecost, PentecostMonday, 
    #       TrinitySunday CorpusChristi. 
    #   Holidays Related to Christmas:
    #       ChristTheKing, Advent1st, Advent1st, Advent3rd, 
    #       Advent4th, ChristmasEve, ChristmasDay, BoxingDay, 
    #       NewYearsDay. 
    #   Other Ecclestical Feasts:
    #       SolemnityOfMary, Epiphany, PresentationOfLord, 
    #       Annunciation, TransfigurationOfLord, AssumptionOfMary, 
    #       AssumptionOfMary, BirthOfVirginMary, CelebrationOfHolyCross, 
    #       MassOfArchangels, AllSaints, AllSouls. 
    #   CHZurich - Public Holidays:
    #       CHBerchtoldsDay, CHSechselaeuten, CHAscension, 
    #       CHConfederationDay, CHKnabenschiessen. 
    #   GBLondon - Public Holidays:
    #       GBMayDay, GBBankHoliday, GBSummerBankHoliday, 
    #       GBNewYearsEve.
    #   DEFrankfurt - Public Holidays:
    #       DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve,
    #       DENewYearsEve. 
    #   FRParis - Public Holidays:
    #       FRFetDeLaVictoire1945, FRAscension, FRBastilleDay, 
    #       FRAssumptionVirginMary, FRAllSaints, FRArmisticeDay. 
    #   ITMilano - Public Holidays:
    #       ITEpiphany, ITLiberationDay, ITRepublicAnniversary, 
    #       ITAssumptionOfVirginMary, ITAllSaints, ITWWIVictoryAnniversary, 
    #       ITStAmrose, ITImmaculateConception. 
    #   USNewYork/USChicago - Public Holidays:
    #       USNewYearsDay, USInaugurationDay, USMLKingsBirthday, 
    #       USLincolnsBirthday, USWashingtonsBirthday, USMemorialDay, 
    #       USIndependenceDay, USLaborDay,  USColumbusDay, USElectionDay, 
    #       USVeteransDay, USThanksgivingDay, USChristmasDay, 
    #       USCPulaskisBirthday, USGoodFriday. 
    #   CAToronto/CAMontreal - Public Holidays:
    #       CAVictoriaDay, CACanadaDay, CACivicProvincialHoliday, 
    #       CALabourDay, CAThanksgivingDay, CaRemembranceDay. 
    #   JPTokyo/JPOsaka - Public Holidays:
    #       JPNewYearsDay, JPGantan, JPBankHolidayJan2, JPBankHolidayJan3,
    #       JPComingOfAgeDay, JPSeijinNoHi, JPNatFoundationDay,
    #       JPKenkokuKinenNoHi, JPGreeneryDay, JPMidoriNoHi, 
    #       JPConstitutionDay, JPKenpouKinenBi, JPNationHoliday, 
    #       JPKokuminNoKyujitu, JPChildrensDay, JPKodomoNoHi, 
    #       JPMarineDay, JPUmiNoHi, JPRespectForTheAgedDay,
    #       JPKeirouNoHi, JPAutumnalEquinox, JPShuubun-no-hi, 
    #       JPHealthandSportsDay, JPTaiikuNoHi, JPNationalCultureDay, 
    #       JPBunkaNoHi, JPThanksgivingDay, JPKinrouKanshaNohi, 
    #       JPKinrou-kansha-no-hi, JPEmperorsBirthday,
    #       JPTennou-tanjyou-bi, JPTennou-tanjyou-bi. 
    #   All the holiday functions are listed in the data file "holidays.R"
    #   Additional holidays, which are not yet available there, can be added
    #   to this data base file.
    
    # FUNCTION:
        
    # Determine Function:
    # Modified for SPlus Compatibility:
    FUN = match.fun(Holiday)
    ans = FUN(year)
    
    # Classify as simple integer ISO date format CCYYMMDD
    ans = timeDate(as.character(ans))
    
    # Return Value:
    ans
}


################################################################################
# HOLIDAY CALENDAR


holiday.NYSE = 
function(y = currentYear)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns 'timeDate' object for full-day NYSE holidays 
    
    # Arguments:
    #   year - an integer variable or vector for the year(s)
    #       ISO-8601 formatted as "CCYY" where easter or
    #       easter related feasts should be computed.
    
    # Value:
    #   Returns the holiday calendar for the NYSE formatted as 
    #   'timeDate' object.
    
    # Details:
    #   The "New York Stock Exchange" calendar starts from year 1885.
    #   The rules are listed at the web site http://www.nyse.com.
    
    # Example:
    #   > holiday.NYSE(2004)
    #   [1] "America/New_York"
    #   [1] [2004-01-01] [2004-01-19] [2004-02-16] [2004-04-09]
    #   [5] [2004-05-31] [2004-07-05] [2004-09-06] [2004-11-25]
    
    # FORMULA:
    
    #  Settings:
    years = y
    holidays = NULL
    
    # Iterate years:
    for (y in years ) { 
        if (y >= 1885) 
            holidays = c(holidays, as.character(USNewYearsDay(y)))
        if (y >= 1885) 
            holidays = c(holidays, as.character(USIndependenceDay(y)))    
        if (y >= 1885) 
            holidays = c(holidays, as.character(USThanksgivingDay(y)))   
        if (y >= 1885)
            holidays = c(holidays, as.character(USChristmasDay(y)))
        if (y >= 1887)
            holidays = c(holidays, as.character(USLaborDay(y)))
        if (y != 1898 & y != 1906 & y != 1907)
            holidays = c(holidays, as.character(USGoodFriday(y)))
        if (y >= 1909 & y <= 1953) 
            holidays = c(holidays, as.character(USColumbusDay(y)))       
        if (y >= 1998)
            holidays = c(holidays, as.character(USMLKingsBirthday(y)))        
        if (y >= 1896 & y <= 1953) 
            holidays = c(holidays, as.character(USLincolnsBirthday(y)))
        if (y <= 1970) 
            holidays = c(holidays, as.character(USWashingtonsBirthday(y)))
        if (y >= 1970) 
            holidays = c(holidays, as.character(USPresidentsDay(y))) 
        if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953)) 
            holidays = c(holidays, as.character(USVeteransDay(y)))        
        if (y <= 1968 | y == 1972 | y == 1976 | y == 1980) 
            holidays = c(holidays, as.character(USElectionDay(y)))       
        if (y <= 1970) 
            holidays = c(holidays, as.character(USDecorationMemorialDay(y)))     
        if (y >= 1971) 
            holidays = c(holidays, as.character(USMemorialDay(y)))
    }  

    # Sort and Convert to 'timeDate':
    holidays = sort(holidays)
    ans = timeDate(holidays)
    
    # Move Sunday Holidays to Monday:
    ans = ans + as.integer(as.POSIXlt(as.POSIXct(ans))$wday==0) * 24 * 3600
   
    # After July 3, 1959, move Saturday holidays to Friday
    # ... except if at the end of monthly/yearly accounting period 
    # this is the last business day of a month.
    posix = as.POSIXlt(as.POSIXct(ans))
    y = posix$year + 1900
    m = posix$mon + 1
    lastday = as.POSIXlt(as.POSIXct(timeCalendar(y = y+(m+1)%/%13, 
        m = m+1-(m+1)%/%13*12, d = 1)-24*3600))$mday
    ExceptOnLastFriday = timeDate(as.character(
        .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)))
    ans = ans - as.integer(ans >= timeDate("1959-07-03") &
        as.POSIXlt(as.POSIXct(ans))$wday == 0  &
        ans != ExceptOnLastFriday ) * 24 * 3600 
    
    # Remove Remaining Weekend Dates:
    ans = ans[!(as.POSIXlt(as.POSIXct(ans))$wday == 0 | 
        as.POSIXlt(as.POSIXct(ans))$wday == 6)]
    ans@FinCenter = "NewYork"
    
    # Return Value:
    ans
}


################################################################################
# N-DAY FAMILY
#   This is a family of four functions for calculating the n-th occurence
#   of n-Days for given months and years. The value returned by these 
#   functions is an ISO-8601 formatted date, written as integer in the 
#   form CCYYMMDD.


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
    ##  (nday-day.of.week(month, day, year))%%7
    .sdate = year*10000+month*100+day
    ans = .sdate(.sjulian(.sdate)+(nday-day.of.week(month, day, year))%%7)
    
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
    ##  (-(nday-day.of.week(month,day,year)))%%7
    .sdate = year*10000+month*100+day
    ans = .sdate(.sjulian(.sdate)-(-(nday-day.of.week(month,day,year)))%%7)
    
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
    ##  (nday-day.of.week(year,month,7*nth-6))%%7
    .sdate = year*10000+month*100+1
    ans = .sdate(.sjulian(.sdate)+(nth-1)*7+(nday-day.of.week(month,1,year))%%7) 
    
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
    ##  (day.of.week(year,month,lastday)-nday)%%7
    .sdate = year*10000 + month*100 + lastday
    ans = .sdate(.sjulian(.sdate)-(-(nday-day.of.week(month,lastday,year)))%%7)
    
    # Return Value:
    ans
}


################################################################################
# .sdate - FAMILY:
#   What is the  '.sdate' format? 
#   '.sdate' is a very simple class of objects of integer formatted ISO  
#   dates CCYYMMDD, e.g. 20040101. I introduced this class long ago 
#   before R had POSIX date/time objects. Now I use this simple date
#   format for managing holiday calendars.
#   The following functions return objects of class '.sdate':
#   easter(), holiday()
#   .sdate(), .sday.of.week(), .sleap.year()
#   .on.or.after(), .on.or.before(), .nth.of.nday(), .last.of.nday()



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
    
    # FUNCTION:
    
    # Internal Function:
    month.day.year = function(jul, origin = c(1, 1, 1960)) {
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
    
    # Julian Day Numbers to ISO-8601 Gregorian Dates:
    year0 = origin%/%10000
    month0 = (origin-10000*year0)%/%100
    day0 = origin-10000*year0-100*month0
    
    # Month - Day - Year Function:
    mdylist = month.day.year(julians, origin = c(month0, day0, year0))
 
    # In '.sdate' Format:
    ans = mdylist$year*10000 + mdylist$month*100 + mdylist$day
    
    # Return Value:
    class(ans) = ".sdate"
    ans
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
        
    # FUNCTION:
    
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
    
    # FUNCTION::fBasic  
    
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


# ------------------------------------------------------------------------------


.print.sdate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for objects of class ".sdate".
    
    # Arguments:
    #   x - anobject of class ".sdate"
    
    # FUNCTION:
    
    # Print in the same format as 'timeDate' objects:
    if (class(version) != "Sversion") {
        print(timeDate(x, ...))
    } else {
        print(timeDate(as.character(x), in.format = "%04y%02m%02d", 
            format = "[%04Y-%02m-%02d]"), ...)
    }
    
    # Return Value:
    invisible()
}


# ******************************************************************************


.fjulian = 
function(fdates, origin = 19600101, order = 'mdy', cc = NULL, swap = 20)
{   # # A function implemented by Diethelm Wuertz

    # Description:
    #   Transforms formatted dates (fdates) from several formats 
    #   as 8/11/73 11Aug1973, ... into ISO-8601 Gregorian dates
    #   ... makes use of C-Program char_date.c implemented by 
    #   Terry Therneau
    
    # Notes:
    #   cc - Century, becoming obsolete with the introduction of
    #       swap.
 
    # Requirements:
    #   R-package "date"
    #   Splus Like Function .julian
    
    # Function Calls:
    #   C: char_date()
    
    # FUNCTION:
    
    # Formats:
    order.vec = switch(order,
        'ymd'= c(1,2,3),
        'ydm'= c(1,3,2),
        'mdy'= c(2,3,1),
        'myd'= c(2,1,3),
        'dym'= c(3,1,2),
        'dmy'= c(3,2,1),
        stop("Invalid value for 'order' option"), 
        PACKAGE = "fCalendar")
    nn = length(fdates)
    temp = .C("char_date", 
        as.integer(nn),
        as.integer(order.vec),
        as.character(fdates),
        month = integer(nn),
        day = integer(nn),
        year = integer(nn),
        PACKAGE = "fCalendar")
    month = temp[[4]]
    day = temp[[5]]
    year = temp[[6]]
    yy = year - 100 * floor (year/100)
    
    # Swap:
    cc = 19 + trunc(sign(swap-yy)+1)/2
    year = cc*100 + yy
    
    # Origin:
    cc0 = origin %/% 1000000
    yymmdd0 = origin - cc0*1000000
    yy0 = yymmdd0 %/% 10000
    mm0 = yymmdd0 %/% 100 - yy0*100
    dd0 = yymmdd0 - yy0*10000 - mm0*100

    # Result:
    ans = .julian(month, day, year, origin = c(mm0, dd0, cc0*100+yy0))
    
    # Return Value:
    ans
}


# ******************************************************************************


.julian =
function(m, d, y, origin = c(month = 1, day = 1, year = 1960))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   This function is a synonyme for Splus' "julian()" with the
    #   same list of arguments.
    
    # Note:
    #   SPlus like function.

    # FUNCTION:
    
    # Selection:
    .R = TRUE
    .S = FALSE
    
    # Implementation under R:
    if(.R) {    
        only.origin = all(missing(m), missing(d), missing(y))
        if(only.origin) m = d = y = NULL    # return days since origin
        nms = names(d)
        max.len = max(length(m), length(d), length(y))  
        # prepend new origin value and rep out to common max. length:
        m = c(origin[1], rep(m, length = max.len))
        d = c(origin[2], rep(d, length = max.len))
        y = c(origin[3], rep(y, length = max.len))  
        # code from julian date in the S book (p.269)
        y = y + ifelse(m > 2, 0, -1)
        m = m + ifelse(m > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        out = (146097 * c) %/% 4 + (1461 * ya) %/% 4 + 
            (153 * m + 2) %/% 5 + d + 1721119   
        # now subtract the new origin from all dates
        if(!only.origin) {
            if(all(origin == 0)) out = out[-1] else out = out[-1] - out[1] }    
        names(out) = nms
        result = out }
    
    # Synonyme for S:
    if(.S) {
        result = julian(m = m, d = d, y = y, origin. = origin)}

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


month.day.year = 
function(jul, origin = c(month = 1, day = 1, year = 1960))
{   # # A function implemented by Diethelm Wuertz

    # Description:
    #   This function is a synonyme for Splus' "month.day.year()" with
    #   the same list of arguments.
    
    # Note:
    #   Splus like function.
    
    # FUNCTION: 
    
    # Selection:
    .R = TRUE
    .S = FALSE
    
    # Implementation under R:
    if (.R) {
        shift = .julian(1, 1, 1960, 0)  
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
        result = list(month = m, day = d, year = y)}    
    # Synonyme for S:
    if (.S) { 
        result = month.day.year(jul = jul, origin. = origin)}   
            
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


leap.year = 
function(y)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Decides if a year is a leap year or not
    
    # Arguments:
    #   y - an integer value or vector giving the year.
    
    # Example:
    #   leap.year(1980:2005)
    
    # FUNCTION:
    
    # Result:
    ans = y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


day.of.week = 
function(month, day, year) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A Synonyme for .sday.of.week() function
    
    # Note:
    #   SPlus like.

    # FUNCTION:
    
    # Result:
    ans = .sday.of.week(year * 10000 + month * 100 + day)
    
    # Return Value:
    ans
}


################################################################################

