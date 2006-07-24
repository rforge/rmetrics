
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
#  holiday             Returns a holiday date of G7 and CH 
#  holidayNYSE         Returns 'timeDate' object for full-day NYSE holidays
#  holidayZURICH       Returns 'timeDate' object for ZURICH holidays
################################################################################


################################################################################
#  holiday             Returns a holiday date of G7 and CH 
#  holidayNYSE         Returns 'timeDate' object for full-day NYSE holidays
#  holidayZURICH       Returns 'timeDate' object for ZURICH holidays


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
    
    # Changes:
    #
    
    # FUNCTION:
        
    # Determine Function:
    # Modified for SPlus Compatibility:
    nHolidays = length(Holiday)
    ans = NULL
    for (i in 1:nHolidays) {
        FUN = match.fun(Holiday[i])
        ans = c(ans, as.character(FUN(year)))
    }
    
    # Classify as simple integer ISO date format CCYYMMDD
    ans = timeDate(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


holidayNYSE = 
function(year = currentYear)
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
    
    # Changes:
    #
    
    # FUNCTION:
    
    #  Settings:
    years = year
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
    ans = ans + as.integer(as.POSIXlt(ans@Data)$wday==0) * 24 * 3600
   
    # After July 3, 1959, move Saturday holidays to Friday
    # ... except if at the end of monthly/yearly accounting period 
    # this is the last business day of a month.
    posix = as.POSIXlt(ans@Data)
    y = posix$year + 1900
    m = posix$mon + 1
    lastday = as.POSIXlt((timeCalendar(y = y+(m+1)%/%13, 
        m = m+1-(m+1)%/%13*12, d = 1)-24*3600)@Data)$mday
    ExceptOnLastFriday = timeDate(as.character(
        .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)))
    ans = ans - as.integer(
        ans >= timeDate("1959-07-03") &
        as.POSIXlt(ans@Data)$wday == 0  &
        ans != ExceptOnLastFriday ) * 24 * 3600 
    
    # Remove Remaining Weekend Dates:
    ans = ans[
        !(as.POSIXlt(ans@Data)$wday == 0 | 
        as.POSIXlt(ans@Data)$wday == 6)]
    ans@FinCenter = "NewYork"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
     
    
holidayZURICH = 
function(year = currentYear) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a holiday Calendar for Zurich in Switzerland

    # Details:
    #   Inspect the holiday database in "data/holiday.db.R"
    #   ... You can add there additional holidays!
    #       NewYearsDay         Jan, 1st
    #       GoodFriday          2 days before Easter
    #       EasterMonday        1 day after Easter
    #       LaborDay            May, 1st  
    #       PentecostMonday     50 days after Easter
    #       ChristmasDay        Dec, 25 
    #       BoxingDay           Dec, 26  
    #       CHBerchtoldsDay     Jan, 2nd
    #       CHSechselaeuten     3rd Monday in April 
    #                           1 week later if it coincides with Easter Monday
    #       CHAscension         39 days after Easter
    #       CHConfederationDay  Aug, 1st
    #       CHKnabenschiessen   2nd Saturday to Monday in Sep
    
    # FUNCTION:
    
    # Calendar:
    years = y = year
    holidays = NULL
    # Iterate Years:
    for (y in years ) { 
        holidays = c(holidays, NewYearsDay(y))
        holidays = c(holidays, GoodFriday(y))   
        holidays = c(holidays, EasterMonday(y)) 
        holidays = c(holidays, LaborDay(y))
        holidays = c(holidays, PentecostMonday(y))  
        holidays = c(holidays, ChristmasDay(y)) 
        holidays = c(holidays, BoxingDay(y)) 
        holidays = c(holidays, CHBerchtoldsDay(y))
        holidays = c(holidays, CHSechselaeuten(y))
        holidays = c(holidays, CHAscension(y))
        holidays = c(holidays, CHConfederationDay(y))
        holidays = c(holidays, CHKnabenschiessen(y)) }
    
    # Sort and Convert to 'timeDate':
    holidays = as.character(sort(holidays))
    ans = timeDate(holidays, format = "%Y%m%d", FinCenter = "GMT")

    # Remove Remaining Weekend Dates:
    ans = ans[!( (ans@Data)$wday == 0 | (ans@Data)$wday == 6 )]

    # Set Financial Center:
    ans@FinCenter = "Europe/Zurich"

    # Return Value:
    ans 
}


################################################################################


