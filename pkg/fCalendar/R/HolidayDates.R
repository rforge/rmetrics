
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


# fCalendar::5A-HolidayDates.R
################################################################################
# FUNCTION:                 DESCRIPTION:
#  ...                       Holiday Functions
# FUNCTION:                 DESCRIPTION:
#  .holidayList              Prints all public and ecclestical holidays
#  .easter                   Returns date of easter or related feasts 
#  .easterSunday             Easter Algorithm 
# FUNCTION:                 DESCRIPTION:
#  .on.or.after              Computes date in month that is a nday ON OR AFTER
#  .on.or.before             Computes date in month that is a nday ON OR BEFORE
#  .nth.of.nday              Computes nth ocurrance of a nday in year/month
#  .last.of.nday             Computes the last nday in year/month
# FUNCTION:                 DESCRIPTION:
#  .sdate                    Computes ISO-8601 dates from Julian day numbers
#  .sjulian                  Computes Julian day numbers from ISO-8601 dates
#  .sday.of.week             Computes day of the week for ISO-8601 dates 
#  .sleap.year               Returns TRUE/FALSE for leap years or not
################################################################################


################################################################################
# FUNCTION:                 DESCRIPTION:
#  ...                       Holiday Functions


Septuagesima = 
function(year = currentYear) {
    ans = .easter(year, -63)
    timeDate(as.character(ans)) }
    
Quinquagesima = 
function(year = currentYear) {
    ans = .easter(year, -49)
    timeDate(as.character(ans)) }
    
AshWednesday = 
function(year = currentYear) {
    ans = .easter(year, -46)
    timeDate(as.character(ans)) }
    
PalmSunday = 
function(year = currentYear) {
    ans = .easter(year, -7)
    timeDate(as.character(ans)) }
    
GoodFriday = 
function(year = currentYear) {
    ans = .easter(year, -2) 
    timeDate(as.character(ans)) } 
    
Easter = 
function(year = currentYear) {
    ans = .easter(year)    
    timeDate(as.character(ans)) } 
    
EasterSunday = 
function(year = currentYear) {
    ans = .easter(year = currentYear)
    timeDate(as.character(ans)) } 
    
EasterMonday = 
function(year = currentYear) {
    ans = .easter(year, 1)
     
    timeDate(as.character(ans)) } 
    
RogationSunday = 
function(year = currentYear) {
    ans = .easter(year, 35)   
    timeDate(as.character(ans)) } 
    
Ascension = 
function(year = currentYear) {
    ans = .easter(year, 39)
    timeDate(as.character(ans)) } 
    
Pentecost = 
function(year = currentYear) {
    ans = .easter(year, 49) 
    timeDate(as.character(ans)) } 
    
PentecostMonday =  
function(year = currentYear) {
    ans = .easter(year, 50)     
    timeDate(as.character(ans)) } 
    
TrinitySunday = 
function(year = currentYear) {
    ans = .easter(year, 56)     
    timeDate(as.character(ans)) } 
    
CorpusChristi = 
function(year = currentYear) {
    ans = .easter(year, 60)    
    timeDate(as.character(ans)) } 

    
# ------------------------------------------------------------------------------


ChristTheKing = 
function(year = currentYear) {
    ans = .on.or.after(year, 11, 20, 0)
    timeDate(as.character(ans)) } 
    
Advent1st = 
function(year = currentYear) {
    ans = .on.or.after(year, 11, 27, 0)
    timeDate(as.character(ans)) } 
    
Advent2nd = 
function(year = currentYear) {
    ans = .on.or.after(year, 12,  4, 0) 
    timeDate(as.character(ans)) } 
    
Advent3rd = 
function(year = currentYear) {
    ans = .on.or.after(year, 12, 11, 0)  
    timeDate(as.character(ans)) } 
    
Advent4th = 
function(year = currentYear) {
    ans = .on.or.after(year, 12, 18, 0)     
    timeDate(as.character(ans)) } 
    
ChristmasEve = 
function(year = currentYear) {
    ans = year*10000 + 1224    
    timeDate(as.character(ans)) } 
    
ChristmasDay = 
function(year = currentYear) {
    ans = year*10000 + 1225   
    timeDate(as.character(ans)) }
    
BoxingDay = 
function(year = currentYear) {
    ans = year*10000 + 1226    
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------

    
SolemnityOfMary = 
function(year = currentYear) {
    ans = year*10000 + 0101    
    timeDate(as.character(ans)) }
    
Epiphany = 
function(year = currentYear) {
    ans = year*10000 + 0106   
    timeDate(as.character(ans)) }
    
PresentationOfLord = 
function(year = currentYear) {
    ans = year*10000 + 0202    
    timeDate(as.character(ans)) }
    
Annunciation = 
function(year = currentYear) {
    ans = year*10000 + 0325    
    timeDate(as.character(ans)) }
    
TransfigurationOfLord = 
function(year = currentYear) {
    ans = year*10000 + 0806  
    timeDate(as.character(ans)) }
    
AssumptionOfMary = 
function(year = currentYear) {
    ans = year*10000 + 0815   
    timeDate(as.character(ans)) }
    
BirthOfVirginMary = 
function(year = currentYear) {
    ans = year*10000 + 0908   
    timeDate(as.character(ans)) }
    
CelebrationOfHolyCross = 
function(year = currentYear) {
    ans = year*10000 + 0914   
    timeDate(as.character(ans)) }
    
MassOfArchangels = 
function(year = currentYear) {
    ans = year*10000 + 0929   
    timeDate(as.character(ans)) }
    
AllSaints = function(year = currentYear) {
    ans = year*10000 + 1101  
    timeDate(as.character(ans)) }
    
AllSouls = 
function(year = currentYear) {
    ans = year*10000 + 1102   
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


NewYearsDay = 
function(year = currentYear) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
LaborDay = 
function(year = currentYear) {
    ans = year*10000 + 0501  
    timeDate(as.character(ans)) }
    
    
# ------------------------------------------------------------------------------
    

CHBerchtoldsDay = 
function(year = currentYear) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }
    
CHSechselaeuten = 
function(year = currentYear) {
    ans = NULL
    for (y in year) {
        theDate = .nth.of.nday(y, 4, 1, 3)
        if (as.character(theDate) == as.character(.easter(y, +1))) {
            theDate = .nth.of.nday(y, 4, 1, 4) 
        }
        ans = c(ans, theDate) 
    }
    timeDate(as.character(ans)) }
    
CHAscension = 
function(year = currentYear) {
    ans = .easter(year, 39)    
    timeDate(as.character(ans)) }
    
CHConfederationDay = 
function(year = currentYear) {
    ans = year*10000 + 0801
    timeDate(as.character(ans)) }
    
CHKnabenschiessen = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 9, 1, 2)     
    timeDate(as.character(ans)) } 

        
# ------------------------------------------------------------------------------


GBMayDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 5, 1, 1)  
    timeDate(as.character(ans)) } 
    
GBBankHoliday = 
function(year = currentYear) {
    ans = .last.of.nday(year, 5, 31, 1)     
    timeDate(as.character(ans)) } 
    
GBSummerBankHoliday = 
function(year = currentYear) {
    ans = .last.of.nday(year, 8, 31, 1)     
    timeDate(as.character(ans)) } 
    
GBMilleniumDay = 
function(year = currentYear) {
    ans = 19991231   
    timeDate(as.character(ans)) }

        
# ------------------------------------------------------------------------------


DEAscension = 
function(year = currentYear) {
    ans = .easter(year, 39)    
    timeDate(as.character(ans)) } 
    
DECorpusChristi =  
function(year = currentYear) {
    ans = .easter(year, 60)  
    timeDate(as.character(ans)) } 
    
DEGermanUnity = 
function(year = currentYear) {
    ans = year*10000 + 1003 
    timeDate(as.character(ans)) }
    
DEChristmasEve = 
function(year = currentYear) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }
    
DENewYearsEve = 
function(year = currentYear) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }

        
# ------------------------------------------------------------------------------


FRFetDeLaVictoire1945 = 
function(year = currentYear) {
    ans = year*10000 + 0508
    timeDate(as.character(ans)) }
    
FRAscension = 
function(year = currentYear) {
    ans = .easter(year, 39)
    timeDate(as.character(ans)) }
    
FRBastilleDay = 
function(year = currentYear) {
    ans = year*10000 + 0714
    timeDate(as.character(ans)) }
    
FRAssumptionVirginMary = 
function(year = currentYear) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }
    
FRAllSaints = 
function(year = currentYear) {
    ans = year*10000 + 1101  
    timeDate(as.character(ans)) }
    
FRArmisticeDay = 
function(year = currentYear) {
    ans = year*10000 + 1111 
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


ITEpiphany = 
function(year = currentYear) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }
    
ITLiberationDay =  
function(year = currentYear) {
    ans = year*10000 + 0425
    timeDate(as.character(ans)) }
    
ITAssumptionOfVirginMary = 
function(year = currentYear) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }
    
ITAllSaints = 
function(year = currentYear) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }
    
ITStAmrose = 
function(year = currentYear) {
    ans = year*10000 + 1207
    timeDate(as.character(ans)) }
    
ITImmaculateConception = 
function(year = currentYear) {
    ans = year*10000 + 1208  
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


USNewYearsDay = 
function(year = currentYear) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
USInaugurationDay = 
function(year = currentYear) {
    ans = year*10000 + 0120 
    timeDate(as.character(ans)) }
    
USMLKingsBirthday = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 1, 1, 3)
    timeDate(as.character(ans)) } 
    
USLincolnsBirthday = 
function(year = currentYear) {
    ans = year*10000 + 0212
    timeDate(as.character(ans)) }
    
USWashingtonsBirthday = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 2, 1, 3)
    timeDate(as.character(ans)) } 
    
USMemorialDay = 
function(year = currentYear) {
    ans = .last.of.nday(year, 5, 31, 1)
    timeDate(as.character(ans)) } 
    
USIndependenceDay = 
function(year = currentYear) {
    ans = year*10000 + 0704
    timeDate(as.character(ans)) }
    
USLaborDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) } 
    
USColumbusDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) } 
    
USElectionDay = 
function(year = currentYear) {
    ans = .on.or.after(year, 11, 2, 2)
    timeDate(as.character(ans)) } 
    
USVeteransDay = 
function(year = currentYear) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }
    
USThanksgivingDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 11, 4, 4)
    timeDate(as.character(ans)) }
    
USChristmasDay = 
function(year = currentYear) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }
    
USCPulaskisBirthday = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 3, 1, 1)  
    timeDate(as.character(ans)) }
    
USGoodFriday = 
function(year = currentYear) {
    ans = .easter(year, -2)
    timeDate(as.character(ans)) } 
    
USPresidentsDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 2, 1, 3) 
    timeDate(as.character(ans)) }
    
USDecorationMemorialDay = 
function(year = currentYear) {
    ans = year*10000 + 0530
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


CAVictoriaDay = 
function(year = currentYear) {
    ans = .on.or.before(year, 5, 24, 1) 
    timeDate(as.character(ans)) } 
    
CAFamilyDay =
function(year = currentYear)
{   # Description:
    #   Adds the new Family Day
    # Note:
    #   Check ...
    #   www.sbhlawyers.com/media/ELD%20Oct%2019%202007%20Public%20Holidays%20and%20Family%20Day.pdf
    #   Family Day will fall on the third Monday of 
    #       every February, beginning in 2008.
    # Family Day:
    charvec = paste(year, "02", "01", sep = "-")
    ans = timeNthNdayInMonth(charvec, nday = 1, nth = 3)
    # Return Value:
    ans
} 
    
CACanadaDay = 
function(year = currentYear) {
    ans = year*10000 + 0701
    timeDate(as.character(ans)) }
    
CACivicProvincialHoliday = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 8, 1, 1)
    timeDate(as.character(ans)) }
    
CALabourDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }
    
CAThanksgivingDay = 
function(year = currentYear) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }
    
CaRemembranceDay = 
function(year = currentYear) {
    ans = year*10000 + 1111 
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


JPVernalEquinox <- 
function(year = currentYear) 
{
    # Author:
    #   Parlamis Franklin wrote:
    #   It's me again, with Japanese calendar minutiae I'm sure you've all  
    #   been dying to brush up on. The fCalendar functions don't include 
    #   the Japanese Vernal Equinox holiday. this is perhaps because there 
    #   is no easy way to calculate it. at any rate, here's a function I
    #   wrote to fill the gap.
    
    # Notes:
    #   Origin and End Date data from 
    #   http://aa.usno.navy.mil/data/docs/EarthSeasons.html
    #   The function Vernal.Equinox delivers correct values at the  
    #   endpoints of the above data. There may be minor variances 
    #   (+/- a few minutes) in the intermediate values, because the 
    #   function linearly approximates a phenomenon that is apparently
    #   nonlinear in recorded time.
    
    Equinox.Origin <- timeCalendar(1992, 3, 20, 8, 48, 0, FinCenter = "GMT")
    Data.EndDate <- timeCalendar(2020, 3, 20, 3, 49, 0, FinCenter = "GMT")
    Total.Seconds <- as.numeric(Data.EndDate-Equinox.Origin)*24*60*60
    Mean.Annual.Seconds <- Total.Seconds / (atoms(Data.EndDate)$Y -
        atoms(Equinox.Origin)$Y)
    Vernal.Equinox <- function(year) 
    {
        Equinox.Origin + 
        unclass((year-atoms(Equinox.Origin)$Y)*Mean.Annual.Seconds)
    }
    
    # Nota bene: JP Vernal Equinox is celebrated when the equinox  
    #   occurs in the Japanese time zone (see, e.g., 2006, where GMT 
    #   Vernal Equinox is on 20 March, but Japanese Equinox holiday is 
    #   21 March)
    
    # Return Value:
    trunc(timeDate(as.character(Vernal.Equinox(year)), FinCenter = "Tokyo"))
}


JPNewYearsDay = 
function(year = currentYear) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
JPGantan = 
function(year = currentYear) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
JPBankHolidayJan2 = 
function(year = currentYear) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }
    
JPBankHolidayJan3 = 
function(year = currentYear) {
    ans = year*10000 + 0103
    timeDate(as.character(ans)) }
    
JPComingOfAgeDay = 
function(year = currentYear) {
    ans = year*10000 + 0115
    timeDate(as.character(ans)) }
    
JPSeijinNoHi = 
function(year = currentYear) {
    ans = year*10000 + 0115
    timeDate(as.character(ans)) }
    
JPNatFoundationDay = 
function(year = currentYear) {
    ans =year*10000 + 0211
    timeDate(as.character(ans)) }
    
JPKenkokuKinenNoHi = 
function(year = currentYear) {
    ans = year*10000 + 0211
    timeDate(as.character(ans)) }
    
JPGreeneryDay = 
function(year = currentYear) {
    ans = year*10000 + 0429
    timeDate(as.character(ans)) }
    
JPMidoriNoHi = 
function(year = currentYear) {
    ans = year*10000 + 0429 
    timeDate(as.character(ans)) }
    
JPConstitutionDay = 
function(year = currentYear) {
    ans = year*10000 + 0503
    timeDate(as.character(ans)) }
    
JPKenpouKinenBi = 
function(year = currentYear) {
    ans = year*10000 + 0503
    timeDate(as.character(ans)) }
    
JPNationHoliday = 
function(year = currentYear) {
    ans = year*10000 + 0504
    timeDate(as.character(ans)) }
    
JPKokuminNoKyujitu = 
function(year = currentYear) {
    ans = year*10000 + 0504
    timeDate(as.character(ans)) }
    
JPChildrensDay = 
function(year = currentYear) {
    ans = year*10000 + 0505
    timeDate(as.character(ans)) }
    
JPKodomoNoHi = 
function(year = currentYear) {
    ans = year*10000 + 0505
    timeDate(as.character(ans)) }
    
JPMarineDay = 
function(year = currentYear) {
    ans = year*10000 + 0720
    timeDate(as.character(ans)) }
    
JPUmiNoHi = 
function(year = currentYear) {
    ans = year*10000 + 0720
    timeDate(as.character(ans)) }
    
JPRespectForTheAgedDay = 
function(year = currentYear) {
    ans = year*10000 + 0915
    timeDate(as.character(ans)) }
    
JPKeirouNOhi = 
function(year = currentYear) {
    ans = year*10000 + 0915
    timeDate(as.character(ans)) }
    
JPAutumnalEquinox = 
function(year = currentYear) {
    ans = year*10000 + 0924
    timeDate(as.character(ans)) }
    
JPShuubunNoHi = 
function(year = currentYear) {
    ans =year*10000 + 0924
    timeDate(as.character(ans)) }
    
JPHealthandSportsDay = 
function(year = currentYear) {
    ans = year*10000 + 1010 
    timeDate(as.character(ans)) }
    
JPTaiikuNoHi = 
function(year = currentYear) {
    ans = year*10000 + 1010
    timeDate(as.character(ans)) }
    
JPNationalCultureDay = 
function(year = currentYear) {
    ans = year*10000 + 1103
    timeDate(as.character(ans)) }
    
JPBunkaNoHi = 
function(year = currentYear) {
    ans = year*10000 + 1103
    timeDate(as.character(ans)) }
    
JPThanksgivingDay = 
function(year = currentYear) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPKinrouKanshaNoHi = 
function(year = currentYear) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPEmperorsBirthday = 
function(year = currentYear) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPTennouTanjyouBi = 
function(year = currentYear) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPBankHolidayDec31 = 
function(year = currentYear) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }

    
################################################################################
# Holiday Database:
# Copyright 1997, Diethelm Wuertz
#   www.rmetrics.org
# Required "Holiday" Functions:
#   "easter", ".on.or.after", ".nth.of.nday", ".last.of.nday", 
# The functions return an object of class ".sdate"
#   ISO-8601 formatted integers, i.e. CCYYMMDD


.holidayList = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints all public and ecclestical holidays
    
    # Changes:
    #
    
    # FUNCTION:
    
    # List:
    myList = c(
        "Septuagesima",
        "Quinquagesima",
        "AshWednesday",
        "PalmSunday",
        "GoodFriday",
        "Easter",
        "EasterSunday",
        "EasterMonday",
        "RogationSunday",
        "Ascension",
        "Pentecost",
        "PentecostMonday",
        "TrinitySunday",
        "CorpusChristi",
        "ChristTheKing",
        "Advent1st",
        "Advent2nd",
        "Advent3rd",
        "Advent4th",
        "ChristmasEve",
        "ChristmasDay",
        "BoxingDay",
        "NewYearsDay",
        "SolemnityOfMary",
        "Epiphany",
        "PresentationOfLord",
        "Annunciation",
        "TransfigurationOfLord",
        "AssumptionOfMary",
        "BirthOfVirginMary",
        "CelebrationOfHolyCross",
        "MassOfArchangels",
        "AllSaints",
        "AllSouls",
        "LaborDay",
        "CHBerchtoldsDay",
        "CHSechselaeuten",
        "CHAscension",
        "CHConfederationDay",
        "CHKnabenschiessen",
        "GBMayDay",
        "GBBankHoliday",
        "GBSummerBankHoliday",
        "GBMilleniumDay",
        "DEAscension",
        "DECorpusChristi",
        "DEGermanUnity",
        "DEChristmasEve",
        "DENewYearsEve",
        "FRFetDeLaVictoire1945",
        "FRAscension",
        "FRBastilleDay",
        "FRAssumptionVirginMary",
        "FRAllSaints",
        "FRArmisticeDay",
        "ITEpiphany",
        "ITLiberationDay",
        "ITAssumptionOfVirginMary",
        "ITAllSaints",
        "ITStAmrose",
        "ITImmaculateConception",
        "USDecorationMemorialDay",
        "USPresidentsDay",
        "USNewYearsDay",
        "USInaugurationDay",
        "USMLKingsBirthday",
        "USLincolnsBirthday",
        "USWashingtonsBirthday",
        "USMemorialDay",
        "USIndependenceDay",
        "USLaborDay",
        "USColumbusDay",
        "USElectionDay",
        "USVeteransDay",
        "USThanksgivingDay",
        "USChristmasDay",
        "USCPulaskisBirthday",
        "USGoodFriday",
        "CAVictoriaDay",
        "CACanadaDay",
        "CACivicProvincialHoliday",
        "CALabourDay",
        "CAThanksgivingDay",
        "CaRemembranceDay",
        "JPNewYearsDay",
        "JPGantan",
        "JPBankHolidayJan2",
        "JPBankHolidayJan3",
        "JPComingOfAgeDay",
        "JPSeijinNoHi",
        "JPNatFoundationDay",
        "JPKenkokuKinenNoHi",
        "JPGreeneryDay",
        "JPMidoriNoHi",
        "JPConstitutionDay",
        "JPKenpouKinenBi",
        "JPNationHoliday",
        "JPKokuminNoKyujitu",
        "JPChildrensDay",
        "JPKodomoNoHi",
        "JPMarineDay",
        "JPUmiNoHi",
        "JPRespectForTheAgedDay",
        "JPKeirouNOhi",
        "JPAutumnalEquinox",
        "JPShuubunNoHi",
        "JPHealthandSportsDay",
        "JPTaiikuNoHi",
        "JPNationalCultureDay",
        "JPBunkaNoHi",
        "JPThanksgivingDay",
        "JPKinrouKanshaNoHi",
        "JPEmperorsBirthday",
        "JPTennouTanjyouBi",
        "JPBankHolidayDec31")
        
    # Return Value:
    ans = data.frame(matrix(sort(myList), ncol = 1))
    colnames(ans) = "HOLIDAYS"
    ans
}


# ------------------------------------------------------------------------------


.easter = 
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
    #   .easter()            # date of easter this year
    #   .easter(2000:2009))  # easter for the 2k decade  
    #   timeDate(.easter())  # Convert to timeDate
    #   class(.easter())     # what class?
    
    # Notes:
    #   The variable currentYear is set in ".FirstLib"
    #   Calls ".month.day.year" and ".sjulian"
    
    # Changes:
    #
    
    # FUNCTION:

    # Shift and Compute Easter:
    mdy = .month.day.year(.sjulian(.easterSunday(year))+shift)
    ans = as.integer(mdy$year*10000 + mdy$month*100 + mdy$day)
    
    # Classify as simple integer ISO date format CCYYMMDD
    ans = timeDate(as.character(ans)) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.easterSunday =
function(year) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the 'timeDate' of Easter Sunday
    
    # FUNCTION:
    
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


################################################################################


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
    
    # Changes:
    #
    
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
    
    # Changes:
    #
    
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
    
    # Changes:
    #
    
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
    
    # Changes:
    #
    
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


