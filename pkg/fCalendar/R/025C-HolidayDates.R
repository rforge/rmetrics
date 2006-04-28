
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
# Holiday Database:
# Copyright 1997, Diethelm Wuertz
#   www.rmetrics.org
# Required "Holiday" Functions:
#   "easter", ".on.or.after", ".nth.of.nday", ".last.of.nday", 
# The functions return an object of class ".sdate"
#   ISO-8601 formatted integers, i.e. CCYYMMDD
################################################################################


.holidayList = 
function() 
{
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


Septuagesima = 
function(year) {
    ans = easter(year, -63)
    timeDate(as.character(ans)) }
    
Quinquagesima = 
function(year) {
    ans = easter(year, -49)
    timeDate(as.character(ans)) }
    
AshWednesday = 
function(year) {
    ans = easter(year, -46)
    timeDate(as.character(ans)) }
    
PalmSunday = 
function(year) {
    ans = easter(year, -7)
    timeDate(as.character(ans)) }
    
GoodFriday = 
function(year) {
    ans = easter(year, -2) 
    timeDate(as.character(ans)) } 
    
Easter = 
function(year) {
    ans = easter(year)    
    timeDate(as.character(ans)) } 
    
EasterSunday = 
function(year) {
    ans = easter(year)
    timeDate(as.character(ans)) } 
    
EasterMonday = 
function(year) {
    ans = easter(year, 1)
     
    timeDate(as.character(ans)) } 
    
RogationSunday = 
function(year) {
    ans = easter(year, 35)   
    timeDate(as.character(ans)) } 
    
Ascension = 
function(year) {
    ans = easter(year, 39)
    timeDate(as.character(ans)) } 
    
Pentecost = 
function(year) {
    ans = easter(year, 49) 
    timeDate(as.character(ans)) } 
    
PentecostMonday =  
function(year) {
    ans = easter(year, 50)     
    timeDate(as.character(ans)) } 
    
TrinitySunday = 
function(year) {
    ans = easter(year, 56)     
    timeDate(as.character(ans)) } 
    
CorpusChristi = 
function(year) {
    ans = easter(year, 60)    
    timeDate(as.character(ans)) } 

    
# ------------------------------------------------------------------------------


ChristTheKing = 
function(year) {
    ans = .on.or.after(year, 11, 20, 0)
    timeDate(as.character(ans)) } 
    
Advent1st = 
function(year) {
    ans = .on.or.after(year, 11, 27, 0)
    timeDate(as.character(ans)) } 
    
Advent2nd = 
function(year) {
    ans = .on.or.after(year, 12,  4, 0) 
    timeDate(as.character(ans)) } 
    
Advent3rd = 
function(year) {
    ans = .on.or.after(year, 12, 11, 0)  
    timeDate(as.character(ans)) } 
    
Advent4th = 
function(year) {
    ans = .on.or.after(year, 12, 18, 0)     
    timeDate(as.character(ans)) } 
    
ChristmasEve = 
function(year) {
    ans = year*10000 + 1224    
    timeDate(as.character(ans)) } 
    
ChristmasDay = 
function(year) {
    ans = year*10000 + 1225   
    timeDate(as.character(ans)) }
    
BoxingDay = 
function(year) {
    ans = year*10000 + 1226    
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------

    
SolemnityOfMary = 
function(year) {
    ans = year*10000 + 0101    
    timeDate(as.character(ans)) }
    
Epiphany = 
function(year) {
    ans = year*10000 + 0106   
    timeDate(as.character(ans)) }
    
PresentationOfLord = 
function(year) {
    ans = year*10000 + 0202    
    timeDate(as.character(ans)) }
    
Annunciation = 
function(year) {
    ans = year*10000 + 0325    
    timeDate(as.character(ans)) }
    
TransfigurationOfLord = 
function(year) {
    ans = year*10000 + 0806  
    timeDate(as.character(ans)) }
    
AssumptionOfMary = 
function(year) {
    ans = year*10000 + 0815   
    timeDate(as.character(ans)) }
    
BirthOfVirginMary = 
function(year) {
    ans = year*10000 + 0908   
    timeDate(as.character(ans)) }
    
CelebrationOfHolyCross = 
function(year) {
    ans = year*10000 + 0914   
    timeDate(as.character(ans)) }
    
MassOfArchangels = 
function(year) {
    ans = year*10000 + 0929   
    timeDate(as.character(ans)) }
    
AllSaints = function(year) {
    ans = year*10000 + 1101  
    timeDate(as.character(ans)) }
    
AllSouls = 
function(year) {
    ans = year*10000 + 1102   
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


NewYearsDay = 
function(year) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
LaborDay = 
function(year) {
    ans = year*10000 + 0501  
    timeDate(as.character(ans)) }
    
    
# ------------------------------------------------------------------------------
    

CHBerchtoldsDay = 
function(year) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }
    
CHSechselaeuten = 
function(year) {
    ans = NULL
    for (y in year) {
        theDate = .nth.of.nday(y, 4, 1, 3)
        if (theDate == easter(y, +1)) {
            theDate = .nth.of.nday(y, 4, 1, 4) }
        ans = c(ans, theDate) 
    }
    timeDate(as.character(ans)) }
    
CHAscension = 
function(year) {
    ans = easter(year, 39)    
    timeDate(as.character(ans)) }
    
CHConfederationDay = 
function(year) {
    ans = year*10000 + 0801
    timeDate(as.character(ans)) }
    
CHKnabenschiessen = 
function(year) {
    ans = .nth.of.nday(year, 9, 1, 2)     
    timeDate(as.character(ans)) } 

        
# ------------------------------------------------------------------------------


GBMayDay = 
function(year) {
    ans = .nth.of.nday(year, 5, 1, 1)  
    timeDate(as.character(ans)) } 
    
GBBankHoliday = 
function(year) {
    ans = .last.of.nday(year, 5, 31, 1)     
    timeDate(as.character(ans)) } 
    
GBSummerBankHoliday = 
function(year) {
    ans = .last.of.nday(year, 8, 31, 1)     
    timeDate(as.character(ans)) } 
    
GBMilleniumDay = 
function(year) {
    ans = 19991231   
    timeDate(as.character(ans)) }

        
# ------------------------------------------------------------------------------


DEAscension = 
function(year) {
    ans = easter(year, 39)    
    timeDate(as.character(ans)) } 
    
DECorpusChristi =  
function(year) {
    ans = easter(year, 60)  
    timeDate(as.character(ans)) } 
    
DEGermanUnity = 
function(year) {
    ans = year*10000 + 1003 
    timeDate(as.character(ans)) }
    
DEChristmasEve = 
function(year) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }
    
DENewYearsEve = 
function(year) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }

        
# ------------------------------------------------------------------------------


FRFetDeLaVictoire1945 = 
function(year) {
    ans = year*10000 + 0508
    timeDate(as.character(ans)) }
    
FRAscension = 
function(year) {
    ans = easter(year, 39)
    timeDate(as.character(ans)) }
    
FRBastilleDay = 
function(year) {
    ans = year*10000 + 0714
    timeDate(as.character(ans)) }
    
FRAssumptionVirginMary = 
function(year) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }
    
FRAllSaints = 
function(year) {
    ans = year*10000 + 1101  
    timeDate(as.character(ans)) }
    
FRArmisticeDay = 
function(year) {
    ans = year*10000 + 1111 
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


ITEpiphany = 
function(year) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }
    
ITLiberationDay =  
function(year) {
    ans = year*10000 + 0425
    timeDate(as.character(ans)) }
    
ITAssumptionOfVirginMary = 
function(year) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }
    
ITAllSaints = 
function(year) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }
    
ITStAmrose = 
function(year) {
    ans = year*10000 + 1207
    timeDate(as.character(ans)) }
    
ITImmaculateConception = 
function(year) {
    ans = year*10000 + 1208  
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


USNewYearsDay = 
function(year) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
USInaugurationDay = 
function(year) {
    ans = year*10000 + 0120 
    timeDate(as.character(ans)) }
    
USMLKingsBirthday = 
function(year) {
    ans = .nth.of.nday(year, 1, 1, 3)
    timeDate(as.character(ans)) } 
    
USLincolnsBirthday = 
function(year) {
    ans = year*10000 + 0212
    timeDate(as.character(ans)) }
    
USWashingtonsBirthday = 
function(year) {
    ans = .nth.of.nday(year, 2, 1, 3)
    timeDate(as.character(ans)) } 
    
USMemorialDay = 
function(year) {
    ans = .last.of.nday(year, 5, 31, 1)
    timeDate(as.character(ans)) } 
    
USIndependenceDay = 
function(year) {
    ans = year*10000 + 0704
    timeDate(as.character(ans)) }
    
USLaborDay = 
function(year) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) } 
    
USColumbusDay = 
function(year) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) } 
    
USElectionDay = 
function(year) {
    ans = .on.or.after(year, 11, 2, 2)
    timeDate(as.character(ans)) } 
    
USVeteransDay = 
function(year) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }
    
USThanksgivingDay = 
function(year) {
    ans = .nth.of.nday(year, 11, 4, 4)
    timeDate(as.character(ans)) }
    
USChristmasDay = 
function(year) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }
    
USCPulaskisBirthday = 
function(year) {
    ans = .nth.of.nday(year, 3, 1, 1)  
    timeDate(as.character(ans)) }
    
USGoodFriday = 
function(year) {
    ans = easter(year, -2)
    timeDate(as.character(ans)) } 
    
USPresidentsDay = 
function(year) {
    ans = .nth.of.nday(year, 2, 1, 3) 
    timeDate(as.character(ans)) }
    
USDecorationMemorialDay = 
function(year) {
    ans = year*10000 + 0530
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


CAVictoriaDay = 
function(year) {
    ans = .on.or.before(year, 5, 24, 1) 
    timeDate(as.character(ans)) } 
    
CACanadaDay = 
function(year) {
    ans = year*10000 + 0701
    timeDate(as.character(ans)) }
    
CACivicProvincialHoliday = 
function(year) {
    ans = .nth.of.nday(year, 8, 1, 1)
    timeDate(as.character(ans)) }
    
CALabourDay = 
function(year) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }
    
CAThanksgivingDay = 
function(year) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }
    
CaRemembranceDay = 
function(year) {
    ans = year*10000 + 1111 
    timeDate(as.character(ans)) }

    
# ------------------------------------------------------------------------------


JPNewYearsDay = 
function(year) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
JPGantan = 
function(year) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }
    
JPBankHolidayJan2 = 
function(year) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }
    
JPBankHolidayJan3 = 
function(year) {
    ans = year*10000 + 0103
    timeDate(as.character(ans)) }
    
JPComingOfAgeDay = 
function(year) {
    ans = year*10000 + 0115
    timeDate(as.character(ans)) }
    
JPSeijinNoHi = 
function(year) {
    ans = year*10000 + 0115
    timeDate(as.character(ans)) }
    
JPNatFoundationDay = 
function(year) {
    ans =year*10000 + 0211
    timeDate(as.character(ans)) }
    
JPKenkokuKinenNoHi = 
function(year) {
    ans = year*10000 + 0211
    timeDate(as.character(ans)) }
    
JPGreeneryDay = 
function(year) {
    ans = year*10000 + 0429
    timeDate(as.character(ans)) }
    
JPMidoriNoHi = 
function(year) {
    ans = year*10000 + 0429 
    timeDate(as.character(ans)) }
    
JPConstitutionDay = 
function(year) {
    ans = year*10000 + 0503
    timeDate(as.character(ans)) }
    
JPKenpouKinenBi = 
function(year) {
    ans = year*10000 + 0503
    timeDate(as.character(ans)) }
    
JPNationHoliday = 
function(year) {
    ans = year*10000 + 0504
    timeDate(as.character(ans)) }
    
JPKokuminNoKyujitu = 
function(year) {
    ans = year*10000 + 0504
    timeDate(as.character(ans)) }
    
JPChildrensDay = 
function(year) {
    ans = year*10000 + 0505
    timeDate(as.character(ans)) }
    
JPKodomoNoHi = 
function(year) {
    ans = year*10000 + 0505
    timeDate(as.character(ans)) }
    
JPMarineDay = 
function(year) {
    ans = year*10000 + 0720
    timeDate(as.character(ans)) }
    
JPUmiNoHi = 
function(year) {
    ans = year*10000 + 0720
    timeDate(as.character(ans)) }
    
JPRespectForTheAgedDay = 
function(year) {
    ans = year*10000 + 0915
    timeDate(as.character(ans)) }
    
JPKeirouNOhi = 
function(year) {
    ans = year*10000 + 0915
    timeDate(as.character(ans)) }
    
JPAutumnalEquinox = 
function(year) {
    ans = year*10000 + 0924
    timeDate(as.character(ans)) }
    
JPShuubunNoHi = 
function(year) {
    ans =year*10000 + 0924
    timeDate(as.character(ans)) }
    
JPHealthandSportsDay = 
function(year) {
    ans = year*10000 + 1010 
    timeDate(as.character(ans)) }
    
JPTaiikuNoHi = 
function(year) {
    ans = year*10000 + 1010
    timeDate(as.character(ans)) }
    
JPNationalCultureDay = 
function(year) {
    ans = year*10000 + 1103
    timeDate(as.character(ans)) }
    
JPBunkaNoHi = 
function(year) {
    ans = year*10000 + 1103
    timeDate(as.character(ans)) }
    
JPThanksgivingDay = 
function(year) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPKinrouKanshaNoHi = 
function(year) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPEmperorsBirthday = 
function(year) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPTennouTanjyouBi = 
function(year) {
    ans = year*10000 + 1123
    timeDate(as.character(ans)) }
    
JPBankHolidayDec31 = 
function(year) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }

    
################################################################################

