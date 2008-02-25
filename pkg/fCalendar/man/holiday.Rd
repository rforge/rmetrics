\name{holiday}

\alias{holiday}

\title{Holiday Dates}


\description{
    
    Returns the date of a holiday. 
               
}


\usage{
holiday(year = currentYear, Holiday = "Easter")
}


\arguments{

    \item{Holiday}{
        the unquoted function name of an ecclestial or public holiday 
        in the G7 countries or Switzerland, see the list below.
        }
    \item{year}{
        an integer value or vector of years, formatted as \code{YYYY}.
        }
        
}


\value{

    The function \code{holiday} returns an object of class
    \code{timeDate}. 

}


\details{

    Easter is the central ecclestial holiday. Many other holidays
    are related to this feast. The function \code{Easter} computes
    the dates of Easter and related ecclestial holidays for the 
    requested year vector. \code{holiday} calculates the dates of 
    ecclestial or publich holidays in the G7 countries, e.g. 
    \code{holiday(2003, "GoodFriday")}. Rmetrics contains holiday 
    functions automatically loaded at startup time. The user can 
    add easily additional holiday functions. The information for 
    the holidays is collected from several web pages about holiday 
    calendars.
    The following ecclestial and public \code{[HOLIDAY]} functions 
    in the G7 countries and Switzerland are available: \cr
    
    
    \emph{Holidays Related to Easter:} 

    Septuagesima, Quinquagesima, AshWednesday, PalmSunday,
    GoodFriday,  EasterSunday, Easter, EasterMonday, 
    RogationSunday, Ascension, Pentecost, PentecostMonday, 
    TrinitySunday CorpusChristi. \cr 
     
    
    \emph{Holidays Related to Christmas:} 

    ChristTheKing, Advent1st, Advent1st, Advent3rd, 
    Advent4th, ChristmasEve, ChristmasDay, BoxingDay, 
    NewYearsDay. \cr
    
    
    \emph{Other Ecclestical Feasts:} 

    SolemnityOfMary, Epiphany, PresentationOfLord, 
    Annunciation, TransfigurationOfLord, AssumptionOfMary, 
    AssumptionOfMary, BirthOfVirginMary, CelebrationOfHolyCross, 
    MassOfArchangels, AllSaints, AllSouls. \cr
    
    
    \emph{CHZurich - Public Holidays:} 

    CHBerchtoldsDay, CHSechselaeuten, CHAscension, 
    CHConfederationDay, CHKnabenschiessen. \cr
    
    
    \emph{GBLondon - Public Holidays:} 

    GBMayDay, GBBankHoliday, GBSummerBankHoliday, 
    GBNewYearsEve. \cr

    
    \emph{DEFrankfurt - Public Holidays:} 

    DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve,
    DENewYearsEve. \cr
    
    
    \emph{FRParis - Public Holidays:} 
    
    FRFetDeLaVictoire1945, FRAscension, FRBastilleDay, 
    FRAssumptionVirginMary, FRAllSaints, FRArmisticeDay. \cr
    
    
    \emph{ITMilano - Public Holidays:} 

    ITEpiphany, ITLiberationDay, ITRepublicAnniversary, 
    ITAssumptionOfVirginMary, ITAllSaints, ITWWIVictoryAnniversary, 
    ITStAmrose, ITImmaculateConception. \cr
    
    
    \emph{USNewYork/USChicago - Public Holidays:} 

    USNewYearsDay, USInaugurationDay, USMLKingsBirthday, 
    USLincolnsBirthday, USWashingtonsBirthday, USMemorialDay, 
    USIndependenceDay, USLaborDay,  USColumbusDay, USElectionDay, 
    USVeteransDay, USThanksgivingDay, USChristmasDay, 
    USCPulaskisBirthday, USGoodFriday. \cr 
    
    
    \emph{CAToronto/CAMontreal - Public Holidays:} 

    CAVictoriaDay, CACanadaDay, CACivicProvincialHoliday, 
    CALabourDay, CAThanksgivingDay, CaRemembranceDay. \cr
    
    
    \emph{JPTokyo/JPOsaka - Public Holidays:} 

    JPNewYearsDay, JPGantan, JPBankHolidayJan2, JPBankHolidayJan3,
    JPComingOfAgeDay, JPSeijinNoHi, JPNatFoundationDay,
    JPKenkokuKinenNoHi, JPGreeneryDay, JPMidoriNoHi, 
    JPConstitutionDay, JPKenpouKinenBi, JPNationHoliday, 
    JPKokuminNoKyujitu, JPChildrensDay, JPKodomoNoHi, 
    JPMarineDay, JPUmiNoHi, JPRespectForTheAgedDay,
    JPKeirouNoHi, JPAutumnalEquinox, JPShuubun-no-hi, 
    JPHealthandSportsDay, JPTaiikuNoHi, JPNationalCultureDay, 
    JPBunkaNoHi, JPThanksgivingDay, JPKinrouKanshaNohi, 
    JPKinrou-kansha-no-hi, JPEmperorsBirthday,
    JPTennou-tanjyou-bi, JPTennou-tanjyou-bi. \cr
    
}


\examples{
## holiday -
   # Dates for GoodFriday from 2000 until 2010:                     
   holiday(2000:2010, "GoodFriday")                  
   
## Easter -   
   Easter(2000:2010)
   
## GoodFriday -                           
   GoodFriday(2000:2010)
   Easter(2000:2010, -2) 
}


\keyword{chron}

