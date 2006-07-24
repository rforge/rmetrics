\name{HolidayCalendars}

\alias{HolidayCalendars}

\alias{holiday}

\alias{holidayNYSE}


\title{Holiday Calendars and Utilities}


\description{
    
    A collection and description of functions and 
    methods dealing with holiday dates and holiday
    calendars. 
    \cr
    
    The functions for the generation of holiday calendars are: 
      
    \tabular{ll}{   
    \code{holiday} \tab Returns a holiday date of G7 and CH as 'sdate', \cr
    \code{holidayNYSE} \tab Computes 'timeDate' object for full-day NYSE holidays, \cr
    \code{holidayZURICH} \tab Computes 'timeDate' object for Zurich holidays. }       
                
}


\usage{
holiday(year, Holiday = Easter)
holidayNYSE(year = currentYear)
holidayZURICH(year = currentYear)
}


\arguments{

    \item{Holiday}{
        the unquoted function name of an ecclestial or public holiday 
        in the G7 countries or Switzerland, see the list below.
        }
    \item{year}{
        an integer vector of years, [CCYY].
        [day.of.week] - cr
        vector of integer year numbers.
        }
        
}


\value{

    \code{holiday}\cr
    \code{holidayNYSE}
    \code{holidayZURICH}
    \cr
    return an ISO-8601 formatted calendar date \code{CCYYMMDD}, an
    object of class \code{"sdate"} which represents an integer
    value or vector. 

}


\details{

    \bold{Holiday Calendar:}
    \cr\cr
    Easter is the central ecclestial holiday. Many other holidays
    are related to this feast. The function \code{easter} computes
    the dates of easter and related ecclestial holidays for the 
    requested year vector. \code{holiday} calculates the dates of 
    ecclestial or publich holidays in the G7 countries, e.g. 
    \code{holiday(2003, "GoodFriday")}. \code{holidays.db} is our 
    Data Base of World Wide Holidays. The database contains holiday 
    functions automatically loaded by the \code{fBasics} library at 
    startup time. The user can add additional holiday functions to 
    this data base. The information for the holiday data base were 
    collected from several web pages about holiday calendars.
    The following ecclestial and public \code{[HOLIDAY]} functions 
    in the G7 countries and Switzerland are available: 
    \cr\cr
    
    \emph{Holidays Related to Easter:} 
    \cr
    Septuagesima, Quinquagesima, AshWednesday, PalmSunday,
    GoodFriday,  EasterSunday, Easter, EasterMonday, 
    RogationSunday, Ascension, Pentecost, PentecostMonday, 
    TrinitySunday CorpusChristi. 
     
    \emph{Holidays Related to Christmas:} 
    \cr
    ChristTheKing, Advent1st, Advent1st, Advent3rd, 
    Advent4th, ChristmasEve, ChristmasDay, BoxingDay, 
    NewYearsDay. 
    
    \emph{Other Ecclestical Feasts:} 
    \cr
    SolemnityOfMary, Epiphany, PresentationOfLord, 
    Annunciation, TransfigurationOfLord, AssumptionOfMary, 
    AssumptionOfMary, BirthOfVirginMary, CelebrationOfHolyCross, 
    MassOfArchangels, AllSaints, AllSouls. 
    
    \emph{CHZurich - Public Holidays:} 
    \cr
    CHBerchtoldsDay, CHSechselaeuten, CHAscension, 
    CHConfederationDay, CHKnabenschiessen. 
    
    \emph{GBLondon - Public Holidays:} 
    \cr
    GBMayDay, GBBankHoliday, GBSummerBankHoliday, 
    GBNewYearsEve.

    \emph{DEFrankfurt - Public Holidays:} 
    \cr
    DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve,
    DENewYearsEve. 
    
    \emph{FRParis - Public Holidays:} 
    \cr
    FRFetDeLaVictoire1945, FRAscension, FRBastilleDay, 
    FRAssumptionVirginMary, FRAllSaints, FRArmisticeDay. 
    
    \emph{ITMilano - Public Holidays:} 
    \cr
    ITEpiphany, ITLiberationDay, ITRepublicAnniversary, 
    ITAssumptionOfVirginMary, ITAllSaints, ITWWIVictoryAnniversary, 
    ITStAmrose, ITImmaculateConception. 
    
    \emph{USNewYork/USChicago - Public Holidays:} 
    \cr
    USNewYearsDay, USInaugurationDay, USMLKingsBirthday, 
    USLincolnsBirthday, USWashingtonsBirthday, USMemorialDay, 
    USIndependenceDay, USLaborDay,  USColumbusDay, USElectionDay, 
    USVeteransDay, USThanksgivingDay, USChristmasDay, 
    USCPulaskisBirthday, USGoodFriday. 
    
    \emph{CAToronto/CAMontreal - Public Holidays:} 
    \cr
    CAVictoriaDay, CACanadaDay, CACivicProvincialHoliday, 
    CALabourDay, CAThanksgivingDay, CaRemembranceDay. 
    
    \emph{JPTokyo/JPOsaka - Public Holidays:} 
    \cr
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
    JPTennou-tanjyou-bi, JPTennou-tanjyou-bi. 
    
    All the holiday functions are listed in the data file \code{holidays.R}.
    Additional holidays, which are not yet available there, can be added
    to this data base file.

}


\references{

ISO-8601 (1988); 
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

}


\examples{
## SOURCE("fCalendar.5B-HolidayCalendars")

## easter -
   # Dates for GoodFriday from 2000 until 2010:                     
   holiday(2000:2010, "GoodFriday")                  
   
## holidays -   
   Easter(2000:2010)                                
   GoodFriday(2000:2010)
  
## holidayNYSE -
   holidayNYSE(currentYear)
   
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

