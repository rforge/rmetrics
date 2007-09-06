\name{holiday}

\alias{holiday}

\title{Holidays}


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

    \bold{Holidays:}
    \cr\cr
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
    
}


\references{

ISO-8601 (1988); 
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## easter -
   # Dates for GoodFriday from 2000 until 2010:                     
   holiday(2000:2010, "GoodFriday")                  
   
## holidays -   
   Easter(2000:2010)                                
   GoodFriday(2000:2010)
}


\keyword{chron}

