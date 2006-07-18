\name{HolidayCalendars}

\alias{HolidayCalendars}

\alias{easter}
\alias{holiday}

\alias{holiday.NYSE}

%\alias{on.or.after}
%\alias{on.or.before}
%\alias{nth.of.nday} 
%\alias{last.of.nday}

%\alias{sjulian}
%\alias{sdate}
%\alias{sday.of.week}
%\alias{sleap.year}
%\alias{print.sdate}

%\alias{fjulian}
\alias{.julian}
\alias{month.day.year}
\alias{leap.year}
\alias{day.of.week}


\title{Holiday Calendars and Utilities}


\description{
    
    A collection and description of functions and 
    methods dealing with holiday dates and holiday
    calendars. 
    \cr
    
    The functions for the generation of holiday calendars are: 
      
    \tabular{ll}{   
    \code{easter} \tab Returns date of easter or related feasts as 'sdate'-s, \cr
    \code{holiday} \tab Returns a holiday date of G7 and CH as 'sdate', \cr
    \code{holiday.NYSE} \tab Computes 'timeDate' object for full-day NYSE holidays. }       
        
    SPlus like Functions are:    
    
    \tabular{ll}{   
    %\code{fjulian} \tab Calculates Julian counts from different formats, \cr
    \code{.julian} \tab Converts formatted calendar dates to Julian counts, \cr
    \code{month.day.year} \tab Returns months, days, and years from Julian counts, \cr
    \code{leap.year} \tab Deciding if a year is a leap year or not, \cr
    \code{day.of.week} \tab Determines the day of week, starting with Sunday as 0. }
                
}


\usage{
easter(year, shift = 0)
holiday(year, Holiday = Easter)
holiday.NYSE(y = currentYear)

.julian(m, d, y, origin = c(month = 1, day = 1, year = 1960))
month.day.year(jul, origin = c(month = 1, day = 1, year = 1960))
leap.year(y) 
day.of.week(month, day, year)
}


\arguments{

    \item{day}{ 
        an integer vector of day numbers, [DD].
        [day.of.week] - cr
        vector of integer day numbers.
        }
    \item{Holiday}{
        the unquoted function name of an ecclestial or public holiday 
        in the G7 countries or Switzerland, see the list below.
        }
    \item{jul}{
        [month.day.year] - \cr
        vector of Julian dates (the number of days since origin).
        } 
    \item{m, d, y}{
        [.julian] - \cr
        vector of month, day and year numbers. The years should be 
        entered in their full four-digit form; it is assumed that 
        two-digit years refer to the first century.
        }       
    \item{month}{
        an integer vector of month numbers, [MM].\cr
        [day.of.week] - cr
        vector of integer month numbers.
        }
    \item{origin}{
        the origin date of the counter, in ISO-8601 date format,
        [CCYYMMDD]. By default Januar 1st, 1960.
        }
    \item{shift}{
        a numeric value, the n-day shift. By default the shift is
        set to zero and the function returns the date of Easter.
        }
    \item{year}{
        an integer vector of years, [CCYY].
        [day.of.week] - cr
        vector of integer year numbers.
        }
        
}


\value{

    \code{easter}\cr
    \code{holiday}\cr
    \code{holiday.NYSE}
    \cr
    return an ISO-8601 formatted calendar date \code{CCYYMMDD}, an
    object of class \code{"sdate"} which represents an integer
    value or vector.
    \cr
    
    \code{leap.year} 
    \cr
    returns a logical vector indicating whether the input years are leap 
    years or not.
    \cr
    
    \code{day.of.week} 
    \cr
    returns a number between 0 and 6 that specifies the day of the week 
    of the given date; 0 refers to Sunday. 

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


\note{

    Most of the functions were written for R Version 1.5. With the new
    \code{Date Class} becoming available in R 1.9.0 these functions will 
    undergo a revision in the near future.
    
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
   # Dates for Easter and Pentecost from 2000 until 2010:
   easter(2000:2010)
   easter(2000:2010, -2) # This is Good Friday
   holiday(2000:2010, "Easter")                      
   holiday(2000:2010, "GoodFriday")                  
   
## holidays -   
   Easter(2000:2010)                                
   GoodFriday(2000:2010)
  
## holiday.NYSE -
   holiday.NYSE(currentYear)
   
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

