\name{HolidayCalendars}

\alias{HolidayCalendars}

\alias{easter}
\alias{holiday}

\alias{holiday.NYSE}

\alias{on.or.after}
\alias{on.or.before}
\alias{nth.of.nday} 
\alias{last.of.nday}

\alias{sjulian}
\alias{sdate}
\alias{sday.of.week}
\alias{sleap.year}
\alias{print.sdate}

\alias{fjulian}
\alias{.julian}
\alias{month.day.year}
\alias{leap.year}
\alias{day.of.week}


\title{Holiday Calendars and Utilities}


\description{
    
    A collection and description of functions and 
    methods dealing with holiday calendars. Included 
    are functions for the computation of the date of 
    easter and related feasts. These functions allow 
    the generation of holiday calendars. Included are 
    some utility functions to compute special dates 
    from n-dates, and to manage dates formatted in 
    the ISO-8601 integer CCYYMMDD calendar format. 
    Furthermore, functions are available to convert 
    Julian counts to formatted calendar dates and  
    vice versa. These are SPlus like functions.
    \cr
    
    The functions for the generation of holiday calendars are: 
      
    \tabular{ll}{   
	\code{easter} \tab Returns date of easter or related feasts as 'sdate'-s, \cr
	\code{holiday} \tab Returns a holiday date of G7 and CH as 'sdate', \cr
	\code{holiday.NYSE} \tab Computes 'timeDate' object for full-day NYSE holidays. }       
    	
    The N-Day functions are: 
      
    \tabular{ll}{ 		
	\code{on.or.after} \tab Date in month that is a "n-day" on-or-after a "date", \cr
	\code{on.or.before} \tab Date in month that is a "n-day" on-or-before a "date", \cr
	\code{nth.of.nday} \tab The "n-th" ocurrance of a "n"day in year/month, \cr
	\code{last.of.nday} \tab The last "n"day in a given year/month. }
            
    The ISO-8601 day functions and methods are: 
      
    \tabular{ll}{ 	       
	\code{sjulian} \tab Julian day counts from ISO formatted dates, \cr
	\code{sdate} \tab ISO-8601 formatted dates from Julian day counts, \cr
	\code{sday.of.week} \tab day of week from \code{CCYYMMDD} formatted dates, \cr
	\code{sleap.year} \tab Decides if \code{CCYYMMDD} is a leap year or not, \cr
	\code{print} \tab S3 Print method for objects of class \code{sdate}. }           
            
    SPlus like Functions are:    
    
    \tabular{ll}{   
	\code{fjulian} \tab Calculates Julian counts from different formats, \cr
	\code{.julian} \tab Converts formatted calendar dates to Julian counts, \cr
	\code{month.day.year} \tab Returns months, days, and years from Julian counts, \cr
	\code{leap.year} \tab Deciding if a year is a leap year or not, \cr
	\code{day.of.week} \tab Determines the day of week, starting with Sunday as 0. }
                
}


\usage{
easter(year, shift = 0)
holiday(year, Holiday = Easter)
holiday.NYSE(y = currentYear)

on.or.after(year, month, day, nday)
on.or.before(year, month, day, nday)
nth.of.nday(year, month, nday, nth) 
last.of.nday(year, month, lastday, nday)

sjulian(sdates, origin = 19600101)
sdate(julians, origin = 19600101)
sday.of.week(sdates)
sleap.year(sdates)

\method{print}{sdate}(x, \dots)

fjulian(fdates, origin = 19600101, order = 'mdy', cc = NULL, swap = 20)
.julian(m, d, y, origin = c(month = 1, day = 1, year = 1960))

month.day.year(jul, origin = c(month = 1, day = 1, year = 1960))
leap.year(y) 
day.of.week(month, day, year)
}


\arguments{

    \item{cc, swap}{
    	[fjulian] - \cr
		originally a numeric value denoting the century. Is now 
		obsolete with the introduction of \code{swap}. \code{swap}
		is an integer value which determines when we swap from the
		19th to 20th century, by default 20, i.e. we swap 1920.
		}
	\item{day}{ 
        an integer vector of day numbers, [DD].
        [day.of.week] - cr
        vector of integer day numbers.
        }
    \item{fdates}{
		[fjulian] - \cr
		a character vector of formatted Gregorian dates.
		}
	\item{Holiday}{
        the unquoted function name of an ecclestial or public holiday 
        in the G7 countries or Switzerland, see the list below.
        }
    \item{jul}{
		[month.day.year] - \cr
		vector of Julian dates (the number of days since origin).
		} 
	\item{julians}{
        a numeric vector of Julian Day Counts.
        }
    \item{lastday}{
        the last day in the calendar month.
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
    \item{nday}{
        an integer vector with entries ranging from 
        \code{0} (Sunday) to \code{6} (Saturday).
        }
    \item{nth}{
        an integer vector numbering the n-th occurence.
        }
    \item{order}{
    	[fjulian] - \cr
		a character string, indicating the order of year, month 
		and day.
		}
    \item{origin}{
        the origin date of the counter, in ISO-8601 date format,
        [CCYYMMDD]. By default Januar 1st, 1960.
        }
    \item{sdates}{
        a numeric vector of ISO-8601 formatted Gregorian dates,
        [CCYYMMDD].
        }
    \item{shift}{
        a numeric value, the n-day shift. By default the shift is
        set to zero and the function returns the date of Easter.
        }
    \item{x}{
        an object of class \code{"sdate"}. This is an integer
        value or integer vector formatted as ISO-8601 CCYYMMDD.
        }
    \item{year}{
        an integer vector of years, [CCYY].
        [day.of.week] - cr
        vector of integer year numbers.
        }
    \item{\dots}{
        additional optional arguments to be passed.     
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

    \code{on.or.after}\cr
    \code{on.or.before}\cr
    \code{nth.of.nday}\cr
    \code{last.of.nday}
    \cr
    return like the holiday functions an ISO-8601 formatted calendar 
    date integer value or vector.
    \cr
    
    \code{sjulian}\cr
    \code{sdates}\cr
    \code{sday.of.week}\cr
    \code{sleap.year}
    \cr
    the Julian day counter returns a numeric vector of Julian day counts,
    \code{sdates} returns a numeric vector of ISO-8601 formatted dates, 
    i.e [CCYYMMDD], the day of the week function returns a numeric vector 
    with entries  between \code{0} (Sunday) and \code{6} (Saturday) and
    the leap year function returns a logical vector with entries TRUE or 
    FALSE, weather the date falls in a leap year or not.
    \cr
    
    \code{fjulian} 
  	\cr
  	returns a numeric vector of Julian Day Counts.
	When \code{julian} is called, a vector of Julian counts corresponding 
	to the input values is returned. If \code{month.day.year} is called, 
	a list with members \code{month}, \code{day}, and \code{year} is 
	returned for the given Julian dates. 
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
    \cr\cr

    
    \bold{n-Day Functions:}
    \cr\cr
    The function \code{on.or.after}
    calculates the date in a given month and year that is a n-day 
    ON OR AFTER a given date.
        
    The function \code{on.or.before}
    calculates the date in a given month and year that is a n-day 
    ON OR BEFORE a given date. 
      
    The function \code{nth.of.nday}
    calculates the n-th ocurrance of a n-day in a given month and 
    year. 
       
    The function code{last.of.nday}
    calculates the last n-day in a given month and year.
    \cr\cr
       
    \bold{sdate Class:}
    \cr\cr   
    \code{"sdates"} represents a class of integer formatted \code{CCYYMMDD}
    ISO-8601 calendar dates.
    Note, that the \code{s*} indicates "standard" or "simple" ISO-8601 
    date format where \code{CC} denotes the century, \code{YY} the year, 
    \code{MM} the month, and \code{DD} the day, e.g 20031213. This
    formated was included to the \code{fbasics} library already some
    years ago. It is likely that this date format will be replaced 
    in the near future by the new R objects of class. \code{"Date"}.

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
    
Therneau T. (1991); 
	\emph{S-plus Date Routines}, 
	www.statlib.org

}


\examples{
## SOURCE("fCalendar.25B-HolidayCalendars")

## easter -
   xmpBasics("\nStart: Easter and Easter Related Feasts > ")
   # Dates for Easter and Pentecost from 2000 until 2010:
   easter(2000:2010)
   easter(2000:2010, -2) # This is Good Friday
   holiday(2000:2010, Easter)
   holiday(2000:2010, GoodFriday)  
   
## holidays -   
   xmpBasics("\nNext: Alternatively You can use > ")
   Easter(2000:2010)
   GoodFriday(2000:2010)
  
## holiday.NYSE -
   xmpBasics("\nNext: Holiday Calendar for the New York Stock Exchange > ")
   holiday.NYSE(currentYear)
   
## on.or.after -
   xmpBasics("\nStart: on.or.after > ")
   # What date had the first advent 2001?
   # The first advent is the Sunday on or after November 27th.
   on.or.after(year = 2001, month = 11, day = 27, nday = 0)
   
## on.or.before -
   xmpBasics("\nNext: on.or.before > ")
   # What date had Monday on or after April, 1st, 2001?
   on.or.before(year = 2001, month = 11, day = 27, nday = 1)
   
## nth.of.nday -
   xmpBasics("\nNext: nth.of.nday > ")
   # What date had the second Sunday in October 1980?
   nth.of.nday(year = 1980, month = 10, nday = 0, nth = 2)
  
## last.of.nday -
   xmpBasics("\nNext: last.of.nday > ")   
   # What date had the last Monday in September, 2002?
   last.of.nday(year = 1996, month = 9, lastday = 31, nday = 1)  
   
## fdates - 
## fjulian -
   xmpBasics("\nStart: Convert Formatted Gregorian Dates > ")
   fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
   fdates
   fjulian(fdates) 
   
## fdates - 
## fjulian -
   xmpBasics("\nNext: Convert with 'dmy' Order > ")
   fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
   fjulian(fdates, order = 'dmy')
   
## .julian - 
## day.of.week -
   xmpBasics("\nNext: Day of Week > ")
   # The number of days from January 1, 1990 to each of:
   # January 15, 1990, February 15, 1991, March 15, 1992, etc.
   .julian(1:12, rep(15,12), 1990+(0:11), origin = c(1, 1, 1990))
   # November 12, 98, was a Wednesday.
   day.of.week(m = 11, d = 12, y = 98)
}


\author{

    Therry Therneau for the C program used by \code{fjulian},\cr
	Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

