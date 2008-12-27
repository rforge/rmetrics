\name{timeDate}


\title{'timeDate' Objects from Scratch}


\alias{timeDate}
\alias{timeDate,ANY-method}
\alias{timeDate,character-method}
\alias{timeDate,Date-method}
\alias{timeDate,missing-method}
\alias{timeDate,numeric-method}
\alias{timeDate,POSIXt-method}
\alias{timeDate,timeDate-method}
\alias{initialize,timeDate-method}

\alias{strptimeDate}


\description{

    Create a  'timeDate' object from scratch using a character vector.

}


\usage{
timeDate(charvec, format = NULL, zone = "", FinCenter = "")
    
strptimeDate(x, format = whichFormat(x), tz = "")
}


\arguments{

    \item{charvec}{
        a character string or vector of dates and times.
        }
    \item{format}{
        the format specification of the input character vector.
        }
    \item{tz}{
        a character with the the location of the financial center 
        named as \code{"continent/city"}, or short \code{"city"}.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{x}{
        a character string or vector of dates and times.
        }
    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }

}


\value{

    returns an object of class \code{timeDate}.

}


\examples{
## timeDate -

   # Character Vector Strings:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")

   dts ; tms

   t1 <- timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
   t1
   
   stopifnot(identical(t1, timeDate(dts, FinC = "GMT"))) # auto-format
   
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")
   
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
        zone = "GMT", FinCenter = "GMT")
   
   timeDate(paste(dts, tms), 
        zone = "Europe/Zurich", FinCenter = "Europe/Zurich")
   
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
        zone = "GMT", FinCenter = "Europe/Zurich")

   ## non standard format:
   timeDate(paste(20:31, "03.2005", sep="."), format = "\%d.\%m.\%Y")

   ## Note, ISO and American Formats are Auto-Detected:
   timeDate("2004-12-31", FinCenter = "GMT")
   timeDate("12/11/2004", FinCenter = "GMT")
   timeDate("1/31/2004") # auto-detect American format

   ## from POSIX?t, and using NAs
   lsec <- as.POSIXlt(.leap.seconds) ; lsec[c(2,4:6)] <- NA
   timeDate(lsec)

   dtms <- paste(dts,tms)
   dtms[2:3] <- NA
   timeDate(dtms, FinCenter = "Europe/Zurich")# but in GMT

   % would need  change in R :
   %    tms[3] <- dts[2] <- NA
   %    timeDate(paste(dts,tms), FinCenter = "Europe/Zurich")# but in GMT

}


\keyword{chron}

