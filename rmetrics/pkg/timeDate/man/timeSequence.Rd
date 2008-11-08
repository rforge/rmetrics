\name{timeSequence}


\title{Regularly spaced 'timeDate' objects}


\alias{timeSequence}
\alias{seq.timeDate}


\description{

    Create a regularly spaced object of class 'timeDate'. 

}


\usage{
timeSequence(from, to = Sys.timeDate(), 
    by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
    length.out = NULL, format = NULL,
    zone = "", FinCenter = "")

\method{seq}{timeDate}(from, to, by, length.out = NULL, \dots)
}


\arguments{

    \item{from, to}{
        starting date, required, and end date, optional. If supplied
        \code{to} must be after \code{from}.
        }
    \item{by}{
        a character string, containing one of "sec", "min",
        "hour", "day", "week", "month" or "year".
        This can optionally be preceded by an integer and a
        space, or followed by "s".
        }       
    \item{length.out}{
        length.out integer, optional. Desired length
        of the sequence, if specified "to" will be ignored.
        }    
    \item{format}{
        the format specification of the input character vector.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }
    \item{\dots}{
        arguments passed to other methods.
        }    

}


\note{

     \code{seq} is a synonyme generic function for \code{timeSequence}.
     
}


\value{

    returns a S4 object of \code{\link{class}} \code{"timeDate"}.
      
}


\examples{
## timeSequence -
   
   timeSequence(from = "2004-03-12", to = "2004-04-11",
        format = "\%Y-\%m-\%d", FinCenter = "GMT")
        
   timeSequence(from = "2004-03-12", to = "2004-04-11",
        format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")
}


\keyword{chron}

