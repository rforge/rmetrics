\name{ISO8601Standard}

\alias{ISO8601Standard}

\alias{isISO8601}


\title{Summary of ISO8601 Standard}


\description{
        
    A summary and description how Rmetrics uses the
    ISO8601 standard.
    \cr
    
    \tabular{ll}{
    \code{isISO8601} \tab test if the time/date is ISO8601 formatted. }
                        
}


\usage{
isISO8601(x)
}


\arguments{
  
    \item{x}{
        an argument to be checked.
        }
        
}


\value{

    \code{isISO8601}
    \cr
    returns \code{TRUE} if \code{x} is ISO8601 formated, i.e. as
    \code{"\%Y-\%m-\%d \%H:\%M:\%S"}, as
    \code{"\%Y-\%m-\%d"}, as
    \code{"\%Y\%m\%d\%H\%M\%S"}, or as
    \code{"\%Y\%m\%d"}. 
    \cr
    
}


\author{

    Diethelm Wuertz for this summary.
    
}


\examples{
## SOURCE("fCalendar.1A-ISO8601Standard")

## isISO8601 -
    isISO8601(c("2007-01-01", "2007-12-31" ))
    # Don't print control attribute:
    isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    # Nin-ISO8601 %b Format:
    isISO8601("2007-Jan-01")[[1]]
    isISO8601("2007-01-01 15:00:000")[[1]]
}


\keyword{programming}

