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
        an object of class \code{timeDate}.
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
## SOURCE("fCalendar.021A-ISO8601Standard")

}


\keyword{programming}

