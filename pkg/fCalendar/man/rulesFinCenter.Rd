\name{rulesFinCenter}


\title{Financial Centers DST Rules}


\alias{rulesFinCenter}


\description{

    Returns DST rules for a financial center.

}


\usage{
rulesFinCenter(FinCenter = myFinCenter)
}


\arguments{

    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }
        
}


\value{

    returns a list of time zones and DST rules available in the database.

}


\details{


    The function \code{rulesFinCenter}, lists the daylight saving rules 
    for a selected financial center.
    
    There is no dependency on the POSIX implementation of your operating
    system because all time zone and day light saving time information 
    is stored locally in ASCII files. 
    
}


\examples{
## rulesFincenter - 
   rulesFincenter(Zurich)
}


\keyword{chron}

