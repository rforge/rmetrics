\name{listHolidays}


\alias{listHolidays}


\title{List of Holidayss}


\description{
    
    Returns the list of holidays.     
                
}


\usage{
listHolidays(pattern = ".*") 
}


\arguments{

    \item{pattern}{
        a pattern character string as required
        by the \code{\link{grep}} function.
        }

}


\value{

    returns a list of holidays as a character vector.

}


\examples{
## listHolidays -

   # All Holidays:
   listHolidays()
   
   # Local Swiss Holidays:
   listHolidays("CH") 
}

\keyword{chron}

