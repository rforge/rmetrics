\name{Easter}


\alias{Easter}


\title{Date of Easter}


\description{
    
    Returns the date of Easter.
                
}


\usage{
Easter(year = currentYear, shift = 0)
}


\arguments{

    \item{year}{
        an integer value or integer vector for the year(s).
        }
    \item{shift}{
        an integer value, the number of days shifted from the Easter
        date. Negative integers are allowed.
        }

}


\details{

    Holidays may have two origins, ecclesiastical and public/federal. 
    The ecclesiastical calendars of Christian churches are based on 
    cycles of moveable and immoveable feasts. Christmas, December 25th, 
    is the principal immoveable feast. Easter is the principal moveable 
    feast, and dates of most other moveable feasts are determined with 
    respect to Easter. 
    
    The date of Easter is evaluated by a complex procedure whose detailed 
    explanation goes beyond this description. The reason that the calculation 
    is so complicate is, because the date of Easter is linked to (an 
    inaccurate version of) the Hebrew calendar. But nevertheless a short 
    answer to the question "When is Easter?" is the following: Easter 
    Sunday is the first Sunday after the first full moon after vernal 
    equinox. For the long answer we refer to Toendering (1998).
    
    The algorithm computes the date of Easter based on the algorithm of 
    Oudin (1940). It is valid for any Gregorian Calendar year.

}


\value{

    returns the date of Easter as an object of class \code{timeDate}.

}


\examples{
## Easter -

   # Current Year:
   Easter()
   
   # From 2001 to 2010:
   Easter(2001-2010)
}


\keyword{chron}

