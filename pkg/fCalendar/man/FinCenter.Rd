\name{TimeDateFinCenter}
\title{timeDate Financial Centers}
%
\alias{TimeDateFinCenter}


\alias{myFinCenter}
\alias{rulesFinCenter}
\alias{listFinCenter}


\description{

    A collection and description of functions to use 
    financial centers in contex with timeDate objects.
    \cr

    The functions for Financial Centers are:

    \tabular{ll}{
    \code{rulesFinCenter} \tab Returns DST rules for a financial center, \cr
    \code{listFinCenter} \tab Lists all supported financial centers. }

}


\usage{
myFinCenter

rulesFinCenter(FinCenter = myFinCenter)
listFinCenter(pattern = ".*")
}


\arguments{

    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }
    \item{pattern}{
        a pattern character string as required
        by the \code{\link{grep}} function.
        }

}


\value{

    \code{rulesFinCenter}
    \cr
    returns a printed list of DST rules.
    \cr
    
    \code{listFinCenter}
    \cr
    returns a list of time zones available in the database.

}


\details{

    \bold{Financial Centers}
    \cr\cr
    There are two functions concerned with the financial centers. The
    first, \code{rulesFinCenter}, lists the daylight saving rules for a
    selected financial center, and the second, \code{listFinCenter},
    lists all centers available in the database. There is no dependency
    on the POSIX implementation of your operating system because all
    time zone and day light saving time information is stored locally
    in ASCII files. It is important to say, that the \code{TZ} environment
    variable must set to \code{"GMT"} in your system environment that
    there are no conflicts with the POSIX time zone management.

    Through the definition of financial centers it becomes possible to
    introduce in the future a specification structure for financial
    centers, which includes further information for a center like
    holiday calendars, currency and interest rate conventions or many
    others.

}


\seealso{

    \code{timeDateCoercion}, \code{timeDateSpecDates}

    We also recommend to inspect the help pages for the POSIX time and 
    date class, \code{\link{Dates}}, and the help pages from the contributed 
    R packages \code{chron} and \code{date}.
    
}


\author{
    Diethelm Wuertz for the Rmetrics \R-port; some improvements: % bug fixes
    Martin Maechler.
}


\examples{
## myFinCenter - the global setting currently used:
   myFinCenter

## Other Financial Centers:
   listFinCenter("Asia/")
   listFinCenter("^A") # all beginning with "A"
   listFinCenter("^[^A]") # all *not* beginning with "A"
   listFinCenter(".*/L")  # cities with L*

   stopifnot(identical(sort(listFinCenter()), ## 'A' and 'not A' == everything:
        sort(union(listFinCenter("^A"),
            listFinCenter("^[^A]")))))
}


\keyword{chron}

