\name{listFinCenter}


\title{List of Financial Centers}


\alias{listFinCenter}


\description{

     Lists supported financial centers. 

}


\usage{
listFinCenter(pattern = ".*")
}


\arguments{

    \item{pattern}{
        a pattern character string as required
        by the \code{\link{grep}} function.
        }

}


\value{

    returns a list of supported financial centers.

}


\details{


    The function \code{rulesFinCenter}, lists the daylight saving rules 
    for a selected financial center.
    
    There is no dependency on the POSIX implementation of your operating
    system because all time zone and day light saving time information 
    is stored locally in ASCII files. 
    
}


\examples{
## myFinCenter - the global setting currently used:
   myFinCenter

## Other Financial Centers:
   listFinCenter("Asia/")
   listFinCenter("^A")    # all beginning with "A"
   listFinCenter("^[^A]") # all *not* beginning with "A"
   listFinCenter(".*/L")  # cities with L*

   stopifnot(identical(sort(listFinCenter()), ## 'A' and 'not A' == everything:
        sort(union(listFinCenter("^A"),
            listFinCenter("^[^A]")))))
}


\keyword{chron}

