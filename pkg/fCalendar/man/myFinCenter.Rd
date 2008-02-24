\name{myFinCenter}


\title{myFinCenter Variable}


\alias{myFinCenter}


\description{

    A character string with the name of my 
    financial center. 
    
}


\usage{
myFinCenter
}


\note{

    Can be modified by the user to his own or any
    other financial center. The default is "GMT".
    To list all supported financial center use the
    function \code{listFinCenter}.
   
}


\value{

    returns the name of myFinCenter.

}


\seealso{

    \code{\link{listFinCenter}}
    
}


\examples{
## myFinCenter - the global setting currently used:
   myFinCenter
   
## Change to another Financiel Center:
   # myFinCenter = "Zurich"
   
## Do not take care about DST ...
   # myFinCenter = "GMT" 
}


\keyword{chron}

