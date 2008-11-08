\name{myFinCenter}


\title{myFinCenter Variable}


\alias{myFinCenter}


\description{

    A character string with the name of my 
    financial center. 
    
}



\note{

    Can be modified by the user to his own or any
    other financial center. The default is "GMT".
    To list all supported financial center use the
    function \code{listFinCenter}.
   
}


\seealso{

    \code{\link{listFinCenter}}
    
}


\examples{
## myFinCenter - the global setting currently used:
   getRmetricsOptions("myFinCenter")
   
## Change to another Financiel Center:
   # setRmetricsOptions(myFinCenter = "Zurich")
   
## Do not take care about DST ...
   # setRmetricsOptions(myFinCenter = "GMT") 
}


\keyword{chron}

