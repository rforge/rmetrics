\name{print-methods}


\title{Print a Time Series}


\alias{show,timeSeries-method}
\alias{print,timeSeries-method}

\description{
    
    Print 'timeSeries' pbjects.
    
}


% \usage{
% show.timeSeries(object)
% }


\arguments{

    \item{object}{  
        an object of class \code{timeSeries}.
        }
        
}


\value{

    prints an object of class \code{timeSeries}.
        
}


\examples{
## print -
   setRmetricsOptions(myFinCenter = "GMT")
   LPP = as.timeSeries(data(LPP2005REC))[1:12, 1:4]
   colnames(LPP) <- abbreviate(colnames(LPP), 6)
   print(LPP)
}


\keyword{chron}

