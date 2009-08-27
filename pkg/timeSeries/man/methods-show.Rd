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
## Load Micsrosoft Data - 
   setRmetricsOptions(myFinCenter = "GMT")
   LPP = MSFT[1:12, 1:4]

## Abbreviate Column Names - 
   colnames(LPP) <- abbreviate(colnames(LPP), 6)
   
## Print Data Set -
   print(LPP)
   
## Alternative Use, Show Data Set - 
   show(LPP)
}


\keyword{chron}

