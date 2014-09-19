\name{start}  


\alias{start,timeSeries-method}
\alias{start.timeSeries}

\alias{end,timeSeries-method}
\alias{end.timeSeries}                   
                                    
                                    
\title{Start and End of a 'timeSeries'}


\description{

    Returns start and/or end time stamps of a 'timeSeries' object.
    
}



\usage{
\S4method{start}{timeSeries}(x, \dots)

\S4method{end}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{
        an uni- or multivariate \code{timeSeries} object.
        }
    \item{\dots}{
        optional arguments passed to other methods.
        }
}


\value{

    returns a \code{timeSeries} object.
           
}


\examples{   
## Create Dummy timeSeries -    
   tS <- dummySeries()[, 1]
   tS
   
## Return Start and end Time Stamp -
   c(start(tS), end(tS))
   range(time(tS))
   
}


\keyword{chron}

