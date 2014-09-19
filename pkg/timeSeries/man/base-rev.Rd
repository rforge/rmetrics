\name{rev}     


\alias{rev,timeSeries-method}
\alias{rev.timeSeries}
                                                 
                                    
\title{Reversion of a 'timeSeries'}


\description{

    Reverses an uni- or multivariate 'timeSeries' object by 
    reversing the order of the time stamps.
    
}


\usage{
\S4method{rev}{timeSeries}(x)
}


\arguments{

    \item{x}{
        an uni- or multivariate 'timeSeries' object.
        } 
        
}


\value{

    Returns a reversed 'timeSeries' object.
    
}


\examples{   
## Create Dummy timeSeries -    
   tS <- dummySeries()
   
## Reverse Series -
   rev(tS)
}


\keyword{chron}

