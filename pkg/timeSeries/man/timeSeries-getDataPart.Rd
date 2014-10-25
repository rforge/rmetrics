\name{DataPart,timeSeries-method}


\title{DataPart,timeSeries-method}


\alias{getDataPart,timeSeries-method}
\alias{setDataPart,timeSeries-method}


\description{
     Utilities called to implement object@.Data of \code{timeSeries} objects.
}


\examples{
## Load Microsoft Data -
   X <- MSFT[1:10, 1:4]
   
## Get Data Part - 
   DATA <- getDataPart(X)
   class(DATA)
}


\keyword{chron}

