\name{sample}                     
                                    
                                    
\title{sample}


\description{

    Takes a sample of the specified size from the elements of a 'timeSeries.
    
}


\value{

    Returns a resampled 'timeSeries' object.
    
}


\examples{   
## Monthly Calendar Series -    
   x <- daily2monthly(LPP2005REC[, 1:2])[3:14, ]
   
## Resample the Series with respect to the time stamps -    
   resampled <- sample(x)
   resampled
   is.unsorted(resampled)
}


\keyword{chron}
 
