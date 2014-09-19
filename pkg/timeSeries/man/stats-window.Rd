\name{window}                     
                                    
                                    
\title{window}


\alias{window}


\description{

    Extracts a part from a 'timeSeries Object
    
}


\examples{
## Load LPP Benchmark Returns -
   x <- LPP2005REC[, 7:9]
   range(time(x))
  
## Extract Data for January 2006 - 
   window(x, "2006-01-01", "2006-01-31")
}


\keyword{chron}

