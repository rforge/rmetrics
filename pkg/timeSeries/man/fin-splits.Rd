\name{splits}                     
                                    
                                    
\title{splits}


\alias{splits}


\description{

    Searches for outlier splits in a 'timeSeries' object.
    
}


\details{

    This function is thought to find splits in financial
    price or index series If a price or index is splitted we
    observe in the returns a big jump of several standard
    deviations which is identified usual as an outlier.
    
}


\usage{
splits(x, sd = 3, complement = TRUE, ...)
}


\arguments{

    \item{x}{ 
        a 'timeSeries' object.
        }
    \item{sd}{ 
        a numeric value of standard deviations, e.g. 5 means that 
        values larger or smaller than five times the standard 
        deviation of the series will be detected.
        }
    \item{complement}{
        a logical flag, should the outlier series or its complements 
        be returned?
        }
    \item{\dots}{
        arguments to be passed.
        }
    
}


\examples{
## Create a Return Series with a Split - 
   data <- runif(12, -1, 1)
   data[6] <- 20
   x <- timeSeries(data, timeCalendar(), units="RUNIF")
   x
   
## Search for the Split:
   splits(x, sd=3, complement=TRUE)
   splits(x, sd=3, complement=FALSE)
}

 
