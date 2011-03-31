\name{sort}  


\alias{sort,timeSeries-method}
\alias{sort.timeSeries}                 
                                    
                                    
\title{Sorting a 'timeSeries' by Time Stamps}


\description{

    Sorts a 'timeSeries' according to increasing or
    decreasing order of the time stamps.
    
}



\usage{
\S4method{sort}{timeSeries}(x, decreasing = FALSE, \dots)
}


\arguments{

    \item{x}{
        an uni- or multivariate \code{timeSeries} object.
        }
    \item{decreasing}{
        a logical flag. Should we sort in increasing or 
        decreasing order?  By default FALSE.
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
   tS <- dummySeries()
   
## Sort the Series in Decreasing Order -    
   sort(tS, decreasing = TRUE)
     
## Sort the Series -
   sort(tS)
   

}


\keyword{chron}

