\name{pdl}


\alias{pdl}


\title{Polynomial Distributed Lags}


\description{
    
    Returns a regressor matrix for polynomial distributed 
    lags.

}


\usage{
pdl(x, d = 2, q = 3, trim = FALSE)
}


\arguments{

    \item{x}{
        a numeric vector.
        } 
    \item{d}{
        an integer specifying the order of the polynomial. 
        }
    \item{q}{
        an integer specifying the number of lags to use in 
        creating polynomial distributed lags. This must be 
        greater than d. 
        }   
    \item{trim}{
        a logical flag; if TRUE, the missing values at 
        the beginning of the returned matrix will be trimmed.  
        }   

}


\seealso{

    \code{\link{tslag}}.
    
}
    

\examples{
## pdl -
   #                                      
}


\keyword{math}

