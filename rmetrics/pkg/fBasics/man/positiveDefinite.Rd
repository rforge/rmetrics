\name{positiveDefinite}


\alias{positiveDefinite}
\alias{isPositiveDefinite}
\alias{makePositiveDefinite}


\title{Positive Definite Matrixes}


\description{
    
    Checks if a matrix is positive definite and/or
    forces a matrix to be positive definite.   

}


\usage{
isPositiveDefinite(x)
makePositiveDefinite(x)
}


\arguments{

    \item{x}{
        a square numeric matrix.
        }
        
}


\details{
 
    The function \code{isPositiveDefinite} checks if a square 
    matrix is positive definite.
    
    The function \code{makePositiveDefinite} forces 
    a matrix to be positive definite.
  
}


\author{

    Korbinian Strimmer.
    
}


\examples{
## isPositiveDefinite - 
   # the 3x3 Pascal Matrix is positive define 
   isPositiveDefinite(pascal(3))                
}


\keyword{math}

