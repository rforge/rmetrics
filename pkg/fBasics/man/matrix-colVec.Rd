\name{colVec}


\alias{colVec}
\alias{rowVec}


\title{Column and Row Vectors}


\description{
    
    Creates a column or row vector from a numeric
    vector. 
  
}


\usage{
colVec(x)
rowVec(x)
}


\arguments{

    \item{x}{
        a numeric vector.
        }
        
}


\details{
    
    
    The functions \code{colVec} and \code{rowVec} transform a vector into 
    a column and row vector, respectively. A column vector is a matrix 
    object with one column, and a row vector is a matrix object with one 
    row.
   
}


\examples{
## Create a numeric Vector:
   x = rnorm(5)
   
## Column and Row Vectors:
   colVec(x)
   rowVec(x)                        
}


\keyword{math}

