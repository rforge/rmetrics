\name{vec}


\alias{vec}
\alias{vech}


\title{Stacking Vectors and Matrixes}


\description{
    
    Stacks either a lower triangle matrix or a matrix.
     
}


\usage{
vec(x)
vech(x)
}


\arguments{

   
    \item{x}{
        a numeric matrix.
        }
        
}


\details{
    
    The function \code{vec} implements the operator that stacks a matrix
    as a column vector, to be more precise in a matrix with one column.
    vec(X) = (X11, X21, ..., XN1, X12, X22, ..., XNN).
    
    The function \code{vech} implements the operator that stacks the lower 
    triangle of a NxN matrix as an N(N+1)/2x1 vector:
    vech(X) =(X11, X21, X22, X31, ..., XNN), to be more precise in a 
    matrix with one row.
  
}


\examples{
## Create Pascal Matrix:
   P = pascal(3)
   
## Stack a matrix
   vec(P) 
   
## Stack the lower triangle
   vech(P)                                        
}


\keyword{math}

