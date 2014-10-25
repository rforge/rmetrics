\name{pascal}


\alias{pascal}


\title{Pascal Matrix}


\description{
    
    Creates a Pascal matrix. 

}


\usage{
pascal(n)
}


\arguments{

    \item{n}{
        an integer value, the dimension of the square matrix.
        }
        
}


\details{
    
    The function \code{pascal} generates a Pascal matrix of order \code{n} 
    which is a symmetric, positive, definite matrix with integer entries 
    made up from Pascal's triangle. The determinant of a Pascal matrix is 1. 
    The inverse of a Pascal matrix has integer entries. If \code{lambda} 
    is an eigenvalue of a Pascal matrix, then \code{1/lambda} is also an 
    eigenvalue of the matrix. Pascal matrices are ill-conditioned. 
         
}


\references{

Call G.S., Velleman D.J., (1993);
    \emph{Pascal's matrices}, 
    American Mathematical Monthly 100, 372--376.
     
Edelman A., Strang G., (2004);
    \emph{Pascal Matrices}, 
    American Mathematical Monthly 111, 361--385.

}


\examples{
## Create Pascal Matrix:
   P = pascal(5)
   P 
   
## Determinant
   det(pascal(5)) 
   det(pascal(10))   
   det(pascal(15))   
   det(pascal(20))                                       
}


\keyword{math}

