\name{inv}


\alias{inv}


\title{The Inverse of a Matrix}


\description{
    
    Returns the inverse of a matrix.      

}


\usage{
inv(x)
}


\arguments{

    \item{x}{
        a numeric matrix.
        }
        
}


\note{

    The function \code{inv} is a synonyme to the function \code{solve}.

}


\value{

    returns the inverse matrix.
       
}


\references{

Golub, van Loan, (1996);
    \emph{Matrix Computations}, 
    3rd edition. Johns Hopkins University Press. 
    
}


\examples{
## Create Pascal Matrix:
   P = pascal(5)
   P
         
## Compute the Inverse Matrix:
   inv(P)
   
## Check:
   inv(P) %*% P    
   
## Alternatives:
   chol2inv(chol(P))
   solve(P)                     
}


\keyword{math}

