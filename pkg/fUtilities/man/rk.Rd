\name{rk}


\alias{rk}


\title{The rank of a Matrix}


\description{
    
    Returns the rank of a matrix.

}


\usage{
rk(x, method = c("qr", "chol"))
}


\arguments{

    \item{x}{
        a numeric matrix.
        }
    \item{method}{
        a character string. For \code{method = "qr"} the rank is
        computed as \code{qr(x)\$rank}, or alternatively for 
        \code{method="chol"} the rank is
        computed as \code{attr(chol(x, pivot=TRUE), "rank")}.
        }
        
}


\details{
    
    The function \code{rk} computes the rank of a matrix which is 
    the dimension of the range of the matrix corresponding to the number 
    of linearly independent rows or columns of the matrix, or to the 
    number of nonzero singular values. 
    
    The rank of a matrix is also named inear map. 
    
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
   
## Compute the Rank:
   rk(P)
   rk(P, "chol")                                
}


\keyword{math}

