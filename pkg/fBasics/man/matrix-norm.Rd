\name{norm}


\alias{norm2}


\title{Matrix Norm}


\description{
    
    Returns the norm of a matrix.

}


\usage{
norm2(x, p = 2)
}


\arguments{

    \item{x}{
        a numeric matrix.
        }
    \item{p}{
        an integer value, \code{1}, \code{2} or \code{Inf}.
        \code{p=1} - The maximum absolute column sum norm which is defined 
        as the maximum of the sum of the absolute valued elements of columns 
        of the matrix.
        \code{p=2} - The spectral norm is "the norm" of a matrix \code{X}. 
        This value is computed as the square root of the maximum eigenvalue 
        of \code{CX} where \code{C} is the conjugate transpose.
        \code{p=Inf} - The maximum absolute row sum norm is defined 
        as the maximum of the sum of the absolute valued elements
        of rows of the matrix.
        }
        
}


\value{

    returns the value of the norm of the matrix.
    
}


\details{
    

    The function \code{norm2} computes the norm of a matrix. Three choices 
    are possible: 
     
    \code{p=1} - The maximum absolute column sum norm which is defined 
    as the maximum of the sum of the absolute valued elements of columns 
    of the matrix. 
    
    \code{p=2} - The spectral norm is "the norm" of a matrix \code{X}. 
    This value is computed as the square root of the maximum eigenvalue 
    of \code{CX} where \code{C} is the conjugate transpose.
    
    \code{p=Inf} - The maximum absolute row sum norm is defined 
    as the maximum of the sum of the absolute valued elements
    of rows of the matrix.
    
    Note, the function \code{fBasics::norm()} has become obsolete, since 
    \code{base::norm()} has become available in the R base environment.
    To avoid conflicts with \code{norm()} we have renamed in the fBasics
    package as \code{norm2}.
    
}


\references{

Golub, van Loan, (1996);
    \emph{Matrix Computations}, 
    3rd edition. Johns Hopkins University Press. 

}


\examples{
## Create Pascal Matrix:
   P <- pascal(5)
   P                  
     
## Return the Norm of the Matrix:                      
   norm2(P)                                                       
}


\keyword{math}

