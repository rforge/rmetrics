\name{tslag}


\alias{tslag}


\title{Lagged or Leading Vector/Matrix}


\description{
    
    Creates a lagged or leading vector/matrix of selected order(s).  

}


\usage{
tslag(x, k = 1, trim = FALSE)
}


\arguments{

    \item{k}{
        an integer value, the number of positions the new series is 
        to lag or to lead the input series. 
        }
    \item{x}{
        a numeric vector or matrix, missing values are allowed.
        }
     \item{trim}{
        a logical flag, if TRUE, the missing values at the 
        beginning ans/or end of the returned series will be trimmed. 
        The default value is FALSE. 
        }
        
}


\seealso{

    \code{\link{pdl}}.
    
}


\examples{
## tslag -
   #                                       
}


\keyword{math}

