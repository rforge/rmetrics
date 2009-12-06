\name{print}


\alias{print}
\alias{print.control}


\title{Print Control}


\description{

    Unlists and prints a control object.

}


\usage{
\method{print}{control}(x, \dots)
}


\arguments{
  
    \item{x}{
        the object to be printed.      
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{print.control}
    
    prints control.
    
}


\examples{   
## print -
   control = list(n = 211, seed = 54, name = "generator")
   print(control) 
   class(control) = "control"
   print(control)  
}


\keyword{programming}

