\name{ArchimedeanGenerator}

\alias{ArchimedeanGenerator}


\alias{archmList}
\alias{archmParam}
\alias{archmRange}
\alias{archmCheck}     

\alias{Phi}
\alias{PhiSlider}

\alias{Kfunc}
\alias{KfuncSlider}


\title{Bivariate Archimedean Copulae}


\description{
    
    A collection and description of functions 
    concerned with the generator function for 
    the Archimedean copula and with functions
    for setting and checking the distributional
    parameters.
    \cr
    
    Functions:
    
    \tabular{ll}{
    \code{evList} \tab Returns list of implemented Archimedean copulae, \cr
    \code{archmParam} \tab Sets default parameters for an Archimedean copula, \cr
    \code{archmRange} \tab returns the range of valid rho values, \cr
    \code{archmCheck} \tab checks if rho is in the valid range, \cr
     
    \code{Phi} \tab Computes generator Phi, inverse and derivatives, \cr
    \code{PhiSlider} \tab displays interactively generator function, \cr
    
    \code{Kfunc} \tab computes copula density and its inverse, \cr
    \code{KfuncSlider} \tab displays interactively density function. }
    
}


\usage{
archmList()
archmParam(type = archmList())
archmRange(type = archmList(), B = Inf)
archmCheck(alpha, type = archmList())
      
Phi(x, alpha = NULL, type = archmList(), inv = FALSE, deriv = paste(0:2))
PhiSlider(B = 5)

Kfunc(x, alpha = NULL, type = archmList(), inv = FALSE, lower = 1.0e-8)
KfuncSlider(B = 5)
}


\arguments{
  
    \item{alpha}{
        [Phi*][*archmCopula] - \cr
        the parameter of the Archemedean copula. A numerical value.
        }
    \item{B}{
        [archmRange] - \cr
        the maximum slider menu value when the boundary value is infinite. 
        By default this is set to \code{B=Inf}.\cr
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is infinite. 
        By default this is set to \code{B=5}.
        }
    \item{deriv}{
        [Phi] - \cr
        an integer value. Should the function itself, \code{deriv="0"}, 
        or the first \code{deriv="1"}, or second \code{deriv="2"} derivative 
        be evaluated?
        }
    \item{inv}{
        [Phi][Kfunc] - \cr
        a logical flag. Should the inverse function be computed?
        }
    \item{lower}{
        [Kfunc] - \cr
        a numeric value setting the lower bound for the internal root 
        finding function \code{uniroot}.
        }
    \item{type}{
        [*archmCopula][Phi][Kfunc] - \cr
        the type of the Archimedean copula. A character string ranging 
        beween \code{"1"} and \code{"22"}. By default copula No. 1 will 
        be chosen.
        }
    \item{x}{
        [Kfunc] - \cr
        a numeric value or vector ranging between zero and one.\cr
        [Phi] - \cr
        a numeric value or vector.
        }   
            
}


\value{
    
    The function \code{Phi} returns a numeric vector with the values
    computed from the Archemedean generator, its derivatives, or its
    inverse.
    \cr
    
    The function \code{Kfunc} returns a numeric vector with the 
    values of the density and inverse for Archimedian copulae.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{  
## archmList -
   # Return list of implemented copulae:
   archmList()
}


\keyword{models}

