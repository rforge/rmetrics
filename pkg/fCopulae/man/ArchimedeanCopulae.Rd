\name{ArchimedeanCopulae}

\alias{ArchimedeanCopulae}


\alias{rarchmCopula}
\alias{parchmCopula}
\alias{darchmCopula}      

\alias{rarchmSlider}     
\alias{parchmSlider}
\alias{darchmSlider}  

\alias{rgumbelCopula}   
\alias{pgumbelCopula}
\alias{dgumbelCopula}  


\title{Bivariate Archimedean Copulae}


\description{
    
    A collection and description of functions to 
    investigate bivariate Archimedean copulae. 
    \cr
    
    Archimedean Copulae Functions:
    
    \tabular{ll}{
    \code{rarchmCopula} \tab Generates Archimedean copula variates, \cr
    \code{parchmCopula} \tab computes Archimedean copula probability, \cr
    \code{darchmCopula} \tab computes Archimedean copula density, \cr
    
    \code{rarchmSlider} \tab displays interactive plots of variates, \cr
    \code{parchmSlider} \tab displays interactive plots of probability, \cr
    \code{darchmSlider} \tab displays interactive plots of density.  }
    
    
    Special Copulae Functions:
    
    \tabular{ll}{
    \code{rgumbelCopula} \tab Generates Gumbel copula variates, \cr
    \code{pgumbelCopula} \tab computes Gumbel copula probability, \cr
    \code{dgumbelCopula} \tab computes Gumbel copula density.  }
    
}


\usage{
rarchmCopula(n, alpha = NULL, type = archmList())
parchmCopula(u = 0.5, v = u, alpha = NULL, type = archmList(), output = 
    c("vector", "list"), alternative = FALSE )
darchmCopula(u = 0.5, v = u, alpha = NULL, type = archmList(), output = 
    c("vector", "list"), alternative = FALSE )
   
rarchmSlider(B = 10) 
parchmSlider(type = c("persp", "contour"), B = 10)
darchmSlider(type = c("persp", "contour"), B = 10)

rgumbelCopula(n, alpha = 2)
pgumbelCopula(u = 0.5, v = u, alpha = 2, output = c("vector", "list"))
dgumbelCopula(u = 0.5, v = u, alpha = 2, output = c("vector", "list"))
}


\arguments{
  
    \item{alpha}{
        [Phi*][*archmCopula] - \cr
        the parameter of the Archemedean copula. A numerical value.
        }
    \item{alternative}{
        [*Copula] - \cr
        Should the probability be computed alternatively ...
        }
    \item{B}{
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is infinite. 
        By default this is set to 10.
        }
    \item{n}{
        [rarchmCopula] - \cr         
        the number of random deviates to be generated, an integer value.
        }   
    \item{output}{
        [*archmCopula] - \cr
        output - a character string specifying how the output should
        be formatted. By default a vector of the same length as \code{u} 
        and \code{v}. If specified as \code{"list"} then \code{u} and 
        \code{v} are expected to span a two-dimensional grid as outputted 
        by the function \code{grid2d} and the function returns a list with
        elements \code{\$x}, \code{\$y}, and \code{\$z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{type}{
        [*archmCopula] - \cr
        the type of the Archimedean copula. A character string ranging 
        beween \code{"1"} and \code{"22"}. By default copula No. 1 will 
        be chosen.\cr
        [*archmSlider] - \cr
        the type of the plot. A charcter string either specifying a 
        perspective or contour plot.
        }   
    \item{u, v}{
        [*archmCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{\$x} and \code{\$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
        }    
        
}


\value{
           
    The function \code{pcopula} returns a numeric matrix of probabilities 
    computed at grid positions \code{x}|\code{y}.
    \cr
    
    The function \code{parchmCopula} returns a numeric matrix with values
    computed for the Archemedean copula.
    \cr
    
    The function \code{darchmCopula} returns a numeric matrix with values
    computed for thedensity of the Archemedean copula.
    \cr
    
    The functions \code{Phi*} return a numeric vector with the values
    computed from the Archemedean generator, its derivatives, or its
    inverse.
    \cr
    
    The functions \code{cK} and \code{cKInv} return a numeric vector with the 
    values of the density and inverse for Archimedian copulae.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}

  
% \examples{
% ## fCOPULA -
%    # getClass("fCOPULA")
   
% ## pcopula -
%    # The default Normal Copula:
%    # contour(pcopula())
% }


\keyword{models}

