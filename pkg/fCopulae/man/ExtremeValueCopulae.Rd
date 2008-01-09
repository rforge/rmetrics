\name{ExtremeValueCopulae}

\alias{ExtremeValueCopulae}


\alias{revCopula}
\alias{pevCopula}
\alias{devCopula}

\alias{revSlider}
\alias{pevSlider}
\alias{devSlider}


\title{Bivariate Extreme Value Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate extreme value copulae. 
    \cr
    
     
    Extreme Value Copulae Functions:
    
    \tabular{ll}{
    \code{revCopula} \tab Generates extreme value copula random variates, \cr
    \code{pevCopula} \tab computes extreme value copula probability, \cr
    \code{devCopula} \tab computes extreme value copula density, \cr
    
    \code{revSlider} \tab displays interactive plots of extreme value random variates, \cr
    \code{pevSlider} \tab displays interactive plots of extreme value probability, \cr
    \code{devSlider} \tab displays interactive plots of extreme value density. }
    
}


\usage{
revCopula(n, param = NULL, type = evList())
pevCopula(u = 0.5, v = u, param = NULL, type = evList(), 
    output = c("vector", "list"), alternative = FALSE )
devCopula(u = 0.5, v = u, param = NULL, type = evList(), 
    output = c("vector", "list"), alternative = FALSE )   

revSlider(B = 10)
pevSlider(type = c("persp", "contour"), B = 10)
devSlider(type = c("persp", "contour"), B = 10)
}


\arguments{
  
    \item{alternative}{
        [evRho][evTau][*evCopula] - \cr
        Should the probability be computed alternatively in a
        direct way from the probability formula or by default 
        via the dependency function?  
        }
    \item{B}{
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is 
        infinite. By default this is set to 10.
        }
    %\item{error}{
    %    [evRho] - \cr
    %    the error bound to be achieved by the \code{integrate2d} 
    %    integration formula. A numeric value, by default \code{error=1.0e-5}.
    %    }
    \item{n}{
        [revCopula][evCopulaSim] - \cr         
        the number of random deviates to be generated, an integer value.
        }
    \item{output}{
        [*evCopula] - \cr
        output - a character string specifying how the output should
        be formatted. By default a vector of the same length as 
        \code{u} and \code{v}. If specified as \code{"list"} 
        then \code{u} and \code{v} are
        expected to span a two-dimensional grid as outputted by the 
        function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{param}{
        [*ev*][A*] - \cr
        distribution and copulae parameters.
        A numeric value or vector of named parameters as required by 
        the copula specified by the variable \code{type}.
        If set to \code{NULL}, then the default parameters will be taken.
        }
    \item{type}{
        [*ev*][Afunc] - \cr
        the type of the extreme value copula. A character
        string selected from: "gumbel", "galambos", "husler.reiss", 
        "tawn", or "bb5".
        \cr
        [evSlider] - \cr
        a character string specifying the plot type. Either a 
        perspective plot which is the default or a contour plot 
        with an underlying image plot will be created.
        }
    \item{u, v}{
        [*evCopula][*archmCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{$x} and \code{$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
       }
    \item{\dots}{
        [evCopulaFit] - \cr
        arguments passed to the optimization function \code{nlminb}.
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
    
    The functions \code{cK} and {cKInv} return a numeric vector with the 
    values of the density and inverse for Archimedian copulae.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## fCOPULA -
   getClass("fCOPULA")
   getSlots("fCOPULA")
   
## revCopula -
   # Not yet implemented
   # revCopula(n = 10, type = "galambos")
   
## pevCopula -
   pevCopula(u = grid2d(), type = "galambos", output = "list")
   
## devCopula -
   devCopula(u = grid2d(), type = "galambos", output = "list")
   
## AfuncSlider -
   # Generator, try:
   if (require(tcltk)) {
     AfuncSlider()
   }
}


\keyword{models}

