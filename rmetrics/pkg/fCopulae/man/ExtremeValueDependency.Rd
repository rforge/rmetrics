\name{ExtremeValueDependency}

\alias{ExtremeValueDependency}


\alias{evTau}         
\alias{evRho}        

\alias{evTailCoeff}     
\alias{evTailCoeffSlider}  


\title{Bivariate Extreme Value Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate extreme value copulae. 
    \cr
    
     
    Extreme Value Copulae Functions:
    
    \tabular{ll}{
    \code{evTau} \tab Computes Kendall's tau for extreme value copulae, \cr
    \code{evRho} \tab computes Spearman's rho for extreme value copulae, \cr
    
    \code{evTailCoeff} \tab computes tail dependence for extreme value copulae, \cr
    \code{evTailCoeffSlider} \tab plots tail dependence for extreme value copulae. }
    
}


\usage{
evTau(param = NULL, type = evList(), alternative = FALSE)
evRho(param = NULL, type = evList(), alternative = FALSE)
    
evTailCoeff(param = NULL, type = evList())
evTailCoeffSlider(B = 10)
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

