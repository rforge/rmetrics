\name{ExtremeValueCopulae}

\alias{ExtremeValueCopulae}

\alias{pevCopula}
\alias{devCopula}

\alias{Afunc}
\alias{AfuncFirstDer}
\alias{AfuncSecondDer}
\alias{AfuncSlider}

\alias{pevSlider}
\alias{devSlider}


\alias{pempiricalCopula}
\alias{dempiricalCopula}
\alias{qlogisticTable}


\title{Bivariate Extreme Value Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate extreme value copulae. 
    \cr
    
     
    Extreme Value Copulae:
    
    \tabular{ll}{
    \code{pevCopula} \tab computes extreme value copula probability,\cr
    \code{devCopula} \tab computes extreme value copula density,\cr
    
    \code{Afunc} \tab computes dependence function, \cr
    \code{AFirstDer} \tab computes derivative of dependence function, \cr
    \code{ASecondDer} \tab computes 2nd derivative of dependence function, \cr
    \code{ASlider} \tab displays interactively dependence function, \cr

    \code{pevSlider} \tab displays interactive plots of probability,\cr
    \code{devSlider} \tab displays interactive plots of density. }
    
    Empirical Copulae:
    
    \tabular{ll}{
    \code{pempiricalCopula} \tab computes empirical copula probability,\cr
    \code{dempiricalCopula} \tab computes empirical copula density. }
    
}


\usage{

pevCopula(u = 0.5, v = u, param = NULL, type = c("gumbel", "galambos", 
    "husler.reiss", "tawn", "bb5"), output = c("vector", "list"), 
    alternative = FALSE )
devCopula(u = 0.5, v = u, param = NULL, type = c("gumbel", "galambos", 
    "husler.reiss", "tawn", "bb5"), output = c("vector", "list"), 
    alternative = FALSE )   

pevSlider(type = c("persp", "contour"), B = 10)
devSlider(type = c("persp", "contour"), B = 10)

Afunc(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5") )
AfuncFirstDer(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5"), eps = 1.0e-6 )
AfuncSecondDer(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5") )
AfuncSlider()

pempiricalCopula(u, v, N = 10)
dempiricalCopula(u, v, N = 10)
}


\arguments{
  
    \item{alternative}{
        [*evCopula] - \cr
        Should the probability be computed alternatively in a
        direct way from the probability formula or by default 
        via the dependency function?  
        }
    \item{B}{
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is 
        infinite. By default this is set to 10.
        }
    \item{eps}{
        [AFirstDer] - \cr
        data below \code{eps} and above \code{1-eps} will be set to
        the border limits.
        }
    \item{N}{
        [empiricalCopula] - \cr
        ... .
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
        the type of the maximum extreme value copula. A character
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
    \item{x}{
        [A*] - \cr
        a numeric value or vector ranging between zero and one.
        \cr
        }
    
}


\value{
    
    Th functions \code{*Spec} return an S4 object 
    of class \code{"fCOPULA"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@copula}{
        the name of the copula.
        }
    \item{@param}{
        a list whose elements specify the model parameters.
        }
    \item{@title}{
        a character string with the name of the copula. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.
        }
        
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
## SOURCE("fCopulae.E4-ExtremeValueCopulae")

## fCOPULA -
   # getClass("fCOPULA")
   
## pcopula -
   # The default Normal Copula:
   # contour(pcopula())
}


\keyword{models}

