\name{ExtremeValueCopulae}

\alias{ExtremeValueCopulae}


\alias{revCopula}
\alias{pevCopula}
\alias{devCopula}

\alias{revSlider}
\alias{pevSlider}
\alias{devSlider}

\alias{evTau}         
\alias{evRho}        

\alias{evTailCoeff}     
\alias{evTailPlot}  

\alias{Afunc}
\alias{AfuncSlider}

\alias{evCopulaSim}
\alias{evCopulaFit}


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
    \code{devSlider} \tab displays interactive plots of extreme value density, \cr
    
    \code{Afunc} \tab computes dependence function, \cr
    \code{AfuncSlider} \tab displays interactively dependence function, \cr
    
    \code{evTau} \tab computes Kendall's tau for extreme value copulae, \cr
    \code{evRho} \tab computes Spearman's rho for extreme value copulae, \cr
    
    \code{evTailCoeff} \tab computes tail dependence for extreme value copulae, \cr
    \code{evTailPlot} \tab plots tail dependence for extreme value copulae, \cr
    
    \code{evCopulaSim} \tab simulates an extreme value copula, \cr
    \code{evCopulaFit} \tab fits the parameters of an extreme value copula. }
    
}


\usage{

revCopula(n, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5"))
pevCopula(u = 0.5, v = u, param = NULL, type = c("gumbel", "galambos", 
    "husler.reiss", "tawn", "bb5"), output = c("vector", "list"), 
    alternative = FALSE )
devCopula(u = 0.5, v = u, param = NULL, type = c("gumbel", "galambos", 
    "husler.reiss", "tawn", "bb5"), output = c("vector", "list"), 
    alternative = FALSE )   

revSlider(B = 10)
pevSlider(type = c("persp", "contour"), B = 10)
devSlider(type = c("persp", "contour"), B = 10)

evTau(param = NULL, type = 1:22, lower = 1.0e-10)
evRho(param = NULL, type = 1:22, method = c("integrate2d", "adapt"), error = 1.0e-5)
    
evTailCoeff(param = NULL, type = 1:22)
evTailPlot(param = NULL, type = 1:22, tail = c("Upper", "Lower"))

Afunc(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5") )
AfuncSlider()

evCopulaSim(n, param = NULL,  type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5"))
evCopulaFit(u, v = NULL, type = c("gumbel", "galambos", "husler.reiss", 
    "tawn", "bb5"), \dots)
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
    \item{error}{
        [evRho] - \cr
        the error bound to be achieved by the \code{integrate2d} 
        integration formula. A numeric value, by default \code{error=1.0e-5}.
        }
    \item{lower}{
        [evTau] - \cr
        a numeric value setting the lower bound for the internal integration
        function \code{integrate}.
        }
    \item{n}{
        [revCopula][evCopulaSim] - \cr         
        the number of random deviates to be generated, an integer value.
        }
    \item{method}{
        [evRho] - \cr         
        a character string that determines which integration should be 
        used, either \code{"integrate2d"} or \code{"adapt"}. If the second
        method is selected the condtributed R package \code{"adapt"} is 
        required.
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
    \item{tail}{
        [evTailPlot] - \cr
        a character string, either \code{"Upper"} or \code{"Lower"} denoting 
        which of the two tails should be displayed. By default the upper
        tail dependence will be considered.
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
    \item{\dots}{
        [evCopulaFit] - \cr
        arguments passed to the optimization function \code{nlminb}.
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
## SOURCE("fCopulae.064A-ExtremeValueCopulae")
\dontrun{
## fCOPULA -
   # getClass("fCOPULA")
   
## pcopula -
   # The default Normal Copula:
   # contour(pcopula())
}
}


\keyword{models}

