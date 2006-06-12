\name{ArchimedeanCopulae}

\alias{ArchimedeanCopulae}


\alias{rarchmCopula}
\alias{parchmCopula}
\alias{darchmCopula}      

\alias{rarchmSlider}     
\alias{parchmSlider}
\alias{darchmSlider}       

%alias{archmTau}          MISSING
%alias{archmRho}          MISSING

%alias{archmTailCoeff}    MISSING
%alias{archmTailPlot}     MISSING

\alias{Phi}
\alias{PhiSlider}

\alias{Kfunc}
\alias{KfuncSlider}

\alias{archmTau}
\alias{archmRho}

\alias{Kfunc}
\alias{KfuncSlider}

%alias{archmCopulaSim}    MISSING
%alias{archmCopulaFit}    MISSING

\alias{archmCopulaSim}
\alias{archmCopulaFit}

\title{Bivariate Archimedean Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate Archimedean copulae. 
    \cr
    
    Archimedean Copula Functions:
    
    \tabular{ll}{
    \code{rarchmCopula} \tab Generates Archimedean copula variates, \cr
    \code{parchmCopula} \tab computes Archimedean copula probability, \cr
    
    \code{Phi} \tab Computes generator Phi, inverse and derivatives, \cr
    \code{PhiSlider} \tab displays interactively generator function, \cr
    
    \code{Kfunc} \tab computes copula density and its inverse, \cr
    \code{archmTau} \tab computes Kendall's tau for Archimedean copulae, \cr
    \code{archmRho} \tab computes Spearman's rho for Archimedean copulae, \cr
    \code{KfuncSlider} \tab displays interactively density function, \cr
    
    \code{parchmSlider} \tab displays interactive plots of probability, \cr
    \code{darchmSlider} \tab displays interactive plots of density. }
    
}


\usage{
    
rarchmCopula(n, alpha = NULL, type = 1:22)
parchmCopula(u = 0.5, v = u, alpha = NULL, type = 1:22, output = 
    c("vector", "list"), alternative = FALSE )
darchmCopula(u = 0.5, v = u, alpha = NULL, type = 1:22, output = 
    c("vector", "list"), alternative = FALSE )
    
parchmSlider(type = c("persp", "contour"))
    
archmTau(alpha = NULL, type = 1:22, lower = 1.0e-10)
archmRho(alpha = NULL, type = 1:22, method = c("integrate2d", "adapt"),
    error = 1.0e-5)
    
Phi(x, alpha = NULL, type = 1:22, inv = FALSE, deriv = c(0,1,2))
PhiSlider()

Kfunc(x, alpha = NULL, type = 1:22, inv = FALSE, lower = 1.0e-8)
KfuncSlider()

archmCopulaSim(n, alpha = NULL, type = 1:22)
archmCopulaFit(u, v = NULL, type = 1:22, \dots)

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
    \item{deriv}{
        [Phi] - \cr
        an integer value. Should the function itself, \code{deriv=0}, 
        or the first \code{deriv=1}, or second \code{deriv=2} derivative 
        be evaluated?
        }
    \item{error}{
        [archmRho] - \cr
        the error bound to be achieved by the \code{integrate2d} 
        integration formula. A numeric value, by default \code{error=1.0e-5}.
        }
    \item{inv}{
        [Phi][Kfunc] - \cr
        a logical flag. Should the inverse function be computed?
        }
    \item{lower}{
        [invK] - \cr
        a numeric value setting the lower bound for the internal root 
        finding function \code{uniroot}.
        \cr
        [archmTau] - \cr
        a numeric value setting the lower bound for the internal integration
        function \code{integrate}.
        }
    \item{method}{
        [archmRho] - \cr         
        a character string that determines which integration should be 
        used, either \code{"integrate2d"} or \code{"adapt"}. If the second
        method is selected the condtributed R package \code{"adapt"} is 
        required.
        }
    \item{n}{
       [rarchmCopula] - \cr         
       the number of random deviates to be generated, an integer value.
       }
    \item{output}{
        [*archmCopula] - \cr
        output - a character string specifying how the output should
        be formatted. By default a vector of the same length as 
        \code{u} and \code{v}. If specified as \code{"list"} 
        then \code{u} and \code{v} are
        expected to span a two-dimensional grid as outputted by the 
        function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{type}{
        [*archmCopula][Phi][Kfunc] - \cr
        the type of the Archimedean copula. An integer ranging beween
        1 and 22. Alternatively type can be specified by a 
        string value of the number. By default the first copula will 
        be chosen.
        }
    \item{u, v}{
        [*archmCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{$x} and \code{$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
       }
    \item{x}{
        [Kfunc] - \cr
        a numeric value or vector ranging between zero and one.
        \cr
        [Phi] - \cr
        a numeric value or vector.
        }
    \item{\dots}{
        [archmCopulaFit] - \cr
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
## SOURCE("fCopulae.E3-ArchimedeanCopulae")

## fCOPULA -
   # getClass("fCOPULA")
   
## pcopula -
   # The default Normal Copula:
   # contour(pcopula())
}


\keyword{models}

