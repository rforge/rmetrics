\name{ArchimedeanDependency}

\alias{ArchimedeanDependency}


\alias{archmTau}         
\alias{archmRho}         

\alias{archmTailCoeff}     
\alias{archmTailPlot}      


\title{Bivariate Archimedean Copulae}


\description{
    
    A collection and description of functions to 
    investigate bivariate Archimedean copulae. 
    \cr
    
    Archimedean Copulae Functions:
    
    \tabular{ll}{ 
    \code{archmTau} \tab Computes Kendall's tau for Archimedean copulae, \cr
    \code{archmRho} \tab computes Spearman's rho for Archimedean copulae, \cr
    
    \code{archmTailCoeff} \tab computes tail dependence for Archimedean copulae, \cr
    \code{archmTailPlot} \tab plots tail dependence for Archimedean copulae. }
    
}


\usage{   
archmTau(alpha = NULL, type = archmList(), lower = 1.0e-10)
archmRho(alpha = NULL, type = archmList(), method = c("integrate2d", "adapt"), 
    error = 1.0e-5)
    
archmTailCoeff(alpha = NULL, type = archmList())
archmTailPlot(alpha = NULL, type = archmList(), tail = c("Upper", "Lower"))
}


\arguments{
  
    \item{alpha}{
        the parameter of the Archemedean copula. A numerical value.
        }
    \item{error}{
        [archmRho] - \cr
        the error bound to be achieved by the \code{integrate2d} 
        integration formula. A numeric value, by default \code{error=1.0e-5}.
        }
    \item{lower}{
        [archmTau] - \cr
        a numeric value setting the lower bound for the internal integration
        function \code{integrate}.
        }
    \item{tail}{
        [archmTailPlot] - \cr
        a character string, either \code{"Upper"} or \code{"Lower"} denoting 
        which of the two tails should be displayed. By default the upper
        tail dependence will be considered.
        }
    \item{type}{
        the type of the Archimedean copula. A character string ranging 
        beween \code{"1"} and \code{"22"}. By default copula No. 1 will 
        be chosen.
        }
    \item{method}{
        [archmRho] - \cr         
        a character string that determines which integration method should be 
        used, either \code{"integrate2d"} or \code{"adapt"}. If the second
        method is selected the contributed R package \code{"adapt"} is 
        required.
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
   # getClass("fCOPULA")
   
## pcopula -
   # The default Normal Copula:
   # contour(pcopula())
}


\keyword{models}

