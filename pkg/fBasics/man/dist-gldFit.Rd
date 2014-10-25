\name{gldFit}


\alias{gldFit}


\title{GH Distribution Fit}


\description{

    Estimates the distrinbutional parameters for a 
    generalized lambda distribution.

}


\usage{
gldFit(x, lambda1 = 0, lambda2 = -1, lambda3 = -1/8, lambda4 = -1/8, 
    method = c("mle", "mps", "gof", "hist", "rob"),
    scale = NA, doplot = TRUE, add = FALSE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, \dots) 
}


\arguments{
  
    \item{x}{
        a numeric vector. 
        }
    \item{lambda1, lambda2, lambda3, lambda4}{
        are numeric values where
        \code{lambda1} is the location parameter,
        \code{lambda2} is the location parameter,
        \code{lambda3} is the first shape parameter, and
        \code{lambda4} is the second shape parameter.
        }
    \item{method}{
        a character string, the estimation approach to
        fit the distributional parameters, see details.
        }
    \item{scale}{
        not used.
        %a logical flag, by default \code{TRUE}. Should the time series
        %be scaled by its standard deviation to achieve a more stable
        %optimization?
        }
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }    
    \item{add}{
        a logical flag. Should a new fit added to an existing plot?
        }     
    \item{span}{
        x-coordinates for the plot, by default 100 values 
        automatically selected and ranging between the 0.001, 
        and 0.999 quantiles. Alternatively, you can specify
        the range by an expression like \code{span=seq(min, max,
        times = n)}, where, \code{min} and \code{max} are the 
        left and rigldt endpoints of the range, and \code{n} gives 
        the number of the intermediate points.
        }        
    \item{trace}{
        a logical flag. Should the parameter estimation process be
        traced?
        }
    \item{title}{
        a character string which allows for a project title.
        }     
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{\dots}{
        parameters to be parsed.
        }

}


\value{
  
    returns a list with the following components:
  
    \item{estimate}{
        the point at which the maximum value of the log liklihood 
        function is obtained.
        }
    \item{minimum}{
        the value of the estimated maximum, i.e. the value of the
        log liklihood function.
        }
    \item{code}{
        an integer indicating why the optimization process terminated.\cr
        1: relative gradient is close to zero, current iterate is probably 
            solution; \cr
        2: successive iterates within tolerance, current iterate is probably 
            solution; \cr
        3: last global step failed to locate a point lower than \code{estimate}. 
            Either \code{estimate} is an approximate local minimum of the 
            function or \code{steptol} is too small; \cr
        4: iteration limit exceeded; \cr
        5: maximum step size \code{stepmax} exceeded five consecutive times. 
            Either the function is unbounded below, becomes asymptotic to a 
            finite value from above in some direction or \code{stepmax} 
            is too small.
            }
    \item{gradient}{
        the gradient at the estimated maximum.
        }
    \item{steps}{
        number of function calls.
        }
              
}


\details{

    The function \code{\link{nlminb}} is used to minimize the objective 
    function. The following approaches have been implemented:
    
    \code{"mle"}, maximimum log likelihoo estimation.
    
    \code{"mps"}, maximum product spacing estimation.
    
    \code{"gof"}, goodness of fit approaches, 
        \code{type="ad"} Anderson-Darling, 
        \code{type="cvm"} Cramer-vonMise,
        \code{type="ks"} Kolmogorov-Smirnov.
    
    \code{"hist"}, histogram binning approaches,   
        \code{"fd"} Freedman-Diaconis binning,
        \code{"scott"}, Scott histogram binning,
        \code{"sturges"}, Sturges histogram binning.
    
    \code{"rob"}, robust moment matching.
       
}


\examples{    
## gldFit -
   # Simulate Random Variates:
   set.seed(1953)
   s = rgld(n = 1000, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8) 

## gldFit -  
   # Fit Parameters:
   gldFit(s, lambda1=0, lambda2=-1, lambda3=-1/8, lambda4=-1/8, 
     doplot = TRUE, trace = TRUE) 
}


\keyword{distribution}

