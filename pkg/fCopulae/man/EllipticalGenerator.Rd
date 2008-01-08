\name{EllipticalGenerator}

\alias{EllipticalGenerator}


\alias{ellipticalList}
\alias{ellipticalParam}
\alias{ellipticalRange}
\alias{ellipticalCheck}

\alias{gfunc}
\alias{gfuncSlider}


\title{Bivariate Elliptical Copulae}


\description{
    
    A collection and description of functions 
    concerned with the generator function for 
    the elliptical copula and with functions
    for setting and checking the distributional
    parameters.
    \cr
    
    Functions:
    
    \tabular{ll}{
    \code{ellipticalList} \tab Returns list of implemented elliptical copulae, \cr
    \code{ellipticalParam} \tab Sets default parameters for an elliptical copula, \cr
    \code{ellipticalRange} \tab returns the range of valid rho values, \cr
    \code{ellipticalCheck} \tab checks if rho is in the valid range, \cr
    
    \code{gfunc} \tab Generator function for elliptical distributions, \cr
    \code{gfuncSlider} \tab Slider for generator, density and probability. }
    
}


\usage{
ellipticalList()
ellipticalParam(type = ellipticalList())
ellipticalRange(type = ellipticalList())
ellipticalCheck(rho = 0.75, param = NULL, type = ellipticalList())
     
gfunc(x, param = NULL, type = ellipticalList())
gfuncSlider(B = 10)
}


\arguments{
  
    \item{B}{
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is infinite. 
        By default this is set to 10.
        }
    \item{rho}{
        [*ellipticalCopula] - \cr
        is the numeric value setting the correlation strength, ranging
        between minus one and one.
        }
    \item{param}{
        [*ellipticalCopula][gfunc] - \cr
        additional distributional parameters: for the Sudent-t distribution
        this is "nu", for the Kotz distribution this is "r", and for the
        Exponential Power distribution these are "r" and "s". If the
        argument \code{param=NULL} then default values are taken. These are
        for the Student-t \code{param=c(nu=4))}, for the Kotz distribution
        \code{param=c(r=1))}, and for the exponential power distribution
        \code{param=c(r=1,s=1)}. Note, that the Kotz and exponential power
        copulae are independent of \code{r}, and that \code{r} only enters
        the generator, the density, the probability and the quantile 
        functions.
        }
    \item{type}{
        [*ellipticalCopula][gfunc] - \cr
        the type of the elliptical copula. A character string selected 
        from: "norm", "cauchy", "t", "logistic", "laplace", "kotz", 
        or "epower".
        [*ellipticalSlider] - \cr
        a character string which indicates what kind of plot should be 
        displayed, either a perspective plot if \code{type="persp"}, the
        default value, or a contour plot if \code{type="contour"}.
        }
    \item{x}{
        [gfunc] - \cr
        a numeric value or vector out of the range \code{[0,Inf)} at
        which the generator will be computed.
        }
    \item{\dots}{
        [ellipticalCopulaFit] - \cr
        arguments passed to the optimization function \code{nlminb}.
        }
    
}


\value{
        
    \bold{Copula Functions:}
    \cr\cr
    
    The functions \code{[rpd]ellipticalCopula} return a numeric vector
    of random variates, probabilities, or densities for the specified 
    copula computed at grid coordinates \code{u}|\code{v}.
    \cr
    
    The functions \code{[rpd]ellipticalSlider} display an interactive
    graph of an perspective copula plot either for random variates,
    probabilities or densities. Alternatively, an image underlayed
    contour plot can be shown.
    \cr
    
    \bold{Copula Dependence Measures:}
    \cr\cr
    
    The functions \code{ellipticalTau} and \code{ellipticalRho} return
    a numericc value for Kendall's Tau and Spearman's Rho.
    \cr
    
    \bold{Copula Tail Coefficient:}
    \cr\cr
    
    The function \code{ellipticalTailCoeff} returns the coefficient of
    tail dependence for a specified copula. The function 
    \code{ellipticalTailPlot} displays a whole plot for the upper or
    alternatively for the lower tail dependence as a function of 
    \code{u} for a set of nine \code{rho} values.
    \cr
    
    \bold{Copula Generator Function:}
    \cr\cr
    
    The function \code{gfunc} computes the generator function for the
    specified copula, by default the normal copula. If the argument
    \code{x} is missing, then the normalization constand lambda will
    be returned, otherwise if \code{x} is specified the values for the
    function \emph{g(x)} will be freturned. The selected type of copula
    is added to the output as an attribute named \code{"control"}. 
    The function \code{gfuncSlider} allows to display interactively 
    the generator function, the marginal density, the marginal 
    probability, and the contours of the the bivariate density.
    \cr
    
    \bold{Copula Simulation and Parameter Fitting:}
    \cr\cr
    
    The function \code{ellipticalCopulaSim} returns a numeric two-column
    matrix with randomly generated variates for the specified copula.
    \cr
    
    The function \code{ellipticalCopulaFit} returns a fit to empirical
    data for the specified copula. The returned object is a list with
    elements from the function \code{nlminb}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## ellipticalList -
   # List implemented copulae:
   ellipticalList()

## gfunc - 
   # Generator Function:    
   gfunc(x = (0:10)/10, param = 2, type = "t")
   
## gfuncSlider -
   # Try:   
   # gfuncSlider()
}


\keyword{models}

