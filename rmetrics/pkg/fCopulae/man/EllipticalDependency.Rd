\name{EllipticalDependency}

\alias{EllipticalDependency}


\alias{ellipticalTau}
\alias{ellipticalRho}

\alias{ellipticalTailCoeff}
\alias{ellipticalTailPlot}


\title{Bivariate Elliptical Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate elliptical copulae. 
    \cr
    
    Elliptical Copulae Functions:
    
    \tabular{ll}{
    \code{ellipticalTau} \tab Computes Kendall's tau for elliptical copulae, \cr
    \code{ellipticalRho} \tab computes Spearman's rho for elliptical copulae, \cr
    
    \code{ellipticalTailCoeff} \tab computes tail dependence for elliptical copulae, \cr
    \code{ellipticalTailPlot} \tab plots tail dependence for elliptical copulae. }
    
}


\usage{   
ellipticalTau(rho)
ellipticalRho(rho, param = NULL, type = ellipticalList(), subdivisions = 500)
    
ellipticalTailCoeff(rho, param = NULL, type = c("norm", "cauchy", "t"))
ellipticalTailPlot(param = NULL, type = c("norm", "cauchy", "t"), 
    tail = c("Lower", "Upper"))
}


\arguments{
  
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
    \item{subdivisions}{
        [ellipticalRho] - \cr
        an integer value with the number of subdivisons in each direction
        on the two dimensional unit square to compute the mean value of
        Spearman's Rho. By default 500 subdivisions are used.
        }
    \item{tail}{
        [ellipticalTailPlot] - \cr
        a character string, either \code{"Upper"} or \code{"Lower"} denoting 
        which of the two tails should be displayed. By default the upper
        tail dependence will be considered.
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
## [rp]ellipticalCopula -
   # Default Normal Copula:
   rellipticalCopula(10)
   pellipticalCopula(10)

## [rp]ellipticalCopula -   
   # Student-t Copula Probability and Density:
   u = grid2d(x = (0:25)/25)
   pellipticalCopula(u, rho = 0.75, param = 4, 
     type = "t", output = "list")
   d = dellipticalCopula(u, rho = 0.75, param = 4, 
     type = "t", output = "list")   
   persp(d, theta = -40, phi = 30, col = "steelblue")
   
## ellipticalTau -
## ellipticalRho -
   # Dependence Measures:
   ellipticalTau(rho = -0.5)
   ellipticalRho(rho = 0.75, type = "logistic", subdivisions = 100)
   
## ellipticalTailCoeff -
   # Student-t Tail Coefficient:
   ellipticalTailCoeff(rho = 0.25, param = 3, type = "t")

## gfunc -
   # Generator Function:
   plot(gfunc(x = 0:10), main = "Generator Function")
   
## ellipticalCopulaSim -
## ellipticalCopulaSim -
   # Simualtion and Parameter Fitting:
   rv = ellipticalCopulaSim(n = 100, rho = 0.75)
   ellipticalCopulaFit(rv)
}


\keyword{models}

