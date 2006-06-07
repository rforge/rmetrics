\name{EllipticalCopulae}

\alias{EllipticalCopulae}


\alias{rellipticalCopula}
\alias{pellipticalCopula}
\alias{dellipticalCopula}

\alias{rellipticalSlider}
\alias{pellipticalSlider}
\alias{dellipticalSlider}

\alias{ellipticalTau}
\alias{ellipticalRho}
\alias{ellipticalTailCoeff}
\alias{ellipticalTailPlot}

\alias{gfunc}
\alias{gfuncSlider}

\alias{ellipticalCopulaSim}
\alias{ellipticalCopulaFit}


\title{Bivariate Elliptical Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate elliptical copulae. 
    \cr
    
    Elliptical Copula Functions:
    
    \tabular{ll}{
    \code{rellipticalCopula} \tab Generates elliptical copula variates, \cr
    \code{pellipticalCopula} \tab computes elliptical copula probability, \cr
    \code{dellipticalCopula} \tab computes elliptical copula density, \cr
        
    \code{rellipticalSlider} \tab displays interactive plots of variates, \cr
    \code{pellipticalSlider} \tab displays interactive plots of probability, \cr
    \code{dellipticalSlider} \tab displays interactive plots of density, \cr
    
    \code{ellipticalTau} \tab computes Kendall's tau for elliptical copulae, \cr
    \code{ellipticalRho} \tab computes Spearman's rho for elliptical copulae, \cr
    \code{ellipticalTailCoeff} \tab computes tail dependence for elliptical copulae, \cr
    \code{ellipticalTailPlot} \tab plots tail dependence for elliptical copulae, \cr
    
    \code{gfunc} \tab Generator function for elliptical distributions, \cr
    \code{gfuncSlider} \tab Slider for generator, density and probability, \cr 
    
    \code{ellipticalCopulaSim} \tab simulates an elliptical copula, \cr
    \code{ellipticalCopulaSim} \tab fits the parameters of an elliptical copula. }
    
}


\usage{ 
rellipticalCopula(n, rho = 0.75, param = NULL, type = c("norm", "cauchy", 
    "t"))
pellipticalCopula(u = 0.5, v = u, rho = 0.75, param = NULL, type = c("norm", 
    "cauchy", "t", "logistic", "laplace", "kotz", "epower"), output = 
    c("vector", "list"), border = TRUE)
dellipticalCopula(u = 0.5, v = u, rho = 0.75, param = NULL, type = c("norm", 
    "cauchy", "t", "logistic", "laplace", "kotz", "epower"), output = 
    c("vector", "list"), border = TRUE)

rellipticalSlider(B = 100)
pellipticalSlider(type = c("persp", "contour"), B = 20)
dellipticalSlider(type = c("persp", "contour"), B = 20)
    
ellipticalTau(rho)
ellipticalRho(rho, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
    "laplace", "kotz", "epower"), subdivisions = 500)
ellipticalTailCoeff(rho, param = NULL, type = c("norm", "cauchy", "t",
    "logistic", "laplace", "kotz", "epower"))
ellipticalTailPlot(param = NULL, type = c("norm", "cauchy", "t", "logistic", 
    "laplace", "kotz", "epower"), tail = c("Upper", "Lower"))

gfunc(x, param = NULL, type = c("norm", "cauchy", "t", "logistic", "laplace", 
    "kotz", "epower"))
gfuncSlider()

ellipticalCopulaSim(n, rho = 0.75, param = NULL, type = c("norm", "cauchy", "t"))
ellipticalCopulaFit(u, v, type = c("norm", "cauchy", "t"))
}


\arguments{
  
    \item{B}{
        [*Slider] - \cr
        the maximum slider menu value when the boundary value is infinite. 
        By default this is set to 10.
        }
    \item{border}{
        [pellipticalCopula][dellipticalCopula] - \cr
        a logical flag. If the argument \code{u} is an integer, say \code{N}, 
        greater than one than all points on a square grid \code{[(0:N)/N]^2}
        are computed. If border is FALSE than the border points are removed 
        from the returned value, by default this is not the case.
        }
    \item{n}{
        [rellipticalCopula] - \cr         
        the number of random deviates to be generated, an integer value.
        }
    \item{output}{
        [pellipticalCopula][dellipticalCopula] - \cr
        a character string specifying how the output should be formatted. 
        By default a vector of the same length as \code{u} and \code{v} 
        is returned. If specified as \code{"list"} then \code{u} and \code{v} 
        are expected to span a two-dimensional grid as outputted by the 
        function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions. For the grid version,
        when \code{u} is specified as an integer greater than one, always 
        the output in form of a list will be returned.
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
    \item{u, v}{
        [*ellipticalCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{$x} and \code{$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}. If \code{u} is an integer value greater than one,
        say \code{N}, than the values for all points on the 
        \code{[(0:N)/N]^2} grid spanning the unit square will be returned.
        }
    \item{x}{
        [gfunc] - \cr
        a numeric value or vector out of the range \code{[0,Inf)} at
        which the generator will be computed.
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
## SOURCE("fCopulae.E2-EllipticalCopulae")
   
## pelliptical -
   # The default Normal Copula:
   # contour(pcopula())
}


\keyword{models}

