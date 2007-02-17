\name{PortfolioClass}

\alias{PortfolioClass}

\alias{fPORTFOLIO}
\alias{fPORTFOLIO-class}

\alias{show.fPORTFOLIO}
\alias{show,fPORTFOLIO-method}

\alias{feasiblePortfolio}
\alias{cmlPortfolio}
\alias{tangencyPortfolio}
\alias{minvariancePortfolio}
\alias{frontierPortfolio}
\alias{portfolioFrontier}
      
\alias{plot.fPORTFOLIO}

\alias{weightsSlider}     
\alias{frontierSlider}


\title{Portfolio Class}


\description{
  
    A collection and description of ...
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{feasiblePortfolio} \tab Returns a feasible portfolio, \cr
    \code{cmlPortfolio} \tab Returns capital market line, \cr
    \code{tangencyPortfolio} \tab Returns the tangency portfolio, \cr
    \code{minvariancePortfolio} \tab Returns the minimum variance portfolio, \cr
    \code{frontierPortfolio} \tab Returns a frontier portfolio, \cr
    \code{portfolioFrontier} \tab Returns the efficient frontier of a portfolio, \cr
    \code{show} \tab Print method for 'fPPORTFOLIO' objects, \cr   
    \code{plot} \tab Plot method for objects of class fPFOLIO, \cr   
    \code{weightsSlider} \tab Weights Slider, \cr           
    \code{frontierSlider} \tab Efficient Frontier Slider. }
    
}
    

\usage{
portfolioFrontier(data, spec = portfolioSpec(), constraintsStrings = NULL, 
    title = NULL, description = NULL)

feasiblePortfolio(data, spec = portfolioSpec(), constraintsStrings = NULL)
cmlPortfolio(data, spec = portfolioSpec(), constraintsStrings = NULL)
tangencyPortfolio(data, spec = portfolioSpec(), constraintsStrings = NULL)
minvariancePortfolio(data, spec = portfolioSpec(), constraintsStrings = NULL)
frontierPortfolio(data, spec = portfolioSpec(), constraintsStrings = NULL)

show.fPORTFOLIO(object)
\method{plot}{fPORTFOLIO}(x, which = "ask", control = list(), \dots)

weightsSlider(object, control = list(), \dots)     
frontierSlider(object, control = list(), \dots)
}


\details{

    \bold{Portfolio Class:}
    \cr\cr
    Describe it ...
    \cr
    
    
    \bold{Portfolio Frontier:}
    \cr\cr
    Describe it ...
    \cr
    
    
    \bold{Single Portfolios:}
    \cr\cr   
    Describe it ...
    \cr
    
    \bold{Portfolio Sliders:}
    \cr\cr   
    Describe it ...   

}


\arguments{

    \item{constraintsStrings}{
        ...
        }
    \item{control}{
        ...
        }
    \item{data}{
        ...
        }
    \item{description}{
        ...
        }
    \item{object}{
        ...
        }
    \item{spec}{
        ...
        }
    \item{title}{
        ...
        }
    \item{which}{
        ...
        }
    \item{x}{
        ...
        }
    \item{\dots}{
        ...
        }

}


\value{
  
    To do ...
  
}


%\references{}


\seealso{

    \code{xxx}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{

## fun -
   # fun()
 
}


\keyword{models}

