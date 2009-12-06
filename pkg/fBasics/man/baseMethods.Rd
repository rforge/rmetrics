\name{baseMethods}


\alias{baseMethods}


\alias{volatility}
\alias{volatility.default}

\alias{termPlot}
\alias{termPlot.default}

\alias{stdev}
\alias{stdev.default}


\title{Generic Functions Extensions}


\description{

    Basic extensions which which add and/or modify
    additional functionality which is not available
    in R's basic packages.
    \cr
    
    Added and/or modified functions:
    
    \tabular{ll}{
    \code{attach} \tab extends attach function, \cr
    \code{rank} \tab extends rank function, \cr
    \code{stdev} \tab adds stdev function, \cr
    \code{termPlot} \tab adds term plot function, \cr
    \code{volatility} \tab adds volatility function. }

}


\usage{
\method{stdev}{default}(x, na.rm = FALSE)

\method{termPlot}{default}(model, \dots)
   
\method{volatility}{default}(object, \dots)
}


\arguments{

    \item{na.rm}{
        an logical value - should the NA values be removed.
        }  
    \item{model}{
        a fitted model object.
        }
    \item{object}{
        an object from which to extract the volatility.
        }
    \item{x}{
        [align] - \cr
        x-coordinates of the points to be aligned.\cr
        [log][sort][var] - \cr
        first argument.\cr
        [print.control] - cr
        prints an unlisted object of class control.\cr
        [as.matrix.ts][as.matrix.mts] - \cr
        an univariate or multivariate time series object
        of class \code{"ts"} or \code{"mts"} which will be
        transformed into an one-column or multi-column rectangular
        object of class \code{"matrix"}.\cr
        [as.POSIXlt] - \cr
        an object to be converted.
        }
    \item{\dots}{
        arguments to be passed.
    }

}


\details{

    For details we refer to the original help pages.

}


\keyword{programming}

