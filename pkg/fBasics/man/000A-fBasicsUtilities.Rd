\name{fBasicsUtilities}

\alias{fBasicsUtilities}

\alias{log}
\alias{log.default}

\alias{outlier}
\alias{outlier.default}

\alias{round}
\alias{round.default}

\alias{sample}
\alias{sample.default}

\alias{sort}
\alias{sort.default}

\alias{var}
\alias{var.default}

\alias{rownames<-}
\alias{rownames<-.default}

\alias{colnames<-}
\alias{colnames<-.default}

\alias{as.POSIXlt}
\alias{as.POSIXlt.default}

\alias{as.matrix.ts}
\alias{as.matrix.mts}
            

\title{Masked Objects and Additional Generic Functions}


\description{
    
    A collection and description of functions which
    mask function from R's base installation to generate
    generic functions and some additional generic
    functions which may be useful.
    \cr
    
}


\usage{
log.default(x, base = exp(1))

outlier.default(x, sd = 5, complement = TRUE, \dots)

round.default(x, digits, \dots)

sample.default(x, size, replace = FALSE, prob = NULL, \dots)

sort.default(x, partial = NULL, na.last = NA, decreasing = FALSE, 
    method = c("shell", "quick"), index.return = FALSE, \dots) 

var.default(x, y = NULL, na.rm = FALSE, use)
    
rownames(x) <- value
colnames(x) <- value

as.POSIXlt(x, tz = "")

as.matrix.ts(x)
as.matrix.mts(x)
}


\arguments{

    \item{base}{
        [log] - \cr
        additional argument to the \code{log} function.
        }
    \item{digits}{
        [round] - \cr
        an integer indicating the precision to be used.
        }
    \item{partial, na.last, decreasing, method, index.return}{
        [sort] - \cr
        additional arguments to the \code{sort} function.
        }
    \item{sd, complement}{
        [outlier] - \cr
        sd - a numeric value of standard deviations, e.g. 5 means that 
            values larger or smaller tahn five times the standard 
            deviation of the series will detected.
        complement - a logical flag, should the outlier series
            or its complements be returned ?
        }
    \item{size, replace, prob}{
        [sample] - \cr
        size - is a non-negative integer giving the number of items 
            to choose, \cr
        replace - a logical flag. Should sampling be with 
            replacement? \cr
        prob -  a vector of probability weights for obtaining the 
            elements of the vector being sampled.
        }
    \item{tz}{
        [as.POSIXlt] - \cr
        time zone specification.
        }
    \item{value}{
        [colnames][rownames] - \cr
        additional arguments to the \code{colnames} and \code{rownames}
        functions.
        }
    \item{x}{
        [log][sort][var] - \cr
        first argument.
        \cr
        [as.matrix.ts][as.matrix.mts] - \cr
        an univariate or multivariate time series object
        of class \code{"ts"} or \code{"mts"} which will be
        transformed into an one-column or multi-column rectangular 
        object of class \code{"matrix"}.\cr
        [as.POSIXlt] - \cr
        an object to be converted.
        }
    \item{y, na.rm, use}{
        [var] - \cr
        additional arguments to the \code{var} function.
        }
    \item{\dots}{
        arguments to be passed.
    }
 
}


\details{

    For details we refer to the original help pages.
    
}
    


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{programming}

