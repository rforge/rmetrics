\name{fCalendarUtilities}

\alias{fCalendarUtilities}

\alias{align}
\alias{align.default}

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

\alias{modify}
\alias{modify.default}

\alias{atoms}
\alias{atoms.default}

\alias{currentYear}
\alias{myUnits}


\title{Masked Objects and Additional Generic Functions}


\description{
    
    A collection and description of functions which
    mask function from R's base installation to generate
    generic functions and some additional generic
    functions which may be useful.
    \cr
    
}


\usage{
align.default(x, y, xout, method = "linear", n = 50, rule = 1, f = 0, 
    ties = mean, \dots)

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

currentYear
myUnits
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
    \item{f}{
        [align] - \cr
        for \code{method="constant"} the value of \code{f} takes a 
        number between 0 and 1 inclusive, indicating a compromise between 
        left- and right-continuous step functions. If \code{y0} and 
        \code{y1} are the values to the left and right of the point then 
        the value is \code{y0*(1-f)+y1*f} so that \code{f=0} is 
        right-continuous and \code{f=1} is left-continuous.
        }
    \item{method}{
        [align] - \cr
        a character string which specifies the alignment method to be used.  
        Choices are \code{"linear"} or \code{"constant"}.
        }
    \item{n}{
        [align] - \cr
        if \code{xout} is not specified, alignment takes place at \code{n}
        equally spaced points spanning the interval \code{range(x)}.
        }
    \item{partial, na.last, decreasing, index.return}{
        [sort] - \cr
        additional arguments to the \code{sort} function.
        }
    \item{rule}{
        [align] - \cr
        an integer describing how alignment is to take place
        outside the interval \code{range(x)}. If \code{rule=1}
        then \code{NA}'s are returned for such points and if
        is \code{rule=2}, the value at the closest data extreme is used.
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
    \item{ties}{
        [align] - \cr
        handles tied \code{x} values. Either a function with a single
        vector argument returning a single number result or the
        string \code{"ordered"}.
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
        [align] - \cr
        x-coordinates of the points to be aligned.\cr
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
    \item{xout}{
        [align] - \cr
        a set of values specifying where interpolation is to take place.}
    \item{y, na.rm, use}{
        [align] - \cr
        y-coordinates of the points to be aligned.\cr
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


\examples{
## SOURCE("fCalendar.1A-fCalendarUtilities")
}


\keyword{programming}

