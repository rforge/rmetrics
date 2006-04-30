\name{fBasicsMasks}

\alias{fBasicsMasks}

\alias{sort}
\alias{sort.default}

\alias{sample}
\alias{sample.default}

\alias{round}
\alias{round.default}


\alias{log}
\alias{log.default}

\alias{outlier}
\alias{outlier.default}

\alias{var}
\alias{var.default}

\alias{rownames<-}
\alias{rownames<-.default}

\alias{colnames<-}
\alias{colnames<-.default}
            

\title{Masked Objects and Additional Generic Functions}


\description{
    
    A collection and description of functions which
    mask function from R's base installation to generate
    generic functions and some additional generic
    functions which may be useful.
    \cr
    
}


\usage{
sort.default(x, partial = NULL, na.last = NA, decreasing = FALSE, 
    method = c("shell", "quick"), index.return = FALSE, \dots) 

sample.default(x, size, replace = FALSE, prob = NULL, \dots) 

round.default(x, digits, \dots)

log.default(x, base = exp(1))

outlier.default(x, sd = 5, complement = TRUE, \dots)

var.default(x, y = NULL, na.rm = FALSE, use)
    
rownames(x) <- value
colnames(x) <- value
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
    \item{value}{
        [colnames][rownames] - \cr
        additional arguments to the \code{colnames} and \code{rownames}
        functions.
        }
    \item{x}{
        [log][sort][var] - \cr
        first argument.
        }
    \item{y, na.rm, use}{
        [var] - \cr
        additional arguments to the \code{var} function.
        }
    \item{...}{
        [] - \cr
        arguments to be passed
    }
 
}


\details{

    For details we refer to the original help pages.
    
}
    


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{programming}

