\name{fBasicsMasks}

\alias{fBasicsMasks}

\alias{sort}
\alias{sort.default}

\alias{log}
\alias{log.default}

\alias{var}
\alias{var.default}

\alias{rownames<-}
\alias{rownames<-.default}

\alias{colnames<-}
\alias{colnames<-.default}
            

\title{Masked Objects}


\description{
    
    A collection and description of functions which
    mask function from R's base installation.
    \cr
    
}


\usage{
sort(x, partial = NULL, na.last = NA, decreasing = FALSE, 
    method = c("shell", "quick"), index.return = FALSE) 

log(x, base = exp(1))

var(x, y = NULL, na.rm = FALSE, use)
    
rownames(x) <- value
colnames(x) <- value
}


\arguments{

    \item{base}{
        [log] - \cr
        additional argument to the \code{log} function.
        }
    \item{partial, na.last, decreasing, method, index.return}{
        [sort] - \cr
        additional arguments to the \code{sort} function.
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
 
}


\details{

    For details we refer to the original help pages.
    
}
    


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{programming}

