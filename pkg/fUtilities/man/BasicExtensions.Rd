\name{BasicExtensions}

\alias{BasicExtensions}

\alias{align}
\alias{align.default}

\alias{attach}
\alias{attach.default}

\alias{log}
\alias{log.default}

\alias{outlier}
\alias{outlier.default}

\alias{rank}
\alias{rank.default}

%\alias{round}
%\alias{round.default}

\alias{sample}
\alias{sample.default}

%only for R <= 2.3.x: \alias{sort}
% " \alias{sort.default}

\alias{var}
\alias{var.default}

\alias{cov}
\alias{cov.default}

\alias{cor}
\alias{cor.default}

%DW: moved to Column Row Statistics ...
%\alias{colSums}
%\alias{colSums.default}
%\alias{colMeans}
%\alias{colMeans.default}
%\alias{rowSums}
%\alias{rowSums.default}
%\alias{rowMeans}
%\alias{rowMeans.default}

\alias{stdev}

\alias{rownames<-}
\alias{rownames<-.default}

\alias{colnames<-}
\alias{colnames<-.default}

\alias{atoms}
\alias{atoms.default}

\alias{as.POSIXlt}
\alias{as.POSIXlt.default}

\alias{as.matrix.ts}
\alias{as.matrix.mts}


\title{Modify and Extend R Methods}


\description{

    A collection and description of functions which
    modify and extend generic S functions.
    \cr

}


\usage{
\method{align}{default}(x, y, xout, method = "linear", n = 50, rule = 1, 
    f = 0, ties = mean, \dots)

\method{attach}{default}(what, pos = 2, name = deparse(substitute(what)),
    warn.conflicts = TRUE)

\method{log}{default}(x, base = exp(1))

\method{outlier}{default}(x, sd = 5, complement = TRUE, \dots)

\method{rank}{default}(x, na.last = TRUE, 
    ties.method = c("average", "first", "random", "max", "min"))

\method{sample}{default}(x, size, replace = FALSE, prob = NULL, \dots)

\method{var}{default}(x, y = NULL, na.rm = FALSE, use)
\method{cov}{default}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
\method{cor}{default}(x, y = NULL, use = "all.obs", 
    method = c("pearson", "kendall", "spearman"))
    
stdev(x, na.rm = FALSE)

rownames(x) <- value
colnames(x) <- value

atoms(x, \dots)

as.POSIXlt(x, tz = "")

\method{as.matrix}{ts}(x, \dots)
\method{as.matrix}{mts}(x, \dots)
}


\arguments{

    \item{base}{
        [log] - \cr
        additional argument to the \code{log} function.
        See for details \code{help(log,package=base)}.
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
        Choices are \code{"linear"} or \code{"constant"}.\cr
        [cov] - \cr
        a character string indicating which correlation coefficient (or 
        covariance) is to be computed. One of \code{"pearson" (default)}, 
        \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
        }
    \item{n}{
        [align] - \cr
        if \code{xout} is not specified, alignment takes place at \code{n}
        equally spaced points spanning the interval \code{range(x)}.
        }
    \item{na.last}{
        [rank] - \cr
        for controlling the treatment of NAs. If \code{TRUE}, missing 
        values in the data are put last; if \code{FALSE}, they are put 
        first; if \code{NA}, they are removed; if \code{"keep"} they are 
        kept with rank \code{NA}.
        }
    \item{name}{
        [attach] - \cr
        alternative way to specify the database to be attached.
        See for details \code{help(attach,package=base)}.
        }
    \item{pos}{
        [attach] - \cr
        an integer specifying position in \code{search()} where to attach
        the database.
        See for details \code{help(attach,package=base)}.
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
    \item{ties.method}{ 
        [rank] - \cr
        a character string, one of \code{"average"}, \code{"first"}, 
        \code{"random"}, \code{"max"}, \code{"min"} specifying how 
        ties are treated, can be abbreviated, see \code{rank}.
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
    \item{warn.conflicts}{
        [attach] - \cr
        a logical value. If \code{TRUE}, warnings are printed about
        conflicts from attaching the database, unless that database
        contains an object \code{.conflicts.OK}. A conflict is a
        function masking a function, or a non-function masking a
        non-function.
        See for details \code{help(attach,package=base)}.
        }
    \item{what}{
        [attach] - \cr
        database to be attached. This may currently be a timeSeries object,
        a data.frame or a list or a R data file created with save or NULL
        or an environment.
        See for details \code{help(attach,package=base)}.
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
        additional arguments to the \code{var} function.\cr
        [cov] - \cr
        additional arguments to the \code{cov} function.
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
## Outlier Detection:
   # outlier -
   set.seed(4711)
   x = rnorm(1000)
   outlier(x, sd = 3, complement = FALSE)
}


\keyword{programming}

