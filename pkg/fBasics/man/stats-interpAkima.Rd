\name{akimaInterp}
\title{Bivariate Spline Interpolation}
\alias{akimaInterp}
\alias{akimaInterpp}
\description{
    Interpolates bivariate data sets using Akima
    spline interpolation.
}
\usage{
akimaInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE)

akimaInterpp(x, y = NULL, z = NULL, xo, yo, extrap = FALSE)
}
\arguments{
    \item{x, y, z}{
        for \code{akimaInterp} the arguments \code{x} and \code{y} are
        two numeric vectors of grid pounts, and \code{z} is a numeric
        matrix or any other rectangular object which can be transformed
        by the function \code{as.matrix} into a matrix object.
        For \code{akimaInterpp} we  consider either three numeric vectors
        of equal length or if  \code{y} and \code{z} are NULL, a list with
        entries \code{x}, \code{y}, \code{z}, or named data frame with
        \code{x} in the first, \code{y} in the second, and \code{z} in
        the third column.
        }
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x}
        and \code{y} direction.
        }
    \item{xo, yo}{
        for \code{akimaInterp}
        two numeric vectors of data points spanning the grid, and
        for \code{akimaInterpp}
        two numeric vectors of data points building pairs for pointwise
        interpolation.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
}
\details{
    Two options are available gridded and pointwise interpolation.

    \code{akimaInterp} is a function wrapper to the \code{interp}
    function provided by the contributed R package \code{akima}.
    The Fortran code of the Akima spline interpolation routine was
    written by H. Akima.

    Linear surface fitting and krige surface fitting are provided by
    the functions \code{linearInterp} and \code{krigeInterp}.
}

\note{
    IMPORTANT: The contributed package \pkg{akima} is not in the
    dependence list of the DESCRIPTION file due to license conditions.
    The user has to install this package from CRAN on
    his own responsibility, please check the license conditions.
}

\value{
  \describe{
    \item{akimaInterp}{
      returns a list with at least three entries, \code{x}, \code{y}
      and \code{z}. Note, that the returned values, can be directly
      used by the  \code{persp} and \code{contour} 3D plotting methods.
    }
    \item{akimaInterpp}{
      returns a data.frame with columns \code{"x"}, \code{"y"},
      and \code{"z"}.
    }
  }
}
\seealso{
    \code{\link{linearInterp}},
    \code{\link{krigeInterp}}.
}
\references{
    Akima H., 1978,
    \emph{A Method of Bivariate Interpolation and Smooth Surface Fitting for
    Irregularly Distributed Data Points},
    ACM Transactions on Mathematical Software 4, 149-164.

    Akima H., 1996,
    \emph{Algorithm 761: Scattered-Data Surface Fitting that has the Accuracy
    of a Cubic Polynomial},
    ACM Transactions on Mathematical Software 22, 362-371.
}
\examples{
\donttest{ ## Does not run for r-solaris-x86
 ## akimaInterp -- Akima Interpolation:
 if (requireNamespace("akima")) {
   set.seed(1953)
   x <- runif(999) - 0.5
   y <- runif(999) - 0.5
   z <- cos(2*pi*(x^2+y^2))
   ans <- akimaInterp(x, y, z, gridPoints = 41, extrap = FALSE)
   persp(ans, theta = -40, phi = 30, col = "steelblue",
         xlab = "x", ylab = "y", zlab = "z")
   contour(ans)
 }
}% dont

## Use spatial as alternative on r-solaris-x86
## spatialInterp - Generate Kriged Grid Data:
    if (requireNamespace("spatial")) {
      RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
      set.seed(4711, kind = "Marsaglia-Multicarry")
      x <- runif(999)-0.5
      y <- runif(999)-0.5
      z <- cos(2*pi*(x^2+y^2))
      ans <- krigeInterp(x, y, z, extrap = FALSE)
      persp(ans)
      title(main = "Kriging")
      contour(ans)
      title(main = "Kriging")
    }
}
\keyword{programming}

