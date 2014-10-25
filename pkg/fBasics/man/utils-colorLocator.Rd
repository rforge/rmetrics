\name{colorLocator}

\alias{colorLocator}
\alias{colorMatrix}

\title{Color Selection}

\description{
    Displays \R's 657 named colors for selection and
    returns optionally \R's color names.
}

\usage{
colorLocator(locator = FALSE, cex.axis = 0.7)
colorMatrix()
}


\arguments{
  \item{locator}{logical, if true, \code{\link{locator}} is used for
    interactive selection of color names, default is \code{FALSE}.}
  \item{cex.axis}{size of axis labels.}
}


\value{

    Color Locator:

    \code{colorsLocator()} generates a plot with R colors and, when
    \code{locator} is true, returns matrix with graph coordinates and
    names of colors selected.
    \code{colorsMatrix()} quietly returns the matrix of names.
}

\details{

    Color Locator:

    The \code{colorLocator} function plots R's 657 named colors.  If
    \code{locator=TRUE} then you can interactively point and click to
    select the colors for which you want names. To end selection, right
    click on the mouse and select 'Stop', then R returns the selected
    color names.

    The functions used here are wrappers to the functions provided by
    Tomas Aragon in the contributed R package. \code{epitools}.
}

\seealso{
  \code{\link{colorPalette}},
  \code{\link{colorTable}}.
}

\examples{
 colorLocator()
}

\keyword{programming}

