\name{plotShapeTriangle}
\alias{plotShapeTriangle}

\title{
Plot the Shape Triangle
}
\description{
Plots the shape triangle for a hyperbolic distribution or generalized
hyperbolic distribution. For the hyperbolic distribution the parameter
\eqn{\chi}{chi} is related to the skewness, and the parameter
\eqn{\xi}{xi} is related to the kurtosis. See Barndorff-Nielsen, O. and
\enc{Blæsild}{Blaesild}, P. (1981).
}
\usage{
plotShapeTriangle(xgap = 0.025, ygap = 0.0625/2,
                  main = "Shape Triangle", ...)
}
\arguments{
  \item{xgap}{Gap between the left- and right-hand edges of the shape
    triangle and the border surrounding the graph.
}
  \item{ygap}{Gap between the top and bottom of the shape
    triangle and the border surrounding the graph.
}
  \item{main}{Title for the plot.
}
  \item{\dots}{Values of other graphical parameters.
}
}

\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1981).
  Hyperbolic distributions and ramifications: contributions to theory
  and application.
  In \emph{Statistical Distributions in Scientific Work},
  eds., Taillie, C., Patil, G. P., and Baldessari, B. A., Vol. 4,
  pp. 19--44. Dordrecht: Reidel.
}
\author{David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
plotShapeTriangle()
}
\keyword{distribution}
