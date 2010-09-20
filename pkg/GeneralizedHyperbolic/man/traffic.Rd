\name{traffic}
\docType{data}
\alias{traffic}
\title{Intervals Between Vehicles on a Road}
\usage{data(traffic)}
\description{
  Intervals between the times that 129 successive vehicles pass a point
  on a road, measured in seconds.
}
\format{
  The \code{traffic} data frame is a vector of 128 observations.
}
\source{
  Bartlett, M.S. (1963)
  Statistical estimation of density functions
  \emph{Sankhya: The Indian Journal of Statistics},
  \bold{Series A, Vol. 25, No. 3}, 245--254.

  \enc{Jörgensen}{Jorgensen}, B. (1982)
  Statistical Properties of the Generalized Inverse Gaussian Distribution.
  \emph{Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York}
}

\examples{
data(traffic)
str(traffic)
attach(traffic)

### Fit the generalized inverse Gaussian distribution
gigFit(traffic)
}
\keyword{datasets}
