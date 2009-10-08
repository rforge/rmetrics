\name{lrdji}
\alias{lrdji}
\docType{data}
\title{Dow Jones Log Return Data}
\description{
  Log returns of daily closing value data from the dow
  jones index, from 04/JAN/1999 to 08/JUL/2003. The original data used
  to calculate these was the dji data set available in the QRMlib package.
}
\usage{
data(lrdji)
}
\format{
  A vector of 1132 observations.
}
\source{
  library(QRMlib)
  data(dji)
}
\references{
  McNeil, A. & Ulman, S. (2008).
  QRMlib \emph{http://cran.r-project.org/web/packages/QRMlib/index.html}
}
\examples{
data(lrdji)
##fit a skew hyperbolic student t-distribution to the data
fit <- skewhypFit(lrdji, plot = TRUE, print = TRUE)
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\keyword{datasets}
