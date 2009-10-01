\name{lrnokeur}
\alias{lrnokeur}
\title{Log Returns of the NOK/EUR Exchange Rate}
\description{
  Log returns of daily closing value data of the NOK/EUR (Norwegian
  Kroner/Euro) exchange rate, from 04/JAN/1999 to 08/JUL/2003. The
  original data was downloaded from the oanda website. The data was
  selected to be as similar as possible to the data used in the Aas &
  Haff article (see References).
}
\usage{
data(lrnokeur)
}
\format{
  A vector of 1647 observations.
}
\source{
  http://www.oanda.com
}
\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's \emph{t}-distribution,
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.
}
\examples{
##Fit the skew hyperbolic students-t distribution to the data
data(lrnokeur)
fit <- skewhypFit(lrnokeur, method="nlm", plot=TRUE, print=TRUE)
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\keyword{datasets}
