\name{MillsRatio}
\alias{MillsRatio}
\alias{millsR}


\title{Mills Ratio}
\description{
  Calculates the Mills ratio
}
\usage{
millsR(y, log = FALSE)

}
\arguments{
  \item{y}{Numeric, Value at which the mills ratio is evaluated.}
  \item{log}{Logical; if \code{log = TRUE}, mills ratios are given as
   log(millsR).}

}

\value{
  The Mill's Ratio is
  \deqn{R(z)=\frac{1-\Phi(z)}{\phi(z)}}{}%

}

\details{
  the function was written to allow us to calculate the mills ratio.
  since the Mill's Ratio is not stable for z greater than 7
  or so, we use the log function to get around the instability.


}

\author{David Scott \email{d.scott@auckland.ac.nz}, Jason Shicong Fu}



\examples{

## compare the millsR calculated directly with the millsR calculated
## by transforming to log scale and then back-transformed
unlist(lapply(1:10,millsR))
exp(unlist(lapply(1:10,millsR, log = TRUE)))
exp(unlist(lapply(10*(1:10),millsR)))
exp(unlist(lapply(10*(1:10),millsR, log = TRUE)))


}

\keyword{math}
