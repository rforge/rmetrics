\name{mamquam}
\docType{data}
\alias{mamquam}
\title{Size of Gravels from Mamquam River}
\usage{data(mamquam)}
\description{
  Size of gravels collected from a sandbar in the Mamquam River,
  British Columbia, Canada. Summary data, giving the frequency of
  observations in 16 different size classes.
}
\format{
  The \code{mamquam} data frame has 16 rows and 2 columns.
  \tabular{rll}{
    [, 1] \tab midpoints  \tab midpoints of intervals (psi units) \cr
    [, 2] \tab counts      \tab number of observations in interval \cr
  }
}
\details{
  Gravel sizes are determined by passing clasts through templates of
  particular sizes. This gives a range in which the size of each clast
  lies. Sizes (in mm) are then converted into psi units by taking the
  base 2 logarithm of the size. The midpoints specified are the midpoints
  of the psi unit ranges, and counts gives the number of observations
  in each size range. The classes are of length 0.5 psi units. 
  There are 3574 observations.
}
\source{
  Rice, Stephen and Church, Michael (1996)
  Sampling surficial gravels: the precision of size distribution
  percentile estimates.
  \emph{J. of Sedimentary Research},
  \bold{66}, 654--665.
}

\examples{
data(mamquam)
str(mamquam)
attach(mamquam)
### Construct data from frequency summary, taking all observations
### at midpoints of intervals
psi <- rep(midpoints, counts)
barplot(table(psi))
### Fit the hyperbolic distribution
hyperbFit(psi)

### Actually hyperbFit can deal with frequency data
hyperbFit(midpoints, freq=counts)
}
\keyword{datasets}
