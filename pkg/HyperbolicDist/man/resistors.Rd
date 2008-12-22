\name{resistors}
\docType{data}
\alias{resistors}
\title{Resistance of One-half-ohm Resistors }
\description{
    This data set gives the resistance in ohms of 500 nominally one-half-ohm
    resistors, presented in Hahn and Shapiro (1967). Summary data giving the
    frequency of observations in 28 intervals.
}
\usage{data(resistors)}
\format{
  The \code{resistors} data frame has 28 rows and 2 columns.
  \tabular{rll}{
    [, 1] \tab midpoints  \tab midpoints of intervals (ohm) \cr
    [, 2] \tab counts     \tab number of observations in interval \cr
  }    
}
\source{
    Hahn, Gerald J. and Shapiro, Samuel S. (1967)
    \emph{Statistical Models in Engineering}.
    New York: Wiley,
    page 207.
}
\references{
    Chen, Hanfeng, and Kamburowska, Grazyna (2001) 
    Fitting data to the Johnson system.
    \emph{J. Statist. Comput. Simul.},
    \bold{70}, 21--32.
}
\examples{
data(resistors)
str(resistors)
attach(resistors)
### Construct data from frequency summary, taking all observations
### at midpoints of intervals
resistances <- rep(midpoints,counts)
hist(resistances)
logHist(resistances)
## Fit the hyperbolic distribution
hyperbFit(resistances) 

## Actually fit.hyperb can deal with frequency data
hyperbFit(midpoints, freq=counts)

}
\keyword{datasets}
