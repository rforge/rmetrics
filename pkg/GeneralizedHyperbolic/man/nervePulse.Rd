\name{nervePulse}
\docType{data}
\alias{nervePulse}
\title{Intervals Between Pulses Along a Nerve Fibre}
\usage{data(nervePulse)}
\description{
  Times between successive electric pulses on the surface of isolated
  muscle fibres.
  A vector of data containing 799 observations.
}
\format{
  The \code{nervePulse} data frame is a vector with 799 observations.
}
\details{
  The end-plates of resting muscle fibres are the seat of spontaneous
  electric discharges. The occurence of these spontaneous discharges at
  apparently normal synapses is studied in depth in Fatt and Katz
  (1951).
  The frequency and amplitute of these discharges was recorded. The
  times between each discharge were taken in milliseconds and this has
  been converted into the number of 1/50 sec intervals between
  successive pulses.
  There are 799 observations.
}
\source{
  Fatt, P., Katz, B. (1952)
  Spontaneous subthreshold activity at motor nerve endings.
  \emph{J. of Physiology},
  \bold{117}, 109--128.

  \enc{Jörgensen}{Jorgensen}, B. (1982)
  Statistical Properties of the Generalized Inverse Gaussian Distribution.
  \emph{Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York}
}

\examples{
data(nervePulse)
str(nervePulse)
attach(nervePulse)

### Fit the generalized inverse Gaussian distribution
gigFit(nervePulse)

}
\keyword{datasets}
