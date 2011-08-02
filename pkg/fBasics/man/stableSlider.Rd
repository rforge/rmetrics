\name{StableSlider}
\title{Slider GUI for Stable Distribution}
\alias{stableSlider}
\description{
  The \code{stableSlider()} function provides interactive displays of
  density and probabilities of stable distributions.
}
\usage{
stableSlider(col= "steelblue", col.med = "gray30")
}
\arguments{
  \item{col, col.med}{optional arguments for the slider.}
}

\value{
    The \code{stableSlider()} function displays densities and
    probabilities of the skew stable distribution, for educational
    purposes.
}
\author{
  Diethelm Wuertz for the Rmetrics \R-port.
}

\references{
  see those in \code{\link[stabledist]{dstable}}, in package \pkg{stabledist}.
}

\examples{
if(dev.interactive())
   stableSlider()
}
\keyword{distribution}
