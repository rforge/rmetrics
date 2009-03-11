\name{logHist}
\alias{logHist}
\title{Plot Log-Histogram}
\description{
  Plots a log-histogram, as in for example Feiller, Flenley and Olbricht
  (1992).
  
  The intended use of the log-histogram is to examine the fit of a
  particular density to a set of data, as an alternative to a histogram
  with a density curve. For this reason, only the log-density histogram
  is implemented, and it is not possible to obtain a log-frequency
  histogram.
  
  The log-histogram can be plotted with histogram-like dashed vertical
  bars, or as points marking the tops of the log-histogram bars, or with
  both bars and points.
}

\usage{
  logHist(x, breaks = "Sturges", 
          include.lowest = TRUE, right = TRUE, 
          main = paste("Log-Histogram of", xName), 
          xlim = range(breaks), ylim = NULL, xlab = xName, 
          ylab = "Log-density", nclass = NULL, htype = "b", ...)

}
\arguments{
  \item{x}{A vector of values for which the log-histogram is desired.}
  \item{breaks}{One of:
    \itemize{
      \item a vector giving the breakpoints between log-histogram cells;
      \item a single number giving the number of cells for the log-histogram;
      \item a character string naming an algorithm to compute the
      number of cells (see \bold{Details});
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only.
  }
  \item{include.lowest}{Logical. If \code{TRUE},
    an `x[i]' equal to the `breaks' value will be included in the first
    (or last, for \code{right = FALSE}) bar.}
  \item{right}{Logical. If \code{TRUE}, the log-histograms cells are
    right-closed (left open) intervals.}
  \item{main, xlab, ylab}{These arguments to \code{title} have useful
    defaults here.}
  \item{xlim}{Sensible default for the range of x values.}
  \item{ylim}{Calculated by \code{logHist}, see \bold{Details}.}
  \item{nclass}{Numeric (integer).  For compatibility with \code{hist} only,
    \code{nclass} is equivalent to \code{breaks} for a scalar or
    character argument.}
      \item{htype}{Type of histogram. Possible types are:
	\itemize{
	\item '"h"' for a *h*istogram only;
	\item '"p"' for *p*oints marking the top of the histogram bars only;
	\item '"b"' for *b*oth.
	}
}
  \item{\dots}{Further graphical parameters for calls
		to \code{plot} and \code{points}.}
}
\details{
  Uses \code{\link{hist.default}} to determine the cells or classes and
  calculate counts.
   
  To calculate \code{ylim} the following procedure is used. The upper
  end of the range is given by the maximum value of the log-density,
  plus 25\% of the absolute value of the maximum. The lower end of the
  range is given by the smallest (finite) value of the log-density, less
  25\% of the difference between the largest and smallest (finite) values 
  of the log-density.
  
  A log-histogram in the form used by Feiller, Flenley and Olbricht (1992)
  is plotted. See also Barndorff-Nielsen (1977) for use of log-histograms.
}
\value{
  Returns a list with components:
  \item{breaks}{The \eqn{n+1} cell boundaries (= \code{breaks} if that
    was a vector).}
  \item{counts}{\eqn{n} integers; for each cell, the number of
    \code{x[]} inside.}
  \item{logDensity}{Log of \eqn{\hat f(x_i)}{f^(x[i])}, which are estimated
    density values. 
    
    If \code{all(diff(breaks) == 1)}, estimated density values are the
    relative frequencies \code{counts/n} and in general satisfy
    \eqn{\sum_i \hat f(x_i) (b_{i+1}-b_i) = 1}{sum[i; f^(x[i])
      (b[i+1]-b[i])] = 1}, where \eqn{b_i}{b[i]} = \code{breaks[i]}.}
  \item{mids}{The \eqn{n} cell midpoints.}
  \item{xName}{A character string with the actual \code{x} argument name.}
  \item{heights}{The location of the tops of the vertical segments used
    in drawing the log-histogram.}
  \item{ylim}{The value of \code{ylim} calculated by \code{logHist}.}
}
\references{ 
  Barndorff-Nielsen, O. (1977)
  Exponentially decreasing distributions for the logarithm of particle size,
  \emph{Proc. Roy. Soc. Lond.},
  A\bold{353}, 401--419.
  
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions. 
  In \emph{Encyclopedia of Statistical Sciences}, 
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.
  
  Fieller, N. J., Flenley, E. C. and Olbricht, W. (1992)
  Statistics of particle size data.
  \emph{Appl. Statist.},
  \bold{41}, 127--146.
}
\author{David Scott \email{d.scott@auckland.ac.nz}, Richard Trendall,
  Thomas Tran
}

\seealso{\code{\link{hist}}}

\examples{
data(SandP500)
### Consider proportional changes in the index
change <- SandP500[-length(SandP500)]/SandP500[-1]
hist(change)
logHist(change)
### Show points only
logHist(change, htype = "p", pch = 20, cex = 0.5)
### Fit the hyperbolic distribution to the changes
hyperbFit(change)
}
\keyword{hplot}
\keyword{distribution}
