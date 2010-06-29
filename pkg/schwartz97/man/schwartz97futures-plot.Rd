\name{futures-plot}
\alias{futuresplot}
\title{Visualization of Futures Data}

% <---------------------------------------------------------------------->
\description{

  Visualization of historical commodity futures prices and
  remaining time to maturity. This function is intended to be fed with
  the futures data contained in this package (see
  \code{\link{futures-data}}).

}

% <---------------------------------------------------------------------->
\usage{

futuresplot(futures, type = c("forward.curve", "ttm"), plot.legend = TRUE, ...)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{futures}{A \code{list} with elements \code{price} and
    \code{ttm}. Usually an element of \code{\link{futures}}.}
  \item{type}{Which information shall be plotted. \code{"forward.curve"}
    or \code{"ttm"} (time to maturity).}
  \item{plot.legend}{Whether a \code{\link{legend}} shall be plotted.}
  \item{\dots}{Optional arguments passed to \code{plot}.}

}

% <---------------------------------------------------------------------->
% \value{
% }

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{\code{\link{futures-data}}}

% <---------------------------------------------------------------------->
\examples{

  data(futures)

  futuresplot(futures$corn, type = "ttm")

  futuresplot(lapply(futures$copper, function(x)x[100:500,]),
              type = "forward.curve")
}

% <---------------------------------------------------------------------->
\keyword{hplot}

