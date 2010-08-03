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

futuresplot(futures, type = c("forward.curve", "ttm"), ...)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{futures}{A \code{list} with elements \code{price} and
    \code{ttm}. Usually an element of \code{\link{futures}}.}
  \item{type}{Which information shall be plotted. \code{"forward.curve"}
    or \code{"ttm"} (time to maturity).}
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

### Plot time to maturity of corn data
futuresplot(futures$corn, type = "ttm")

### Plot forward curves of wheat data since Jan 2010
wheat.2010 <- lapply(futures$wheat, function(x)x[as.Date(rownames(x)) > "2010-01-01",])
futuresplot(wheat.2010, type = "forward.curve")

}
% <---------------------------------------------------------------------->
\keyword{hplot}

