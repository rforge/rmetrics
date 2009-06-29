\name{futures-plot}
\alias{futuresplot}
\title{Visualization of Futures Data}

% <---------------------------------------------------------------------->
\description{

  Visualization of historical commodity futures or spot prices and
  remaining time to maturity.  This function is intended to be fed with
  the futures data contained in this package (see
  \code{\link{futures-data}}).

}

% <---------------------------------------------------------------------->
\usage{

% \method{plot}{futures}(x, type = c("futures", "ttm"), contr = 1:8,
%              range = c(x$date[1], x$date[length(x$date)]), \dots)
futuresplot(x, type = c("futures", "ttm"), contr = 1:8,
             range = c(x$date[1], x$date[length(x$date)]), ...)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{x}{A \code{list} with elements \code{$date}, \code{$futures}
    and \code{$ttm} containing historical commodity futures data.}
  \item{type}{Which information shall be plotted. \code{"futures"} for
    futures prices and \code{"ttm"} for the remaining time to maturity
    function.}
  \item{contr}{Selection of the contracts to be plottet. \code{1} stand
    for the contract closest to maturity, \code{2} represents the
    futures contract second-closest to maturity and so on.}
  \item{range}{Time window.}
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

  futuresplot(soybeans, type = "fut", contr = c(1, 2, 6),
               range = c("01.01.1978", "31.12.1982"))

  futuresplot(crude.oil, type = "ttm", contr = c(1, 3),
               range = c("01.01.2008", "31.12.2008"))

  futuresplot(copper)
}

% <---------------------------------------------------------------------->
\keyword{datasets}
