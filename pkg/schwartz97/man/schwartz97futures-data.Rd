\name{futures-data}
\alias{futures-data}
\alias{copper}
\alias{crude.oil}
\alias{soybeans}
\docType{data}
% <---------------------------------------------------------------------->
\title{Daily futures prices}
% <---------------------------------------------------------------------->
\description{

  Prices of the eigth closest to maturity futures contracts of different
  commoditites. The prices are historical prices provided by Bloomberg
  plus Gaussian white noise with one percent relative standard
  deviation.

}
% <---------------------------------------------------------------------->
\usage{data(futures)}
% <---------------------------------------------------------------------->
\format{
  \describe{
    \item{\code{date}}{The observation date as character.}
    \item{\code{futures}}{Futures prices of eight different futures
      contracts as a \code{\link{matrix}}. The colnames are
      \dQuote{price1}, \dots, \dQuote{price8} where \dQuote{price1}
      denotes the closest to maturity contract.}
    \item{\code{ttm}}{The time to maturity of the futures contracts
      as a \code{\link{matrix}}. The colnames are
      \dQuote{mat1}, \dots, \dQuote{mat8} (see \bold{Details}.)}
    % \item{\code{spot}}{A numeric vector containing the spot price.}
    }
  }
% <---------------------------------------------------------------------->
\details{

  The elements \code{$futures} and \code{$ttm} have the following
  interpretation: \code{$futures[i,j]} denotes the price of the futures
  contract with time to maturity \code{$ttm[i,j]}.

  Since the datasets also include holidays and weekends, the element
  \code{$futures} contains \code{NA} values.

}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit2factor}},
  \code{\link{rfutures}},
  \code{\link{futuresplot}}

}
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
