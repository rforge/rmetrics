\name{futures-data}
\alias{futures-data}
\alias{futures}
\docType{data}
% <---------------------------------------------------------------------->
\title{Daily futures prices}
% <---------------------------------------------------------------------->
\description{

  Futures prices, time to maturity, open interest, volume, underlying
  tickers, and last trade date of ten different commoditites: corn,
  wheat, soybean, soybean meal, soybean oil, lumber, live cattle,
  coffee, heating oil, copper.

  There are, depending on the liquidity of the commodity, between 4 and
  10 \sQuote{clean} closest to maturity futures price series.

}
% <---------------------------------------------------------------------->
\usage{data(futures)}
% <---------------------------------------------------------------------->
\format{
 
  A list containing ten commodities as lists:
  \dQuote{corn}, \dQuote{wheat}, \dQuote{soybean}, \dQuote{soybean.meal},
  \dQuote{soybean.oil}, \dQuote{lumber}, \dQuote{live.cattle},
  \dQuote{coffee}, \dQuote{heating.oil}, \dQuote{copper}. \cr

  Each list contains six dimnamed matrices:
  \describe{
    \item{\code{price}}{Daily futures prices.}
    \item{\code{ttm}}{The time to maturity of the futures contracts
      in units of days (see \bold{Details}.)}
    \item{\code{oi}}{Open interest.}
    \item{\code{vol}}{Volume.}
    \item{\code{underl.tickers}}{Underlying tickers / contracts.}
    \item{\code{last.trade.dt}}{Last trade date as character in the ISO
      8601 international standard format.}
  }

  The i-th column of each matrix contains data for the i-th closest to
  maturity contract. The i-th column name is the ticker of the i-th
  generic futures.

  \tabular{rllll}{
    \bold{Commodity} \tab \bold{# Contracts} \tab \bold{Exchange} \tab \bold{Start date} \tab \bold{End date}\cr
    Corn \tab  6 \tab CBOT \tab 1997-01-02 \tab 2010-04-07 \cr
    Wheat \tab 5 \tab CBOT \tab 1995-01-03 \tab 2010-04-07 \cr
    Soybean \tab 7 \tab CBOT \tab 1995-01-03 \tab 2010-04-07 \cr
    Soybean meal \tab 6 \tab CBOT \tab 2000-01-03 \tab 2010-04-07 \cr
    Soybean oil \tab 6 \tab CBOT \tab 1995-01-03 \tab 2010-04-07 \cr
    Lumber \tab 4 \tab CME \tab 1995-01-03 \tab 2010-04-07 \cr
    Live cattle \tab 6 \tab CME \tab 2004-07-01 \tab 2010-04-07 \cr
    Coffee \tab 5 \tab ICE \tab 1995-01-03 \tab 2010-04-07 \cr
    Heating oil \tab 10 \tab NYMEX \tab 1995-01-03 \tab 2010-03-31 \cr
    Copper \tab 8 \tab COMEX \tab 1996-01-02 \tab 2010-02-24 \cr
  }

}
% <---------------------------------------------------------------------->
\details{

  The elements of \code{price} and \code{ttm} have the following
  interpretation: \code{price[i,j]} denotes the futures price whose time
  to maturity was \code{ttm[i,j]} days when it was observed. 

}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

    \code{\link{futuresplot}}, \code{\link{fit.schwartz2f}}.

}
% <---------------------------------------------------------------------->
\examples{

data(futures)

## Plot forward curves of lumber
futuresplot(futures$lumber, type = "forward.curve")

## Plot time to maturity of heating oil data
futuresplot(futures$heating.oil, type = "ttm")

\dontrun{
## Convert to zoo-objects:
require(zoo)
futures.zoo <- rapply(futures, function(x)zoo(x, as.Date(rownames(x))),
                      classes = "matrix", how = "list")

## ...and plot it nicely using plot.zoo:
plot(futures.zoo$heating.oil$ttm)
plot(futures.zoo$wheat$vol)
plot(futures.zoo$copper$oi)
}
}

% <---------------------------------------------------------------------->
\keyword{datasets}
