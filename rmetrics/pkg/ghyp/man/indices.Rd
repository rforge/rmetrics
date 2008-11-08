\name{indices}
\alias{indices}
\docType{data}
% <---------------------------------------------------------------------->
\title{Monthly returns of five indices}
% <---------------------------------------------------------------------->
\description{

  Monthly returns of indices representing five asset/investment classes
  \emph{Bonds}, \emph{Stocks}, \emph{Commodities}, \emph{Emerging
  Markets} and \emph{High Yield Bonds}.

}
% <---------------------------------------------------------------------->
\usage{data(indices)}
% <---------------------------------------------------------------------->
\format{
  \describe{
    \item{\code{hy.bond}}{JPMorgan High Yield Bond A (Yahoo symbol \dQuote{OHYAX}).}
    \item{\code{emerging.mkt}}{Morgan Stanley Emerging Markets Fund Inc. (Yahoo symbol \dQuote{MSF}).}
    \item{\code{commodity}}{Dow Jones-AIG Commodity Index (Yahoo symbol \dQuote{DJI}).}
    \item{\code{bond}}{Barclays Global Investors Bond Index (Yahoo symbol \dQuote{WFBIX}).}
    \item{\code{stock}}{Vanguard Total Stock Mkt Idx (Yahoo symbol \dQuote{VTSMX}).}
    }
  }
% <---------------------------------------------------------------------->
\seealso{\code{\link{smi.stocks}}}
% <---------------------------------------------------------------------->
\examples{
  data(indices)

  pairs(indices)
}
% <---------------------------------------------------------------------->
\keyword{datasets}
