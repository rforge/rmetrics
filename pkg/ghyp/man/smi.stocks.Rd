\name{smi.stocks}
\alias{smi.stocks}
\docType{data}
% <---------------------------------------------------------------------->
\title{Daily returns of five swiss blue chips and the SMI}
% <---------------------------------------------------------------------->
\description{

  Daily returns from January 2000 to January 2007 of five swiss blue
  chips and the Swiss Market Index (SMI).

}
% <---------------------------------------------------------------------->
\usage{data(smi.stocks)}
% <---------------------------------------------------------------------->
\format{
  \describe{
    \item{\code{SMI}}{Swiss Market Index.}
    \item{\code{Novartis}}{Novartis pharma.}
    \item{\code{CS}}{Credit Suisse.}
    \item{\code{Nestle}}{Nestle.}
    \item{\code{Swisscom}}{Swiss telecom company.}
    \item{\code{Swiss.Re}}{Swiss reinsurer.}
    }
  }
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  pairs(smi.stocks)
}
% <---------------------------------------------------------------------->
\keyword{datasets}
