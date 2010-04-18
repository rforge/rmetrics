\name{schwartz97-package}
\alias{schwartz97-package}
\alias{schwartz97}
\docType{package}
\title{

  Two-factor Commodity Model
  
}
% <---------------------------------------------------------------------->
\description{
  
  This package contains an implementation of the Schwartz' two-factor
  commodity model, that is, the joint dynamics of the the spot price and
  the spot convenience according to Schwartz (1997). The parameter
  estimation function constitutes the core of this package. Once the
  parameters are known, futures and European call and put options can be
  priced, risk measures can be calculated and the usual distribution
  operations d/p/q/r can be carried out on the state varibles as well as
  on the futures price.

}
% <---------------------------------------------------------------------->
\details{
\tabular{ll}{
Package: \tab schwartz97\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2009-09-03\cr
License: \tab  GPL (GNU Public Licence), Version 2 or later \cr
}
% <---------------------------------------------------------------------->
\strong{Initialization:}
\tabular{ll}{
  \code{\link{schwartz2f}} \tab       Initialize a Schwartz 2-factor object.\cr
}

\strong{Density, distribution function, quantile function,
  random number generation, and trajectories of the state processes:}
\tabular{ll}{
  \code{\link{dstate}} \tab         Density of the spot and the convenience yield.\cr
  \code{\link{pstate}} \tab         Distribution of the spot and the convenience yield.\cr
  \code{\link{qstate}} \tab         Quantile of the spot and the convenience yield.\cr
  \code{\link{rstate}} \tab         Random number generation of the spot and the convenience yield.\cr
  \code{\link{simstate}} \tab       Trajectory of the spot and the convenience yield.\cr
}

\strong{Density, distribution function, quantile function, and
  random number generation of the futures price:}
\tabular{ll}{
  \code{\link{dfutures}} \tab         Density of the futures price.\cr
  \code{\link{pfutures}} \tab         Distribution of the futures price.\cr
  \code{\link{qfutures}} \tab         Quantile of the futures price.\cr
  \code{\link{rfutures}} \tab         Random number generation of the futures price.\cr
}


% <---------------------------------------------------------------------->
\strong{Package vignettes:} \cr

The \R package \code{schwartz97} contains two vignettes:\cr

A \emph{Technical Document} which gives the necessary relations and
tools to fully understand the internals of the package.

A \emph{User Guide} with explanations and some examples.

}
% <---------------------------------------------------------------------->
\author{
David Luethi \cr
Clariden Leu SA
(\url{http://www.claridenleu.com}) \cr

Juri Hinz \cr
National University of Singapore
(\url{http://www.math.nus.edu.sg}) \cr

Philipp Erb \cr
Zurich Global Corporate Switzerland
(\url{http://www.zurich.ch}) \cr

Simon Otziger \cr
Institute of Data Analysis and Process Design
(\url{http://www.idp.zhaw.ch}) \cr

Maintainer: Simon Otziger <otis@zhaw.ch>
}
% <---------------------------------------------------------------------->
\references{

    \emph{Stochastic Convenience Yield and the Pricing of Oil Contingent
    Claims} by Rajna Gibson and Eduardo S. Schwartz \cr The Journal of
    Finance 45, 1990, 959-976 \cr

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973 \cr

    \emph{Pricing of Options on Commodity Futures with Stochastic Term
    Structures of Convenience Yields and Interest Rates} by Kristian
    R. Miltersen and Eduardo S. Schwartz \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 33-59 

}
% <---------------------------------------------------------------------->
\keyword{package}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}
\keyword{datagen}
