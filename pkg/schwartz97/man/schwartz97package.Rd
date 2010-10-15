\name{schwartz97-package}
\alias{schwartz97-package}
\alias{schwartz97}
\docType{package}
\title{

  Two-factor Commodity Model
  
}
% <---------------------------------------------------------------------->
\description{
  
  This package contains an implementation of the Schwartz two-factor
  commodity model, that is, the joint dynamics of the spot price and the
  spot convenience yield according to Schwartz (1997). The parameter
  estimation function constitutes the core of this package. Once the
  parameters are estimated, futures and European call and put options
  can be priced, term structures can be calculated and the usual
  distribution operations d/p/q/r can be carried out on the state
  variables as well as on futures prices. The package is accompanied by
  a variety of utility functions, futures data of ten commodities, and
  two vignettes describing technical details and usage of the package.

}
% <---------------------------------------------------------------------->
\details{
\tabular{ll}{
Package: \tab schwartz97\cr
Type: \tab Package\cr
Version: \tab 0.0.2\cr
Date: \tab 2010-10-15\cr
License: \tab  GPL (GNU Public License), Version 2 or later \cr
}
% <---------------------------------------------------------------------->
\strong{Initialization:}
\tabular{ll}{
  \code{\link{schwartz2f}} \tab       Initialize a Schwartz two-factor object.\cr
}
\strong{Density, distribution function, quantile function,
  random number generation, and trajectories of the state variables:}
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
\strong{Parameter estimation:}
\tabular{ll}{
  \code{\link{fit.schwartz2f}} \tab         Estimate parameters of the two-factor model.\cr
  \code{\link[=fitted.schwartz2f.fit]{fitted}} \tab Extract the model's fitted values.\cr
  \code{\link[=resid.schwartz2f.fit]{resid}} \tab Extract model residuals.\cr
}
\strong{Pricing:}
\tabular{ll}{
  \code{\link{pricefutures}} \tab  Compute arbitrage-free futures prices.\cr
  \code{\link{priceoption}} \tab  Compute arbitrage-free European option prices.\cr
}
\strong{Utilities:}
\tabular{ll}{
  \code{\link[=coef.schwartz2f]{coef}} \tab Extract model coefficients of \code{\link[=schwartz2f-class]{schwartz2f}}-objects.\cr
  \code{\link[=mean.schwartz2f]{mean}} \tab Extract the mean of \code{\link[=schwartz2f-class]{schwartz2f}}-objects.\cr
  \code{\link[=vcov.schwartz2f]{vcov}} \tab Extract the covariance matrix of \code{\link[=schwartz2f-class]{schwartz2f}}-objects.\cr
  \code{\link{filter.schwartz2f}} \tab Filter futures prices to get the spot price and convenience yield.\cr
  \code{\link[=plot.schwartz2f.fit]{plot}} \tab Plot \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}}-objects.\cr
  \code{\link[=plot.schwartz2f]{plot}} \tab Plot trajectories of \code{\link[=schwartz2f-class]{schwartz2f}}-objects.\cr
  \code{\link{futures}} \tab Use \code{data(futures)} to get data of 10 commodities.\cr
}



% <---------------------------------------------------------------------->
\strong{Package vignette:} \cr

The \R package \code{schwartz97} contains two vignettes:\cr

The vignette \emph{Technical Document} gives the necessary relations and
tools to fully understand the internals of the package. \cr

The vignette \emph{User Guide} discusses implementation details and
gives numerous examples and intuitive explanations. \cr

}
% <---------------------------------------------------------------------->
\author{

  David Luethi, Philipp Erb, Juri Hinz, Simon Otziger \cr

  Maintainer: David Luethi <luethid@gmail.com>

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
    Quantitative Analysis 33, 1998, 33-59\cr

    \emph{Valuation of Commodity Futures and Options under Stochastic
      Convenience Yields, Interest Rates, and Jump Diffusions in the Spot}
    by Jimmy E. Hilliard and Jorge Reis \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 61-86

}
% <---------------------------------------------------------------------->
\keyword{package}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}
\keyword{datagen}
