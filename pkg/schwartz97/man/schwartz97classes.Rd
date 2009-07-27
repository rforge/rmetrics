\name{schwartz2factor-class-hierarchy}
\docType{class}
\alias{schwartz2factor-class}
\alias{fit.schwartz2factor}
\alias{fit.schwartz2factor-class}
\alias{show.schwartz2factor}
\alias{show.fit.schwartz2factor}
\alias{show,schwartz2factor-method}
\alias{show,fit.schwartz2factor-method}
% <---------------------------------------------------------------------->
\title{Classes schwartz2factor and fit.schwartz2factor}
% <---------------------------------------------------------------------->
\description{

  The \code{schwartz2factor} class stores parameters which appear in the
  dynamics of the state variables (under the objective measure). The
  class \code{fit.schwartz2factor} inherits from the
  \code{schwartz2factor} class. The class \code{fit.schwartz2factor}
  adds slots containing data regarding the fit and parameters of the
  risk-neutral dynamics. In particular, it adds the market-price-of
  convenience-yield risk \code{lambda} and the interest rate.

}
% <---------------------------------------------------------------------->
\section{Objects from the Class}{

  Objects should only be created by calls to the constructors
  \code{\link{schwartz2factor}} and \code{\link{fit2factor}}.

}
% <---------------------------------------------------------------------->
\section{Slots}{
  \strong{Slots of class \dQuote{schwartz2factor}:}
    \describe{
    \item{\code{call}:}{The function-call of class \code{call}.}
    \item{\code{s0}:}{Initial commodity spot-price of class \code{numeric}.}
    \item{\code{delta0}:}{Initial value of the convenience-yield of
      class \code{numeric}.}
    \item{\code{mu}:}{Enters the drift of the commodity spot-price (in
      the Objective measure, see \bold{Details}) of class \code{numeric}.}
    \item{\code{sigmaS}:}{Diffusion parameter of the spot-price process of class \code{numeric}.}
    \item{\code{kappaE}:}{Speed of mean-reversion of the
      convenience-yield process of lass \code{numeric}.}
    \item{\code{alpha}:}{Mean-level of the convenience-yield process of class \code{numeric}.}
    \item{\code{sigmaE}:}{Diffusion parameter of the convenience-yield process of class \code{numeric}.}
    \item{\code{rhoSE}:}{Correlation between the two Brownian motions
      which drive the spot-price and convenience-yield processes of
      class \code{numeric}.}
  }

  \strong{Slots added by class \dQuote{fit.schwartz2factor}:}
  \describe{
    \item{\code{n.iter}:}{The number of iterations of class
      \code{numeric}.}
    \item{\code{llh}:}{The log likelihood value of class
      \code{numeric}.}
    \item{\code{converged}:}{A boolean whether the fit converged or not.
                             Object of class \code{logical}.}
    \item{\code{error.code}:}{An error code of class \code{numeric}.}
    \item{\code{error.message}:}{An error message of class
      \code{character}.}
    \item{\code{fitted.params}:}{A boolean vector stating which
      parameters were fitted of class \code{logical}.}
    \item{\code{trace.pars}:}{Contains the parameter value evolution
    during the fitting procedure of class \code{matrix}.}
    \item{\code{r}:}{The risk-free interest rate of class \code{numeric}.}
    \item{\code{alphaT}:}{The mean-value of the convenience-yield
    process under the equivalent martingale measure of class
    \code{numeric} (see \bold{Details}).}
    \item{\code{lambda}:}{The market price of convenience-yield risk of
      class \code{numeric}.}
    \item{\code{meas.sd}:}{The standard deviation of the measurement equantion of class
      \code{numeric}.}
%     \item{\code{state}:}{The spot-price and the convenience-yield
%     filtered based on the fitted parameters of class \code{matrix}.}
  \item{\code{deltat}:}{The time-increment of the transition equation
    of class \code{numeric}.}

  }
}
% <---------------------------------------------------------------------->
\section{Details}{

  The joint dynamics of the spot-price and the convenience yield are
  given by the stochastic differential equations \deqn{dS_t = (\mu -
  \delta_t) S_t dt + \sigma_S~S_t~dW_t^1,}{dS = (mu - delta) * S * dt +
  sigmaS * S * dW1,} \deqn{d \delta_t = \kappa (\alpha - \delta_t) dt +
  \sigma_E dW_t^2}{d delta = kappa * (alpha - delta) * dt + sigmaE *
  dW2} \deqn{dW_t^1~dW_t^2 = \rho dt,}{dW1 * dW2 = rho dt,} where
  \eqn{W^1, \, W^2}{W1, W2} are Brownian motions under the objective
  measure.

  Under an equivalent martingale measure (the pricing measure) the
  dynamics is \deqn{dS_t = (r - \delta_t) S_t dt +
  \sigma_S~S_t~d\tilde{W}_t^1}{dS = (r - delta) * S * dt + sigmaS * S *
  dW1*} \deqn{d \delta_t = \kappa (\tilde{\alpha} - \delta_t) dt +
  \sigma_E d\tilde{W}_t^2,}{d delta = kappa * (alphaT - delta) * dt +
  sigmaE * dW2*,} where \eqn{\tilde{W}^1, \, \tilde{W}^2}{W1*, W2*} are
  Brownian motions with respect to the martingale measure.

  \eqn{\tilde{\alpha} = \alpha - \lambda / \kappa}{alphaT = alpha -
  lambda / kappa} where \eqn{\lambda}{lambda} is the market price of
  convenience-yield risk.

}
% <---------------------------------------------------------------------->
\section{Extends}{
Class \dQuote{fit.schwartz2factor} extends class \dQuote{schwart2factor}, directly.
}
% <---------------------------------------------------------------------->
\section{Methods}{
   A \dQuote{coef} method.
}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}
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
\seealso{

  \code{\link{schwartz2factor}} to initialize
  objects. \code{\link{fit2factor}} to fit the two-factor model to data
  and get a \code{fit.schwartz2factor} object.

}
% <---------------------------------------------------------------------->
\examples{
  obj <- schwartz2factor()
  coef(obj)
}
% <---------------------------------------------------------------------->
\keyword{classes}

