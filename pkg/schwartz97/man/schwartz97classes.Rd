\name{schwartz2f-class-hierarchy}
\docType{class}
\alias{schwartz2f-class}
\alias{schwartz2f.fit}
\alias{schwartz2f.fit-class}
\alias{show.schwartz2f}
\alias{show.schwartz2f.fit}
\alias{show,schwartz2f-method}
\alias{show,schwartz2f.fit-method}
% <---------------------------------------------------------------------->
\title{Classes schwartz2f and schwartz2f.fit}
% <---------------------------------------------------------------------->
\description{

  The \code{schwartz2f} class stores parameters which determine initial
  values and the dynamics of the state variables. The class
  \code{schwartz2f.fit} inherits from the \code{schwartz2f} class. The
  class \code{schwartz2f.fit} adds slots which contain data regarding
  the estimation procedure and parameters of the risk-neutral
  dynamics. In particular, it adds the market price of convenience yield
  risk \code{lambda} and the interest rate.

}
% <---------------------------------------------------------------------->
\section{Objects from the Class}{

  Objects should only be created by calls to the constructors
  \code{\link{schwartz2f}} and \code{\link{fit.schwartz2f}}.

}
% <---------------------------------------------------------------------->
\section{Slots}{
  \strong{Slots of class \dQuote{schwartz2f}:}
    \describe{
    \item{\code{call}:}{The function-call of class \code{call}.}
    \item{\code{s0}:}{Initial commodity spot-price of class \code{numeric}.}
    \item{\code{delta0}:}{Initial value of the convenience yield of
      class \code{numeric}.}
    \item{\code{mu}:}{Enters the drift of the commodity spot price (under
      the objective measure, see \bold{Details}) of class \code{numeric}.}
    \item{\code{sigmaS}:}{Diffusion parameter of the spot price process
      of class \code{numeric}.} 
    \item{\code{kappaE}:}{Speed of mean-reversion of the
      convenience-yield process of class \code{numeric}.}
    \item{\code{alpha}:}{Mean level of the convenience-yield process of
      class \code{numeric}.}
    \item{\code{sigmaE}:}{Diffusion parameter of the convenience-yield
      process of class \code{numeric}.} 
    \item{\code{rhoSE}:}{Correlation between the two Brownian motions
      which drive the spot price and convenience-yield processes of
      class \code{numeric}.}
  }

  \strong{Slots added by class \dQuote{schwartz2f.fit}:}
  \describe{
    \item{\code{n.iter}:}{The number of iterations of class
      \code{numeric}.}
    \item{\code{llh}:}{The log likelihood value of class
      \code{numeric}.}
    \item{\code{converged}:}{A \code{logical} stating whether the fit converged or
      not.}
    \item{\code{error.code}:}{An error code of class \code{numeric}. The
      value of \code{\link{optim}}'s \dQuote{convergence}. If an unknown
      error occurs the value -1 is returned.}
    \item{\code{error.message}:}{An error message of class
      \code{character}.}
    \item{\code{fitted.params}:}{A \code{logical} vector stating which
      parameters were fitted.}
    \item{\code{trace.pars}:}{Contains the parameter value evolution
    during the fitting procedure of class \code{matrix}.}
    \item{\code{r}:}{The risk-free interest rate of class \code{numeric}.}
    \item{\code{alphaT}:}{The mean-value of the convenience yield
      process under the equivalent martingale measure of class 
    \code{numeric} (see \bold{Details}).}
    \item{\code{lambda}:}{The market price of convenience yield risk of
      class \code{numeric}.}
    \item{\code{meas.sd}:}{The standard deviation of the measurement
      equantion of class \code{numeric}.}
  \item{\code{deltat}:}{The time-increment of the transition equation
    of class \code{numeric}.}

  }
}
% <---------------------------------------------------------------------->
\section{Details}{

  The joint dynamics of the spot-price and the convenience yield are
  given by the stochastic differential equations
  \deqn{dS_t = (\mu - \delta_t) S_t dt + \sigma_S~S_t~dW_t^1,}{dS = (mu - delta) * S * dt + sigmaS * S * dW1,}
  \deqn{d \delta_t = \kappa (\alpha - \delta_t) dt +\sigma_E dW_t^2}{d delta = kappa * (alpha - delta) * dt + sigmaE * dW2}
  \deqn{dW_t^1~dW_t^2 = \rho dt,}{dW1 * dW2 = rho dt,} where
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
Class \dQuote{schwartz2f.fit} extends class \dQuote{schwart2factor}, directly.
}
% <---------------------------------------------------------------------->
% \section{Methods}{
%   Methods for \dQuote{schwartz2f}-objects:\cr
%   \code{\link{coef}}, ...
% }
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
    Quantitative Analysis 33, 1998, 33-59\cr

    \emph{Valuation of Commodity Futures and Options under Stochastic
      Convenience Yields, Interest Rates, and Jump Diffusions in the Spot}
    by Jimmy E. Hilliard and Jorge Reis \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 61-86

}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{schwartz2f}} to initialize
  \code{schwartz2f}-objects. \code{\link{fit.schwartz2f}} to fit the
  two-factor model to data and get a \code{schwartz2f.fit} object,
  \code{\link{schwartz97-package}} for an overview.

}
% <---------------------------------------------------------------------->
\examples{

obj <- schwartz2f() # create an object of class schwartz2f
obj          # print it
coef(obj)    # get coefficients 
unclass(obj) # see the slots 

## create an object of class schwartz2f.fit
data(futures)
fit.obj <- fit.schwartz2f(futures$wheat$price, futures$wheat$ttm / 260,
                          deltat = 1 / 260, control = list(maxit = 3))
fit.obj          # print it
coef(fit.obj)    # get coefficients 
unclass(fit.obj) # see the slots 

}
% <---------------------------------------------------------------------->
\keyword{classes}
\keyword{models}
