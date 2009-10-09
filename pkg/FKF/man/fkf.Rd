\name{fkf}
\alias{fkf}
%\alias{FKF}
\title{Fast Kalman filter}
% <---------------------------------------------------------------------->
\description{

  This function allows for fast and flexible Kalman filtering. The
  state-space parameters are allowed to be time-varying and intercepts
  are supported in both, the transition equation and the measurement
  equation. In addition \dQuote{NA}-values in the observations are
  supported. \code{fkf} wraps the \code{C}-function \code{FKF} which
  fully relies on linear algebra subroutines contained in BLAS and
  LAPACK.

}
% <---------------------------------------------------------------------->
\usage{

fkf(a0, P0, dt, ct, Tt, Zt, HHt, GGt, yt, check.input = TRUE)

}
% <---------------------------------------------------------------------->
\arguments{
  \item{a0}{A \code{vector} giving the initial value/estimation of the state variable.}
  \item{P0}{A \code{matrix} giving the variance of \code{a0}.}
  \item{dt}{A \code{matrix} giving the intercept of the transition
    equation (see \bold{Details}).}
  \item{ct}{A \code{matrix} giving the intercept of the measurement
    equation (see \bold{Details}).}
  \item{Tt}{An \code{array} giving the factor of the transition
    equation (see \bold{Details}).}
  \item{Zt}{An \code{array} giving the factor of the measurement
    equation (see \bold{Details}).}
  \item{HHt}{An \code{array} giving the variance of the innovations of
    the transition equation (see \bold{Details}).}
  \item{GGt}{An \code{array} giving the variance of the disturbances of
    the measurement equation (see \bold{Details}).}
  \item{yt}{A \code{matrix} containing the
    observations. \dQuote{NA}-values are allowed (see \bold{Details}).}
  \item{check.input}{A \code{logical} stating whether the input shall be
    checked for consistency (\dQuote{storage.mode}, \dQuote{class}, and
    dimensionality) (see \bold{Details}).}
}
% <---------------------------------------------------------------------->
\details{
% <------------------------------------->
  \strong{The filter:}

  The state space model is represented by the transition equation and
  the measurement equation. Let \eqn{m}{m} be the dimension of the state
  variable, \eqn{d}{d} be the dimension of the observations, and \eqn{n}
  the number of observations. The transition equation and the
  measurement equation are given by \deqn{\alpha_{t + 1} = d_t + T_t
  \cdot \alpha_t + H_t \cdot \eta_t}{alpha(t + 1) = d(t) + T(t) alpha(t)
  + H(t) eta(t)} \deqn{y_t = c_t + Z_t \cdot \alpha_t + G_t \cdot
  \epsilon_t,}{y(t) = c(t) + Z(t) alpha(t) + G(t) epsilon(t),}

 where \eqn{\eta_t}{eta(t)} and \eqn{\epsilon_t}{epsilon(t)} are iid
 \eqn{N(0, I_m)}{N(0, I(m))} and iid \eqn{N(0, I_d)}{N(0, I(d))},
 respectively, and \eqn{\alpha_t}{alpha(t)} denotes the state
 variable. The parameters admit the following dimensions:

 \tabular{lll}{
   \eqn{a_t \in R^m}{a[t] in R^m} \tab \eqn{d_t \in R^m}{d[t] in R^m}
   \tab \eqn{\eta_t \in R^m}{eta[t] in R^m} \cr
   \eqn{T_t \in R^{m \times m}}{d[t] in R^(m * m)} \tab \eqn{H_t \in R^{m
       \times m}}{d[t] in R^(m * m)} \tab \cr
   \eqn{y_t \in R^d}{y[t] in R^d} \tab \eqn{c_t \in R^d}{c[t] in R^d}
   \tab \eqn{\epsilon_t \in R^d}{epsilon[t] in R^d}. \cr
   \eqn{Z_t \in R^{d \times m}}{Z[t] in R^(d * m)} \tab \eqn{G_t \in R^{d
       \times d}}{G[t] in R^(d * d)} \tab
  }

  Note that \code{fkf} takes as input \code{HHt} and \code{GGt} which
  corresponds to \eqn{H_t H_t'}{H[t] \%*\% t(H[t])} and \eqn{G_t
    G_t'}{G[t] \%*\% t(G[t])}. \cr

% <------------------------------------->
\strong{NA-values:}

  NA-values in the observation matrix \code{yt} are supported.  If
  particular observations \code{yt[,i]} contain NAs, the NA-values are
  removed and the measurement equation is adjusted accordingly.

  When the full vector \code{yt[,i]} is missing the Kalman filter
  reduces to a prediction step.

  See package vignette for details.

% <------------------------------------->
\strong{Parameters:}

  The parameters can either be constant or deterministic
  time-varying. Assume the number of observations is \eqn{n}
  (i.e. \eqn{y = (y_t)_{t = 1, \ldots, n}, y_t = (y_{t1}, \ldots,
  y_{td})}{y = y[,1:n]}). Then, the parameters admit the following
  classes and dimensions:

  \tabular{ll}{
    \code{dt} \tab either a \eqn{m \times n}{m * n} (time-varying) or a \eqn{m \times
    1}{m * 1} (constant) matrix. \cr
    \code{Tt} \tab either a \eqn{m \times m \times n}{m * m * n} or a
    \eqn{m \times m \times 1}{m * m * 1} array. \cr
    \code{HHt} \tab either a \eqn{m \times m \times n}{m * m * n} or a
    \eqn{m \times m \times 1}{m * m * 1} array. \cr

    \code{ct} \tab either a \eqn{d \times n}{d * n} or a \eqn{d \times
    1}{d * 1} matrix. \cr
    \code{Zt} \tab either a \eqn{d \times m \times n}{d * m * n} or a
    \eqn{d \times m \times 1}{d * m * 1} array. \cr
    \code{GGt} \tab either a \eqn{d \times d \times n}{d * d * n} or a
    \eqn{d \times d \times 1}{d * d * 1} array. \cr
    \code{yt} \tab a \eqn{d \times n}{d * n} matrix.
  }

  If \code{check.input} is \code{TRUE} each argument will be checked for
  correctness of the dimensionality, storage mode, and class.

  \code{check.input} should always be \code{TRUE} unless the performance
  becomes crucial and correctness of the arguments concerning
  dimensions, class, and storage.mode is ensured.

  \bold{Note:}\cr
  The class of the arguments if of importance. For instance, to check
  whether a parameter is constant the \code{dim} attribute is
  accessed. If, e.g., \code{Zt} is a constant, it could be a \eqn{d
  \times d}{d * d}-matrix. But the third dimension
  (i.e. \code{dim(Zt)[3]}) is needed to check for constancy. This
  requires \code{Zt} to be an \eqn{d \times d \times 1}{d * d *
  1}-array.\cr

  % <------------------------------------->
%   \strong{BLAS and LAPACK routines used:}

%   The \R function \code{fkf} basically wraps the \code{C}-function
%   \code{FKF}, which entirely relies on linear algebra subroutines
%   provided by BLAS and LAPACK. The following functions are used:

%   \tabular{rl}{
%     BLAS: \tab \code{dcopy}, \code{dgemm}, \code{daxpy}. \cr
%     LAPACK: \tab \code{dpotri}, \code{dpotrf}.
%   }

%   \code{FKF} is called through the \code{.Call} interface.  Internally,
%   \code{FKF} extracts the dimensions, allocates memory, and initializes
%   the \R-objects to be returned. \code{FKF} subsequently calls
%   \code{cfkf} which performs the Kalman filtering.

%   The only critical part is to compute the inverse of \eqn{F_t}{F[,,t]}
%   and the determinant of \eqn{F_t}{F[,,t]}. If the inverse can not be
%   computed, the filter stops and returns the corresponding message in
%   \code{status} (see \bold{Value}). If the computation of the
%   determinant fails, the filter will continue, but the log-likelihood in
%   \code{logLik} will be \dQuote{NA}.

%   The inverse is computed in
%   two steps: First, the Cholesky factorization of \eqn{F_t}{F[,,t]} is
%   calculated by \code{dpotrf}. Second, \code{dpotri} calculates the
%   inverse based on the output of \code{dpotrf}. \cr
%   The determinant of \eqn{F_t}{F[,,t]} is computed using again the
%   Cholesky decomposition.

}
% <---------------------------------------------------------------------->
\value{
  An S3-object of class \dQuote{fkf}, which is a list with the following
  elements:

  \tabular{rl}{
  \code{att} \tab A \eqn{m \times n}{m * n}-matrix containing the
  filtered state variables, i.e. \eqn{a_{t|t} = E(\alpha_t | y_t)}{att[,t] = E(alpha[t] | y[,t])}.\cr
  \code{at} \tab A \eqn{m \times (n + 1)}{m * (n + 1)}-matrix containing
  the predicted state variables, i.e. \eqn{a_t = E(\alpha_t | y_{t - 1})}{at[,t] =
    E(alpha[t] | y[,t - 1])}.\cr
  \code{Ptt} \tab A \eqn{m \times m \times n}{m * m * n}-array
  containing the variance of \code{att}, i.e. \eqn{P_{t|t} =
  var(\alpha_t | y_t)}{Ptt[,,t] = var(alpha[t] | y[,t])}.\cr
  \code{Pt} \tab A \eqn{m \times m \times (n + 1)}{m * m * (n +
    1)}-array containing the variances of \code{at}, i.e. \eqn{
    P_t = var(\alpha_t | y_{t - 1})}{Pt[,,t] = var(alpha[t] | y[,t - 1])}.\cr
  \code{vt} \tab A \eqn{d \times n}{d * n}-matrix of the prediction
  errors given by \eqn{v_t = y_t - c_t - Z_t a_t}{vt[,t] = yt[,t] - ct[,t] -
    Zt[,,t] \%*\% at[,t]}.\cr
  \code{Ft} \tab A \eqn{d \times d \times n}{d * d * n}-array which
  contains the variances of \code{vt}, i.e. \eqn{F_t = var(v_t)}{Ft[,,t]
    = var(v[,t])}.\cr
  \code{Kt} \tab A \eqn{m \times d \times n}{m * d * n}-array containing the \dQuote{Kalman gain}. \cr
  \code{logLik} \tab The log-likelihood. \cr
  \code{status} \tab A \code{vector} which contains the status of LAPACK's
  \code{dpotri} and \code{dpotrf}. \eqn{(0, 0)} means successful exit.\cr
  \code{sys.time} \tab The time elapsed as an object of class \dQuote{proc\_time}.
}

The first element of both \code{at} and \code{Pt} is filled with the
function arguments \code{a0} and \code{P0}, and the last, i.e. the (n +
1)-th, element of \code{at} and \code{Pt} contains the predictions
(\eqn{at[,n + 1] = E(\alpha_{n + 1} | y_n)}{at[,n + 1] =
    E(alpha[n + 1] | y[,n])} and \eqn{Pt[,,n + 1] = var(\alpha_{n + 1} |
    y_n)}{Pt[,,n + 1] = var(alpha[n + 1] | y[,n])}.

}
% <---------------------------------------------------------------------->
\references{

  Harvey, Andrew C. (1990). \emph{Forecasting, Structural Time Series
    Models and the Kalman Filter}.  Cambridge University Press.

  Hamilton, James D. (1994). \emph{Time Series Analysis}.  Princeton
  University Press.

  Koopman S. J. \& Shephard N. \& Doornik J. A. (1999).
  \emph{Statistical algorithms for models in state space using SsfPack
  2.2}. Econometrics Journal, Royal Economic Society, vol. 2(1), pages
  107-160.

  Zivot, E. and Wang, J. (2005). \emph{Modeling Financial Time Series
    with S-PLUS}.  Springer-Verlag, New York.

}
% <---------------------------------------------------------------------->
\author{David Luethi, Philipp Erb, Simon Otziger}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=plot.fkf]{plot}} to visualize and analyze
  \code{fkf}-objects,
  function \link[dlm]{dlmFilter} from package \code{dlm}, and function
  \link[sspir]{kfilter} from package \code{sspir}.}
% <---------------------------------------------------------------------->
\examples{
## This example shows how to fit an ARMA(2, 1) model using the Kalman filter.
n <- 1000

## Set the AR parameters
ar1 <- 0.6
ar2 <- 0.2
ma1 <- -0.2
sigma <- sqrt(0.2)

## Sample from an ARMA(2, 1) process
a <- arima.sim(model = list(ar = c(ar1, ar2), ma = ma1), n = n,
               innov = rnorm(n) * sigma)

## Create a state space representation out of the four ARMA parameters
arma21ss <- function(ar1, ar2, ma1, sigma) {
    Tt <- matrix(c(ar1, ar2, 1, 0), ncol = 2)
    Zt <- matrix(c(1, 0), ncol = 2)
    ct <- matrix(0)
    dt <- matrix(0, nrow = 2)
    GGt <- matrix(0)
    H <- matrix(c(1, ma1), nrow = 2) * sigma
    HHt <- H \%*\% t(H)
    a0 <- c(0, 0)
    P0 <- matrix(1e6, nrow = 2, ncol = 2)
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,
                HHt = HHt))
}

## The objective function passed to 'optim'
objective <- function(theta, yt) {
    sp <- arma21ss(theta["ar1"], theta["ar2"], theta["ma1"], theta["sigma"])
    ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
               Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
    return(-ans$logLik)
}

theta <- c(ar = c(0, 0), ma1 = 0, sigma = 1)
fit <- optim(theta, objective, yt = rbind(a), hessian = TRUE)
fit

## Confidence intervals
rbind(fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))),
      fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))

## Filter the series with estimated parameter values
sp <- arma21ss(fit$par["ar1"], fit$par["ar2"], fit$par["ma1"], fit$par["sigma"])
ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
           Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = rbind(a))

## Compare the prediction with the realization
plot(ans, at.idx = 1, att.idx = NA, CI = NA)
lines(a, lty = "dotted")

## Compare the filtered series with the realization
plot(ans, at.idx = NA, att.idx = 1, CI = NA)
lines(a, lty = "dotted")

## Check whether the residuals are Gaussian
plot(ans, type = "resid.qq")

## Check for linear serial dependence through 'acf'
plot(ans, type = "acf")
}
% <---------------------------------------------------------------------->
\keyword{algebra}
\keyword{models}
\keyword{multivariate}
