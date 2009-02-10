\name{fkf}
\alias{fkf}
\title{Fast Kalman filter}
% <---------------------------------------------------------------------->
\description{

  This function provides a fast and flexible Kalman filter. The
  state-space parameters are allowed to be time-varying and intercepts
  are supported in both, the transition equation and the measurement
  equation. \code{fkf} wraps the \code{C}-function \code{FKF} which
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
  \item{yt}{A \code{matrix} containing the observations. \code{yt} must
    not contain \dQuote{NA}-values (see \bold{Details}).}
  \item{check.input}{A \code{logical} stating whether the input shall be
    checked for consistency (\dQuote{storage.mode}, \dQuote{class},
    dimensionality, and \dQuote{NA's}) (see \bold{Details}).}
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
\strong{Parameters:}

  The parameters can either be constant or deterministic
  time-varying. Assume the number of observations is \eqn{n}
  (i.e. \eqn{y = (y_t)_{t = 1, \ldots, n}}{y = y[,1:n]}). Then, the
  parameters admit the following classes and dimensions:

  \tabular{ll}{
    \code{dt} \tab either a \eqn{m \times n}{m * n} (time-varying) or a \eqn{m \times
    1}{m * 1} (constant) matrix. \cr
    \code{Tt} \tab either a \eqn{m \times m \times n}{m * m * n} or a
    \eqn{m \times m \times 1}{m * m * 1} array. \cr
    \code{HHt} \tab either a \eqn{m \times m \times n}{m * m * n} or a
    \eqn{m \times m \times 1}{m * m * 1} array. \cr

    \code{ct} \tab either a \eqn{d \times n}{d * n} or a \eqn{d \times
    1}{d * 1} matrix. \cr
    \code{Zt} \tab either a \eqn{d \times d \times n}{d * d * n} or a
    \eqn{d \times d \times 1}{d * d * 1} array. \cr
    \code{GGt} \tab either a \eqn{d \times d \times n}{d * d * n} or a
    \eqn{d \times d \times 1}{d * d * 1} array. \cr
    \code{yt} \tab a \eqn{d \times n}{d * n} matrix.
  }

  If \code{check.input} is \code{TRUE} each argument will be checked for
  correctness of the dimensionality, storage mode, and class.

  \code{check.input} should always be \code{TRUE} unless the performance
  becomes crucial and correctness of the arguments concerning
  dimensions, class, storage.mode and absence of 'NA's is ensured.

  \bold{Note:}\cr
  The class of the arguments if of importance. For instance, to check
  whether a parameter is constant the \code{dim} attribute is
  accessed. If, e.g., \code{Zt} is a constant, it could be a \eqn{d
  \times d}{d * d}-matrix. But the third dimension
  (i.e. \code{dim(Zt)[3]}) is needed to check for constancy. This
  requires \code{Zt} to be an \eqn{d \times d \times 1}{d * d *
  1}-array.\cr

  % <------------------------------------->
  \strong{BLAS and LAPACK routines used:}

  The \R function \code{fkf} basically wraps the \code{C}-function
  \code{FKF}, which entirely relies on linear algebra subroutines
  provided by BLAS and LAPACK. The following functions are used:

  \tabular{rl}{
    BLAS: \tab \code{dcopy}, \code{dgemm}, \code{daxpy}. \cr
    LAPACK: \tab \code{dpotri}, \code{dpotrf}.
  }

  \code{FKF} is called through the \code{.Call} interface.  Internally,
  \code{FKF} extracts the dimensions, allocates memory, and initializes
  the \R-objects to be returned. \code{FKF} subsequently calls
  \code{cfkf} which performs the Kalman filtering.

  The only critical part is to compute the inverse of \eqn{F_t}{F[,,t]}
  and the determinant of \eqn{F_t}{F[,,t]}. If the inverse can not be
  computed, the filter stops and returns the corresponding message in
  \code{status} (see \bold{Value}). If the computation of the
  determinant fails, the filter will continue, but the log-likelihood in
  \code{logLik} will be \dQuote{NA}.

  The inverse is computed in
  two steps: First, the Cholesky factorization of \eqn{F_t}{F[,,t]} is
  calculated by \code{dpotrf}. Second, \code{dpotri} calculates the
  inverse based on the output of \code{dpotrf}. \cr
  The determinant of \eqn{F_t}{F[,,t]} is computed using again the
  Cholesky decomposition.

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
  containing the variance of \code{att}, i.e. \eqn{P_{t|t} = var(a_{t|t})}{Ptt[,,t] = var(att[,t])}.\cr
  \code{Pt} \tab A \eqn{m \times m \times (n + 1)}{m * m * (n +
    1)}-array containing the variances of \code{at}, i.e. \eqn{
    P_t = var(a_t)}{Pt[,,t] = var(at[,t])}.\cr
  \code{vt} \tab A \eqn{d \times n}{d * n}-matrix of the prediction
  errors given by \eqn{v_t = y_t - c_t - Z_t a_t}{vt[,t] = yt[,t] - ct[,t] -
    Zt[,,t] \%*\% at[,t]}.\cr
  \code{Ft} \tab A \eqn{d \times d \times n}{d * d * n}-array which
  contains the variances of \code{vt}, i.e. \eqn{F_t = var(v_t)}{Ft[,,t]
    = var(v[,t])}.\cr
  \code{Kt} \tab A \eqn{m \times d \times n}-array containing the \dQuote{Kalman gain}. \cr
  \code{logLik} \tab The log-likelihood. \cr
  \code{status} \tab A \code{vector} which contains the status of LAPACK's
  \code{dpotri} and \code{dpotrf}. \eqn{(0, 0)} means sucessful exit.\cr
  \code{sys.time} \tab The time elapsed as an object of class \dQuote{proc\_time}. \cr
  }
}
% <---------------------------------------------------------------------->
\references{

  Harvey, Andrew C. (1990) \emph{Forecasting, Structural Time Series
    Models and the Kalman Filter}.  Cambridge University Press.

  Hamilton, James D. (1994). \emph{Time Series Analysis}.  Princeton
  University Press.

  Zivot, E. and Wang, J. (2005). \emph{Modeling Financial Time
    Series with S-PLUS}.
  Springer-Verlag, New York.

}
% <---------------------------------------------------------------------->
\author{David Luethi, Philipp Erb}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=plot.fkf]{plot}} to visualize and analyze
  \code{fkf}-objects,
  function \link[dlm]{dlmFilter} from package \code{dlm}, and function
  \link[sspir]{kfilter} from package \code{sspir}.}
% <---------------------------------------------------------------------->
\examples{
## This example shows how to fit an AR(2) model using the Kalman filter.

n <- 200

## Set the AR parameters
ar1 <- -0.24
ar2 <- 0.6
sigma <- .4

## Sample from an AR(2) process
a <- arima.sim(model = list(ar = c(ar1, ar2)),
               n = n, innov = rnorm(n) * sigma)

## Create a state space representation out of the three AR parameters
ar2.to.state.space <- function(ar1, ar2, sigma){
    Tt <- matrix(c(ar1, 1, ar2, 0), ncol = 2)
    Zt <- matrix(c(1, 0), ncol = 2)
    ct <- matrix(0.0)
    dt <- matrix(0.0, nrow = 2)
    GGt <- matrix(0.0)
    HHt <- matrix(c(sigma^2, 0, 0, 0), ncol = 2)
    a0 <- as.numeric(c(0, 0))
    P0 <- matrix(c(100,0,0,100), ncol = 2)
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt,
                Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
}

## The objective function passed to 'optim'
objective <- function(theta, yt)
{
    sp <- ar2.to.state.space(theta["ar1"], theta["ar2"], theta["sigma"])

    ans <- fkf(a0 = sp$a0, P0 = sp$P0,
               dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt,
               HHt = sp$HHt, GGt = sp$GGt, yt = yt, check.input = FALSE)

    return(-ans$logLik)
}

theta <- c(ar1 = 0, ar2 = 0, sigma = 1)
fit <- optim(theta, objective, yt = rbind(a))
fit

## Filter the series with estimated parameter values
sp <- ar2.to.state.space(fit$par["ar1"], fit$par["ar2"], fit$par["sigma"])

ans <- fkf(a0 = sp$a0, P0 = sp$P0,
           dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt,
           HHt = sp$HHt, GGt = sp$GGt, yt = rbind(a))

## Compare the prediction with the realization
plot(ans, at.idx=1, att.idx = NA, CI = NA)
lines(a, lty = "dotted")

## Compare the filtered series with the realization
plot(ans, at.idx=NA, att.idx = 1, CI = NA)
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


