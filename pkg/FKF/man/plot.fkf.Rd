\name{plot.fkf}
\alias{plot.fkf}
\title{Plotting fkf Objects}
% <---------------------------------------------------------------------->
\description{

  Plotting method for objects of class \code{\link{fkf}}. This function
  provides tools for graphical analysis of the Kalman filter output:
  Visualization of the state vector, QQ-plot of the individual
  residuals, QQ-plot of the Mahalanobis distance, auto- as well as
  crosscorrelation function of the residuals.

}
% <---------------------------------------------------------------------->
\usage{

\method{plot}{fkf}(x, type = c("state", "resid.qq", "qqchisq", "acf"),
     CI = 0.95, at.idx = 1:nrow(x$at), att.idx = 1:nrow(x$att), \dots)

}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{The output of \code{\link{fkf}}.}
  \item{type}{A string stating what shall be plotted (see \bold{Details}).}
  \item{CI}{The confidence interval in case \code{type == "state"}. Set
    \code{CI} to \code{NA} if no confidence interval shall be plotted.}
  \item{at.idx}{An vector giving the indexes of the predicted state variables
    which shall be plotted if \code{type == "state"}.}
  \item{att.idx}{An vector giving the indexes of the filtered state variables
    which shall be plotted if \code{type == "state"}.}
  \item{\dots}{Arguments passed to either \code{\link{plot}},
    \code{\link{qqnorm}}, \code{\link{qqplot}} or \code{\link{acf}}.}

}
% <---------------------------------------------------------------------->
\details{
  The argument \code{type} states what shall be plotted. \code{type}
  must partially match one of the following:
  \describe{
    \item{\code{state}}{The state variables are plotted. By the
      arguments \code{at.idx} and \code{att.idx}, the user can specify
      which of the predicted (\eqn{a_{t}}{at}) and filtered
      (\eqn{a_{t|t}}{att}) state variables will be drawn.}
      \item{\code{resid.qq}}{Draws a QQ-plot for each residual-series in\code{vt}.}
      \item{\code{qqchisq}}{A Chi-Squared QQ-plot will be drawn to graphically
	test for multivariate normality of the residuals based on the
	Mahalanobis distance.}
      \item{\code{acf}}{Creates a pairs plot with the autocorrelation
	function (\code{\link{acf}}) on the diagonal panels and the
	crosscorrelation function (\code{\link{ccf}}) of the residuals on the
	off-diagnoal panels.}
    }
}
% <---------------------------------------------------------------------->
\value{
  Invisibly returns an list with components:
   \tabular{rl}{
  \code{distance} \tab The Mahalanobis distance of the residuals as a
  vector of length \eqn{n}. \cr
  \code{std.resid} \tab The standardized residuals as an \eqn{d \times
    n}{d * n}-matrix. It should hold that \eqn{std.resid_{ij} \; iid \sim N_d(0, I)}{std.resid[i,j] iid N_d(0,
      I)},
}
where \eqn{d} denotes the dimension of the data and \eqn{n} the number
of observations.
}
% <---------------------------------------------------------------------->
\author{David Luethi, Philipp Erb, Simon Otziger}
% <---------------------------------------------------------------------->
\seealso{\code{\link{fkf}}}
% <---------------------------------------------------------------------->
\examples{
## This example shows how to filter a series by an AR(2) filter

ar1 <- -0.44
ar2 <- 0.4
sigma <- .4

## Sample from an AR(2) process and put it into the Kalman filter
a <- arima.sim(model = list(ar = c(ar1, ar2)),
               n = 60, innov = rnorm(60) * sigma)


## Set up the state space representation for an AR(2) model:
## Transition equation:
## alpha[, t + 1] = matrix(ar1, 1, ar2, 0, ncol = 2) * alpha[,t] + e[t]
## Measurement equation:
## y[,t] = c(1, 0) * a[,t] */
## Initial values:
## a0 = c(0, 0), P0 = diag(c(10, 10))

ans <- fkf(a0 = c(0, 0), P0 = diag(c(10, 10)),
           dt = rbind(0, 0), ct = matrix(0),
           Tt = matrix(c(ar1, 1, ar2, 0), ncol = 2),
           Zt = cbind(1, 0),
           HHt = matrix(c(sigma^2, 0, 0, 0), ncol = 2),
           GGt = matrix(0), yt = rbind(a))

## Compared the filtered series with the simulated AR(2) series:
plot(ans, att.idx = 1, at.idx = NA)
lines(a, lty = "dotted")

## Compared the predicted series with the simulated AR(2) series.
## Additionally, a 99% confidence interval band of at[1,] is drawn.
plot(ans, att.idx = NA, at.idx = 1, CI = .99)
lines(a, lty = "dotted")

## Check the residuals for normality
plot(ans, type = "resid.qq")

## Test for autocorrelation
plot(ans, type = "acf")

}
% <---------------------------------------------------------------------->
\keyword{hplot}



