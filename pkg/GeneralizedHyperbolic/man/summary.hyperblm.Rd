\name{summary.hyperblm}
\alias{summary.hyperblm}
\alias{print.summary.hyperblm}

\title{Summary Output of Hyperbolic Regression}
\description{
  It obtains summary output from class 'hyperblm' object. The summary
  output incldes the standard error, t-statistics, p values of the
  coefficients estimates. Also the estimated parameters of hyperbolic
  error distribution, the maximum likelihood, the stage one optimization
  method, the two-stage alternating iterations and the convergence code.
}

\usage{
\method{summary}{hyperblm}(object, hessian = FALSE,
                           nboots = 1000, ...)

\method{print}{summary.hyperblm}(x,
                                 digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{object}{An object of class \code{"hyperblm"}.}
  \item{x}{An object of class \code{"summary.hyperblm"} resulting from a
    call to \code{summary.hyperblm}.}
  \item{hessian}{Logical. If is \code{TRUE}, the standard error is
    calculated by the hessian matrix and the also hessian matrix is
    returned. Otherwise, the standard error is approximated by
    bootstrapping. See \bold{Details}.}
  \item{nboots}{Numeric. Number of bootstrap simulations to obtain the
    bootstrap estimate of parameters standard errors.}
  \item{digits}{Numeric. Desired number of digits when the object is
    printed.}
  \item{\dots}{Passes additional arguments to functions \code{bSE},
    \code{hyperblmhessian}.}
}

\details{
  The function \code{summary.hyperblm} provides two approaches to obtain
  the standard error of parameters due to the fact that approximated
  hessian matrix is not stable for such complex optimization. The first
  approach is by approximated hessian matrix. The setting in the
  argument list is \code{hessian = TRUE}. The Hessian matrix is
  approximated by function \code{\link{tsHessian}}. However it may not
  be reliable for some error distribution parameters, for instance, the
  function obtains negative variance from the Hessian matrix. The second
  approach is by parametric bootstrapping. The setting in the argument
  list is \code{hessian = FALSE} which is also the default setting. The
  default number of bootstrap stimulations is 1000, but users can
  increase this when accuracy has priority over efficiency. Although the
  bootstrapping is fairly slow, it provides reliable standard errors.
}

\value{

  \code{summary.hyperblm} returns an object of class
  \code{summary.hyperblm} which is a list containing:
  \item{coefficients}{A names vector of regression coefficients.}
  \item{distributionParams}{A named vector of fitted hyperbolic error
    distribution parameters.}
  \item{fitted.values}{The fitted mean values.}
  \item{residuals}{The remaining after subtract fitted values from
    response.}
  \item{MLE}{The maximum likelihood value of the model.}
  \item{method}{The optimization method for stage one.}
  \item{paramStart}{The start values of parameters that the user specified
    (only where relevant).}
  \item{residsParamStart}{The start values of parameters returned by
    \code{hyperbFitStand} (only where relevant).}
  \item{call}{The matched call.}
  \item{terms}{The \code{terms} object used.}
  \item{contrasts}{The contrasts used (only where relevant).}
  \item{xlevels}{The levels of the factors used in the fitting (only where
    relevant).}
  \item{offset}{The offset used (only where relevant).}
  \item{xNames}{The names of each explanatory variables. If explanatory
    variables don't have names then they shall be named \code{x}.}
  \item{yVec}{The response vector.}
  \item{xMatrix}{The explanatory variables matrix.}
  \item{iterations}{Number of two-stage alternating iterations to
    convergency.}
  \item{convergence}{The convergence code for two-stage optimization: 0
    if the system converged; 1 if first stage did not converge, 2 if the
    second stage did not converge, 3 if the both stages did not converge.}
  \item{breaks}{The cell boundaries found by a call the
    \code{\link{hist}}.}
  \item{hessian}{Hessian Matrix. Only where \code{Hessian = TRUE}.}
  \item{tval}{\emph{t}-statistics of regression coefficient estimates.}
  \item{rdf}{Degrees of freedom.}
  \item{pval}{P-values of regression coefficients estimates.}
  \item{sds}{Standard errors of regression coefficient estimates.}
}

\references{
  Barndorff-Nielsen, O. (1977). Exponentially Decreasing Distribution
  for the Logarithm of Particle Size.
  In \emph{Proceedings of the Royal Society of London. Series A,
  Mathematical and Physical Sciences}, Vol. 353, pp. 401--419.

  Prause, K. (1999). \emph{The generalized hyperbolic models:
    Estimation, financial derivatives and risk measurement}.
  PhD Thesis, Mathematics Faculty, University of Freiburg.

  Trendall, Richard (2005). \emph{hypReg: A Function for Fitting a
    Linear Regression Model in R with Hyperbolic Error}.
  Masters Thesis, Statistics Faculty, University of Auckland.

  Paolella, Marc S. (2007). \emph{Intermediate Probability: A
    Compitational Approach}.
  pp. 415 -Chichester: Wiley.

  Scott, David J. and Wurtz, Diethelm and Chalabi, Yohan,
  (2011). \emph{Fitting the Hyperbolic Distribution with R: A Case Study
    of Optimization Techniques}.
  In preparation.

  Stryhn, H. and Christensen, J. (2003). \emph{Confidence intervals by
  the profile likelihood method, with applications in veterinary
  epidemiology}. ISVEE X.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Xinxing Li \email{xli053@aucklanduni.ac.nz}
}

\seealso{
  \code{\link{print.summary.hyperblm}} prints the summary output in a
  table.
  \code{\link{hyperblm}} fits linear model with hyperbolic
  error distribution.
  \code{\link{print.hyperblm}} prints the regression result in a table.
  \code{\link{coef.hyperblm}} obtains the regression coefficients and
  error distribution parameters of the fitted model.
  \code{\link{plot.hyperblm}} obtains a residual vs fitted value plot, a
  histgram of residuals with error distribution density curve on top, a
  histgram of log residuals with error distribution error density curve
  on top and a QQ plot.
  \code{\link{tsHessian}}
}

\examples{
## stackloss data example

# airflow <- stackloss[, 1]
# temperature <- stackloss[, 2]
# acid <- stackloss[, 3]
# stack <- stackloss[, 4]

# hyperblm.fit <- hyperblm(stack ~ airflow + temperature + acid,
#                          tolerance = 1e-11)

# coef.hyperblm(hyperblm.fit)
# plot.hyperblm(hyperblm.fit, breaks = 20)
# summary.hyperblm(hyperblm.fit, hessian = FALSE)

}
