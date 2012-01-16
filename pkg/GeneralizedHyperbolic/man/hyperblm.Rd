\name{hyperblm}
\alias{hyperblm}
\alias{print.hyperblm}
\alias{coef.hyperblm}
\alias{plot.hyperblm}


\title{Fitting Linear Models with Hyperbolic Errors}
\description{
  Fits linear models with hyperbolic errors. Can be used to carry out
  linear regression for data exhibiting heavy tails and skewness.
  Displays the histogram, log-histogram (both with fitted error
  distribution), Q-Q plot and residuals vs. fitted values plot for the
  fitted linear model.
}

\usage{
hyperblm(formula, data, subset, weights, na.action,
         xx = FALSE, y = FALSE, contrasts = NULL,
         offset, method = "Nelder-Mead",
         startMethod = "Nelder-Mead", startStarts = "BN",
         paramStart = NULL,
         maxiter = 100, tolerance = 0.0001,
         controlBFGS = list(maxit = 1000),
         controlNM = list(maxit = 10000),
         maxitNLM = 10000,
         controlCO = list(), silent = TRUE, ...)

\method{print}{hyperblm}(x, digits = max(3, getOption("digits")-3), ...)

\method{coef}{hyperblm}(object, ...)

\method{plot}{hyperblm}(x, breaks = "FD",
                        plotTitles = c("Residuals vs Fitted Values",
                                       "Histogram of residuals",
                                       "Log-Histogram of residuals",
                                       "Q-Q Plot"),
                        ...)
}

\arguments{
    \item{formula}{an object of class \code{"\link{formula}"} (or one that
    can be coerced to that class): a symbolic description of the
    model to be fitted.  The details of model specification are given
    under \sQuote{Details}.}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{lm} is called.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{weights}{an optional vector of weights to be used in the fitting
    process.  Should be \code{NULL} or a numeric vector.
    If non-NULL, weighted least squares is used with weights
    \code{weights} (that is, minimizing \code{sum(w*e^2)}); otherwise
    ordinary least squares is used.  See also \sQuote{Details},}

  \item{na.action}{A function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.  Value \code{\link{na.exclude}} can be useful.}

  \item{xx, y}{Logicals. If \code{TRUE}, the corresponding components of the
    fit (the explanatory matrix and the response vector) are returned.}

  \item{contrasts}{An optional list. See the \code{contrasts.arg}
    of \code{\link{model.matrix.default}}.}

  \item{offset}{An optional vector. See \bold{Details}.}

  \item{method}{Character. Possible values are \code{"BFGS"},
    \code{"Nelder-Mead"} and \code{"nlm"}. See \bold{Details}.}

  \item{startMethod}{Character. Possible values are \code{"BFGS"} and
    \code{"Nelder-Mead"}. See \bold{Details}.}

  \item{startStarts}{Character. Possible values are \code{"BN"},
    \code{"FN"}, \code{"SL"}, \code{"US"} and \code{"MoM"}. See
    \bold{Details}.}

  \item{paramStart}{An optional vector. A vector of parameter
    start values for the optimization routine. See \bold{Details}.}

  \item{maxiter}{Numeric. The maximum number of two-stage optimization
    alternating iterations. See \bold{Details}.}

  \item{tolerance}{Numeric. The two-stage optimization convergence
    ratio. See \bold{Details}.}

  \item{controlBFGS, controlNM}{Lists. Lists of control parameters for
    \code{optim} when using corresponding (BFGS, Nelder-Mead) optimisation
    method in first stage. See \code{\link{optim}}.}

  \item{maxitNLM}{Numeric. The maximum number of iterations for the NLM
    optimizer.}

  \item{controlCO}{List. A list of control parameters for
    \code{\link{constrOptim}} in second stage.}

  \item{silent}{Logical. If \code{TRUE}, the error messgae of optimizer will
    not be displayed.}

  \item{x}{An object of class \code{"hyperblm"}.}

  \item{object}{An object of class \code{"hyperblm"}.}

  \item{breaks}{May be a vector, a single number or a character
    string. See \code{\link{hist}}.}

  \item{plotTitles}{Titles to appear above the plots.}

  \item{digits}{Numeric. Desired number of digits when the object is
    printed.}

  \item{\dots}{Passes additional arguments to function
    \code{\link{hyperbFitStand}}, \code{\link{optim}} and
    \code{\link{constrOptim}}.}
}

\details{

  Models for \code{hyperblm} are specified symbolically.  A typical
  model has the form \code{response ~ terms} where \code{response} is
  the (numeric) response vector and \code{terms} is a series of terms
  which specifies a linear predictor for \code{response}.  A terms
  specification of the form \code{first + second} indicates all the
  terms in \code{first} together with all the terms in \code{second}
  with duplicates removed.  A specification of the form
  \code{first:second} indicates the set of terms obtained by taking the
  interactions of all terms in \code{first} with all terms in
  \code{second}.  The specification \code{first*second} indicates the
  \emph{cross} of \code{first} and \code{second}.  This is the same as
  \code{first + second + first:second}.

  If the formula includes an \code{\link{offset}}, this is evaluated and
  subtracted from the response.

  If \code{response} is a matrix a linear model is fitted separately by
  least-squares to each column of the matrix.

  See \code{\link{model.matrix}} for some further details.  The terms in
  the formula will be re-ordered so that main effects come first,
  followed by the interactions, all second-order, all third-order and so
  on.

  A formula has an implied intercept term.  To remove this use either
  \code{y ~ x - 1} or \code{y ~ 0 + x}.  See \code{\link{formula}} for
  more details of allowed formulae.

  Non-\code{NULL} \code{weights} can be used to indicate that different
  observations have different variances (with the values in
  \code{weights} being inversely proportional to the variances); or
  equivalently, when the elements of \code{weights} are positive
  integers \eqn{w_i}, that each response \eqn{y_i} is the mean of
  \eqn{w_i} unit-weight observations (including the case that there are
  \eqn{w_i} observations equal to \eqn{y_i} and the data have been
  summarized).

  \code{hyperblm} calls the lower level function
  \code{\link{hyperblmFit}} for the actual numerical computations.

  All of \code{weights}, \code{subset} and \code{offset} are evaluated
  in the same way as variables in \code{formula}, that is first in
  \code{data} and then in the environment of \code{formula}.

  \code{hyperblmFit} uses a two-stage alternating optimization
  routine. The quality of parameter start values (especially the error
  distribution parameters) is crucial to the routine's convergence. The
  user can specify the start values via the \code{paramStart} argument,
  otherwise the function finds reliable start values by calling the
  \code{\link{hyperbFitStand}} function.

  \code{startMethod} in the argument list is the optimization method for
  function \code{\link{hyperbFitStandStart}} which finds the start
  values for function \code{\link{hyperbFitStand}}. It is set to
  \code{"Nelder-Mead"} by default due to the robustness of this
  optimizer. The \code{"BFGS"} method is also implemented as it is
  relatively fast to converge. Since \code{"BFGS"} method is a
  quasi-Newton method it will not as robust and for some data will not
  achieve convergence.

  \code{startStarts} is the method used to find the start values for function
  \code{hyperbFitStandStart} which includes:


  \itemize{
    \item{\code{"BN"}}{A method from Barndorff-Nielsen (1977) based on
    estimates of \eqn{\psi}{psi} and \eqn{\gamma}{gamma} the absolute
    slopes of the left and right asymptotes to the log density function}

    \item{\code{"FN"}}{Based on a fitted normal distribution as it is a
      limit of the hyperbolic distribution}

    \item{\code{"SL"}}{Based on a fitted skew-Laplace distribution for
      which the log density has the form of two straight line with
      absolute slopes \eqn{1/\alpha}{1/alpha}, \eqn{1/\beta}{1/beta}}

    \item{\code{"MoM"}}{A method of moment approach}
    \item{\code{"US"}}{User specified}
  }

  \code{method} is the method used in stage one of the two-stage
  alternating optimization routine. As the \code{startMethod}, it is set
  to \code{"Nelder-Mead"} by default. Besides \code{"BFGS"},\code{"nlm"}
  is also implemented as a alternative. Since \code{BFGS} method is a
  quasi-Newton method it will not as robust and for some data will not
  achieve convergence.

  If the maximum of the ratio the change of the individual coefficients
  is smaller than \code{tolerance} then the routine assumes convergence,
  otherwise if the alternating iteration number exceeds \code{maxiter}
  with the maximum of the ratio the change of the individual
  coefficients larger than \code{tolerance}, the routine is considered
  not to have converged.

}

\value{

  \code{hyperblm} returns an object of class \code{"hyperblm"} which is a list
  containing:

  \item{coefficients}{A named vector of regression coefficients.}

  \item{distributionParams}{A named vector of fitted hyperbolic error
    distribution parameters.}

  \item{fitted.values}{The fitted values from the model.}

  \item{residuals}{The remainder after subtracting fitted values from
    response.}

  \item{mle}{The maximum likelihood value of the model.}

  \item{method}{The optimization method for stage one.}

  \item{paramStart}{The start values of parameters that the user
    specified (only where relevant).}

  \item{residsParamStart}{The start values of parameters obtained by
    \code{hyperbFitStand} (only where relevant).}

  \item{call}{The matched call.}

  \item{terms}{The \code{terms} object used.}

  \item{contrasts}{The contrasts used (only where relevant).}

  \item{xlevels}{The levels of the factors used in the fitting (only where
    relevant).}

  \item{offset}{The offset used (only where relevant)}

  \item{xNames}{The names of each explanatory variables. If explanatory
    variables don't have names then they will be named \code{x}.}

  \item{yVec}{The response vector.}

  \item{xMatrix}{The explanatory variables matrix.}

  \item{iterations}{Number of two-stage alternating iterations to
    convergence.}

  \item{convergence}{The convergence code for two stage optimization: 0
    is the system converged, 1 is first stage does not converge, 2 is
    second stage does not converge, 3 is the both stages do not
    converge.}

  \item{breaks}{The cell boundaries found by a call the
    \code{\link{hist}}.}

}

\references{

  Barndorff-Nielsen, O. (1977)
  Exponentially decreasing distributions for the logarithm of particle size,
  \emph{Proc. Roy. Soc. Lond.},
  A\bold{353}, 401--419.

  Prause, K. (1999). \emph{The generalized hyperbolic models:
    Estimation, financial derivatives and risk measurement}.
  PhD Thesis, Mathematics Faculty, University of Freiburg.

  Trendall, Richard (2005). \emph{hypReg: A Function for Fitting a Linear
    Regression Model in R with Hyperbolic Error}.
  Masters Thesis, Statistics Faculty, University of Auckland.

  Paolella, Marc S. (2007). \emph{Intermediate Probability: A Computational
    Approach}. pp. 415 -Chichester: Wiley.

  Scott, David J. and \enc{Würtz}{Wuertz}, Diethelm and Chalabi, Yohan, (2011).
  \emph{Fitting the Hyperbolic Distribution with R: A Case Study of
    Optimization Techniques}. In preparation.

  Stryhn, H. and Christensen, J. (2003). \emph{Confidence intervals by
    the profile likelihood method, with applications in veterinary
    epidemiology}.
  ISVEE X.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Xinxing Li \email{xli053@aucklanduni.ac.nz}
}

\seealso{
  \code{\link{print.hyperblm}} prints the regression result in a table.
  \code{\link{coef.hyperblm}} obtains the regression coefficients and
  error distribution parameters of the fitted model.
  \code{\link{summary.hyperblm}} obtains a summary output of class
  \code{hyperblm} object.
  \code{\link{print.summary.hyperblm}} prints the summary output in a
  table.
  \code{\link{plot.hyperblm}} obtains a residual vs fitted value plot, a
  histgram of residuals with error distribution density curve on top, a
  histgram of log residuals with error distribution error density curve
  on top and a QQ plot.
  \code{\link{hyperblmFit}}, \code{\link{optim}}, \code{\link{nlm}},
  \code{\link{constrOptim}}, \code{\link{hist}},
  \code{\link{hyperbFitStand}}, \code{\link{hyperbFitStandStart}}.
}

\examples{
## stackloss data example

 airflow <- stackloss[, 1]
 temperature <- stackloss[, 2]
 acid <- stackloss[, 3]
 stack <- stackloss[, 4]

 hyperblm.fit <- hyperblm(stack ~ airflow + temperature + acid,
                          tolerance = 1e-11)

 coef.hyperblm(hyperblm.fit)
 plot.hyperblm(hyperblm.fit, breaks = 20)
 summary.hyperblm(hyperblm.fit, hessian = FALSE)
}


