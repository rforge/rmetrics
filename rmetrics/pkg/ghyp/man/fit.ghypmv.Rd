\name{fit.ghypmv}
\alias{fit.ghypmv}
\alias{fit.hypmv}
\alias{fit.NIGmv}
\alias{fit.VGmv}
\alias{fit.tmv}
\alias{fit.gaussmv}
\title{Fitting generalized hyperbolic distributions to multivariate data}
% <---------------------------------------------------------------------->
\description{

   Perform a maximum likelihood estimation of the parameters of a
   multivariate generalized hyperbolic distribution by using an
   Expectation Maximization (EM) based algorithm.

}
\usage{
fit.ghypmv(data, lambda = 1, alpha.bar = 1, mu = NULL, sigma = NULL,
           gamma = NULL, opt.pars = c(lambda = T, alpha.bar = T, mu = T,
                                      sigma = T, gamma = !symmetric),
           symmetric = F, standardize = F, nit = 2000, reltol = 1e-8,
           abstol = reltol * 10, na.rm = F, silent = FALSE, save.data = T,
           trace = TRUE, ...)

fit.hypmv(data,
          opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric),
          symmetric = F, ...)

fit.NIGmv(data,
          opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric),
          symmetric = F, ...)

fit.VGmv(data, lambda = 1,
         opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric),
         symmetric = F, ...)

fit.tmv(data, nu = 3.5,
        opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric),
        symmetric = F, ...)

fit.gaussmv(data, na.rm = T, save.data = T)
}
% <---------------------------------------------------------------------->
\arguments{

  \item{data}{An object coercible to a \code{matrix}.}
  \item{lambda}{Starting value for the shape parameter \code{lambda}.}
  \item{alpha.bar}{Starting value for the shape parameter \code{alpha.bar}.}

  \item{nu}{Starting value for the shape parameter \code{nu} (only used
            in case of a student-t distribution. It determines the
            degree of freedom and is defined as \code{-2*lambda}.)}

  \item{mu}{Starting value for the location parameter \code{mu}.}
  \item{sigma}{Starting value for the dispersion matrix \code{sigma}.}
  \item{gamma}{Starting value for the skewness vecotr \code{gamma}.}
  \item{opt.pars}{A named logical \code{vector} which states which parameters should be fitted.}
  \item{symmetric}{If \code{TRUE} the skewness parameter \code{gamma} keeps zero.}

  \item{standardize}{If \code{TRUE} the sample will be standardized
                     before fitting.  Afterwards, the parameters and
                     log-likelihood et cetera will be back-transformed.}

  \item{save.data}{If \code{TRUE} \code{data} will be stored within the
                   \code{\link[=mle.ghyp-class]{mle.ghyp}} object
                   (cf. \code{\link{ghyp.data}}).}

  \item{trace}{If \code{TRUE} the evolution of the parameter values
               during the fitting procedure will be traced and stored
               (cf. \code{\link{ghyp.fit.info}}).}

  \item{na.rm}{If \code{TRUE} missing values will be removed from \code{data}.}
  \item{silent}{If \code{TRUE} no prompts will appear in the console.}
  \item{nit}{Maximal number of iterations of the expectation maximation algorithm.}
  \item{reltol}{Relative convergence tolerance.}
  \item{abstol}{Absolute convergence tolerance.}
  \item{\dots}{Arguments passed to \code{\link{optim}} and to \code{fit.ghypmv} when
               fitting special cases of the generalized hyperbolic distribution.}
}
% <---------------------------------------------------------------------->
\details{

  This function uses a modified EM algorithm which is called Multi-Cycle
  Expectation Conditional Maximization (MCECM) algorithm. This algorithm
  is sketched in the vignette of this package which can be found in the
  \code{doc} folder. A more detailed description is provided by the book
  \emph{Quantitative Risk Management, Concepts, Techniques and Tools}
  (see \dQuote{References}). \cr

  The general-purpose optimization routine \code{\link{optim}} is used
  to maximize the loglikelihood function of the univariate mixing
  distribution.  The default method is that of Nelder and Mead which
  uses only function values. Parameters of \code{\link{optim}} can be
  passed via the \dots argument of the fitting routines.

}
% <---------------------------------------------------------------------->
\value{
  An object of class \code{\link[=mle.ghyp-class]{mle.ghyp}}.
}
% <---------------------------------------------------------------------->
\references{

  Alexander J. McNeil, Ruediger Frey, Paul Embrechts (2005)
  \emph{Quantitative Risk Management, Concepts, Techniques and Tools}
  \cr

  \code{ghyp}-package vignette in the \code{doc} folder or on
  \url{http://cran.r-project.org/web/packages/ghyp/}.\cr

  S-Plus and R library \emph{QRMlib} (see
  \url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html})

}
% <---------------------------------------------------------------------->
\author{
  Wolfgang Breymann, David Luethi
}
% <---------------------------------------------------------------------->
\note{

  The variance gamma distribution becomes singular when \eqn{\mathbf{x}
  - \mathbf{\mu} = 0}{x - mu = 0}. This singularity is catched and the
  reduced density function is computed. Because the transition is not
  smooth in the numerical implementation this can rarely result in
  nonsensical fits.

  Providing both arguments, \code{opt.pars} and \code{symmetric} respectively,
  can result in a conflict when \code{opt.pars['gamma']} and \code{symmetric}
  are \code{TRUE}. In this case \code{symmetric} will dominate and
  \code{opt.pars['gamma']} is set to \code{FALSE}.
}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.ghypuv}}, \code{\link{fit.hypuv}},
         \code{\link{fit.NIGuv}}, \code{\link{fit.VGuv}},
         \code{\link{fit.tuv}} for univariate fitting routines.
         \code{\link{ghyp.fit.info}} for information regarding the
         fitting procedure.

}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  fit.ghypmv(data = smi.stocks, opt.pars = c(lambda = FALSE), lambda = 2,
             control = list(rel.tol = 1e-5, abs.tol = 1e-5), reltol = 0.01)
}
% <---------------------------------------------------------------------->
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
