\name{ghyp-mle.ghyp-classes}
\docType{class}
\alias{ghyp-class}
\alias{show.ghyp}
\alias{show,ghyp-method}
\alias{mle.ghyp-class}
\alias{show.mle.ghyp}
\alias{show,mle.ghyp-method}
% <---------------------------------------------------------------------->
\title{Classes ghyp and mle.ghyp}
% <---------------------------------------------------------------------->
\description{

  The class \dQuote{ghyp} basically contains the parameters of a
  generalized hyperbolic distribution.  The class \dQuote{mle.ghyp}
  inherits from the class \dQuote{ghyp}. The class \dQuote{mle.ghyp}
  adds some additional slots which contain information about the fitting
  procedure. Namely, these are the number of iterations (\code{n.iter}),
  the log likelihood value (\code{llh}), the Akaike Information
  Criterion (\code{aic}), a boolean vector (\code{fitted.params})
  stating which parameters were fitted, a boolean \code{converged}
  whether the fitting procedure converged or not, an \code{error.code}
  which stores the status of a possible error and the corresponding
  \code{error.message}. In the univariate case the parameter variance is
  also stored in \code{parameter.variance}.

}
% <---------------------------------------------------------------------->
\section{Objects from the Class}{

  Objects should only be created by calls to the constructors
  \code{\link{ghyp}}, \code{\link{hyp}}, \code{\link{NIG}},
  \code{\link{VG}}, \code{\link{student.t}} and \code{\link{gauss}} or
  by calls to the fitting routines like \code{\link{fit.ghypuv}},
  \code{\link{fit.ghypmv}}, \code{\link{fit.hypuv}},
  \code{\link{fit.hypmv}} et cetera.

}
% <---------------------------------------------------------------------->
\section{Slots}{
  \strong{Slots of class ghyp:}
    \describe{
    \item{\code{call}:}{The function-call of class \code{call}.}
    \item{\code{lambda}:}{Shape parameter of class \code{numeric}.}
    \item{\code{alpha.bar}:}{Shape parameter of class \code{numeric}.}
    \item{\code{chi}:}{Shape parameter of an alternative parametrization.
                      Object of class \code{numeric}.}
    \item{\code{psi}:}{Shape parameter of an alternative parametrization.
                      Object of class \code{numeric}.}
    \item{\code{mu}:}{Location parameter of lass \code{numeric}.}
    \item{\code{sigma}:}{Dispersion parameter of class \code{matrix}.}
    \item{\code{gamma}:}{Skewness parameter of class \code{numeric}.}
    \item{\code{model}:}{Model, i.e., (a)symmetric generalized hyperbolic distribution or
                         (a)symmetric special case. Object of class \code{character}.}
    \item{\code{dimension}:}{Dimension of the generalized hyperbolic distribution.
                             Object of class \code{numeric}.}
    \item{\code{expected.value}:}{The expected value of a generalized
                                  hyperbolic distribution.
                                  Object of class \code{numeric}.}
    \item{\code{variance}:}{The variance of a generalized
                            hyperbolic distribution of class \code{matrix}.}
    \item{\code{data}:}{The data-slot is of class \code{matrix}. When an object of class
                        \code{ghypmv} is instantiated the user can decide whether
                        data should be stored within the object or not. This is the default and may be useful
                        when fitting eneralized hyperbolic distributions to data and
                        perform further analysis afterwards.}
    \item{\code{parametrization}:}{Parametrization of the generalized
                                  hyperbolic distribution of class \code{character}.
                                  These are currently either \dQuote{chi.psi}, \dQuote{alpha.bar} or \dQuote{alpha.delta}.}
    }

    \strong{Slots added by class mle.ghyp:}
    \describe{
    \item{\code{n.iter}:}{The number of iterations of class \code{numeric}.}
    \item{\code{llh}:}{The log likelihood value of class \code{numeric}.}
    \item{\code{converged}:}{A boolean whether converged or not.
                             Object of class \code{logical}.}
    \item{\code{error.code}:}{An error code of class \code{numeric}.}
    \item{\code{error.message}:}{An error message of class \code{character}.}
    \item{\code{fitted.params}:}{A boolean vector stating which parameters were fitted of class \code{logical}.}
    \item{\code{aic}:}{The value of the Akaike Information Criterion of class \code{numeric}.}
    \item{\code{parameter.variance}:}{The parameter variance is the inverse
                                      of the fisher information matrix. This slot is filled only in the case of
                                      an univariate fit.
                                      This slot is of class \code{matrix}.}
    \item{\code{trace.pars}:}{Containes the parameter value evolution during the fitting procedure.
                              \code{trace.pars} of class \code{list}.}
  }
}
% <---------------------------------------------------------------------->
\section{Extends}{
Class \dQuote{mle.ghyp} extends class \code{"\linkS4class{ghyp}"}, directly.
}
% <---------------------------------------------------------------------->
\section{Methods}{
   A \dQuote{pairs} method (see \code{\link[=pairs.ghyp]{pairs}}).\cr
   A \dQuote{hist} method (see \code{\link[=hist.ghyp]{hist}}).\cr
   A \dQuote{plot} method (see \code{\link[=plot.ghyp]{plot}}).\cr
   A \dQuote{lines} method (see \code{\link[=lines.ghyp]{lines}}).\cr
   A \dQuote{coef} method (see \code{\link[=coef.ghyp]{coef}}).\cr
   A \dQuote{mean} method (see \code{\link[=mean.ghyp]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghyp]{vcov}}). \cr
   A \dQuote{transform} method (see \code{\link[=transform.ghyp]{transform}}). \cr
   A \dQuote{[.ghyp} method (see \code{\link[=[.ghyp]{[}}). \cr
   A \dQuote{logLik} method for objects of class \dQuote{mle.ghyp} (see \code{\link[=logLik.mle.ghyp]{logLik}}). \cr
   An \dQuote{AIC} method for objects of class \dQuote{mle.ghyp} (see \code{\link[=AIC.mle.ghyp]{AIC}}). \cr
   A \dQuote{summary} method for objects of class \dQuote{mle.ghyp} (see \code{\link[=summary.mle.ghyp]{summary}}).
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\note{

  When showing special cases of the generalized hyperbolic distribution
  the corresponding fixed parameters are not printed.

}
% <---------------------------------------------------------------------->
\seealso{
  \code{\link{optim}} for an interpretation of \code{error.code}, \code{error.message} and \code{parameter.variance}. \cr
  \code{\link{ghyp}}, \code{\link{hyp}}, \code{\link{NIG}}, \code{\link{VG}}, \code{\link{student.t}} and
  \code{\link{gauss}} for constructors of the class \code{ghyp} in the \dQuote{alpha.bar} and \dQuote{chi/psi} parametrization.
  \code{\link[=ghyp.ad]{xxx.ad}} for all the constructors in the \dQuote{alpha/delta} parametrization.
  \code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}} et cetera for the fitting routies and constructors of the class
  \code{mle.ghyp}.
}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)
  multivariate.fit <- fit.ghypmv(data = smi.stocks,
                                 opt.pars = c(lambda = FALSE, alpha.bar = FALSE),
                                 lambda = 2)
  summary(multivariate.fit)

  vcov(multivariate.fit)
  mean(multivariate.fit)
  logLik(multivariate.fit)
  AIC(multivariate.fit)
  coef(multivariate.fit)

  univariate.fit <- multivariate.fit[1]
  hist(univariate.fit)

  plot(univariate.fit)
  lines(multivariate.fit[2])
}
% <---------------------------------------------------------------------->
\keyword{classes}

