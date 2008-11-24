\name{ghyp-package}
\alias{ghyp-package}
\docType{package}
\title{
A package on the generalized hyperbolic distribution and its special cases
}
% <---------------------------------------------------------------------->
\description{

This package provides detailed functionality for working with the
univariate and multivariate Generalized Hyperbolic distribution and its
special cases (Hyperbolic (hyp), Normal Inverse Gaussian (NIG), Variance
Gamma (VG), skewed Student-t and Gaussian distribution). Especially, it
contains fitting procedures, an AIC-based model selection routine, and
functions for the computation of density, quantile, probability, random
variates, expected shortfall and some portfolio optimization and
plotting routines as well as the likelihood ratio test.  In addition, it
contains the Generalized Inverse Gaussian distribution.

}
% <---------------------------------------------------------------------->
\details{
\tabular{ll}{
Package: \tab ghyp\cr
Type: \tab Package\cr
Version: \tab 1.4.0\cr
Date: \tab 2008-10-28\cr
License: \tab  GPL (GNU Public Licence), Version 2 or later \cr
}

\strong{Initialize:}
\tabular{ll}{
  \code{\link{ghyp}} \tab             Initialize a generalized hyperbolic distribution.\cr
  \code{\link{hyp}} \tab              Initialize a hyperbolic distribution.\cr
  \code{\link{NIG}} \tab              Initialize a normal inverse gaussian distribution.\cr
  \code{\link{VG}} \tab               Initialize a variance gamma distribution.\cr
  \code{\link{student.t}} \tab        Initialize a Student-t distribution.\cr
  \code{\link{gauss}} \tab            Initialize a Gaussian distribution.\cr
}

\strong{Density, distribution function, quantile function and
  random generation:}
\tabular{ll}{
  \code{\link{dghyp}} \tab         Density of a generalized hyperbolic distribution.\cr
  \code{\link{pghyp}} \tab         Distribution function of a generalized hyperbolic distribution.\cr
  \code{\link{qghyp}} \tab         Quantile of a univariate generalized hyperbolic distribution.\cr
  \code{\link{rghyp}} \tab         Random generation of a generalized hyperbolic distribution.\cr
}

\strong{Fit to data:}
\tabular{ll}{
  \code{\link{fit.ghypuv}} \tab     Fit a generalized hyperbolic distribution to univariate data.\cr
  \code{\link{fit.hypuv}} \tab      Fit a hyperbolic distribution to univariate data.\cr
  \code{\link{fit.NIGuv}} \tab      Fit a normal inverse gaussian distribution to univariate data.\cr
  \code{\link{fit.VGuv}} \tab       Fit a variance gamma distribution to univariate data.\cr
  \code{\link{fit.tuv}} \tab        Fit a skewed Student-t distribution to univariate data.\cr
  \code{\link{fit.gaussuv}} \tab    Fit a Gaussian distribution to univariate data.\cr
  \code{\link{fit.ghypmv}} \tab     Fit a generalized hyperbolic distribution to multivariate data.\cr
  \code{\link{fit.hypmv}} \tab      Fit a hyperbolic distribution to multivariate data.\cr
  \code{\link{fit.NIGmv}} \tab      Fit a normal inverse gaussian distribution to multivariate data.\cr
  \code{\link{fit.VGmv}} \tab       Fit a variance gamma distribution to multivariate data.\cr
  \code{\link{fit.tmv}} \tab        Fit a skewed Student-t distribution to multivariate data.\cr
  \code{\link{fit.gaussmv}} \tab    Fit a Gaussian distribution to multivariate data.\cr
  \code{\link{stepAIC.ghyp}} \tab   Perform a model selection based on the AIC.\cr
}

\strong{Risk, performance and portfolio optimization:}
\tabular{ll}{
  \code{\link{ESghyp}} \tab        Expected shortfall of a univariate generalized hyperbolic distribution.\cr
  \code{\link{ghyp.omega}} \tab    Performance measure \emph{Omega} based on a univariate \code{ghyp} distribution.\cr
  \code{\link{portfolio.optimize}} \tab     Calculate optimal portfolios with respect to alternative risk measures.\cr
}

\strong{Utilities:}
\tabular{ll}{
  \code{\link[=mean.ghyp]{mean}} \tab       Returns the expected value.\cr
  \code{\link[=vcov.ghyp]{vcov}} \tab       Returns the  variance(-covariance).\cr
  \code{\link{ghyp.skewness}} \tab          Skewness of a univariate \code{ghyp} distribution.\cr
  \code{\link{ghyp.kurtosis}} \tab          Kurtosis of a univariate \code{ghyp} distribution.\cr
  \code{\link[=logLik.mle.ghyp]{logLik}} \tab   Returns Log-Likelihood of fitted ghyp objects.\cr
  \code{\link[=AIC.mle.ghyp]{AIC}} \tab     Returns the Akaike's Information Criterion  of fitted ghyp objects.\cr
  \code{\link{lik.ratio.test}} \tab         Performs a likelihood-ratio test on fitted \code{ghyp} distributions.\cr
  \code{\link[=[.ghyp]{[}} \tab             Extract certain dimensions of a multivariate \code{ghyp} distribution.\cr
  \code{\link[=scale.ghyp]{scale}} \tab Scale \code{ghyp} distribution objects to zero expectation and/or unit variance.\cr
  \code{\link[=transform.ghyp]{transform}} \tab Transform a multivariate generalized hyperbolic distribution.\cr
  \code{\link{ghyp.moment}} \tab            Moments of the univariate \code{ghyp} distribution.\cr
  \code{\link[=coef.ghyp]{coef}} \tab       Parameters of a generalized hyperbolic distribution.\cr
  \code{\link{ghyp.data}} \tab              Data of a (fitted) generalized hyperbolic distribution.\cr
  \code{\link{ghyp.fit.info}} \tab          Information about the fitting procedure, log-likelihood and AIC value.\cr
  \code{\link{ghyp.name}} \tab              Returns the name of the \code{ghyp} distribution or a subclass of it.\cr
  \code{\link{ghyp.dim}} \tab              Returns the dimension of a \code{ghyp} object.\cr
  \code{\link[=summary.mle.ghyp]{summary}} \tab       Summary of a fitted generalized hyperbolic distribution.\cr
}

\strong{Plot functions:}
\tabular{ll}{
  \code{\link{qqghyp}} \tab                 Perform a quantile-quantile plot of a (fitted) univariate \code{ghyp} distribution.\cr
  \code{\link[=hist.ghyp]{hist}} \tab       Plot a histogram of a (fitted) univariate generalized hyperbolic distribution.\cr
  \code{\link[=pairs.ghyp]{pairs}} \tab     Produce a matrix of scatterplots with quantile-quantile plots on the diagonal.\cr
  \code{\link[=plot.ghyp]{plot}} \tab       Plot the density of a univariate \code{ghyp} distribution.\cr
  \code{\link[=lines.ghyp]{lines}} \tab     Add the density of a univariate \code{ghyp} distribution to a graphics device.\cr
}

\strong{Generalized inverse gaussian distribution:}
\tabular{ll}{
  \code{\link{dgig}} \tab          Density of a generalized inverse gaussian distribution\cr
  \code{\link{pgig}} \tab          Distribution function of a generalized inverse gaussian distribution\cr
  \code{\link{qgig}} \tab          Quantile of a generalized inverse gaussian distribution\cr
  \code{\link{ESgig}} \tab         Expected shortfall of a generalized inverse gaussian distribution\cr
  \code{\link{rgig}} \tab          Random generation of a generalized inverse gaussian distribution\cr
}

\strong{Package vignette:} \cr
A document about generalized hyperbolic distributions can be found in the
\code{doc} folder of this package or on \url{http://cran.r-project.org/web/packages/ghyp/}.
}
% <---------------------------------------------------------------------->
\section{Existing solutions}{
  There are already two packages \code{HyperbolicDist} and \code{fBasics} which cover the
  univariate generalized hyperbolic distribution and some of its special cases. However, the univariate case is contained
  in this package as well because we aim to provide a uniform interface to deal with
  generalized hyperbolic distribution. Recently an \R port of the S-Plus library \code{QRMlib}
  was released. The package \code{QRMlib} contains fitting procedures for the multivariate NIG, hyp and
  skewed Student-t distribution but not for the generalized hyperbolic case.
  The package \code{fMultivar} implements
  a fitting routine for multivariate skewed Student-t distributions as well.
}
% <---------------------------------------------------------------------->
\section{Object orientation}{

We follow an object-oriented programming approach in this package and
introduce distribution objects. There are mainly four reasons for that:

\itemize{

  \item Unlike most distributions the GH distribution has quite a few
        parameters which have to fulfill some consistency
        requirements. Consistency checks can be performed uniquely when
        an object is initialized.

  \item Once initialized the common functions belonging to a
        distribution can be called conveniently by passing the
        distribution object. A repeated input of the parameters is
        avoided.

  \item Distributions returned from fitting procedures can be directly
        passed to, e.g., the density function since fitted distribution
        objects add information to the distribution object and
        consequently inherit from the class of the distribution object.

  \item Generic method dispatching can be used to provide a uniform
        interface to, e.g., plot the probability density of a specific
        distribution like \code{plot(distribution.object)}.
        Additionally, one can take advantage of generic programming
        since \R provides virtual classes and some forms of
        polymorphism.

      }
    }
% <---------------------------------------------------------------------->
\section{Acknowledgement}{

This package has been partially developed in the framework of the
COST-P10 \dQuote{Physics of Risk} project. Financial support by the
Swiss State Secretariat for Education and Research (SBF) is gratefully
acknowledged.

}
% <---------------------------------------------------------------------->
\author{
Wolfgang Breymann, David Luethi \cr

Institute of Data Analyses and Process Design
(\url{http://www.idp.zhaw.ch}) \cr

Maintainer: David Luethi <david.luethi@zhaw.ch>
}
% <---------------------------------------------------------------------->
\references{

\emph{Quantitative Risk Management: Concepts, Techniques and Tools} by
Alexander J. McNeil, Ruediger Frey and Paul Embrechts \cr Princeton
Press, 2005 \cr

\emph{Intermediate probability: A computational approach} by Marc
Paolella \cr Wiley, 2007 \cr

\emph{S-Plus and \R Library for Quantitative Risk Management QRMlib} by
Alexander J. McNeil (2005) and Scott Ulman (\R-port) (2007)\cr
\url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html} and
\code{QRMlib}\cr

}
% <---------------------------------------------------------------------->
\keyword{package}
\keyword{hplot}
\keyword{multivariate}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}
\keyword{datagen}
