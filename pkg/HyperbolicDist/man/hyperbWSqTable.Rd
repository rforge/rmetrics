\name{hyperbWSqTable}
\docType{data}
\alias{hyperbWSqTable}
\title{Percentage Points for the Cramer-von Mises Test of the Hyperbolic
Distribution}
\description{
  This gives Table 5 of Puig & Stephens (2001) which is used for testing
	the goodness-of-fit of the hyperbolic distribution using the
	Cram\'{e}r-von Mises test. It is for internal use by
	\code{hyperbCvMTest} and \code{hyperbCvMTestPValue} only and is not
	intended to be accessed by the user. It is loaded automatically when
	the package \code{HyperbolicDist} is invoked.
}
\usage{data(hyperbWSqTable)}
\format{
	The \code{hyperbWSqTable} matrix has 55 rows and 5 columns, giving
	percentage points of \eqn{W^2}{W^2} for different values of
	\eqn{\xi}{xi} and \eqn{\alpha}{alpha} (the rows), and of
	\eqn{\chi}{chi} (the columns).   
}
\source{
  Puig, Pedro and Stephens, Michael A. (2001),
  Goodness-of-fit tests for the hyperbolic distribution.
  \emph{The Canadian Journal of Statistics/La Revue Canadienne de
	Statistique}, \bold{29}, 309--320.
}
\keyword{datasets}
