\name{hyperbWSqTable}
\title{Percentage Points for the Cramer-von Mises Test of the Hyperbolic Distribution}
\docType{data}
\alias{hyperbWSqTable}
%% Used to be "available" via data() -- now saved in ../R/sysdata.rda
%% Source in ../inst/hyperbWSqTable.R
\description{
  This gives Table 5 of Puig & Stephens (2001) which is used for testing
  the goodness-of-fit of the hyperbolic distribution using the
  \enc{Crämer}{Cramer}-von~Mises test.  It is for internal use by
  \code{\link{hyperbCvMTest}} and \code{\link{hyperbCvMTestPValue}} only and is not
  intended to be accessed by the user.  It is loaded automatically when
  the package \pkg{HyperbolicDist} is invoked.
}
\usage{
hyperbWSqTable
}
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
