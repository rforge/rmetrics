% Part of the Rmetrics project, http://R-metrics.org/
% Copyright 2005-2011 Rmetrics Core Team
% Distributed under GPL 2 or later

\name{fBasics-deprecated}
%------ NOTE:  ../R/fBasics-deprecated.R   must be synchronized with this!
\alias{fBasics-deprecated}
\alias{dstable}
\alias{pstable}
\alias{qstable}
\alias{rstable}
\alias{stableMode}
\title{Deprecated Functions in Package fBasics}
\description{
  These functions are provided for compatibility with older versions of
  the package only, and may be defunct as soon as of the next release.
}
\usage{
dstable(x, alpha, beta, gamma = 1, delta = 0, pm = 0)
pstable(q, alpha, beta, gamma = 1, delta = 0, pm = 0)
qstable(p, alpha, beta, gamma = 1, delta = 0, pm = 0)
rstable(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
stableMode(alpha, beta)
}
\arguments{
  \item{x,q,p,n, alpha, beta, gamma, delta, pm}{%
    see the arguments in \code{\link[stable]{dstable}} etc,
    in package \pkg{stable}.}
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{misc}
