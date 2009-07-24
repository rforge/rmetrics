\name{momRecursion}
\alias{momRecursion}


\title{Computes the moment coefficients recursively for generalized
  hyperbolic and related distributions
}

\description{
  This function computes all of the moments coffecicients by resursion based on
  Scott, W\"{u}rtz and Tran (2008, working paper). See \bold{Details} for the
  formula.
}

\usage{
  momRecursion(order = 12, printMatrix = FALSE)
}

\arguments{
  \item{order}{Numeric. The order of the moment coffecicients to be calculated.
    Not permitted to be a vector. Must be a positive whole number except for
    moments about zero.}
  \item{printMatrix}{Logical. Should the coefficients matrix be printed?}
}

\details{
  The moment coefficients resursively as \eqn{a_{1,1}=1}{a_{1,1}=1} and
    \deqn{a_{k,\ell} = a_{k-1, \ell-1} + (2 \ell - k + 1) a_{k-1, \ell}}{%
    a_{k,l} = a_{k-1,l=1} + (2l - k + 1) a_{k-1,l}} with
    \eqn{a_{k,\ell} = 0}{a_k,l = 0} for \eqn{\ell<\lfloor(k+1)/2\rfloor}{%
    l < [(k + 1)/2]} or \eqn{\ell>k}{l > k}
    where \eqn{k}{k} = \code{order}, \eqn{\ell}{l} is equal to the integers from
    \eqn{(k+1)/2}{(k + 1)/2} to \eqn{k}{k}.

  This formula is given in Scott, W\"{u}rtz and Tran (2008, working paper).

  The function also calculates M which is equal to \eqn{2\ell - k}{2l - k}.
  It is a common term which will appear in the formulee
  for calculating moments of generalized hyperbolic and related distributions.

}

\value{
  \item{a}{The non-zero moment coefficients for the specified order.}
  \item{l}{Integers from (\code{order}+1)/2 to \code{order}. It is used when
           computing the moment coefficients and the mu moments.}
  \item{M}{The commen term used when computing mu moments for generalized
          hyperbolic and related distributions, M = \eqn{2\ell - k}{2l - k},
          \eqn{k}{k}=\code{order}}
  \item{lmin}{The minimum of \eqn{\ell}{l}, which is equal to
          (\code{order}+1)/2.}
}

\references{
  Scott, D. J., Diethelm, W. and Tran, T. T. (2008, working paper)
  Moments of the Generalized Hyperbolic Distribution.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{\code{\link[VarianceGamma]{vgMom}}}


\examples{
  momRecursion(order = 12)

  #print out the matrix
  momRecursion(order = 12, "true")
}

\keyword{distribution}
