\name{momChangeAbout}
\alias{momChangeAbout}

\title{Obtain Moment About a New Location}
\description{
  Using the moments up to a given order about one location, the moment
  of that order about a new location is produced. A generalization
  of using raw moments to obtain a central moment or using central
  moments to obtain a raw moment.
}

\usage{
  momChangeAbout(order, oldMom, oldAbout, newAbout)
}

\arguments{
  \item{order}{Integer. The order of the new moment to be obtained.}
  \item{oldMom}{Numeric, of length greater or equal to \code{order}.
    Moments of orders 1, 2, \dots, \code{order} (at least) about the point
    \code{oldAbout}.}
  \item{oldAbout}{Numeric. The point about which the moments \code{oldMom} have
    been calculated.}
  \item{newAbout}{Numeric. The point about which the desired moments are to be
    obtained.}
}

\details{
  Suppose \eqn{m_k}{m_k} denotes the \eqn{k}{k}-th moment of a random
  variable \eqn{X}{X} about a point \eqn{a}{a}, and \eqn{m_k^*}{m_k^*}
  denotes the \eqn{k}{k}-th moment about \eqn{b}{b}. Then
  \eqn{m_k^*}{m_k^*} may be determined from the moments
  \eqn{m_1,m_2,\dots,m_k}{m_1,m_2,...,m_k} according to the formula
  \deqn{m_k^*=\sum_{i=0}^k (a-b)^i m^{k-i}}{%
  m_k^*=sum_{i=0}^k (a-b)^i m^{k-i}}
  This is the formula implemented by the function
  \code{momChangeAbout}. It is a generalization of the well-known
  formulae used to change raw moments to central moments or to change
  central moments to raw moments. See for example Kendall and Stuart
  (1989), Chapter 3.
}

\value{
  The moment of order given by the argument \code{order} about the
  location \code{newAbout}.

}

\references{
  Kendall, M. G. and Stuart, A. (1969).
  The Advanced Theory of Statistics, Volume 1, 3rd Edition.
  London: Charles Griffin & Company.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}


\examples{
### Gamma distribution
k <- 4
shape <- 2
oldAbout <- 0
newAbout <- 1
sampSize <- 1000000

### Calculate raw moments up to order k
m <- numeric(k)
for (i in 1:k){
   m[i] <- gamma(shape + i)/gamma(shape)
}
m

### Calculate moment of order k about newAbout
momChangeAbout(k, m, oldAbout, newAbout)
### Can also calculate moments about newAbout for orders less than k
momChangeAbout(3, m, oldAbout, newAbout)
momChangeAbout(2, m, oldAbout, newAbout)

### Approximate kth moment about newAbout using sampling
x <- rgamma(sampSize, shape)
mean((x - newAbout)^k)

}
\keyword{distribution}
\keyword{univar}
