\name{momChangeAbout}
\alias{momChangeAbout}

\title{Obtain Moments About a New Location}
\description{
  Using the moments up to a given order about one location, this function
  either returns the moments up to that given order about a new location as a 
  vector or it returns a moment of a specific order defined by users 
  (order <= maximum order of the given moments) about a new location as a single
  number. 
  A generalization of using raw moments to obtain a central moment or using 
  central moments to obtain a raw moment.
}

\usage{
  momChangeAbout(order = "all", oldMom, oldAbout, newAbout)
}

\arguments{
  \item{order}{One of:
    \itemize{
      \item the character string "all", the default;
      \item a positive integer less than the maximum order of \code{oldMom}.
    }
  }
  \item{oldMom}{Numeric. Moments of orders 1, 2, \dots, about the point 
    \code{oldAbout}.}
  \item{oldAbout}{Numeric. The point about which the moments \code{oldMom} have
    been calculated.}
  \item{newAbout}{Numeric. The point about which the desired moment or
  moments are to be
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
  The moment of order \code{order} about the location \code{newAbout} when 
  \code{order} is specified.
  The vector of moments about the location \code{newAbout} from first
  order up to the maximum order of the \code{oldMom} when \code{order}
  takes the value \code{"all"} or is not specified. 
}

\references{
  Kendall, M. G. and Stuart, A. (1969).
  \emph{The Advanced Theory of Statistics, Volume 1, 3rd Edition}.
  London: Charles Griffin & Company.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, 
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}


\examples{
### Gamma distribution
k <- 4
shape <- 2
old <- 0
new <- 1
sampSize <- 1000000

### Calculate 1st to 4th raw moments 
m <- numeric(k)
for (i in 1:k){
   m[i] <- gamma(shape + i)/gamma(shape)
}
m

### Calculate 4th moment about new 
momChangeAbout(k, m, old, new)
### Calculate 3rd about new
momChangeAbout(3, m, old, new)

### Calculate 1st to 4th moments about new
momChangeAbout(oldMom = m, oldAbout = old, newAbout = new)
momChangeAbout(order = "all", m, old, new)
  
### Approximate kth moment about new using sampling
x <- rgamma(sampSize, shape)
mean((x - new)^k) 
}
\keyword{distribution}
\keyword{univar}
