\name{socp}
\alias{socp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
socp(f, A, b, C, d, N, x, z, w, control = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{ ~~Describe \code{f} here~~ }
  \item{A}{ ~~Describe \code{A} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{C}{ ~~Describe \code{C} here~~ }
  \item{d}{ ~~Describe \code{d} here~~ }
  \item{N}{ ~~Describe \code{N} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{w}{ ~~Describe \code{w} here~~ }
  \item{control}{ ~~Describe \code{control} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
 # min x + y
 # s.t. x^2 + y^2 <= 1
 #      y >= 0
 
 f <- c(1,1)
 A <- matrix(c(1,0,0,0,1,0), ncol = 2)
 b <- c(0, 0, 0)
 C <- matrix(c(0,0, 0, 1), ncol = 2)
 d <- c(1,0)
 N <- c(2, 1)
 x <- c(0, 0.5)
 z <- c(1, 0, 0)
 w <- c(2,1)
 fit <- socp(f, A, b, C, d, N, x, z, w)
 fit
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
