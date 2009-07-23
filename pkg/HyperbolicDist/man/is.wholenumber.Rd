\name{is.wholenumber}
\alias{is.wholenumber}

\title{Is Object Numeric and Whole Numbers}
\description{
  Checks whether an object is numeric and if so, are all the elements
  whole numbers, to a given tolerance.
}
\usage{
is.wholenumber(x, tolerance = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{The object to be tested.}
  \item{tolerance}{Numeric \eqn{\ge 0}{>=0}. Absolute differences
    greater than \code{tolerance} are treated as real differences.}
}
\details{
  The object \code{x} is first tested to see if it is numeric. If not
  the function returns \code{'FALSE'}. Then if all the elements of
  \code{x} are whole numbers to within the tolerance given by
  \code{tolerance} the function returns \code{'TRUE'}. If not it returns
  \code{'FALSE'}.
}
\value{
  Either \code{'TRUE'} or \code{'FALSE'} depending on the result of the
  test.
}
\references{Based on a post by Tony Plate <tplate@acm.org> on R-help.}
\author{David Scott  \email{d.scott@auckland.ac.nz}.}
\examples{
is.wholenumber(-3:5)                           # TRUE
is.wholenumber(c(0,0.1,1.3,5))                 # FALSE
is.wholenumber(-3:5 + .Machine$double.eps)     # TRUE
is.wholenumber(-3:5 + .Machine$double.eps^0.5) # FALSE
is.wholenumber(c(2L,3L))                       # TRUE
is.wholenumber(c("2L","3L"))                   # FALSE
is.wholenumber(0i ^ (-3:3))                    # FALSE
is.wholenumber(matrix(1:6, nrow = 3))          # TRUE
is.wholenumber(list(-1:3,2:6))                 # FALSE
is.numeric(list(-1:3,2:6))                     # FALSE
is.wholenumber(unlist(list(-1:3,2:6)))         # TRUE
}
\keyword{classes}
