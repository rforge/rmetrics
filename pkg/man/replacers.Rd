\name{Replacer functions}
\alias{PMatrix<-}
\alias{confidences<-}
\alias{qv<-}
\title{Various functions for modifying fields of view objects}
\description{
    These functions allow for direct replacement of fields of view objects such as the pick
    matrix and vector of confidences.
}
\usage{
PMatrix(views) <- value
confidences(views) <- value
qv(views) <- value
}

\arguments{
  \item{views}{An object of class BLViews or COPViews, except in the case of qv<- which applies only to BLViews}
  \item{value}{ A vector in \code{confidences<-} and \code{qv<-} or a matrix in \code{PMatrix<-}.}
}

\value{
    The object is modified directly
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
    
}
\keyword{utilities}
\keyword{manip}