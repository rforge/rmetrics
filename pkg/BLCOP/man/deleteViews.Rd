\name{deleteViews}
\alias{deleteViews}

\title{Delete individual views from view objects  }
\description{
    A generic function that allows one to delete individual views from objects of class \code{BLViews} or \code{COPViews}.  The inputs are a view object and a numeric
    vector of views to delete, where the entires of the vector map to rows of the pick matrix.
}
\usage{
deleteViews(views, viewsToDel)
}

\arguments{
  \item{views}{An object of class \code{BLViews} or \code{COPViews}}
  \item{viewsToDel}{A numeric vector of views to delete, as described above}
}


\value{
    The original object with the indicated views deleted
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\seealso{ \code{\link{BLViews-class}}, \code{\link{COPViews-class}} }
\examples{
    stocks <- colnames(monthlyReturns)
    pick <- matrix(0, ncol = 6, nrow = 2, dimnames = list(NULL, stocks))
    pick[1,"IBM"] <- 1
    pick[1, "DELL"] <- 0.04
    pick[2, "C"] <- 1
    pick[2, "JPM"] <- 0.6

    confidences <- 1 / c(0.7, 0.1)

    views <- BLViews( P = pick, q = c(0.1,0.1) , confidences = confidences,stocks)
    deleteViews(views, 1)
   
}
\keyword{manip}