\name{Extractors}
\alias{assetSet}
\alias{PMatrix}
\alias{confidences}
\alias{viewMatrix}
\title{Extract various fields of view objects}
\description{
    A collection of functions to extract several fields of BLViews and COPViews
    objects.
}
\usage{
assetSet(views)
viewMatrix(views, dropZeroColumns = TRUE)
PMatrix(views)
confidences(views)
}

\arguments{
  \item{views}{An object of class BLViews or COPViews}
  \item{dropZeroColumns}{Logical flag}
}


\value{
    \item{assetSet}{The names of the assets in the view object's universe}
    \item{confidences}{The set of confidences in each view.}
    \item{PMatrix}{The 'pick' matrix}
    \item{viewMatrix}{The pick matrix augmented with the q vector of the BL model}
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
    pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
    pick[1,4] <- 1
    vdist <- list(distribution("unif", min = -0.02, max = 0))
    views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
    assetSet(views)
    confidences(views)
    PMatrix(views)

}

\keyword{utilities}
