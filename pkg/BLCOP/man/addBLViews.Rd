\name{Build Views}
\alias{addBLViews}
\alias{addCOPViews}
\alias{BLViews}
\alias{COPViews}
\alias{newPMatrix}
\title{Create or add to a BLViews object}

\description{
\code{BLViews} and \code{COPViews} are "constructors" for BLViews and COPViews objects respectively.  
\code{addBLViews} and \code{addCOPViews} allow one to easily add more views to a pre-existing views objects.
\code{newPMatrix} is a utility function for creating pick matrices.
}

\usage{	
addBLViews(pickMatrix, q, confidences, views)
addCOPViews(pickMatrix, viewDist, confidences, views) 
BLViews(P, q, confidences, assetNames)
COPViews(pickMatrix, viewDist, confidences, assetNames) 
newPMatrix(assetNames, numViews, defaultValue = 0)
}

\arguments{
  \item{P}{"Pick" matrix with columns named after assets to which views correspond}
  \item{pickMatrix}{"Pick" matrix with columns named after assets to which views correspond}
  \item{q}{"q" vector of views}
  \item{confidences}{Vector of confidences in views.  Note that confidences are recipricols of standard deviations}  
  \item{viewDist}{A list of marginal distributions of the views}
  \item{views}{A BLViews object}
  \item{assetNames}{Names of the assets in the universe}
  \item{numViews}{Number of views in the pick matrix}
  \item{defaultValue}{Default value to use to fill the new pick matrix}
}
\value{
  A BLViews or COPViews class object as appropriate.  \code{newPMatrix} creates a matrix.
}

\author{Francisco Gochez}
\seealso{\code{\link{createBLViews}, \code{\link{updateBLViews}}}}
\examples{
    ### example from Thomas M. Idzorek's paper "A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL"
 
    pick <- newPMatrix(letters[1:8], 3)
    pick[1,7] <- 1
    pick[2,1] <- -1 
    pick[2,2] <- 1
    pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
    confidences <- 1 / c(0.00709, 0.000141, 0.000866)
    myViews <- BLViews(pick, q = c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
    myViews
    
    ### Modified COP example from Meucci's "Beyond Black-Litterman: Views on non-normal markets" 
    dispersion <- c(.376,.253,.360,.333,.360,.600,.397,.396,.578,.775) / 1000
    sigma <- BLCOP:::.symmetricMatrix(dispersion, dim = 4)
    caps <- rep(1/4, 4)
    mu <- 2.5 * sigma %*% caps
    dim(mu) <- NULL
    marketDistribution <- mvdistribution("mt", mean = mu, S = sigma, df = 5 )
    pick <- newPMatrix(c("SP", "FTSE", "CAC", "DAX"), 1)
    pick[1,4] <- 1
    vdist <- list(distribution("unif", min = -0.02, max = 0))
    views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
}
\keyword{datagen}
\keyword{manip}