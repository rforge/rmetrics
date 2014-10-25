\name{absMoments}


\alias{absMoments}


\title{Absolute Moments of GARCH Distributions}


\description{
    
    Computes absolute Moments of the skew Normal, 
    skew GED, and standardized skew Student-t 
    distributions
    
}


\usage{
absMoments(n, density = c("dnorm", "dged", "dstd"), \dots)
}


\arguments{

    \item{density}{
        a character string naming the symmetric density function.
        }
    \item{n}{
        the number of absolute Moments.
        } 
    \item{\dots}{
        parameters passed to the density function.
        }
    
}


\value{

    \code{absMoments} returns a numeric vector of length \code{n} with the
    values of the absolute moments of the selected density function.
       
}


\references{
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## absMoment -
    absMoments(4, "dstd", nu = 4)
}


\keyword{distribution}

