\name{portfolioRisk}


\alias{portfolioRisk}

\alias{covRisk}
\alias{varRisk}
\alias{cvarRisk}


\title{portfolioRisk}


\description{

    Computes portfolio risk.

}


\usage{
covRisk(data, weights)
varRisk(data, weights, alpha = 0.05)
cvarRisk(data, weights, alpha = 0.05)
}


\arguments{

    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}.
        }
    \item{weights}{ 
        a numeric vector of weights. 
        }
    \item{alpha}{
        a numeric value, the confidence level, by default \code{alpha=0.05},
        i.e. 5\%.
        }
}


\keyword{models}

