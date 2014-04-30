\name{assets-modeling}


\alias{assetsFit}
\alias{assetsSim}

\title{Modeling Multivariate Asset Sets}


\description{
  
    Fitting and Simulatingassets from multivariate asset sets based 
    on modeling skew normal and related distributions.
    
}
    

\usage{
assetsFit(x, method = c("st", "sn", "sc"), 
  title=NULL, description=NULL, fixed.df=NA, \dots)

assetsSim(n, method=c("st", "sn", "sc"),
    model=list(xi=rep(0, 2), Omega=diag(2), alpha=rep(0, 2), nu=4), 
    assetNames=NULL)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{n}{
        a numeric value which represents the number of random vectors to 
        be drawn.
        }
    \item{method}{
        a character string with the names of the supported distributions:
        \code{sn} skew normal, \code{st} skew Student-t, and \code{sc}
        skew Cauchy
        }
    \item{model}{
        a list with the model parameters.
        \code{xi} a numeric vector, representing the location,
        \code{Omega} a symmetric positive-definite matrix (covariance matrix),
        \code{alpha} a numeric vector which regulates the skew of the density,
        \code{nu} a positive value representing the degrees of freedom.
        }
    \item{fixed.df}{
        a logical value, should the degreess of freedom fitted or held
        fixed?
        }
    \item{title}{  
        an optional project title.
        }
    \item{description}{
        an option project desctiption.
        }
    \item{assetNames}{
        a character vector with optional asset names.
        }
    \item{\dots}{
        optional arguments passed to the underlying functions.
        }
}


\value{
  
    \code{assetsFit} returns the fitted parameters, \code{assetsSim}
    returns a simulated (return) series.
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


%\examples{}


\keyword{models}


