\name{AssetsLPM}

\alias{AssetsLPM}

\alias{assetsLPM}


\title{Estimation of Lower Partial Moments of Asset Sets}


\description{
  
    A collection and description of functions 
    for the estimation of lower partial moments.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsLPM} \tab Computes LPMs and co-LPMs of a set of assets. }
    
}
    

\usage{
assetsLPM(x, tau, a, \dots)
}

\arguments{
  

    \item{a}{
        a xxx .
        }
    \item{tau}{
        a xxx .
        }    
    \item{x}{
        xxx
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\details{

    \bold{Assets Lower Partial Moments:}
    \cr\cr   
    The function \code{assetsLPM} computes xxx.
  
}


\value{
  
  
    \code{assetsLPM}
    \cr
    returns a list with two entries named \code{mu} and Sigma{Sigma}.
    The first denotes the vector of lower partial moments, and the 
    second the co-LPM matrix. Note, that the output of this function 
    can be used as data input for the portfolio functions to compute 
    the LPM efficient frontier.
  
}


%\references{
%    
%Breiman L. (1996); 
%    \emph{Bagging Predictors},
%    Machine Learning 24, 123--140.
%
%}


\seealso{

    \code{assetsMeanCov}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## 
}


\keyword{models}

