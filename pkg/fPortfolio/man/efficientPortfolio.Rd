\name{efficientPortfolio}


\alias{efficientPortfolio}
\alias{cmlPortfolio}
\alias{tangencyPortfolio}
\alias{minvariancePortfolio}


\title{Efficient Portfolios}


\description{
  
    Optimizes efficient portfolios.
    
}
    

\usage{
efficientPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

cmlPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
tangencyPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

minvariancePortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
}



\arguments{

    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }

}


\details{
    
    \bold{Efficient Portfolio:}
    \cr\cr   
    
    An efficient portfolio is a portfolio which lies on the efficient 
    frontier. The weights have to be optimized. 
       
    The function requires three arguments: \code{data}, \code{spec} 
    (specifications), and \code{constraints}, see above. Be sure that
    the specification structure has undefined weights, \code{NULL}.
    In addition, only one, either the target return or the target risk 
    is defined  vector which is different from \code{"NULL"}.  This 
    determines which type of optimization should be done. If the risk 
    is given and the return is missing, then the return will be maximized
    for the given risk. On the other hand, if the return is given and the 
    risk is missing, then the risk will be minimizes for the given return. 
    To assign values either to the target return or target trisk in the 
    specification structure, use the functions \code{settargetReturn}
    and \code{setTargetRisk}. 
    
    The \code{efficeentPortfolio} function returns the properties of 
    the efficient portfolio as an S4 object of class \code{fPORTFOLIO}.
    
    
    \bold{Capital Market Line Portfolio:}
    \cr\cr
    
    This is the portfolio with the highest Sharpe ratio on the
    efficient frontier.
    
    
    \bold{Tangency Portfolio:}
    \cr\cr
    
    This is the CML portfolio with zero risk free rate.
    
    \bold{Minimum Variance Portfolio:}
    \cr\cr
    
    This is the portfolio with the minimal risk on the efficient
    frontier.

}


\value{
    
    \code{feasiblePortfolio} function returns an S4 object of class 
    \code{"fPORTFOLIO"}.
    
}



%\references{}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## data -
   
   # Load Data and Convert to timeSeries Object:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## portfolioSpec -
   # Set Default Specifications:
   Spec = portfolioSpec()
   Spec
   setTargetReturn(Spec) = mean(colMeans(Data))
   
## Allow for unlimited Short Selling:
   Constraints = "LongOnly"
   
## Compute properties of efficient Portfolio
   efficientPortfolio(Data, Spec, Constraints)
   
## Compute properties of tangency Portfolio
   tangencyPortfolio(Data, Spec, Constraints)   
}


\keyword{models}

