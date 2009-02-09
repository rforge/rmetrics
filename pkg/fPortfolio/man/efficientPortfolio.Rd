\name{efficientPortfolio}


\alias{efficientPortfolio}
\alias{maxratioPortfolio}
\alias{tangencyPortfolio}
\alias{minriskPortfolio}
\alias{minvariancePortfolio}
\alias{maxreturnPortfolio}

\title{Efficient Portfolios}


\description{
  
    Returns efficient portfolios.
    
}
    

\usage{
efficientPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

maxratioPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
tangencyPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

minriskPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
minvariancePortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")

maxreturnPortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
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
    
    
    An efficient portfolio is a portfolio which lies on the efficient 
    frontier. 
    The \code{efficientPortfolio} function returns the properties of 
    the efficient portfolio as an S4 object of class \code{fPORTFOLIO}.

    
    \bold{Tangency Portfolio:}
    
    The function \code{tangencyPortfolio} returns the portfolio with 
    the highest return/risk ratio on the efficient frontier. For the
    Markowitz portfolio this is the same as the Sharpae ratio. To find 
    this point on the frontier the return/risk ratio calculated from 
    the target return and target risk returned by the function 
    \code{efficientPortfolio}. 

    
    \bold{Minimum Variance Portfolio:}
    
    The function \code{minvariancePortfolio} returns the portfolio 
    with the minimal risk on the efficient frontier. To find the 
    minimal risk point the target risk returned by the function
    \code{efficientPortfolio} is minimized.
    
    
    \bold{Maximum Return Portfolio:}
    
    The function \code{maxreturnPortfolio} returns the portfolio 
    with the maximal return for a fixed target risk.

}


\value{
    
    returns an S4 object of class \code{"fPORTFOLIO"}.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## data -
   # Load Data and Convert to timeSeries Object:
   Data = SMALLCAP.RET
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## portfolioSpec -
   # Set Default Specifications:
   Spec = portfolioSpec()
   Spec
   # Set Target Return to the grand mean:
   setTargetReturn(Spec) = mean(colMeans(Data))
   
## Allow for LongOnly Constraints:
   Constraints = "LongOnly"
   Constraints
   
## efficientPortfolio -
   # Compute properties of efficient Portfolio:
   efficientPortfolio(Data, Spec, Constraints)
   
## tangency Portfolio -
   # Compute properties of tangency Portfolio:
   tangencyPortfolio(Data, Spec, Constraints)   
   
## minvariancePortfolio -
   # Compute properties of minimum variance Portfolio:
   minvariancePortfolio(Data, Spec, Constraints)
}


\keyword{models}

