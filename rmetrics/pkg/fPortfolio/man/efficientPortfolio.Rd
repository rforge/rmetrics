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
    
    
    \emph{The efficient Portfolio Tree:}
    
    \preformatted{
                       Optimize:    Type:      Constraints:  Solver:
                       
                                               +  short
                                    +--MV------+  lin
                                    |          +  quad 
                                    |                         
                       +-MinRisk----+--LPM-----+  lin
                       |            |          +  quad
                       |            |
                       |            +--CVaR----+  lin
                       |                       +  quad
                       |
    efficientPortfolio-+
                       |                       +  short
                       |            +--MV------+  lin     
                       |            |          +  quad     
                       |            |                   
                       +-MaxReturn--+--LPM-----+  lin     
                                    |          +  quad    
                                    |                   
                                    +--CVaR----+  lin     
                                               +  quad
    }    
    
    \bold{Tangency Portfolio:}
    
    The function \code{tangencyPortfolio} returns the portfolio with 
    the highest return/risk ratio on the efficient frontier. For the
    Markowitz portfolio this is the same as the Sharpae ratio. To find 
    this point on the frontier the return/risk ratio calculated from 
    the target return and target risk returned by the function 
    \code{efficientPortfolio}.
    Note, the default value of the risk free rate is zero. 

    
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


\note{
    
    The \code{cmlPortfolio} function has been removed with Version 2.7.0.
    This function produced some misunderstanding by the users in that way
    they assumed to compute the capital market line. What the function
    really did was to compute the tangency portfolio for a non zero risk
    free market rate. So just set the rate and use the function
    \code{tangencyPortfolio} which will delover the same results as the
    deprecated function \code{cmlPortfolio}.
    
} 



%\references{}


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

