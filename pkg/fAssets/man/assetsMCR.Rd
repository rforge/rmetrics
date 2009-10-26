\name{assetsMCR}


\alias{assetsMCR}

\alias{covarRisk}
\alias{mcr}
\alias{mcrBeta}
\alias{riskContributions}
\alias{riskBudgets}


\title{Marginal Contributions to Covariance Risk}


\description{
  
    Computes marginal contributions to covariance risk and related 
    measures for a portfolio of assets. 

    The functions are:

    \tabular{ll}{
    \code{covarRisk} \tab Computes covariance risk, \cr
    \code{mcr} \tab computes marginal contribution to covariance risk, \cr
    \code{mcrBeta} \tab computes beta, the rescaled mcr to covariance risk, \cr
    \code{riskConstributions} \tab computes covariance risk contributions, \cr
    \code{riskBudgets} \tab computes covariance risk budgets. }
    
}
    

\usage{
covarRisk(data, weights = NULL, FUN = "cov", ...)
mcr(data, weights = NULL, FUN = "cov", ...) 
mcrBeta(data, weights = NULL, FUN = "cov", ...) 
riskContributions(data, weights = NULL, FUN = "cov", ...) 
riskBudgets(data, weights = NULL, FUN = "cov", ...) 
}


\arguments{
  
    \item{data}{
        a multivariate 'timeSeries' object.
        }
    \item{weights}{
        usually a numeric vector which has the length of the number of 
        assets. The vector measures the weights of the individual assets. 
        By default \code{NULL}, then an equally weighted set of assets 
        is assumed. 
        }
    \item{FUN}{
        the name of the covariance estimator function which returns the
        covariance matrix. By default, the sample covariance estimator.
        }
    \item{\dots}{
        optional arguments to be passet to the function \code{FUN}.
        }
            
}


\value{

    \code{covarRisk}
    \cr
    returns the covariance risk (standard deviation), a numeric value.
    \cr
    
    \code{mcr}
    \cr
    returns the marginal contributions to covariance risk for a 
    portfolio of assets, a numeric value of the same length as the
    number of assets.
    \cr
    
    \code{mcrBeta}
    \cr
    returns the marginal contributions to beta for a 
    portfolio of assets, a numeric value of the same length as the
    number of assets.
    \cr
    
    \code{riskContributions}
    \cr
    returns the risk contributions to covariance risk for a 
    portfolio of assets, a numeric value of the same length as the
    number of assets.
    \cr
    
    \code{riskBudgets}
    \cr
    returns the risk budgets to covariance risk for a 
    portfolio of assets, a numeric value of the same length as the
    number of assets.
          
}


\references{

Goldberg L., Hayes M.Y., Menchero J., Mitra. I, (2009);
    \emph{Extreme Risk Management},
    Working Paper, MSCI Barra.
    
Scherer B., (2004);
    \emph{Portfolio Construction and Risk Budgeting},
    Risk Books, Haymarket House.

%Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
%    \emph{Portfolio Optimization with R/Rmetrics}, 
%    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
      
}


\examples{
## covarRisk - 
   # Covariance Risk
   # Sigma = sqrt(W' COV W)
   set.seed(4711)
   data = assetsSim(100, 6)
   covarRisk(data)
   
## mcr -
   # Marginal contribution to Covariance Risk
   # MCR = d Sigma / d W_i
   mcr(data)
    
## mcrBeta -
   # Marginal Beta
   # beta = MCR / Sigma
   mcrBeta(data)

## riskContributions - 
   # Marginal Risk Contributions
   # RC = Sum_i ( W_i MCR )
   riskContributions(data)
   sum(riskContributions(data)) - covarRisk(data)
   
## riskBudgets -  
   # Marginal Risk Budgets
   # RB = RC / Sigma
   riskBudgets(data)
   sum(riskBudgets(data)) 
}


\keyword{math}

