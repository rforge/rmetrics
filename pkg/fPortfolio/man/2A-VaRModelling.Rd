\name{VaRModelling}

\alias{VaRModelling}

\alias{VaR}
\alias{CVaR}
\alias{CVaRplus}
\alias{lambdaCVaR}

\alias{pfolioMaxLoss}
\alias{pfolioReturn}
\alias{pfolioTargetReturn}
\alias{pfolioTargetRisk}
\alias{pfolioSigma}
\alias{pfolioHist}


\title{Value-at-Risk Measures for Portfolios}


\description{
  
    A collection and description of functions to 
    compute Value-at-Risk and related risk measures 
    for a portfolio of assets. In addition utility 
    functions are available to compute the maximum 
    loss, to calculate the total return, and to 
    plot a histogram of the total return.
    \cr
    
    The functions are:

    \tabular{ll}{
    \code{VaR} \tab computes Value-at-Risk for a portfolio of assets, \cr
    \code{CVaRplus} \tab computes Value-at-Risk+ for a portfolio of assets, \cr
    \code{CVaR} \tab computes Conditional Value-at-Risk for a PF of assets, \cr
    \code{lambdaCVaR} \tab computes CVaR's atomic split value lambda, \cr
    \code{pfolioMaxLoss} \tab computes Maximum Loss for a portfolio of assets, \cr
    \code{pfolioReturn} \tab computes return values of a portfolio, \cr
    \code{pfolioTargetReturn} \tab computes the target return of a portfolio, \cr
    \code{pfolioTargetRisk} \tab computes the target risk of a portfolio, \cr
    \code{pfolioHist} \tab plots a histogram of the returns of a portfolio. }
    
}
    

\usage{
VaR(x, weights = NULL, alpha = 0.05) 
CVaRplus(x, weights = NULL, alpha = 0.05) 
CVaR(x, weights = NULL, alpha = 0.05) 
lambdaCVaR(n, alpha = 0.05) 

pfolioMaxLoss(x, weights = NULL)
pfolioReturn(x, weights = NULL) 
pfolioTargetReturn(x, weights = NULL)
pfolioTargetRisk(x, weights = NULL)
pfolioHist(x, weights = NULL, alpha = 0.05, range = NULL, details = TRUE, \dots)
}


\arguments{
  
    \item{alpha}{
        a numeric value, the confidence interval, by default 0.05.
        }
    \item{details}{
        a logical value, should details be printed?
        }
    \item{n}{
        the number of observation from which the CVaR's atomic split 
        value \code{lambda=1-floor(alpha*n)/(alpha*n)} will be 
        evaluated.
        }
    \item{weights}{
        usually a numeric vector which has the length of the number of 
        assets. The weights measures the normalized weights of the 
        individual assets. By default \code{NULL}, then an equally 
        weighted set of assets is assumed. 
        }
    \item{range}{
        a numeric vector of two elements limiting the plot range of
        the histogram. This is quite useful if one likes to compare
        several plots on the same scale. If \code{range=NULL}, the
        default value, then the range will be selected automatically.
        }
    \item{x}{
        a 'timeSeries' object, data frame or any other rectangular
        object which can be expressed as a matrix. The first 
        dimension is the number of observations, we call it \code{n},
        and the second is the number of assets in the data set,
        we call it \code{dim}.
        }
    \item{\dots}{
        optional arguments to be passet to the function \code{hist}.
        }
            
}


\details{

    The percentile measures of loss (or reward) are defined in the 
    following way: Let \eqn{f(x ,y)} be a loss functions depending 
    upon a decision vector \eqn{x = (x_1, ..., x_n )} and a random 
    vector \eqn{y = (y_1, ..., y_m)}, then 
    
    \emph{VaR} is the alpha-percentile of the loss distribution, a 
    smallest value such that the probability that losses exceed or 
    are equal to this value is greater or equal to alpha.
    
    \emph{CVaRplus} or "CVaR+" or the "upper CVaR" are the expected  
    losses strictly exceeding VaR. This is also also called "Mean 
    Excess Loss" and "Expected Shortfall".

    \emph{CVaR} is a weighted average of VaR and CVaRplus defined as
    \eqn{CVaR = lambda*VaR + (1-lambda)} CVaRplus, for 
    \eqn{0 <= lambda <= 1}.
    
    Note, CVaR is convex, but VaR and CVaRplus may be non-convex. The 
    following inequalities are valid: \eqn{VaR <= CVaR <= CVaRplus}.

}


\value{

    \code{VaR}
    \cr
    returns the value of risk, VaR,  for a portfolio of assets, a
    numeric value.
    \cr
    
    \code{CVaRplus}
    \cr
    returns the conditional value of risk plus, CVaRplus, for a 
    portfolio of assets, a numeric value.
    \cr
    
    \code{CVaR}
    \cr
    returns the conditional value of risk, CVaR, for a portfolio
    of assets, a numeric value.
    \cr
    
    \code{lambdaCVaR}
    \cr
    returns CVaR's atomic split value \code{lambda}, a numeric value.
    \cr
    
    \code{pfolioMaxLoss}
    \cr
    returns the maximum loss value of the portfolio, a numeric value.
    \cr
    
    \code{pfolioReturn}
    \cr
    returns the total portfolio return computed from the set of
    assets \code{x}, a numeric vector.
    \cr
    
    \code{pfolioTargetReturn}
    \cr
    returns the total return or target return computed from the set of
    assets \code{x} and weights \code{weights}, a numeric value.
    \cr
    
    \code{pfolioTargetRisk}
    \cr
    returns the total risk (Sigma) or target risk computed from the set 
    of assets \code{x} and \code{weights} via the formual
    \code{sqrt(weights \%*\% cov(x) \%*\% weights)}, a numeric value.
    \cr
    
    \code{pfolioHist}
    \cr
    plots a histogram of portfolio returns and adds the values
    for the VaR (blue), for the CVaRplus (red), and for the 
    maximum loss (green) to the histogram plot. The function 
    invisibly returns a list with the following elements: VaR,
    VaRplus, maxLoss, mean, and sd. If \code{details} is \code{TRUE}, 
    then the result is printed.
          
}


\references{

Uryasev S. (2000);
    \emph{Conditional Value-at-Risk (CVaR): Algorithms and Applications},
    Risk Management and Financial Engineering Lab, University of Florida
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## SOURCE("fPortfolio.102A-VaRModelling")

## assetsSim -
   myAssets = 100/12 * assetsSim(n = 120, dim = 4)
   # Plot Cumulated Returns of the Assets:
   prices = apply(myAssets, 2, FUN = cumsum)
   par(mfrow = c(2, 1), cex = 0.7)
   ts.plot(prices, col = 1:4, ylim = c(-300, 300))
   legend(0, 300, legend = colnames(myAssets), pch = "----", col = 1:4)
   title(main = "Cumulated Returns", ylab = "Cumulated Returns")
   abline(h = 0, lty = 3)
    
## CVaR -
   equalWeights = rep(1/4, 4)
   alpha = 0.10
   # Value at Risk:
   VaR(myAssets, equalWeights, alpha)
   # Conditional Value at Risk Plus:
   CVaRplus(myAssets, equalWeights, alpha)
   # Conditional Value at Risk Plus:
   CVaR(myAssets, equalWeights, alpha)
   # Lambda - Atomic Split Value:
   lambdaCVaR(120, alpha) 

## pfolioHist - 
   # Maximum Loss Value of the Portfolio
   pfolioMaxLoss(myAssets, equalWeights)
   # Compute Portfolio Returns:
   r = pfolioReturn(myAssets, equalWeights) 
   head(r)
   # Target Return and Target Risk:
   pfolioTargetReturn(myAssets, equalWeights)
   pfolioTargetRisk(myAssets, equalWeights)
   # Plot:
   pfolioHist(myAssets, equalWeights, alpha, n = 20)    
}


\keyword{math}

