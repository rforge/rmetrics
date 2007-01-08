\name{TwoAssetsPortfolio}

\alias{TwoAssetsPortfolio}

\alias{fPFOLIO2}
\alias{fPFOLIO2-class}

\alias{show.fPFOLIO2}
\alias{show,fPFOLIO2-method}

\alias{frontierTwoAssetsMarkowitz}
\alias{frontierTwoAssetsCVaR}

\alias{plot.fPFOLIO2}
\alias{summary.fPFOLIO2}


\title{Two Assets Portfolio}


\description{
  
    A collection and description of functions 
    to investigate the efficient frontier for
    a two assets portfolio from a given return 
    series in the mean-variance and CVaR sense 
    when short selling is forbidden. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{twoAssetsMarkowitz} \tab Efficient frontier for Markowitz PF, \cr
    \code{twoAssetsCVaR} \tab Efficient frontier for CVaR PF. }
    
}
    

\usage{
frontierTwoAssetsMarkowitz(x, length = 100, 
    title = NULL, description = NULL) 
frontierTwoAssetsCVaR(x, length= 100, alpha = 0.05,
    title = NULL, description = NULL) 
 
show.fPFOLIO2(object) 
  
\method{plot}{fPFOLIO2}(x, \dots)
\method{summary}{fPFOLIO2}(object, \dots)
}


\arguments{
   
    \item{alpha}{
        a numeric value, the confidence interval, by default 0.05.
        }
    \item{description}{
        a character string, assigning a brief description to an 
        \code{"fPFOLIO"} object.
        }
    \item{length}{
        the number of equidistand return points on the efficient 
        frontier.
        }   
    \item{object}{
        [summary] - cr
        an object of class \code{"fPFOLIO2"}.    
        }
    \item{title}{
        a character string, assigning a title to an \code{"fPFOLIO"} 
        object.
        }
    \item{x}{
        [frontier*] - \cr
        a numeric matrix or multivariate time series consisting of a
        series of returns. \cr
        [print][plot] - cr
        an object of class \code{"fPFOLIO2"}.    
        }
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{

    \code{frontierTwoAssetsMarkowitz}\cr
    \code{frontierTwoAssetsCVaR}
    \cr
    return a S4 object class of class \code{"fPFOLIO2"}, with the following 
    slots:

    \item{@call}{
        the matched function call.
        }
    \item{@data}{
        the input data in form of a data.frame.
        }
    \item{@description}{
        allows for a brief project description.
        }
    \item{@pfolio}{
        the results as a list returned from the underlying
        analytical results. 
        }
    \item{@method}{
        the selected method used by the optimization algorithm,
        here "Exact Analytical Solution".
        }
    \item{@model}{
        the model used for the optimization, here "Two Assets 
        Markowitz Portfolio" or "Two Assets CVaR Portfolio.
        }
    \item{@title}{
        a title string.}
        
    The \code{@pfolio} slot is a list with the following compontents:
    (Note, not all are documented here).

    \item{pm, ps}{
        the portfolios mean (the return) and standard deviation (the
        risk) values .
        }
  
}


\references{

Elton E.J., Gruber M.J. (1991); 
    \emph{Modern Portfolio Theory and Investment Analysis}, 
    4th Edition, Wiley, NY, pp. 65--93.
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## SOURCE("fPortfolio.102B-TwoAssetsPortfolio")

\dontrun{
## berndtInvest -
   data(berndtInvest)
   # Select "IBM" and "DEC"
   twoAssets = berndtInvest[, c("IBM", "DEC")]
   rownames(twoAssets) = berndtInvest[, 1]
   head(twoAssets)
   
## Compute Efficient Frontier:
   myPortfolio = frontierTwoAssetsMarkowitz(twoAssets)
   
## Print Efficient Frontier:
   print(myPortfolio)
    
## Plot Efficient Frontier:
   plot(myPortfolio)
   
## Compute Efficient Frontier:
   myPortfolio = frontierTwoAssetsCVaR(twoAssets)
   
## Print Efficient Frontier:
   print(myPortfolio)
    
## Plot Efficient Frontier:
   plot(myPortfolio)
   
  
  
    show = function(i, j) { 
        w = (0:100)/100 
        alpha = 0.04
        par(mfrow = c(2, 2), cex = 0.5)
        
        print(c(i, j))
        twoAssets = cbind(berndtInvest[, i], berndtInvest[, j])
        print(head(twoAssets))
        
        means = apply(twoAssets, 2, mean)
        x.Return = y.VaR = y.CVaR = y.CVaRplus = NULL
        for (k in 1:101) {
            weights = c(w[k], 1-w[k])
            x.Return = c(x.Return, weights %*% means)
            y.VaR = c(y.VaR, -VaR(twoAssets, weights, alpha))
            y.CVaR = c(y.CVaR, -CVaR(twoAssets, weights, alpha))
            y.CVaRplus = c(y.CVaRplus, -CVaRplus(twoAssets, weights, alpha))
        }
        plot(x = range(x.Return), y = range(c(y.VaR, y.CVaR, y.CVaRplus)), 
            xlab = as.character(i), ylab = as.character(j), type = "n")
        lines(x.Return, as.vector(y.VaR), col ="red")
        lines(x.Return, as.vector(y.CVaR), col = "green")
        lines(x.Return, as.vector(y.CVaRplus), col = "blue")
    }

    par(mfrow = c(1,1)) 
    w = (0:200)/200
    alpha = 0.04
    for (i in 2:17) {
        for (j in (i+1):18) {    
            twoAssets = cbind(berndtInvest[, i], berndtInvest[, j])   
            means = apply(twoAssets, 2, mean)
            x = NULL
            y = NULL
            for (k in 1:201) {
                weights = c(w[k], 1-w[k])
                x = c(x, weights %*% means)
                y = c(y, -CVaRplus(twoAssets, weights, alpha))
            }
            s = round(100*abs( y[201] - y[1] ) / ( max(y) - min(y)))
            print(c(i, j, s))
            plot(x, y, main = paste(as.character(i), as.character(j)))
        }
    }
    
    i = 5; j = 13
    twoAssets = cbind(berndtInvest[, i], berndtInvest[, j])     
    means = apply(twoAssets, 2, mean)
    x = y = NULL
    for (k in 1:201) {
        weights = c(w[k], 1-w[k])
        x = c(x, weights %*% means)
        y = c(y, -VaR(twoAssets, weights, alpha)) 
    }
    s = round(100*abs( y[201] - y[1] ) / ( max(y) - min(y)))
    plot(x, y, main = paste(as.character(i), as.character(j)))
            
}
   
}


\keyword{data}
