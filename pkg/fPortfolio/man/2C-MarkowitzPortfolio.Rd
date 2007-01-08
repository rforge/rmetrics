\name{MarkowitzPortfolio}

\alias{MarkowitzPortfolio}

\alias{fPFOLIO}
\alias{fPFOLIO-class}

\alias{portfolioMarkowitz}
\alias{frontierMarkowitz}
\alias{montecarloMarkowitz}

\alias{print.fPFOLIO}
\alias{plot.fPFOLIO}
\alias{summary.fPFOLIO}


\title{Markowitz Portfolio}


\description{
  
    A collection and description of functions to 
    investigate the efficient frontier for a 
    Markowitz portfolio from a given return 
    series in the mean-variance sense when short 
    selling is forbidden. Tangency, equal weigths, 
    and Monte Carlo portfolios can also be evaluated.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioMarkowitz} \tab Optimize mean variance Markowitz portfolio, \cr
    \code{frontierMarkowitz} \tab computes efficient frontier of portfolio, \cr
    \code{montecarloMarkowitz} \tab creates randomly created portfolios. }
       
}
    

\usage{
portfolioMarkowitz(data, targetReturn, title = NULL, description = NULL)
frontierMarkowitz(data, Rf = 0, short = FALSE, length = 300, r.range = NULL, 
    s.range = NULL, title = NULL, description = NULL, \dots)

montecarloMarkowitz(object, mc = 5000, doplot = FALSE, add = TRUE, \dots)

\method{print}{fPFOLIO}(x, \dots)
\method{plot}{fPFOLIO}(x, alpha = 0.05, mc = 500, which = "ask", \dots)
\method{summary}{fPFOLIO}(object, \dots)
}


\arguments{
  
    \item{alpha}{
        a numeric value, the confidence interval, by default 0.05.
        }
    \item{data}{
        [frontierMarkowitz] - \cr
        either a rectangular time series object which can be
        transformed into a matrix, or a list with two named entries,
        \code{mu}, the returns of the multivariate series, and
        \code{Sigma}, the covariance matrix of the multivariate series.
        }
    \item{description}{
        [portfolioMarkowitz][frontierMarkowitz] - \cr
        a character string, assigning a brief description to an 
        \code{"fPFOLIO"} object.
        }
    \item{doplot, add}{
        [frontierMarkowitz] - \cr
        logical values. Should the results be plotted and/or added to
        the frontier plot?
        } 
    \item{length}{
        [frontierMarkowitz] - \cr
        the number of equidistand return points on the efficient 
        frontier.
        } 
    \item{mc}{
        the number of Monte Carlo portfolios.
        }
    \item{method}{
        [portfolioMarkowitz] - \cr
        the optimization method.
        }
    \item{object}{
        a object of class \code{"fPFOLIO"} representing a portfolio. 
        A list with with elemets listed in the "Value" section.
        }
    \item{r.range, s.range}{
        [frontierMarkowitz] - \cr
        the range of returns and standardard deviations on the 
        efficient frontier. If set to \code{NULL}, the default
        value, then the whole efficient frontier will be investigated.
        }  
    \item{Rf}{
        [frontierMarkowitz] - \cr
        the number of random portfolios to be generated.
        }
    \item{short}{
        [frontierMarkowitz] - \cr
        a logical flag, if set to TRUE short selling would be allowed 
        otherwise not wich is the default setting.
        }
    \item{targetReturn}{
        [portfolioMarkowitz] - \cr
        the target return, a numeric value.
        }
    \item{title}{
        [portfolioMarkowitz][frontierMarkowitz] - \cr
        a character string, assigning a title to an \code{"fPFOLIO"} 
        object.
        }
    \item{which}{
        which of the five plots should be displayed? \code{which} can 
        be either a character string, \code{"all"} (displays all plots) 
        or \code{"ask"} (interactively asks which one to display), or a 
        vector of 6 logical values, for those elements which are set 
        \code{TRUE} the correponding plot will be displayed.
        }
    \item{x}{
        [frontierMarkowitz] - \cr
        a numeric matrix or multivariate time series consisting of a
        series of returns. \cr
        [print][plot] - \cr
        an object of class \code{"fPFOLIO"} representing a portfolio. 
        A list with with elemets listed in the "Value" section.
        } 
    \item{\dots}{
        parameters to be passed.
        }
        
}


\value{

    \code{portfolioMarkowitz}\cr
    \code{frontierMarkowitz}\cr 
    \cr
    return a S4 object class of class \code{"fPFOLIO"}, with the following 
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
        optimization process. 
        }
    \item{@method}{
        the selected method used by the optimization algorithm,
        here "Quadratic Programming".
        }
    \item{@model}{
        the model used for the optimization, here "Mean variance
        Portfolio Optimization".
        }
    \item{@title}{
        a title string.}
        
    The \code{@pfolio} slot is a list with the following compontents:
    (Note, not all are documented here).

    \item{pm, ps}{
        the portfolios mean (the return) and standard deviation (the
        risk) values .
        }
    \item{returns, cov}{
        the returns vector and the covariance matrix of the assets.
        }
    \item{r.range, s.range}{
        the range of returns and of the standard deviations on the 
        efficient frontier.
        }   
    \item{Rf}{
        the risk free rate.
        }
    \item{Rm, Sm}{
        return and standard deviation of the tangency portfolio.
        }
    \item{t.weights}{
        the weights of the tangency portfolio.
        }
    \item{Rew, Sew}{
        Return and standard deviation of the equal weights portfolio.
        }   
    \item{diversification}{
        the number of assets contributing with more than one percent to 
        the efficient frontier portfolio as function of expected returns.
        }
        
    \code{montecarloMarkowitz}
    \cr
    is a functions which adds random generated portfolios to frontier
    plot.
    
    \code{print} 
    \cr
    is the S3 print method for objects of class \code{"fPFOLIO"} returned
    from the function \code{mvFit}. If shows a summary report of
    the optimization results.
    
    \code{plot} 
    \cr
    is the S3 plot method for objects of class \code{"fPFOLIO"}. Six plots 
    are produced.\cr
    The first plot produces a Risk-Return-Graph with the individual assets,
    the efficient frontier, the market line, tangency portfolio and the
    equal weights portfolio.\cr
    The second plot is the same as the first, however, the risk and return
    values for random generated portfolios are added.
    The third plot generates a pie chart of weights for the tangency
    portfolio.\cr
    The third plot generates a pie chart of weights for the tangency
    portfolio.\cr
    The fourth plot shows the effect of diversification.\cr
    The fivth plot shows the cumulated returns for the tangency 
    portfolio.\cr
    The sixth plot shows a histogram of the tangency portfolio returns
    together with the maximum loss, the condition Value-at-Risk and the
    upper CVaR.
    \cr   

    \code{summary}
    \cr
    is the S3 summary method for objects of class \code{"fPFOLIO"}. The 
    summary method prints and plots in one step the results as done by 
    the \code{print} and \code{plot} methods.
    
}


\references{

Elton E.J., Gruber M.J. (1991); 
    \emph{Modern Portfolio Theory and Investment Analysis}, 
    4th Edition, Wiley, NY, pp. 65--93.

Huang C.J., Litzenberger R.H. (1988); 
    \emph{Foundations for Financial Economics}, 
    Elsevier, NY, pp. 59--82.
    
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## SOURCE("fPortfolio.102C-MarkowitzPortfolio")

\dontrun{
## berndtInvest -
   data(berndtInvest)
   # Exclude Date, Market and Interest Rate columns from data frame,
   # then multiply by 100 for percentual returns ...
   berndtAssets = berndtInvest[, -c(1, 11, 18)]
   rownames(berndtAssets) = berndtInvest[, 1]
   head(berndtAssets)
   
## portfolioMarkowitz - 
   myPortfolio = portfolioMarkowitz(x = berndtAssets, targetReturn = 20/100/12)
   print(myPortfolio)
   
## pfolioTargetReturn -
## pfolioTargetRisk -
   pfolioTargetReturn(x = berndtAssets, weights = myPortfolio@pfolio$pw)
   pfolioTargetRisk(x = berndtAssets, weights = myPortfolio@pfolio$pw)
} 
}


\keyword{math}

