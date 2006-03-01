\name{ResampledPortfolio}

\alias{ResampledPortfolio}

\alias{frontierResampled}

\title{Resampled Portfolio Frontier}


\description{
  
    A collection and description of functions to 
    ...
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{frontierResampled} \tab ... . }
       
}
    

\usage{
frontierResampled(x = as.matrix(read.csv("nova-updated.csv", header = FALSE)),
    expectedReturns = read.csv("expected-returns.csv"), frequency = 52,
    type = list(nFrontier = 200, returnChoice = 2, covarianceChoice = 2,
    targetChoice = 6, correlationChoice = 1), confidenceLevel = TRUE, 
    trace = TRUE)
}


\arguments{
  
    \item{x}{
        any rectangular multivariate time series object which can be
        transformed into a matrix throught the function \code{as.matrix}. 
        } 
    \item{expectedReturns}{
        expectedReturns
        } 
    \item{frequency}{
        frequency of time series records. An integer value, by default
        52 which denotes weekly data records.
        }  
    \item{type}{
        a control list with the following entries: ...
        }
    \item{confidenceLevel}{
        a logical flag, if \code{TRUE} then 95% confidence levels are 
        returned, otherwise not. By default \code{TRUE}.
        }
    \item{trace}{
        a logical if \code{TRUE} then the portfolio resampling process
        will be ttraced, otherwise not. By default \code{TRUE}.
        }
       
}

\details{

    I attach the code and some data (list of weekly returns of several
    assets and list of possible expected returns; they have to be in the
    same folder as the R script.
    
    To get at least a glimpse at what is going on, one should be
    familiar with the book  "Efficient Asset Management" by R. Michaud 
    and the concept of resampled efficiency.
    
    Stripped to the bones, the basic idea is to Monte-Carlo simulate the
    returns of the assets in your portfolio (you sample a multivariate
    distribution which is consistent with the "true" covariance matrix C
    and the "true" vector of expected returns v you assume to describe
    your portfolio) and generate many efficient frontiers.
    
    Then one averages out of the simulated frontiers the weights of the
    "statistically equivalent" portfolios. By using them in conjunction
    with C and v one generates a resampled frontier which, compared to
    Markowitz classical treatment, is sub-optimal by definition but is
    numerically more robust (Markowitz is too sensitive to the hypothesis
    of the problem) and leads to more diversified portfolios which make
    investment sense.
    
    I exchanged some emails with Michaud discussing some technicalities of
    the method (of course up to a certain extent: there are two patents
    pending).
    
    A few issues need further thought, but that can be postponed for now.
    Beside this, the code implements some shrinkage techniques for the
    expected returns of the assets and the covariance matrix (Bayesian
    statistics with different priors, see the works by Olivier Ledoit &
    Michael Wolf for the formulas).
    
    Confidence levels coming from a re-interpretation of the research by
    Jobson & Korkie are implemented as in the book by Michaud.

}


\value{

    A list ...
    
}


\references{

Michaud, R.O. (1998); 
    \emph{Efficient Asset Management: A Practical Guide to Stock 
        Portfolio Optimization and Asset Allocation }, 
    President and Fallows of the Harvard College, USA.
    
}


\author{

    Lorenzo Isella for the R code,\cr
    Diethelm Wuertz for this Rmetrics port.
    
}


\examples{
## SOURCE("fPortfolio.102D-ResampledPortfolio")

\dontrun{
## file -
   frontierResampled()
} 

}


\keyword{math}

