\name{ResampledPortfolio}

\alias{ResampledPortfolio}

\alias{frontierResampled}

\title{Resampled Portfolio}


\description{
  
    A collection and description of functions to 
    ...
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{.portfolioResampled} \tab ... . }
       
}
    

\usage{
frontierResampled(trace = TRUE)
}


\arguments{
  
    \item{trace}{
        trace
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

    none
    
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
## SOURCE("fPortfolio.B2-ResampledPortfolio")

\dontrun{
## file -
} 
}


\keyword{math}

