\name{PortfolioData}

\alias{PortfolioData}

\alias{dutchPortfolioData}
\alias{usPortfolioData}
\alias{sm132PortfolioData}


\title{Example Portfolio Data Sets}


\description{
  
    A collection of example data from different sources. The data is either a
    time series or a named list with entries 'mu' and 'Sigma'.
    \cr
    
    The data sets are:
    
    \tabular{ll}{
    \code{dutchPortfolioData} \tab Means and covariance matrix from Dutch stock index AEX, \cr
    \code{usPortfolioData} \tab a time series of US Assets, \cr
    \code{sm132PortfolioData} \tab Means and covariance matrix for a pseudo portfolio
     from Scherer, Martin.
    }
    
}
    

\usage{
dutchPortfolioData() 
usPortfolioData()
sm132PortfolioData()
}


\details{

    \bold{Dutch Portfolio Data Set:}
    \cr\cr
    This data represents seven stocks from the Dutch AEX index, Netherlands blue
    chips. The data is a list of the covariance matrix and the return means and
    is based on daily returns over a period from January 1990 till end of
    October 2003. Companies representing the data are Elsevier, Fortis,
    Getronics, Heineken, Philips, Shell and Unilever.
    \cr 
    
    
    \bold{US Portfolio Data Set:}
    \cr\cr
    The data inherits eight assets being indexes, commodities and bonds.
    The data is a time series of yearly returns from December 1973 till 
    December 1994. Assets are TBills3m, LongBonds, SP500, Wilshire5000, 
    NASDAQComp, LehmanBonds, EAFE, Gold.
    \cr
    
    
    \bold{Scherer-Martin Data Set:}
    \cr\cr   
    This data is taken from chapter 1.3.2 in Scherer, M., Martin, R.D. (2005);
    \emph{Introduction To Modern Portfolio Optimization with NuOPT, S-PLUS and
    S+Bayes}, Springer, Berlin. It is a list of covariance matrix and the return
    means of imaginary assets. It is an example set for learning about 
    optimization.

}


\value{
  
    \code{dutchPortfolioData} 
    \cr
    returns a list of the covariance matrix and the return means.
    \cr

    \code{dutchPortfolioData} 
    \cr
    returns a time series of yearly returns.
    \cr
    
    \code{dutchPortfolioData} 
    \cr
    returns a list of covariance matrix and the return means. 
  
}


\references{
Scherer, M., Martin, R.D. (2005);\cr
    \emph{Introduction To Modern Portfolio Optimization with NuOPT,\cr
    S-PLUS and S+Bayes}, Springer, Berlin

}


\seealso{

   \code{\link{PortfolioSpecification}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioExtractors}},
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## usPortfolioData -
   usPortfolioData()
      
## dutchPortfolioData -
   x = dutchPortfolioData()
   x
}


\keyword{models}

