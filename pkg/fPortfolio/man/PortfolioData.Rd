\name{PortfolioData}

\alias{PortfolioData}

\alias{fPFOLIODATA}
\alias{fPFOLIODATA-class}

\alias{show.fPFOLIODATA}
\alias{show,fPFOLIODATA-method}

\alias{portfolioData}

\alias{portfolioStatistics}


\title{Portfolio Data Handling}


\description{
  
    A collection and description of functions to 
    handle portfolio data. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioStatistics} \tab Estimates 'mu' and 'Sigma' statistics, \cr
    \code{print|show} \tab Print method for \code{fPFOLIOSPEC} objects. }
    
}
    

\usage{
portfolioData(data, spec = portfolioSpec())

portfolioStatistics(data, spec = portfolioSpec())

show.fPFOLIODATA(object)
}


\arguments{

    \item{data}{
        [portfolioStatistics] - \cr
        a time series or a named list, containing either a series of returns or
        named entries 'mu' and 'Sigma' being mean and covariance matrix.
        }
    \item{object}{
        [show] - \cr
        an object of class \code{fPFOLIODATA} as returned by the function
        \code{portfolioData}.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, the specification to be 
        modified, by default the default of the function \code{portfolioSpec()}.
        }

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
    
    
    \bold{Simulated Mean-Cov Data Set:}
    \cr\cr   
    This data is taken from chapter 1.3.2 in Scherer, M., Martin, R.D. (2005);
    \emph{Introduction To Modern Portfolio Optimization with NuOPT, S-PLUS and
    S+Bayes}, Springer, Berlin. It is a list of covariance matrix and the return
    means of imaginary assets. It is an example set for learning about 
    optimization.
    \cr
    
    
    \bold{World Index Returns Data Set:}
    \cr\cr   
    This data set is contributed by D. Locher (2007);
    It is a timeSeries object of four world
    index return data sets including Asia, Eastern Europe, Far East and 
    Latin America.

}


\value{
  
    \code{portfolioStatistics}\cr
    returns a named list of estimated mean \code{$mu} and covariance 
    \code{$Sigma} statistics, from a multivariate time series of assets.
    \cr
    
    \code{portfolioData}\cr
    returns a named list of the time series \code{$series} and the 
    portfolio \code{$statistics} as returned by the function
    \code{portfolioStatistics}.
  
}


%\references{}


\seealso{

   \code{\link{PortfolioSpec}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## ...
}


\keyword{models}

