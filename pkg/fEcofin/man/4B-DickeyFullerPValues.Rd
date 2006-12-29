\name{DickeyFullerPValues}

\alias{DickeyFullerPValues}

\alias{padf}
\alias{qadf}
\alias{adfTable}


\title{Dickey-Fuller p Values}


\description{
    
    A collection and description of functions 
    to compute the distribution and and quantile 
    function for the ADF unit root test statistics. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{padf} \tab the returns cumulative probability for the ADF test, \cr
    \code{qadf} \tab the returns quantiles for the ADF test, \cr
    \code{adfTable} \tab tables p values for ADF test. 
    . }
    
}
 

\usage{
padf(q, N = Inf, trend = c("nc", "c", "ct"), statistic = c("t", "n")) 
qadf(p, N = Inf, trend = c("nc", "c", "ct"), statistic = c("t", "n"))

adfTable(trend = c("nc", "c", "ct"), statistic = c("t", "n"), 
    includeInf = TRUE)
}


\arguments{ 

    \item{includeInf}{
        a logical flag. Should the asymptotic value included into
        the table?
        }
    \item{N}{ 
        the number of observations in the sample from which the 
        quantiles are to be computed.\cr
        }
    \item{p}{ 
        a numeric vector of probabilities. Missing values are 
        allowed. 
        }
    \item{q}{   
        vector of quantiles or test statistics. Missing values 
        are allowed. 
        }       
    \item{statistic}{ 
        a character string describing the type of test statistic. 
        Valid choices are \code{"t"} for t-statistic, and \code{"n"} 
        for normalized statistic, sometimes referred to as the 
        rho-statistic. The default is \code{"t"}. 
        }
    \item{trend}{
        a character string describing the regression from which the 
        quantiles are to be computed. Valid choices are: \code{"nc"} 
        for a regression with no intercept (constant) nor time trend, 
        and \code{"c"} for a regression with an intercept (constant) 
        but no time trend, \code{"ct"} for a regression with an intercept 
        (constant) and a time trend. The default is \code{"c"}. 
        }
}


\value{ 
    
    The function \code{padf} returns the cumulative probability of 
    the finite sample distribution of the unit root test statistics. 
    
    The function \code{qadf} returns the quantiles of the finite sample 
    distribution of the unit root test statistics, given the probabilities. 

}


\note{

    The functions \code{padf} and \code{qadf} use the tables from 
    A. Banerjee et al. (1993).
    
}


\author{
    
    Diethelm Wuertz for the Rmetrics \R-port.

}
    
    
\references{ 

Banerjee A., Dolado J.J., Galbraith J.W., Hendry D.F. (1993);
    \emph{Cointegration, Error Correction, and the Econometric 
        Analysis of Non-Stationary Data},
    Oxford University Press, Oxford. 
    
Dickey, D.A., Fuller, W.A. (1979);
    \emph{Distribution of the estimators for autoregressive time 
        series with a unit root}, 
    Journal of the American Statistical Association 74, 427--431. 

}


\examples{ 
## SOURCE("fEcofin.4B-DickeyFullerPValues")

   
## ADF dftesTable -
   adfTable()
}


\keyword{distribution}

