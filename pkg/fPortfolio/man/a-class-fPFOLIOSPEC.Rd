\name{fPFOLIOSPEC-class}


\alias{fPFOLIOSPEC}
\alias{class-fPFOLIOSPEC}

\alias{fPFOLIOSPEC-class}

\alias{show,fPFOLIOSPEC-method}


\title{Specification of Portfolios}


\description{
  
    Specifies portfolios.
    
}


\usage{       
\S4method{show}{fPFOLIOSPEC}(object)
}


\arguments{

    \item{object}{
        an S4 object of class \code{fPFOLIOSPEC}.
        }

}       


\details{

    \bold{Portfolio Specifcation Structure:}
    \cr\cr
    
    The S4 class \code{fPFOLIOSPEC} specifies the portfolio. The slots 
    are:\cr
    
    \describe{
        \item{@call}{
            a call, returning the matched function call.
            }
        \item{@model}{
            a list, setting the \code{type} of portfolio to be optimized, 
            and the mean/covariance \code{estimator} to be applied:
            \cr
            \code{type=c("MV","CVaR")} a character string denoting the type of
                portfolio, the implemented types are the
                Mean-Variance Markowitz Portfolio, \code{"MV"}, and the
                Mean-CVaR Portfolio, \code{"CVaR"}.
                \cr
            \code{estimator=c("mean","cov")} a vector of two character
                strings, the first denoting the mean estimator, and the 
                second the covariance estimator. Additional meaningful 
                selections include robust covariance estimators, e.g. 
                \code{c("mean","mcd")}, or \code{c("mean","shrink")}.
                \cr
            \code{tailRisk=list()} a list of optional tail risk information,
                currently not used.\cr
            \code{params=list()} a list of optional model parameters,
                currently not used.
            }     
        \item{@portfolio}{
            a list, settings portfolio parameters including predefined
            weights, target return, risk free rate, number of frontier
            points:
            \cr 
            \code{weights=NULL} a numeric vector specifying the portfolio 
                weights.
                \cr
            \code{targetReturn=NULL} a numeric value specifying the target 
                return. The default value sets the target return.
                \cr
            \code{targetRisk=NULL} a numeric value specifying the target 
                risk.\cr
            \code{targetAlpha=NULL} a numeric value specifying the target 
                alpha confidence level for CVaR portfolio optimization. 
                The default value sets the target return.
                \cr
            \code{riskFreeRate=0} a numeric value specifying the risk free 
                rate.
                \cr  
            \code{nFrontierPoints=50} a numeric value determining the number
                of points on the efficient frontier.
            }
        \item{@solver}{
            a list, setting the type of solver to be used for portfolio
            optimization:
            \cr 
            \code{type=c("quadprog", "Rdonlp2", "lpSolve")} a character
                string specifying the name of the solver to be used.\cr
            \code{trace=FALSE} a logical flag, should the optimization be
                traced?
            }
        \item{@title}{
            a title string, with a default project title. 
            }
        \item{@description}{
            a character string, with a default project description.
            }
    }
 
}


\value{
  
    \code{portfolioSpec}\cr
    \cr
    returns an S4 object of class \code{"fPFOLIOSPEC"}.

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## spec -
   # Show Default Portfolio Specifications:
   Spec = portfolioSpec()
   
## setRiskFreeRate -
   # Change Risk Free Rate
   setRiskFreeRate(Spec) = 3
   Spec
}


\keyword{models}

