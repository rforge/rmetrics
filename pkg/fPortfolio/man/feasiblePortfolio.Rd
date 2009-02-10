\name{feasiblePortfolio}


\alias{feasiblePortfolio}


\title{Feasible Portfolios}


\description{
  
    Returns properties of a feasible portfolio.
    
}
    

\usage{
feasiblePortfolio(data, spec = portfolioSpec(), constraints = "LongOnly")
}



\arguments{

    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }

}


\details{  
    
    A feasible portfolio is a portfolio with given weights which
    lies inside the feasible region of portfolios.
       
    The function requires three arguments: \code{data}, \code{spec} 
    (specifications), and \code{constraints}, see above. Be sure that
    the specification structure \code{"spec"} has defined a weights
    vector which is different from \code{"NULL"}.  To assign values
    to the weights in the specification structure, use the function 
    \code{setWeights}. 
    
    The \code{feasiblePortfolio} function returns the properties of 
    the feasible portfolio as an S4 object of class \code{fPORTFOLIO}.

}


\value{
    
    \code{feasiblePortfolio} function returns an S4 object of class 
    \code{"fPORTFOLIO"}.
    
}



\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## data -
   Data = SMALLCAP.RET
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## spec - 
   Spec = portfolioSpec()
   setWeights(Spec) = rep(0.25, times = 4)
   Spec
   
## constraints -
   Constraints = "LongOnly"
   Constraints
   
## feasiblePortfolio - 
   feasiblePortfolio(Data, Spec, Constraints)
}


\keyword{models}

