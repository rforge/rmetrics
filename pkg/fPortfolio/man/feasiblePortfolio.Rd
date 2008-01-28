\name{feasiblePortfolio}


\alias{feasiblePortfolio}


\title{Portfolio Class}


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
        an S4 object of class \code{fPFOLIOSPEC}, containing slots call, model,
        portfolio, title, description, see \code{\link{PortfolioSpec}}
        for a full slot description.
        }

}


\details{
    
    \bold{A Feasible Portfolio:}
    \cr\cr   
    
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
    \code{"fPORTFOLIO"}, with the following slots:

    \item{@call}{
        a call, returning the matched function call.
        }
    \item{@data}{
        a list with two named elements, \code{series} holding the time series
        data if available, otherwise NA, and \code{statistics}, itself a
        named list with two named elements \code{mu} and \code{Sigma}
        holding the vector of means and the matrix of covariances.
        }  
    \item{@description}{
        a character string, allowing for a brief project description.
        }        
    \item{@portfolio}{
        a list, containing parameter specifications for the portfolio:\cr 
        \code{weights} a numeric vector specifying the portfolio 
            weights,\cr
        \code{targetReturn} a numeric value specifying the target 
            return,\cr
        \code{targetRisk} a numeric value specifying the target 
            risk,\cr  
        \code{targetMean} a numeric value specifying the target 
            return determinated with function mean(),\cr
        \code{targetStdev} a numeric value specifying the target risk in 
            standart deviation as risk measure.
        }
    \item{@specification}{
        a list with one named element \code{spec} which represents an object 
        of class \code{fPFOLIOSPEC}, including all information about
        the portfolio specifications, see \code{\link{PortfolioSpec}}
        for further details.
        }
    \item{@title}{
        a title string.
        }
    
}



%\references{}


\seealso{

   \code{portfolioData},
   \code{portfolioSpec},
   \code{portfolioConstraints},
   \code{efficientPortfolio},
   \code{frontierPortfolio}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## data -
   # Load Data and Convert to timeSeries Object:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## portfolioSpec - 
   # Set Default Specifications:
   Spec = portfolioSpec()
   setWeights(Spec) = rep(0.25, times = 4)
   Spec
   
## Allow for unlimited Short Selling:
   Constraints = "LongOnly"
   
## Compute properties of Efficient Portfolio
   feasiblePortfolio(Data, Spec, Constraints)
}


\keyword{models}

