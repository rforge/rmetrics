\name{portfolioConstraints}


\alias{portfolioConstraints}


\title{Portfolio Constraints}


\description{
  
    Computes portfolio constraints given by a constraints string.
    
}    

\usage{
portfolioConstraints(data, spec = portfolioSpec(), constraints = "LongOnly", \dots)

setBoxConstraints(data, spec = portfolioSpec(), constraints = "LongOnly")
setGroupEqConstraints(data, spec = portfolioSpec(), constraints = "LongOnly")
setGroupMatConstraints(data, spec = portfolioSpec(), constraints = "LongOnly")
}


\arguments{

    \item{constraints}{
        a character value or character vector, containing the constraint
        strings. Setting constraints is described in the details section 
        }
    \item{data}{
        a list, having a statistics named list, having named entries 'mu' and
        'Sigma', containing the information of the statistics\cr
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{\dots}{
        arguments passed to the function \code{.setRdonlp2Constraints}.
        For internal use only.
        }

}


\details{

    \bold{How to define constraints?}
    \cr\cr
    Constraints are defined by a character string or a vector of character
    strings.
    
    
    \emph{Special Constraint Settings:}
    
    There are three special cases, the settings \code{constraints=NULL},
    \code{constraints="Short"}, and \code{constraints="LongOnly"}. Note,
    that these three constraint settings are not allowed to be combined 
    with more general constraint definitions.
    
    \code{NULL}: This selection defines the default value and is equivalent
    to the \code{"LongOnly"} case, see below.
    
    \code{"Short"}: This selection defines the case of unlimited short selling.
    i.e. each weight may range between \code{-Inf} and \code{Inf}. 
    Consequently, there are no group constraints. Risk budget constraints
    are not included in the portfolio optimization.
    
    \code{"LongOnly"}: This selection is the same as the default setting.
    Each weight may range between \code{0} ans \code{1}. No group
    constraints and risk budget constraints will be included in the 
    portfolio optimization.
    
    
    \emph{Tailored Weight Constrained Portfolios:}
    
    Weight constrained portfolios may be specified by a vector of
    character strings which describe executable code, setting values to
    to vectors \code{minW}, \code{maxW}, \code{minsumW}, and \code{maxsumW}.
    The individual string elements of the vector have the following form:
    
    \describe{
        \item{box constraints}{
            \code{"minW[Asset(s)]=Value(s)"}, and/or \cr
            \code{"maxW[Asset(s)]=Value(s)"}.
        }
        \item{sector constraints}{
            \code{"minsumW[Asset(s)]=Value(s)"}, and/or \cr
            \code{"maxsumW[Asset(s)]=Value(s)"}. 
        }
    }
    
    \code{Asset(s)} is an index of one or more assets, and \code{value} 
    a numeric value or vector assigning the desired value. Note, if the 
    \code{values} range between zero and one, then we have a long only 
    portfolio allowing for box and group constraints of the weights. If 
    the values are set to negative values, and values larger than one, 
    then (constrained) short selling will be allowed. 
    
    \emph{Tailored Risk Budget Constrained Portfolios:}
    
    By default, risk budgets are not included in the portfolio optimization.
    Covariance risk budgets have to be added explicitely, and have the 
    following form:
    
    \describe{
        \item{box constraints}{
            \code{"minB[Asset(s)]=Value(s)"}, and/or \cr
            \code{"minB[Asset(s)]=Value(s)"}.
        }
    }
    
    Again, \code{Asset(s)} is an index of one or more assets, and \code{value} 
    a numeric value or vector with numbers ranging between zero and one,
    assigning the desired risk budgets. 
    
    
    Note, risk budget constraints will enforce diversification at the 
    expense of return generation. The resulting portfolios will thus lie 
    below the unconstrained efficient frontier.

}


%\value{}


%\details{}


%\references{}


\examples{
## portfolioConstraints -
   # Load Data, i.e. a List of Returns:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## portfolioSpec -
   # Specification:
   Spec = portfolioSpec() 
   
## portfolioConstraints -
   # Portfolio Constraints:
   Constraints = c("minW[1:4] = 0.1", "maxsumW[c(\"GG\",\"GYMB\")] = 0.8")
   portfolioConstraints(Data, Spec, Constraints)
}


\keyword{models}

