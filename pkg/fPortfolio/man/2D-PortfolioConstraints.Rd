\name{PortfolioConstraints}


\alias{PortfolioConstraints}

\alias{portfolioConstraints}


\title{Portfolio Constraints}


\description{
  
    A collection and description of functions, which 
    allow to set, manipulate and/or view portfolio 
    constraints. Type of constraints include box/group 
    constraints of weights, and covariance risk-budget 
    constraints.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioConstraints} \tab Checks portfolio constraints for consistency. }
    
}    

\usage{
portfolioConstraints(data, spec, constraints)
}


\arguments{

    \item{constraints}{
        [setConstraints] - \cr
        a character value or character vector, containing the constraint
        strings. Setting constraints is described in the details section 
        }
    \item{data}{
        [setConstraints] - \cr
        a list, having a statistics named list, having named entries 'mu' and
        'Sigma', containing the information of the statistics\cr
        }
    %\item{object}{
    %    [getConstraints] - \cr
    %    an object of class constraintsMatrix, containing the constraints matrix
    %    and the exposure vector.
    %    }
    \item{spec}{
        [setConstraints] - \cr
        an object of class \code{fPFOLIOSPEC}, containing slots call, model,
        portfolio, title, description.
        }
    %\item{type}{
    %    [setConstraints] - \cr
    %    a character string denoting the type of constraints to set.
    %    }
    %\item{x}{
    %    [print] - \cr
    %    an pbject of class \code{constraintsMatrix} as returned by the 
    %    function \code{getConstraints}.
    %    }
    %\item{\dots}{
    %    [print] - \cr
    %    optional arguments passed to the \code{print} function.
    %    }

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

}


%\value{
%
%}


%\details{
%
%}


%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioExtractors}},\cr
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## portfolioConstraints -
   #
}


\keyword{models}

