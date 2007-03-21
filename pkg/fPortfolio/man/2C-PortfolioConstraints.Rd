\name{PortfolioConstraints}


\alias{PortfolioConstraints}

\alias{setConstraints}
\alias{getConstraints}
\alias{getConstraints.constraintsMatrix}        


\title{Portfolio Constraints}


\description{
  
    A collection of functions, which help to set, 
    manipulate or view the constraints. The user 
    normally should not use this functions, since 
    constraints setting is done within optimization 
    program. The user must only provide constraints
    in the right manner. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{setConstraints} \tab Transforms constraint strings into a list value, \cr
    \code{getConstraints} \tab Transforms a constraint list value into strings.}
    
}    

\usage{
setConstraints(data, spec = portfolioSpec(), constraints = NULL) 
getConstraints(object)
}


\details{

    \bold{How to define constraints:}
    \cr\cr
    The definition of the constraints is done by strings or sting vectors,
    which must have the following form:
      \item{box constraints}{\code{"minW[asset]=percentage"},}
      \item{for sector constraints}{\code{"maxsumW[assets]=percentage"}.}
      \cr
    While the asset(s) "argument" can be either a number or a vector.

}


\arguments{

    \item{constraints}{
        a character string vector, containing the constraints. 
        }
    \item{data}{
        a list, having a statistics named list, having named entries 'mu' and
        'Sigma', containing the information of the statistics\cr
        }
    \item{object}{
        an object of class constraintsMatrix, containing the constraints matrix
        and the exposure vector.
        }
    \item{spec}{
        an object of class \code{fPORTFOLIOSPEC}, containing slots call, model,
        portfolio, title, description.\cr
    }

}


\value{
  
    \code{setConstraints} 
    \cr
    linear constraints can be written as \eqn{A x \ge b_0}. The function returns
    a matrix, which is the constraints matrix \eqn{A} except the last column
    containing the exposure vector \eqn{b_0}. The first two lines are set to 
    equations. They represent full investment constraint, i.e. all weights sum 
    up to one, and the expected return constraints, i.e. all mean weighted
    weights sum up to the desired return. The following lines represent user
    defined constraints. For the lower bound constraints all entries have a
    minus sign in order to change the inequality relation.
    \cr

    \code{getConstraints} 
    \cr
    returns the constraints from an object being the composition of constraints
    matrix \eqn{A} and exposure vector \eqn{b_0}. It has the same from output
    by function setConstraints. 
  
}


%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioExtractors}},
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## setConstraints -
   # Getting Data
   Data = dutchPortfolioData()
   # Setting Data in Right Manner
   Data = list(statistics = Data)
   # Getting Specifications
   Spec = portfolioSpec()
   # Setting Constraints Vector
   Constraints = c("minW[4] = 0.2", "maxW[3] = 0.4", "maxsumW[c(1,2,4)] = 0.9")
   constMatrix = setConstraints(Data, Spec, Constraints)
   constMatrix
   
## getConstraints -
   # Use Constraints Matrix from Example Above
   # demonstrations(constMatrix)
}


\keyword{models}

