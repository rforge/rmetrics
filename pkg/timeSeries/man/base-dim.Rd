\name{dimnames}
  

\title{Time Series Columns and Rows}

\alias{dim,timeSeries-method}
\alias{dim<-,timeSeries-method}

\alias{dimnames,timeSeries-method}
\alias{dimnames<-,timeSeries,list-method}

\alias{colnames<-,timeSeries-method}
\alias{colnames,timeSeries-method}

\alias{rownames,timeSeries-method}
\alias{rownames<-,timeSeries,timeDate-method}
\alias{rownames<-,timeSeries,ANY-method}

\alias{names,timeSeries-method}
\alias{names<-,timeSeries-method}


\description{
    
    Handling columns and rows of 'timeSeries' objects. 
    
}


\details{

    \tabular{ll}{
    \code{dim} \tab Returns the dimension of a 'timeSeries' object \cr
    \code{dimnames} \tab Returns the dimension names of a 'timeSeries' object \cr
    \code{colnames<-} \tab Assigns column names to a 'timeSeries' object \cr
    \code{rownames<-} \tab Assigns row names to a 'timeSeries' object }

}


% \usage{

% %\S4method{dim}{timeSeries}(x)
% %\S4method{dimnames}{timeSeries}(x)
% %\S4method{dimnames}{timeSeries}(x) <- value

% dim(x)

% dimnames(x)
% dimnames(x) <- value

% colnames(x)
% colnames(x) <- value

% rownames(x)
% rownames(x) <- value

% \method{is.array}{timeSeries}(x)

% }


%\arguments{
%   
%    \item{value}{
%        a valid value for names component of \code{dimnames(x)}. 
%         For a \code{"timeSeries"} object this is either \code{NULL} or a 
%         character vector of length the column dimension. Not, row names
%         cannot be assigne for a \code{"timeSeries"} object, the function
%        \code{rownames()} will stop and return an error message.
%        }
%    \item{x}{
%        an object of class \code{timeSeries}.
%        }
%       
%}


\value{

    Returns the dimensions and related numbers of a 'timeSeries' object.
    
}


\examples{
## Load Swiss Pension Fund Benchmark Data -
   X <- LPP2005REC[1:10, 1:3]
   
## Get Dimension - 
   dim(X)
   
## Get Column and Row Names -
   dimnames(X)
   
## Get Column / Row Names -
   colnames(X)
   rownames(X) 
   
## Try your own DIM - 
   DIM <- function(x) {c(NROW(x), NCOL(x))}
   DIM(X) 
   DIM(X[, 1])

## Try length / LENGTH - 
   length(X)
   length(X[, 1])
   LENGTH <- function(X) NROW(X)
   LENGTH(X)
   
## Columns / Rows - 
   ncol(X); NCOL(X)
   nrow(X); NROW(X)
                
## See also - 
   isUnivariate(X)
   isMultivariate(X)
}


\keyword{chron}

