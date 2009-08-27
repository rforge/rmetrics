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
    
    Functions and methods handling columns and rows of 
    'timeSeries' objects. 
    \cr
    
    \tabular{ll}{
    \code{dim} \tab Returns the dimension of a 'timeSeries' object, \cr
    \code{dimnames} \tab Returns the dimension names of a 'timeSeries' object, \cr
    \code{colnames<-} \tab Assigns column names to a 'timeSeries' object, \cr
    \code{rownames<-} \tab Assigns row names to a 'timeSeries' object, \cr
    %    \code{is.array} \tab Allows that NCOL and NROW work properly.
  }
    
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


\arguments{
   
    \item{value}{
        a valid value for names component of \code{dimnames(x)}. 
%         For a \code{"timeSeries"} object this is either \code{NULL} or a 
%         character vector of length the column dimension. Not, row names
%         cannot be assigne for a \code{"timeSeries"} object, the function
%        \code{rownames()} will stop and return an error message.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        }
       
}


\value{

    NA
    
}


\examples{
## Load Swiss Pension Fund Benchmark Data -
   data(LPP2005REC)
   X = LPP2005REC[1:10, 1:3]
   
## Get Dimension - 
   dim(X)
   
## Get Column and Row Names -
   dimnames(X)
   
## Get Column Names -
   colnames(X)
   
## Get Row Names -
   rownames(X) 
}


\keyword{chron}

